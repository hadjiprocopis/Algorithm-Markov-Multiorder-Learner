package Algorithm::Markov::Multiorder::Learner;

use 5.006;
use strict;
use warnings;

our $VERSION = '0.1';

use Exporter qw/import/;
our @EXPORT = qw/learn predict range save_state load_state/;

use Storable;
use Data::Roundtrip qw/perl2dump no-unicode-escape-permanently/;

# * reads in some data (from file or string),
# * splits it to symbols according to separator
#   or it works character-by-character if the separator is left undef,
# * it eventually will have symbols.
# These symbols can be words in text (if separator = word boundary='\b')
# Or they can be letters in text (if separator is left undefined)
# (in this case you must arrange that the input has each word in a line on its own)
# Symbols can are then put together to form "ngrams" and the frequency of
# each ngram is counted.
# Input parameter 'ngram-length' refers to the number of symbols in an ngram.
# For example, with an ngram-length=2 and a word-boundary separator
# we get
#   'hello','there' => 5
#   'hello','you' => 10
#   'goodbye','all' => 5
# etc.
# the numbers are the counts for each ngram as read from the input data.
# Input parameter 'counts' can be used to supply such frequency data
# without re-reading huge corpus each time. The param must be a hashref
# where key=ngrams (separated by anything you like, no quotes necessary
# but use some separator in order not to confuse composite words)
# With that frequency data, we proceed to calculate a probability
# distribution ('dist') which is just normalised frequencies.
# Then we build the cumulative probability distribution but with a twist:
# the ngrams are broken into (n-1)grams and 1grams (from left to right for latin)
# The first ngram is the key. For all possible endings of that (i.e.
# all the 1grams with which it ends) we count frequencies, normalise to
# probabilities and then build the cumulative probability.
# For example consider the following 'counts'
# (let's assume we are dealing with letters, ngram-length=3):
#    HEL => 10
#    HEA => 20
#    HEN => 5
#    ALA => 10
#    ALB => 20
# The cumulative-probability-distribution will be something like this:
#    HE => {
#            L => 10/(10+20+5)
#            A => 10/(10+20+5) + 20(10+20+5)
#            N => 10/(10+20+5) + 20/(10+20+5) + 5/(10+20+5)
#    }
#    AL # left as an exercise for the reader
# The sub returns a hashref made of these three hashrefs
# 'counts' => frequency(=counts) of each ngram as it occured in the input data
# 'dist' => probability distribution (i.e. normalised frequency/counts) of the above
# 'twisted-dist' => as explained before.
# 'cum-twisted-dist' => as explained before.
# Refer to the 'predict()' sub for how the 'cum-twisted-dist' hash is used to make a prediction.

sub	learn {
	my $params = $_[0] // {};

	my $parent = ( caller(1) )[3] || "N/A";
	my $whoami = ( caller(0) )[3];

	my $ngram_length = $params->{'ngram-length'};
	# optional separator, can be left undef in which case we work letter-by-letter
	# if defined it has the form of a regex, e.g. '.' or '\b'
	my $separator = $params->{'separator'};
	my $internal_separator = defined $separator ? $params->{'internal-separator'} // '\|/' : '';
	# optional debug parameter
	my $DEBUG = $params->{'debug'};
	# optionally specify here 'counts' or 'twisted-dist'
	# in order to return them
	my $need = $params->{'need-to-return'};
	# optionally specify here what not to return
	my $avoid = $params->{'avoid-to-return'};
	my $remove_these_characters_from_input = $params->{'remove-these-characters-regex'};
	my $process_line_subref = $params->{'process-line-subref'};
	my $filter_line_subref = $params->{'filter-line-subref'};
	my $process_whole_text_subref = $params->{'process-whole-text-subref'};
	my $process_paragraphs = $params->{'process-paragraphs'};

	if( ! defined($process_line_subref) ){ $process_line_subref = \&_default_process_line }
	if( ! defined($filter_line_subref) ){ $filter_line_subref = \&_default_filter_line }

	if( ! defined($ngram_length) ){ $ngram_length = 1 }
	else { if( $ngram_length < 1 ){ print STDERR "learn() : ngram-length must be a positive integer.\n"; return undef } }
	if( ! defined($DEBUG) ){ $DEBUG = 0 }
	if( ! defined($need) ){ $need = {} }
	if( ! defined($avoid) ){ $avoid = {} }

	# counts the occurence of each combination of symbols of length 'ngram_length'
	# if specified in params, data will be appended and distribution
	# re-calculated with old and new data:
	my $counts = undef;

	# process input and eventually produce symbols
	if( defined($counts=$params->{'counts'}) ){
		my ($anykey) = keys %$counts;
		my $le = length($anykey);
		if( $le != $ngram_length ){ print STDERR "$whoami (via $parent) : input counts data was for ngrams-length of $le and not $ngram_length which you requested.\n"; return undef }
	} else { $counts = {} } # <<< fresh counts

	my ($infile, $instring, $howmany, $m);
	my $remove_chars_regex = 
		defined($remove_these_characters_from_input)
		?
			ref($remove_these_characters_from_input) eq 'Regexp'
			?
				$remove_these_characters_from_input # it's a compiled regex already
			:
				qr/${remove_these_characters_from_input}/
		: undef
	;
	if( defined($infile=$params->{'input-filename'}) ){
		$howmany = 0;
		$counts = {};
		my $FH;
		if( ! open $FH, '<', $infile ){ print STDERR "$whoami (via $parent) : could not open file '$infile' for reading, $!"; return undef }
		if( $process_paragraphs or $process_whole_text_subref ){
			# we need to slurp input file in order to process whole text!
			my $slurped; { local undef $/; $slurped = <$FH> } close($FH);
			# do we need to split on paragraphs?
			_convert_text_to_paragraphs(\$slurped) if $process_paragraphs;
			$process_whole_text_subref->(\$slurped) if $process_whole_text_subref;
			foreach my $line (split /^/, $slurped){
				$line =~ s/${remove_chars_regex}/ /g if defined $remove_chars_regex;
				next unless $filter_line_subref->(\$line);
				$howmany += $process_line_subref->(\$line, $counts, $ngram_length, $separator, $internal_separator)
			}
		} else {
			while( my $line = <$FH> ){
				$line =~ s/${remove_chars_regex}/ /g if defined $remove_chars_regex;
				next unless $filter_line_subref->(\$line);
				$howmany += $process_line_subref->(\$line, $counts, $ngram_length, $separator, $internal_separator)
			}
			close($FH);
		}
		if( $DEBUG ){ print "$whoami (via $parent) : file read OK '$infile': $howmany items read.\n" }
	} elsif( defined($instring=$params->{'input-string'}) ){
		$howmany = 0;
		$counts = {};
		if( process_whole_text($instring) ){
			foreach my $line (split /^/, $instring){
				$line =~ s/${remove_chars_regex}/ /g if defined $remove_chars_regex;
				next unless $filter_line_subref->(\$line);
				$howmany += $process_line_subref->(\$line, $counts, $ngram_length, $separator, $internal_separator)
			}
		}
		if( $DEBUG ){ print "$whoami (via $parent) : string read OK: $howmany items read.\n" }
	} elsif( defined($m=$params->{'input-array-ngrams'}) ){
		$counts = {};
		$howmany = scalar(@$m) - $ngram_length + 1;
		my ($ngram, $i);
		for($i=0;$i<$howmany;$i++){
			$ngram = join($internal_separator, @{$m}[$i..($i+$ngram_length)]);
			$counts->{$ngram}++;
		}
		if( $DEBUG ){ print "$whoami (via $parent) : array-ngrams read OK: $howmany items read.\n" }
	} elsif( defined($m=$params->{'input-array-symbols'}) ){
		$counts = {};
		$howmany = scalar(@$m);
		my $i;
		for($i=0;$i<$howmany;$i++){
			$counts->{$m->[$i]}++;
		}
		if( $DEBUG ){ print "$whoami (via $parent) : array-symbols read OK: $howmany items read.\n" }
	} elsif( ! defined($counts=$params->{'counts'}) ){ print STDERR "$whoami (via $parent) : either 'input-filename', 'input-string' or 'input-array-symbols' or 'input-array-ngrams' or 'counts' (which is a hashref of symbol counts) must be specified.\n"; return undef }

	# at this stage we have a hash (called counts) of 
	#    symbol => frequency(or counts, i.e. how many times it occured)
	#

	# create normalised counts = probability distribution (into %dist)
	my %dist = %$counts;
	my $sum = 0; $sum+=$_ for values %dist;
	$dist{$_} /= $sum for keys %dist;
	if( $DEBUG ){ print "$whoami (via $parent) : normalised counts OK:\n".perl2dump(\%dist)."\n" }

	my ($asy, $lasy, $c, $i, $l, $arr);
	# make a cumulative distribution but break it into 2 parts
	# the last part is the last symbol and the first part are the rest
	# we do a lookup by using the first part so that we have all the available
	# choices for what may follow that first part as cum-prob-dist
	my %twisted_dist = ();
	foreach $asy (keys %$counts){
		$c = $counts->{$asy};
		# internal_separator must be escaped (\Q)...
		my @lasys = split /\Q${internal_separator}/, $asy;
		$lasy = pop @lasys;
		$asy = join $internal_separator, @lasys;
		# ngram-length == 1 then ...
		if( $asy eq '' ){ $asy = '<empty>' }
		if( ! exists $twisted_dist{$asy} ){ $twisted_dist{$asy} = [] }
		push @{$twisted_dist{$asy}}, ($lasy, $c);
	}
	# normalise it, remember: it's an array of 'symbol','count','symbol','count', ...
	foreach $asy (keys %twisted_dist){
		$arr = $twisted_dist{$asy};
		$l = scalar(@$arr);
		$sum = 0; for($i=1;$i<$l;$i+=2){ $sum += $arr->[$i] }
		for($i=1;$i<$l;$i+=2){ $arr->[$i] /= $sum }
	}
	if( $DEBUG ){ print "$whoami (via $parent) : made twisted distribution OK:\n".perl2dump(\%twisted_dist)."\n" }

	my $cum_twisted_dist = undef;
	if( $need->{'all'} or $need->{'twisted-dist'} ){
		$cum_twisted_dist = {};
		foreach $asy (keys %twisted_dist){
			$cum_twisted_dist->{$asy} = [@{$twisted_dist{$asy}}]
		}
	} else { $cum_twisted_dist = \%twisted_dist }

	# do the cumulative thing:
	foreach $asy (keys %$cum_twisted_dist){
		$arr = $cum_twisted_dist->{$asy};
		$l = scalar(@$arr);
		for($i=3;$i<$l;$i+=2){ $arr->[$i] += $arr->[$i-2] }
		# make last item 1 because of rounding errors might be .9999
		$arr->[-1] = 1.0;
	}
	if( $DEBUG ){ print "$whoami (via $parent) : made cumulative twisted distribution OK:\n".perl2dump($cum_twisted_dist)."\n" }

	my %ret = (
		'cum-twisted-dist' => $cum_twisted_dist,
		'dist' => \%dist,
		'N' => $ngram_length
	);
	if( $need->{'all'} or $need->{'twisted-dist'} ){ $ret{'twisted-dist'} = \%twisted_dist }
	if( ! $avoid->{'counts'} ){ $ret{'counts'} = $counts }

	return \%ret
}
# return the next in sequence weighted on probabilities
# and given the previous symbols (inp)
# 'dat' must be built using learn(), it is the 'cum-twisted-dist' entry (in the returned hash)
# returns undef on failure.
# 
# The input is a 'cum-twisted-dist' hashref (as described in 'learn()')
# Assuming that we processed our data with ngram-length=3,
# then this function will take as input the first 2 symbols
# and will predict the 3rd based on the probability distribution
# built from the input data during the 'learn()' stage.
# For example, if built with ngram-length=3 the distribution of
# triplets of bases of the human genome (A,T,C,G), then
# during the 'learn()' stage we would have build the following data
# 'counts', e.g. ATC => 1000
# 'dist', e.g. ATC => 1000/(total number of triplets) # << which is the normalised frequency
# 'cum-twisted-dist', with a twist, e.g.
# AT => {C=>0.2, G=>0.3, A=>0.8, T=>1.0} # << Cumulative probabilities, always sum to 1
# This last 'cum-twisted-dist' is our 'dat' input parameter.
# Then we can ask the system to predict the third base
# given the first two bases, 'AT'.
# The prediction will be random but will be based (weightet) on the cum-twisted-dist.
# In our example, predict(AT) will yield an A most of the times, followed by
# equally-probable C and T and then G (1 in 10).
sub	predict {
	my $dat = $_[0]; # the hashref with the cum-prob distribution ('cum-twisted-dist')
	my $inp = $_[1]; # must be a string containing symbols separated by the internal separator, e.g. ATGC or I|want|to

	my $c = $dat->{$inp};
	if( ! defined $c ){ return undef } 
	my $l = scalar(@$c);
	my $r = rand;
	my $i;
	for($i=1;$i<$l;$i+=2){
		if( $r < $c->[$i] ){ return $c->[$i-1] }
	}
	return undef
}
sub	save_state {
	my $dat = $_[0];
	my $outfile = $_[1];
	if( ! Storable::store($dat, $_[1]) ){ print STDERR 'save_state()'.": call to ".'Storable::store()'." has failed for output file '$outfile'.\n"; return 0 }
	return 1
}
sub	load_state {
	my $infile = $_[0];
	my $ret = Storable::retrieve($infile);
	if( ! defined($ret) ){ print STDERR 'load_state()'.": call to ".'Storable::retrieve()'." has failed for input file '$infile'.\n"; return 0 }
	return $ret
}

# Same as the 'predict()' but it does not make a prediction.
# It returns all the possible options the (n-1)grams input
# has as a hashref of cumulative probabilities.
# will return undef if input has never been seen and does
# not exist in 'dat' (which is a 'cum-twisted-dist',see 'learn()')
sub	range {
	#my $dat = $_[0]; # the hashref with the cum-prob distribution ('cum-twisted-dist')
	#my $inp = $_[1]; # must be a string of length = ngram_length
	#return $dat->{$inp};
	return $_[0]->{$_[1]}
}

# private subs
# processes a line of text which consists of ngram_length words separated by optional separator
# By specifying a separator we can process sentences where words are our basic symbols. In this
# way we can have arbitrary-length symbols.
# On the other hand, the separator can be left undef. In this case we treat a line as character-by-character
# and each symbol consists of exactly ngram_length letters.
# it returns the number of symbols found in this line (>=0)
# It assumes that a line contains only complete words, no half word can end the line.
sub	_default_process_line {
	my $line = ${$_[0]}; # expecting a scalar ref to the line to process
	my $output_ref = $_[1];
	my $ngram_length = $_[2];
	my $sep = $_[3]; # optional, can be left undefined for no separator and work letter-by-letter
	my $intsep = $_[4]; # optional and only if sep is defined, it sets the internal separator between symbols
	# optional subref to filter the input line inplace, so its only param is the $line (which is a REF already)
	# it must return 0 if we should not proceed with splitting the line (i.e. for when the filter removed all
	# items from $line, nothing to split!)
	my $filter_sub_ref = $_[5]; 

	if( defined($filter_sub_ref) ){
		# a filter sub-ref was specified, so use it to clean the line prior to splitting
		# if it returns 0 then it means nothing left in line after filtering, no need to proceed then
		return 0 if $filter_sub_ref->($line)==0
	}
#	} else {
#		$line =~ s/>.*$//;
#		$line =~ s/\s+/ /g; $line =~ s/\s+$//;
#		return 0 if $line =~ /^\s*$/; # nothing to do
#	}

	my ($howmany, $i);
	my $sepregex = qr/${sep}/;

	if( defined $sep ){
		# we have a separator, so we need to regex
		$howmany = 0;
		my $temp;
		#print "processing line '$line'\n";
		# see johngg's comment at https://perlmonks.org/?node_id=11101277
		# other methods there can be faster but this is simple
		my @items = split $sepregex, $line;
		my $N = -$ngram_length+scalar @items;
		for($i=0;$i<=$N;$i++){
			#print "i=$i, N=$N, tex='".join($intsep, @items[$i..$i+$ngram_length-1])."'\n";
			$output_ref->{join $intsep, @items[$i..$i+$ngram_length-1]}++;
			$howmany++;
		}
	} else {
		$howmany = length($line)-$ngram_length+1;
		for(my $from=$howmany;$from-->0;){
			$output_ref->{substr $line, $from, $ngram_length}++;
		}
	}
	return $howmany;
}
sub _default_filter_line {
	# default line-filter sub, removes dup spaces and beg/end-of-line spaces
	# expecting scalar ref of the line so we can modify in-place
	#print "filtering line '${$_[0]}' => ";
	${$_[0]} =~ s/\s+/ /g;
	${$_[0]} =~ s/\s+$//;
	${$_[0]} =~ s/^\s//;
	#print "'${$_[0]}'\n";
	return 0 if ${$_[0]} =~ /^\s*$/; # nothing to do, empty, skip
	return 1 # ok processed and modified in-place, means line is not empty
}
sub _convert_text_to_paragraphs {
	# process the whole text, for example if you want to process in paragraphs
	# then replace newlines with spaces and double newlines(=paragraph) with newline
	# because our processing is line-based
	# expecting scalar ref of the line so we can modify in-place
	${$_[0]} =~ s/\n{2,}/<<<;par;par;>>>/g;
	${$_[0]} =~ s/\n/ /g;
	${$_[0]} =~ s/<<<;par;par;>>>/\n/g;
	return 0 if ${$_[0]} =~ /^\s*$/; # nothing to do, empty, skip
	return 1 # ok processed and modified in-place, means line is not empty
}
1;
# POD begins

=head1 NAME

Algorithm::Markov::Multiorder::Learner - Learn a corpus and predict using a multi-order (multi-dimensional)  Markov learner

=head1 VERSION

Version 0.01


=head1 SYNOPSIS

The idea has been explored many times in the past read a corpus, originally text but we
have expanded it to images and DNA sequences, build a Markov Chain by
counting how many times a word is followed / preceded by another and then predict what word
follows a given word. It has also been extended to cover an order N E<gt> 1 meaning Ngrams.
The Markov process (named after greatest Mathematician Andrey Markov who was born Russian and died Soviet)
is a process which holds the Markov property which means that a system's state can be predicted
based only on current state. One observes the system's behaviour and builds a matrix of transition
probabilities describing the transition from S(t) -> S(t+1). Such a matrix can be built
by first building a 2D histogram and then normalising over the space of events.

For predicting text, we still use the term Markov but we are using a sequence of length N,
hence the term Ngram.

This module takes some input data and N and calculates the transition probabilities (matrix) between
observed states. This is the learning phase. The transition matrix is called the "state"
and can be serialised to disk. During the predict phase, a "state" is read and spews out symbols
given a sequence of N-1 symbols according to the transition probabilities in the "state".

What this module does not do very well is splitting a text into words. It is bearable but
can be improved by, for example, employing another module to convert a "text" to words.
The user can specify their own word-boundaries in the form of regexes. And also ask
to learn text in paragraph-by-paragraph mode as opposed to line-by-line which is the default.

Here is something to get you started:

    use Algorithm::Markov::Multiorder::Learner;
    my $state = learn({
        'ngram-length' => 8,
        'separator' => qq{[-%!\:,;+*\\s\\t.]+},
        # filter all characters except a-zA-Z
        #'remove-these-characters-regex' => qr/[^a-zA-Z]/
        'input-filename' => 'shelley-frankestein.txt'
        # paragraph-by-paragraph mode, default is line-by-line
        #'process-paragraphs' => 1
    });
    die "learn()" unless $state;
    save_state($state, "output.state");

There are also scripts to analyse text, DNA or images in the "bin" directory.

=head1 EXPORT

Exported subs:

=over 3

=item *  C<learn()>

=item *  C<predict()>

=item *  C<save_state()>

=item *  C<load_state()>

=item *  C<range()>

=back

Also see the included scripts:

=over 3

=item * analyse_DNA_sequence.pl

=item * analyse_image.pl

=item * analyse_text.pl

=item * predict_image.pl

=item * predict_text.pl

=item * read_state.pl

=back

=head1 SUBROUTINES/METHODS

=head2 learn()

C<learn()> reads in some raw data from either a
file (given C<input-filename> parameter)
or a scalar/string reference (give C<input-string>).
It then proceeds to split I<raw input> to I<symbols>
according to separator (given C<input-separator>).
If no I<separator> is specified, input is split on
characters (exactly like C<split //, "hello">). This
is useful if you want to learn for example a sequence numbers < 10, a DNA
sequence, etc. If you want to learn words from a text then the
C<input-separator> must be C<'\s'>, and so on. The split logic
is pretty basic and may prove substandard for complex input.
In this case just you have several options:

=over 3

=item C<input-array-symbols> : a ref to an array containing the symbols, no splitting and parsing.

=item C<input-array-ngrams> : a ref to an array of arrays of I<ngrams>, i.e. a collection of N symbols, where N-1 is the order of the Markov Chain we are trying to build.

=item C<process-line-subref> : a ref to a sub which process each line, its prototype is C<($lineref, $counts_hash_ref, $ngram_length, $separator, $internal_separator, $filter_sub_ref)>

=back

Related to the C<process-line-subref> parameter is C<filter-line-subref>
which pre-process the line, e.g. to remove duplicate spaces. Or in
the case of DNA sequence/fasta files to remove '>' characters. Or
in the case of text and literature, to remove punctuation maybe
or expand/remove abbreviations etc. Its return code will decide
whether to proceed with processing this specific line or skip (if for example
becomes empty after filtering). The default filter is:
    sub {
        my $line = ${$_[0]}; # expecting scalar ref of the line
        $line =~ s/\s+/ /g;
        $line =~ s/\s+$//;
        $line =~ s/^\s//;
        return 0 if $line =~ /^\s*$/; # nothing to do
        return 1
    }

The order of the Markov Chain or the number of symbols to look at
in determining the next state is specified by C<ngram-length>-1. It
must be a positive integer. Setting it to 1 makes the order 0 which
it does not exist but we use it so that we get at least word frequency
out.

Internally the symbols are joined using the C<internal-separator>
which is some complex string like '\|/', but one can change it
if this string interferes with input symbols.

The parameter C<remove-these-characters-regex> is either a compiled
regex (C<qr//> allows use of switches like C</i> or a string
containing a regex to be compiled within this sub, for example
C<'[a-z]'>.

Parameter C<need> and C<avoid> are used to specify additional
or less data structures to be returned, for example one may want
to avoid C<counts> hash in order to save memory. Or one
may need C<all> data structures.

Parameter C<debug> makes the sub more verbose.

These symbols can be words in text (if separator = word boundary='\b')
Or they can be letters in text (if separator is left undefined)
(in this case you must arrange that the input has each word in a line on its own)
Symbols can are then put together to form "ngrams" and the frequency of
each ngram is counted.
Input parameter 'ngram-length' refers to the number of symbols in an ngram.
For example, with an ngram-length=2 and a word-boundary separator
we get a 1st-order Markov chain:

   'hello','there' => 5
   'hello','you' => 10
   'goodbye','all' => 5

etc.
the numbers are the counts for each ngram as read from the input data.
Input parameter 'counts' can be used to supply such frequency data
without re-reading huge corpus each time. The param must be a hashref
where key=ngrams (separated by anything you like, no quotes necessary
but use some separator in order not to confuse composite words)
With that frequency data, we proceed to calculate a probability
distribution ('dist') which is just normalised frequencies.
Then we build the cumulative probability distribution but with a twist:
the ngrams are broken into (n-1)grams and 1grams (from left to right for latin)
The first ngram is the key. For all possible endings of that (i.e.
all the 1grams with which it ends) we count frequencies, normalise to
probabilities and then build the cumulative probability.
For example consider the following 'counts'
(let's assume we are dealing with letters, ngram-length=3):
   HEL => 10
   HEA => 20
   HEN => 5
   ALA => 10
   ALB => 20
The cumulative-probability-distribution will be something like this:
   HE => {
           L => 10/(10+20+5)
           A => 10/(10+20+5) + 20(10+20+5)
           N => 10/(10+20+5) + 20/(10+20+5) + 5/(10+20+5)
   }
   AL # left as an exercise for the reader
The sub returns a hashref made of these three hashrefs
'counts' => frequency(=counts) of each ngram as it occured in the input data
'dist' => probability distribution (i.e. normalised frequency/counts) of the above
'twisted-dist' => as explained before.
'cum-twisted-dist' => as explained before.
Refer to the 'predict()' sub for how the 'cum-twisted-dist' hash is used to make a prediction.


=head2 predict()

=head1 AUTHOR

Andreas Hadjiprocopis, C<< <bliako at cpan.org> >>

=head1 HUGS AND DEDICATIONS

Almaz

=head1 BUGS

Please report any bugs or feature requests to C<bug-algorithm-markov-multiorder-learner at rt.cpan.org>, or through
the web interface at L<https://rt.cpan.org/NoAuth/ReportBug.html?Queue=Algorithm-Markov-Multiorder-Learner>.  I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.




=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Algorithm::Markov::Multiorder::Learner


You can also look for information at:

=over 4

=item * RT: CPAN's request tracker (report bugs here)

L<https://rt.cpan.org/NoAuth/Bugs.html?Dist=Algorithm-Markov-Multiorder-Learner>

=item * Wikipedia entry on the Markov Property

L<https://en.wikipedia.org/wiki/Markov_property>

=item * Similar modules at CPAN in no particular order

L<Algorithm::MarkovChain>, L<Hailo>, L<String::Markov>, L<Decision::Markov>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Algorithm-Markov-Multiorder-Learner>

=item * CPAN Ratings

L<https://cpanratings.perl.org/d/Algorithm-Markov-Multiorder-Learner>

=item * Search CPAN

L<https://metacpan.org/release/Algorithm-Markov-Multiorder-Learner>

=back


=head1 ACKNOWLEDGEMENTS

Aldebaran (L<https://perlmonks.org/?node=Aldebaran>)
at L<www.PerlMonks.org> for testing and providing lots
and lots of useful feedback.


=head1 LICENSE AND COPYRIGHT

Copyright 2019 Andreas Hadjiprocopis.

This program is free software; you can redistribute it and/or modify it
under the terms of the the Artistic License (2.0). You may obtain a
copy of the full license at:

L<http://www.perlfoundation.org/artistic_license_2_0>

Any use, modification, and distribution of the Standard or Modified
Versions is governed by this Artistic License. By using, modifying or
distributing the Package, you accept this license. Do not use, modify,
or distribute the Package, if you do not accept this license.

If your Modified Version has been derived from a Modified Version made
by someone other than you, you are nevertheless required to ensure that
your Modified Version complies with the requirements of this license.

This license does not grant you the right to use any trademark, service
mark, tradename, or logo of the Copyright Holder.

This license includes the non-exclusive, worldwide, free-of-charge
patent license to make, have made, use, offer to sell, sell, import and
otherwise transfer the Package with respect to any patent claims
licensable by the Copyright Holder that are necessarily infringed by the
Package. If you institute patent litigation (including a cross-claim or
counterclaim) against any party alleging that the Package constitutes
direct or contributory patent infringement, then this Artistic License
to you shall terminate on the date that such litigation is filed.

Disclaimer of Warranty: THE PACKAGE IS PROVIDED BY THE COPYRIGHT HOLDER
AND CONTRIBUTORS "AS IS' AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES.
THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
PURPOSE, OR NON-INFRINGEMENT ARE DISCLAIMED TO THE EXTENT PERMITTED BY
YOUR LOCAL LAW. UNLESS REQUIRED BY LAW, NO COPYRIGHT HOLDER OR
CONTRIBUTOR WILL BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, OR
CONSEQUENTIAL DAMAGES ARISING IN ANY WAY OUT OF THE USE OF THE PACKAGE,
EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


