#!/usr/bin/env perl

use strict;
use warnings;

use Getopt::Long;
use Data::Dump qw/dump/;

use Algorithm::Markov::Multiorder::Learner;

my $input_corpus_filename = undef;
my $input_state_filename = undef;
my $output_state_filename = undef;
my $output_stats_filename = undef;
my $separator = qq{[-%!\:,;+*\\s\\t.]+};
#my $separator = '\s+';
my $internal_separator = '|';
my $ngram_length = -1;
my $process_paragraphs = 0;
if( ! Getopt::Long::GetOptions(
	'input-corpus=s' => \$input_corpus_filename,
	'input-state=s' => \$input_state_filename,
	'output-state=s' => \$output_state_filename,
	'output-stats=s' => \$output_stats_filename,
	'ngram-length=i' => \$ngram_length,
	'separator=s' => \$separator,
	'process-paragraphs' => \$process_paragraphs,
	'help|h' => sub { print STDERR usage($0); exit(0) }
) ){ print STDERR usage($0) . "\n\nSomething wrong with command-line parameters...\n"; exit(1); }

if( $ngram_length <= 0 ){ print STDERR "$0 : ngram-length must be a positive integer.\n"; exit(1) }

my %params = ();
if( defined($output_state_filename) ){ $params{'need'} = {'all'=>1} }
else { $params{'avoid'} = {'counts'=>1} }
my $state = undef;
if( defined($input_state_filename) ){
	$state = load_state($input_state_filename);
	if( ! defined($state) ){ print STDERR "$0 : call to ".'load_state()'." has failed.\n"; exit(1) }
	$params{'counts'} = $state->{'counts'};
}
if( defined($input_corpus_filename) ){
	$state = learn({
		%params,
		'ngram-length' => $ngram_length,
		'separator' => $separator,
		'internal-separator' => $internal_separator,
		'remove-these-characters-regex' => qr/[^a-zA-Z]/,
		'input-filename' => $input_corpus_filename,
		'process-paragraphs' => $process_paragraphs,
	});
	if( ! defined($state) ){ print STDERR "$0 : call to ".'learn()'." has failed.\n"; exit(1) }
}
if( ! defined($state) ){ print STDERR "$0 : --input-state and/or --input-fasta must be specified.\n"; exit(1) }

if( defined($output_state_filename) ){
	if( ! save_state($state, $output_state_filename) ){ print STDERR "$0 : call to ".'save_state()'." has failed.\n"; exit(1) }
}
if( defined($output_stats_filename) ){
	print Data::Dump::dump($state);
} else {
	print Data::Dump::dump($state);
}
print "\n$0 : done.\n";
exit(0);

sub usage {
	return "Usage : $0 <options>\n"
." --input-corpus I : read text input from this file, which can be in lines or paragraphs and ideally should have all punctuation and other non-word characters removed prior to using this command.\n"
." --output-state S : once read, processed and learned the input text, the 'state' can be saved to a file for later use if with the same file. It will save the time of re-reading, re-processing and re-learning the text. However, a state can be saved and read again later using a different file. In this case, the new file's learning material will be added to the state already loaded. Great for cumulatively learning text as it comes in or too big to do in one go.\n"
." --input-state S : read previous experience or 'state' from file. Typically one reads a corpus, process it, analyzes it and learns it. The learning experience will be in the form of the 'state'. Which can be saved to disk and loaded at another instance, either in order to predict or in order to add more learning experience by reading additional corpus.\n"
." --output-stats X : save some informative statistics to file X, only for information, can not be reloaded or be useful otherwise.\n"
." --ngram-length N : the corpus will be analysed either on a line-by-line or paragraph-by-paragraph mode (see --process-paragraphs) and find words separated by the word separator (see --separator). Ngrams will be formed by running a sliding window of length N on each line/paragraph. Sliding window.\n"
." --process-paragraphs : the default mode is to read input corpus on a line-to-line basis and get the Ngrams (via the sliding window, see --ngram-length) only on that line. No Ngrams will be formed between the last word of one line and the first word of the next line. However, there is also the paragraph-by-paragraph mode which will replace all single newlines with space and hopefully end up with paragraphs, which should be separated by two or more newlines. And slide the Ngram window on the paragraph instead.\n"
." --separator SEP : a string denoting the regex to separate words, for example '\\s+'. Note that some characters in a character class regex need escaping unless they are after or before the square bracket. Default is /${separator}/.\n"
." --help : this text.\n"
."\nHere is an example using data from /data dir:\n"
."bin/analyse_text.pl --input-corpus data/2.short.shelley.txt --ngram-length 8 --separator '$separator'\n"
."\n\n"
."    Program by Andreas Hadjiprocopis (andreashad2\@gmail.com / bliaki\@cpan.org)\n"
."    distributed on the terms of the Artistic License (2.0)\n"
."      2018-2019\n"
}
1;
__END__
