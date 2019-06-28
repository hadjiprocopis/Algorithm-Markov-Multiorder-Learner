#!/usr/bin/env perl

use strict;
use warnings;

use Getopt::Long;
use Data::Dump qw/dump/;

use Markov::Ndimensional;

my $input_corpus_filename = undef;
my $input_state_filename = undef;
my $output_state_filename = undef;
my $output_stats_filename = undef;
my $separator = '\s';
my $internal_separator = '|';
my $seed = undef;
my $num_iterations = 100;
if( ! Getopt::Long::GetOptions(
	'input-state|i=s' => \$input_state_filename,
	'separator=s' => \$separator,
	'num-iterations=i' => $num_iterations,
	'seed=s' => \$seed,
	'help|h' => sub { print STDERR usage($0); exit(0) }
) ){ print STDERR usage($0) . "\n\nSomething wrong with command-line parameters...\n"; exit(1); }

if( ! defined($input_state_filename) ){ print STDERR "$0 : --input-state must be used to specify a state file to read. In order to produce a state use analyse_text.pl\n"; exit(1); }
my $state = load_state($input_state_filename);
if( ! defined($state) ){ print STDERR "$0 : call to ".'load_state()'." has failed.\n"; exit(1) }
my $ngram_length = $state->{'N'};
print "$0 : read state from '$input_state_filename', ngram-length is $ngram_length.\n";

if( defined $seed ){
	my @crap = split /${separator}/, $seed;
	if( scalar(@crap) != ($ngram_length-1) ){ print STDERR "$0 : the ngram-length from state file '$input_state_filename' is $ngram_length but the provided seed assumes length ".(scalar(@crap)+1)."\n"; exit(1) }
	$seed = join $internal_separator, @crap;
} else {
	$seed = (keys %{$state->{'cum-twisted-dist'}})[0]
}
print "$0 : starting with seed '$seed' ...\n";
my %params = ();
$params{'avoid'} = {'counts'=>1};

my $w = $state->{'cum-twisted-dist'};
print $seed;
for(1..$num_iterations){
	my $ret = predict($w, $seed);
	if( ! defined($ret) ){ last }
	print " $ret";
	my @y = split /${separator}/, $seed; shift @y;
	$seed = join ' ', @y, $ret;
}
print "\n$0 : done.\n";
exit(0);

sub usage {
	return "Usage : $0 <options>\n";
}
1;
__END__
