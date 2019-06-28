#!/usr/bin/env perl

use strict;
use warnings;

use Getopt::Long;
use Data::Dump qw/dump/;

use Algorithm::Markov::Multiorder::Learner;

my $input_fasta_filename = undef;
my $input_state_filename = undef;
my $output_state_filename = undef;
my $output_stats_filename = undef;
my $separator = undef;
my $ngram_length = -1;
if( ! Getopt::Long::GetOptions(
	'input-fasta=s' => \$input_fasta_filename,
	'input-state=s' => \$input_state_filename,
	'output-state=s' => \$output_state_filename,
	'output-stats=s' => \$output_stats_filename,
	'ngram-length=i' => \$ngram_length,
	'separator=s' => \$separator,
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
if( defined($input_fasta_filename) ){
	$state = learn({
		%params,
		'ngram-length' => $ngram_length,
		'separator' => $separator,
		'input-filename' => $input_fasta_filename,
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
	return "Usage : $0 <options>\n";
}
1;
__END__
