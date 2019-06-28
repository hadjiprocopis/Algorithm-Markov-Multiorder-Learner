#!/usr/bin/env perl

use strict;
use warnings;

use Getopt::Long;
use Data::Dump qw/dump/;

use lib 'lib';

use Markov::Ndimensional;

my $input_state_filename = undef;
my $output_stats_filename = undef;
if( ! Getopt::Long::GetOptions(
	'input-state|i=s' => \$input_state_filename,
	'output-stats|o=s' => \$output_stats_filename,
	'help|h' => sub { print STDERR usage($0); exit(0) }
) ){ print STDERR usage($0) . "\n\nSomething wrong with command-line parameters...\n"; exit(1); }

my $state = undef;
if( defined($input_state_filename) ){
	$state = load_state($input_state_filename);
	if( ! defined($state) ){ print STDERR "$0 : call to ".'load_state()'." has failed.\n"; exit(1) }
}
if( ! defined($state) ){ print STDERR "$0 : --input-state must be specified.\n"; exit(1) }

if( defined($output_stats_filename) ){
	my $FH;
	open $FH, '>', $output_stats_filename;
	print $FH Data::Dump::dump($state);
	close $FH;
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
