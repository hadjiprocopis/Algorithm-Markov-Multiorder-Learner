#!/usr/bin/env perl

use strict;
use warnings;

use Getopt::Long;
use Data::Dump qw/dump/;
use Image::Base::GD;

use Algorithm::Markov::Multiorder::Learner;

my $input_image_filename = undef;
my $input_state_filename = undef;
my $output_state_filename = undef;
my $output_stats_filename = undef;
my $output_image_filename = undef;
my $separator = '\s';
my $internal_separator = '|';
my $seed = undef;
my $num_iterations = 100;
if( ! Getopt::Long::GetOptions(
	'input-image|i=s' => \$input_image_filename,
	'input-state=s' => \$input_state_filename,
	'output-image|o=s' => \$output_image_filename,
	'separator=s' => \$separator,
	'num-iterations=i' => $num_iterations,
	'seed=s' => \$seed,
	'help|h' => sub { print STDERR usage($0); exit(0) }
) ){ print STDERR usage($0) . "\n\nSomething wrong with command-line parameters...\n"; exit(1); }

if( ! defined($input_image_filename) ){ print STDERR "$0 : --input-image must be used to specify an input image.\n"; exit(1); }
if( ! defined($output_image_filename) ){ print STDERR "$0 : --output-image must be used to specify an output image.\n"; exit(1); }
if( ! defined($input_state_filename) ){ print STDERR "$0 : --input-state must be used to specify a state file to read. In order to produce a state use analyse_text.pl\n"; exit(1); }
my $state = load_state($input_state_filename);
if( ! defined($state) ){ print STDERR "$0 : call to ".'load_state()'." has failed.\n"; exit(1) }
my $ngram_length = $state->{'N'};
print "$0 : read state from '$input_state_filename', ngram-length is $ngram_length.\n";

my $w = $state->{'cum-twisted-dist'};

my $png = Image::Base::GD->new(-file => $input_image_filename);
if( ! defined $png ){ print STDERR "$0 : failed to read PNG input image '$input_image_filename': ".$png->error."\n"; exit(1) }
my $gd = $png->{'-gd'};
my $W = $gd->width();
my $H = $gd->height();
my $outpng = Image::Base::GD->new(-width=>$W, -height=>$H);
if( ! defined $outpng ){ print STDERR "$0 : failed to open new PNG output image '$output_image_filename': ".$png->error."\n"; exit(1) }
my ($x, $y, $predicted, $data);
for($x=$W-1;$x-->1;){
	for($y=$H-1;$y-->1;){
#for(1..100000){ $x = int(rand($W-2))+2; $y = int(rand($H-2))+2;
		$data = join($internal_separator,
			$png->xy($x-1, $y-1),
			$png->xy($x+1, $y-1),
			$png->xy($x-1, $y+1),
			$png->xy($x+1, $y+1)
		);
		$predicted = predict($w, $data) // 'black';
		$outpng->xy($x, $y, $predicted);
	}
}
$outpng->save($output_image_filename);

print "$0 : done.\n";

sub usage {
	return "Usage : $0 <options>\n";
}
1;
__END__
