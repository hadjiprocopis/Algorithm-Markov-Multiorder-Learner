#!perl -T
use 5.006;
use strict;
use warnings;
use Test::More;

plan tests => 1;

BEGIN {
    use_ok( 'Algorithm::Markov::Multiorder::Learner' ) || print "Bail out!\n";
}

diag( "Testing Algorithm::Markov::Multiorder::Learner $Algorithm::Markov::Multiorder::Learner::VERSION, Perl $], $^X" );
