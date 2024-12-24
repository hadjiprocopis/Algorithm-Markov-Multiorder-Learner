#!/usr/bin/env perl

use strict;
use warnings;

use lib 'blib/lib';

use Test::More;
use Algorithm::Markov::Multiorder::Learner;

my $num_tests = 0;

my $input_corpus_filename = 'data/2.short.shelley.txt';
my $separator = qq{[-%!\:,;+*\\s\\t.]+};
#my $separator = '\s+';
my $internal_separator = '|';
my $ngram_length = 8;
my $process_paragraphs = 0;

my $state = learn({
	'ngram-length' => $ngram_length,
	'separator' => $separator,
	'internal-separator' => $internal_separator,
	'remove-these-characters-regex' => qr/[^a-zA-Z]/,
	'input-filename' => $input_corpus_filename,
	'process-paragraphs' => $process_paragraphs,
});
ok(defined $state, "learn(): called"); $num_tests++;

# end
done_testing($num_tests);
1;

