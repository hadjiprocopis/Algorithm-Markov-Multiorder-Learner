use 5.006;
use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
#    INSTALL_BASE      => "$ENV{HOME}/usr",
    NAME             => 'Algorithm::Markov::Multiorder::Learner',
    AUTHOR           => q{Andreas Hadjiprocopis <bliako@cpan.org>},
    VERSION_FROM     => 'lib/Algorithm/Markov/Multiorder/Learner.pm',
    ABSTRACT_FROM    => 'lib/Algorithm/Markov/Multiorder/Learner.pm',
    LICENSE          => 'artistic_2',
    PL_FILES         => {},
    MIN_PERL_VERSION => '5.006',
    CONFIGURE_REQUIRES => {
        'ExtUtils::MakeMaker' => '0',
    },
    EXE_FILES        => [
	'script/analyse_text.pl',
	'script/analyse_image.pl',
	'script/analyse_DNA_sequence.pl',
	'script/predict_image.pl',
	'script/predict_text.pl',
	'script/read_state.pl',
    ],
    BUILD_REQUIRES => {
        'Test::More' => '0',
    },
    PREREQ_PM => {
	'Storable' => '0',
	'Data::Roundtrip' => '0',
    },
    dist  => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean => { FILES => 'Algorithm-Markov-Multiorder-Learner-*' },
);
