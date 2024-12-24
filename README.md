# NAME

Algorithm::Markov::Multiorder::Learner - Learn a corpus and predict using a multi-order (multi-dimensional)  Markov learner

# VERSION

Version 0.01

# SYNOPSIS

The idea has been explored many times in the past read a corpus, originally text but we
have expanded it to images and DNA sequences, build a Markov Chain by
counting how many times a word is followed / preceded by another and then predict what word
follows a given word. It has also been extended to cover an order N > 1 meaning Ngrams.
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

# EXPORT

Exported subs:

- `learn()`
- `predict()`
- `save_state()`
- `load_state()`
- `range()`

Also see the included scripts:

- analyse\_DNA\_sequence.pl
- analyse\_image.pl
- analyse\_text.pl
- predict\_image.pl
- predict\_text.pl
- read\_state.pl

# SUBROUTINES/METHODS

## learn()

`learn()` reads in some raw data from either a
file (given `input-filename` parameter)
or a scalar/string reference (give `input-string`).
It then proceeds to split _raw input_ to _symbols_
according to separator (given `input-separator`).
If no _separator_ is specified, input is split on
characters (exactly like `split //, "hello"`). This
is useful if you want to learn for example a sequence numbers < 10, a DNA
sequence, etc. If you want to learn words from a text then the
`input-separator` must be `'\s'`, and so on. The split logic
is pretty basic and may prove substandard for complex input.
In this case just you have several options:

- `input-array-symbols` : a ref to an array containing the symbols, no splitting and parsing.
- `input-array-ngrams` : a ref to an array of arrays of _ngrams_, i.e. a collection of N symbols, where N-1 is the order of the Markov Chain we are trying to build.
- `process-line-subref` : a ref to a sub which process each line, its prototype is `($lineref, $counts_hash_ref, $ngram_length, $separator, $internal_separator, $filter_sub_ref)`

Related to the `process-line-subref` parameter is `filter-line-subref`
which pre-process the line, e.g. to remove duplicate spaces. Or in
the case of DNA sequence/fasta files to remove '>' characters. Or
in the case of text and literature, to remove punctuation maybe
or expand/remove abbreviations etc. Its return code will decide
whether to proceed with processing this specific line or skip (if for example
becomes empty after filtering). The default filter is:
    sub {
        my $line = ${$\_\[0\]}; # expecting scalar ref of the line
        $line =~ s/\\s+/ /g;
        $line =~ s/\\s+$//;
        $line =~ s/^\\s//;
        return 0 if $line =~ /^\\s\*$/; # nothing to do
        return 1
    }

The order of the Markov Chain or the number of symbols to look at
in determining the next state is specified by `ngram-length`-1. It
must be a positive integer. Setting it to 1 makes the order 0 which
it does not exist but we use it so that we get at least word frequency
out.

Internally the symbols are joined using the `internal-separator`
which is some complex string like '\\|/', but one can change it
if this string interferes with input symbols.

The parameter `remove-these-characters-regex` is either a compiled
regex (`qr//` allows use of switches like `/i` or a string
containing a regex to be compiled within this sub, for example
`'[a-z]'`.

Parameter `need` and `avoid` are used to specify additional
or less data structures to be returned, for example one may want
to avoid `counts` hash in order to save memory. Or one
may need `all` data structures.

Parameter `debug` makes the sub more verbose.

These symbols can be words in text (if separator = word boundary='\\b')
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

## predict()

# AUTHOR

Andreas Hadjiprocopis, `<bliako at cpan.org>`

# HUGS AND DEDICATIONS

Almaz

# BUGS

Please report any bugs or feature requests to `bug-algorithm-markov-multiorder-learner at rt.cpan.org`, or through
the web interface at [https://rt.cpan.org/NoAuth/ReportBug.html?Queue=Algorithm-Markov-Multiorder-Learner](https://rt.cpan.org/NoAuth/ReportBug.html?Queue=Algorithm-Markov-Multiorder-Learner).  I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.

# SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Algorithm::Markov::Multiorder::Learner

You can also look for information at:

- RT: CPAN's request tracker (report bugs here)

    [https://rt.cpan.org/NoAuth/Bugs.html?Dist=Algorithm-Markov-Multiorder-Learner](https://rt.cpan.org/NoAuth/Bugs.html?Dist=Algorithm-Markov-Multiorder-Learner)

- Wikipedia entry on the Markov Property

    [https://en.wikipedia.org/wiki/Markov\_property](https://en.wikipedia.org/wiki/Markov_property)

- Similar modules at CPAN in no particular order

    [Algorithm::MarkovChain](https://metacpan.org/pod/Algorithm%3A%3AMarkovChain), [Hailo](https://metacpan.org/pod/Hailo), [String::Markov](https://metacpan.org/pod/String%3A%3AMarkov), [Decision::Markov](https://metacpan.org/pod/Decision%3A%3AMarkov)

- AnnoCPAN: Annotated CPAN documentation

    [http://annocpan.org/dist/Algorithm-Markov-Multiorder-Learner](http://annocpan.org/dist/Algorithm-Markov-Multiorder-Learner)

- CPAN Ratings

    [https://cpanratings.perl.org/d/Algorithm-Markov-Multiorder-Learner](https://cpanratings.perl.org/d/Algorithm-Markov-Multiorder-Learner)

- Search CPAN

    [https://metacpan.org/release/Algorithm-Markov-Multiorder-Learner](https://metacpan.org/release/Algorithm-Markov-Multiorder-Learner)

# ACKNOWLEDGEMENTS

Aldebaran ([https://perlmonks.org/?node=Aldebaran](https://perlmonks.org/?node=Aldebaran))
at [www.PerlMonks.org](https://metacpan.org/pod/www.PerlMonks.org) for testing and providing lots
and lots of useful feedback.

# LICENSE AND COPYRIGHT

Copyright 2019 Andreas Hadjiprocopis.

This program is free software; you can redistribute it and/or modify it
under the terms of the the Artistic License (2.0). You may obtain a
copy of the full license at:

[http://www.perlfoundation.org/artistic\_license\_2\_0](http://www.perlfoundation.org/artistic_license_2_0)

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
