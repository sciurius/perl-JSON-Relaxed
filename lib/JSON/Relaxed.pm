#! perl

package JSON::Relaxed;
use strict;

# debug tools
# use Debug::ShowStuff ':all';
# use Debug::ShowStuff::ShowVar;

# version
our $VERSION = '0.063_02';

=head1 NAME

JSON::Relaxed -- An extension of JSON that allows for better human-readability

=head1 Relaxed JSON?

There's been increasing support for the idea of expanding JSON to improve
human-readability.
"Relaxed" JSON (RJSON) is a term that has been used to describe a
JSON-ish format that has some human-friendly features that JSON doesn't.
Most notably, RJSON allows the use of JavaScript-like comments and
eliminates the need to quote all keys and values.
An (official) specification can be found on
L<RelaxedJSON.org|https://www.relaxedjson.org>.

I<Note that by definition every valid JSON document is also a valid
RJSON document.>

=head1 SYNOPSIS

    use JSON::Relaxed;

    # Some raw RJSON data.
    my $rjson = <<'RAW_DATA';
    /* Javascript-like comments. */
    {
        // Keys do not require quotes.
        // Single, double and backtick quotes.
        a : 'Larry',
        b : "Curly",
        c : `Phoey`,
        // Simple values do not require quotes.
        d:  unquoted

        // Nested structures.
        e: [
          { a:1, b:2 },
        ],

        // Like Perl, trailing commas are allowed.
        f: "more stuff",
    }
    RAW_DATA

    # Functional parsing.
    my $hash = decode_rjson($rjson);

    # Object-oriented parsing.
    my $parser = JSON::Relaxed->new();
    $hash = $parser->decode($rjson);

=head1 DESCRIPTION

JSON::Relaxed is a lightweight parser and serializer for RJSON.
It is fully compliant to the L<RelaxedJSON.org|https://www.relaxedjson.org/specification> specification.

=head1 EXTENSIONS

=over 4

=item Hash keys without values

JSON::Relaxed supports object keys without a specified value.
In that case the hash element is simply assigned the undefined value.

In the following example, a is assigned 1, and b is assigned undef:

    { a:1, b }

=item String continuation

Long strings can be split over multiple lines by putting a backslash
at the end of the line:

    "this is a " \
    "long string"

Note that this is different from

    "this is a \
    long string"

which B<embeds> the newline into the string.

=item Extended Unicode escapes

Unicode escapes in strings may contain an arbitrary number of hexadecimal
digits enclosed in braces:

    \u{1d10e}

This eliminates the need to use L<surrogates|https://unicode.org/faq/utf_bom.html#utf16-2> to obtain the same character:

    \uD834\uDD0E

=back

=head1 SUBROUTINES

=head2 decode_rjson

    $structure = decode_rjson($data)

C<decode_rjson()> is the simple way to parse an RJSON string.
It is exported by default.
C<decode_rjson> takes a single parameter, the string to be parsed.


Optionally an additional hash with options can be passed
to change the behaviour of the parser.
See L<Object-oriented parsing|JSON::Relaxed::Parser/"Object-oriented-parsing">
in JSON::Relaxed::Parser.

    $structure = decode_rjson( $rjson, %options );

=cut

sub decode_rjson {
    my ( $raw, %options ) = @_;
    use JSON::Relaxed::Parser;
    my $parser = JSON::Relaxed::Parser->new(%options);
    return $parser->decode($raw);
}

=head1 METHODS

=head2 new

To parse using an object, create a C<JSON::Relaxed> object:

    $parser = JSON::Relaxed->new();

Then call the parser's C<decode> method, passing in the RJSON string:

    $structure = $parser->decode($rjson);

For more details, see L<Object-oriented parsing|JSON::Relaxed::Parser/"Object-oriented-parsing"> in JSON::Relaxed::Parser.

=cut

sub new {
    my ($class, %opts) = @_;
    return JSON::Relaxed::Parser->new(%opts);
}

use parent qw(Exporter);
BEGIN {
    our @EXPORT      = qw(decode_rjson);
    our @EXPORT_OK   = ( @EXPORT, qw(from_rjson) );
    our %EXPORT_TAGS = ( all => [ @EXPORT_OK ] );

    # For compatibility with pre-0.50 api.
    *from_rjson = \&decode_rjson;
    *err_id  = \$JSON::Relaxed::Parser::err_id;
    *err_msg = \$JSON::Relaxed::Parser::err_msg;
}

=head1 ERROR HANDLING

If the document cannot be parsed, JSON::Relaxed returns an undefined
value and sets error indicators in $JSON::Relaxed::Parser::err_id and
$JSON::Relaxed::Parser::err_msg. For a full list of error codes, see
L<JSON::Relaxed::ErrorCodes>.

=head1 COMPATIBILITY WITH PRE-0.05 VERSION

The old static method C<from_rjson> has been renamed to C<decode_rjson>,
to conform to many other modules of this kind.
C<from_rjson> is kept as a synonym for C<decode_rjson>.

For the same reason, the old parser method C<parse> has been renamed to C<decode>.
C<parse> is kept as a synonym for C<decode>.

=head1 AUTHOR

Johan Vromans F<jv@cpan.org>

Miko O'Sullivan F<miko@idocs.com>, original version.

=head1 SUPPORT

Development of this module takes place on GitHub:
L<https://github.com/sciurius/perl-JSON-Relaxed>.

You can find documentation for this module with the perldoc command.

  perldoc JSON::Relaxed

Please report any bugs or feature requests using the issue tracker on
GitHub.

=head1 LICENSE

Copyright (c) 2024 by Johan Vromans. All rights reserved. This
program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself. This software comes with B<NO
WARRANTY> of any kind.

Original copyright 2014 by Miko O'Sullivan. All rights reserved. This
program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself. This software comes with B<NO
WARRANTY> of any kind.

=cut

1;
