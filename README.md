# NAME

![Version](https://img.shields.io/github/v/release/sciurius/perl-JSON-Relaxed)
![GitHub issues](https://img.shields.io/github/issues/sciurius/perl-JSON-Relaxed)
[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
![Language Perl](https://img.shields.io/badge/Language-Perl-blue)

 *Note: This is a fork of the abandoned CPAN version by Miko
O'Sullivan. I forked it for my own purposes. If it is useful for you,
enjoy and participate!*

# SYNOPSIS

    use JSON::Relaxed;

    # Some raw RJSON code.
    my $rjson = <<'(RAW)';
    /* Javascript-like comments are allowed */
    {
        // single or double quotes allowed
        a : 'Larry',
        b : "Curly",

        // nested structures allowed like in JSON
        c: [
          {a:1, b:2},
        ],

        // like Perl, trailing commas are allowed
        d: "more stuff",
    }
    (RAW)

    # Subroutine parsing.
    my $hash = decode_rjson($rjson);

    # Object-oriented parsing.
    my $parser = JSON::Relaxed->new();
    $hash = $parser->decode($rjson);

# DESCRIPTION

JSON::Relaxed is a lightweight parser and serializer for an extension
of JSON called Relaxed JSON (RJSON). The intent of RJSON is to provide
a format that is more human-readable and human-editable than JSON.
Most notably, RJSON allows the use of JavaScript-like comments. By
doing so, configuration files and other human-edited files can include
comments to indicate the intention of each configuration.

JSON::Relaxed is currently only a parser that reads in RJSON code and
produces a data structure. JSON::Relaxed does not currently encode
data structures into JSON/RJSON.

### comments

RJSON supports JavaScript-like comments:

    /* inline comments */
    // line-based comments

### trailing commas

Like Perl, RJSON allows treats commas as separators.  If nothing is
after, or between commas, those commas are just ignored:

    [
        "data",
        , // nothing after this comma
    ]

Note that the specification disallows loose commas at the beginning of a list.

### single quotes, double quotes, no quotes

Strings can be quoted with either single or double quotes.  Space-less strings
are also parsed as strings. So, the following data items are equivalent:

    [
        "Starflower",
        'Starflower',
        Starflower
    ]

Quoted strings may contain escaped characters like `\t` for tab and
`\n` for newline.
Arbitrary unicode characters can be escaped with `\u` followed by
four hexadecimal digits.

As an extension to the specification `JSON::Relaxed` allows quoted
strings may be split over multiple lines:

    [
        "Star" \
        "flower"
    ]

This produces a single string, `"Starflower"`.
Note that this is different from

    [
        "Star \
        flower"
    ]

which produces `"Star \n    flower"` (where `\n` is a newline).

Unquoted boolean values are still treated as boolean values, so the
following are NOT the same:

    [
        "true",  // string
        true,    // boolean true

        "false", // string
        false,   // boolean false

        "null", // string
        null, // what Perl programmers call undef
    ]

Because of this ambiguity, unquoted non-boolean strings should be considered
sloppy and not something you do in polite company.

### documents that are just a single string

Early versions of JSON require that a JSON document contains either a single
hash or a single array.  Later versions also allow a single string.  RJSON
follows that later rule, so the following is a valid RJSON document:

    "Hello world"

### hash keys without values

A hash in JSON can have a key that is followed by a comma or a closing `}`
without a specified value.  In that case the hash element is simply assigned
the undefined value.  So, in the following example, `a` is assigned `1`,
`b` is assigned 2, and `c` is assigned undef:

    {
        a: 1,
        b: 2,
        c
    }

### commas are optional between objects pairs and array items

    {
      buy: [ milk eggs butter 'dog bones' ]
      tasks: [ { name:exercise completed:false }
               { name:eat completed:true } ]
    }

## decode\_rjson()

`decode_rjson()` is the simple way to quickly parse an RJSON string. Currently
`decode_rjson()` only takes a single parameter, the string itself. So in the
following example, `decode_rjson()` parses and returns the structure defined in
`$rjson`.

    $structure = decode_rjson( $rjson, %options );

`%options` can be used to change the behaviour of the parser.
See [below](#object-oriented-parsing).

## Object-oriented parsing

To parse using an object, create a `JSON::Relaxed::Parser` object, like this:

    $parser = JSON::Relaxed::Parser->new();

Then call the parser's `decode` method, passing in the RJSON string:

    $structure = $parser->decode($rjson);

**Methods**

- $parser->extra\_tokens\_ok()

    `extra_tokens_ok()` sets/gets the `extra_tokens_ok` property. By default,
    `extra_tokens_ok` is false.  If `extra_tokens_ok` is true then the
    `multiple-structures` isn't triggered and the parser returns the first
    structure it finds.  So, for example, the following code would return undef and
    sets the `multiple-structures` error:

        $parser = JSON::Relaxed::Parser->new();
        $structure = $parser->decode('{"x":1} []');

    However, by setting `multiple-structures` to true, a hash structure is
    returned, the extra code after that first hash is ignored, and no error is set:

        $parser = JSON::Relaxed::Parser->new();
        $parser->extra_tokens_ok(1);
        $structure = $parser->decode('{"x":1} []');

- $parser->err\_id()

    A convenient way to access `$JSON::Relaxed::err_id`,
    see [below](#error-codes).

- $parser->err\_msg()

    A convenient way to access `$JSON::Relaxed::err_msg`,
    see [below](#error-codes).

- new

    `JSON::Relaxed->new()` creates a parser object.
    It is short for calling

        my $parser = JSON::Relaxed::Parser->new;

    See ["JSON::Relaxed::Parser"](#json-relaxed-parser)

## Error codes

When JSON::Relaxed encounters a parsing error it returns `undef` and sets two
global variables: 

- $JSON::Relaxed::err\_id

    `$err_id` is a unique code for a specific error.  Every code is set in only
    one place in JSON::Relaxed.

- $JSON::Relaxed::err\_msg

    `$err_msg` is an English description of the code.  It would be cool to migrate
    towards multi-language support for `$err_msg`.

Following is a list of all error codes in JSON::Relaxed:

- `missing-parameter`

    The string to be parsed was not sent to $parser->decode(). For example:

        $parser->decode()

- `undefined-input`

    The string to be parsed is undefined. For example:

        $parser->decode(undef)

- `zero-length-input`

    The string to be parsed is zero-length. For example:

        $parser->decode('')

- `space-only-input`

    The string to be parsed has no content beside space characters. For example:

        $parser->decode('   ')

- `no-content`

    The string to be parsed has no content. This error is slightly different than
    `space-only-input` in that it is triggered when the input contains only
    comments, like this:

        $parser->decode('/* whatever */')

- `unclosed-inline-comment`

    A comment was started with /\* but was never closed. For example:

        $parser->decode('/*')

- `invalid-structure-opening-character`

    The document opens with an invalid structural character like a comma or colon.
    The following examples would trigger this error.

        $parser->decode(':')
        $parser->decode(',')
        $parser->decode('}')
        $parser->decode(']')

- `multiple-structures`

    The document has multiple structures. JSON and RJSON only allow a document to
    consist of a single hash, a single array, or a single string. The following
    examples would trigger this error.

        $parse->decode('{}[]')
        $parse->decode('{} "whatever"')
        $parse->decode('"abc" "def"')

- `unknown-token-after-key`

    A hash key may only be followed by the closing hash brace or a colon. Anything
    else triggers `unknown-token-after-key`. So, the following examples would
    trigger this error.

        $parse->decode("{a [ }") }
        $parse->decode("{a b") }

- `unknown-token-for-hash-key`

    The parser encountered something besides a string where a hash key should be.
    The following are examples of code that would trigger this error.

        $parse->decode('{{}}')
        $parse->decode('{[]}')
        $parse->decode('{]}')
        $parse->decode('{:}')

- `unclosed-hash-brace`

    A hash has an opening brace but no closing brace. For example:

        $parse->decode('{x:1')

- `unclosed-array-brace`

    An array has an opening brace but not a closing brace. For example:

        $parse->decode('["x", "y"')

- `unexpected-token-after-colon`

    In a hash, a colon must be followed by a value. Anything else triggers this
    error. For example:

        $parse->decode('{"a":,}')
        $parse->decode('{"a":}')

- `missing-comma-between-array-elements`

    In an array, a comma must be followed by a value, another comma, or the closing
    array brace.  Anything else triggers this error. For example:

        $parse->decode('[ "x" "y" ]')
        $parse->decode('[ "x" : ]')

- `unknown-array-token`

    This error exists just in case there's an invalid token in an array that
    somehow wasn't caught by `missing-comma-between-array-elements`. This error
    shouldn't ever be triggered.  If it is please [let me know](#author).

- `unclosed-quote`

    This error is triggered when a quote isn't closed. For example:

        $parse->decode("'whatever")
        $parse->decode('"whatever') }

# INTERNALS

The following documentation is for if you want to edit the code of
`JSON::Relaxed` itself.

## JSON::Relaxed

`JSON::Relaxed` is the parent package. Not a lot actually happens in
`JSON::Relaxed`, it mostly contains [from\_rjson()](#from_rjson) and
definitions of various structures.

- Special character and string definitions

    The following hashes provide information about characters and strings that have
    special meaning in RJSON.

    - Escape characters

        The `%esc` hash defines the six escape characters in RJSON that are
        changed to single characters. `%esc` is defined as follows.

            our %esc = (
                'b'   => "\b",    #  Backspace
                'f'   => "\f",    #  Form feed
                'n'   => "\n",    #  New line
                'r'   => "\r",    #  Carriage return
                't'   => "\t",    #  Tab
                'v'   => chr(11), #  Vertical tab
            );

        Additionally, `\u` followed by exactly 4 hexadecimal digits will
        produce the character corresponding to the Unicode code point.

    - Structural characters

        The `%structural` hash defines the six characters in RJSON that define
        the structure of the data object. The structural characters are defined as
        follows.

            our %structural = (
                '[' => 1, # beginning of array
                ']' => 1, # end of array
                '{' => 1, # beginning of hash
                '}' => 1, # end of hash
                ':' => 1, # delimiter between name and value of hash element
                ',' => 1, # separator between elements in hashes and arrays
            );

    - Quotes

        The `%quotes` hash defines the types of quotes recognized by RJSON: single
        and double quotes, and backticks.
        JSON only allows the use of double quotes to define strings.
        `%quotes` is defined as follows.

            our %quotes = (
                '"' => 1,
                "'" => 1,
                "`" => 1,
            );

    - End of line characters

        End of line characters are used to detect the end of // comments.
        The `%newlines` hash defines the three ways a line can end in a RJSON
        document. Lines in Windows text files end with carriage-return newline
        ("\\r\\n").  Lines in Unixish text files end with newline ("\\n").
        Note that an escaped newline also terminates a comment.
        Lines in some
        operating systems end with just carriage returns ("\\n"). `%newlines` is
        defined as follows.

            our %newlines = (
                "\r\n" => 1,
                "\r" => 1,
                "\n" => 1,
                "\\\n" => 1,
            );

    - Boolean

        The `%boolean` hash defines strings that are boolean values: true, false, and
        null. (OK, 'null' isn't **just** a boolean value, but I couldn't think of what
        else to call this hash.) `%boolean` is defined as follows.

            our %boolean = (
                'null' => 1,
                'true' => 1,
                'false' => 1,
            );

## JSON::Relaxed::Parser

A `JSON::Relaxed::Parser` object parses the raw RJSON string. You don't
need to instantiate a parser if you just want to use the default settings.
In that case just use [from\_rjson()](#from_rjson).

You must create a `JSON::Relaxed::Parser` object if you want to
customize how the string is parsed.

To parse in an object oriented manner, create the parser, then parse.

    $parser = JSON::Relaxed::Parser->new();
    $structure = $parser->decode($string);

- new

    `JSON::Relaxed::Parser->new()` creates a parser object. Its
    simplest and most common use is without any parameters.

        my $parser = JSON::Relaxed::Parser->new( %options );

    Options:

    - extra\_tokens\_ok

        If `extra_tokens_ok` is true then the
        `multiple-structures` isn't triggered and the parser returns the first
        structure it finds.

    - unknown

        The `unknown` option sets the character which creates the
        [unknown object](#json-relaxed-parser-token-unknown). The unknown object
        exists only for testing JSON::Relaxed. It has no purpose in production use.

            my $parser = JSON::Relaxed::Parser->new( unknown => '~' );

- Parser error codes
    - err\_id()

        A convenient way to access `$JSON::Relaxed::err_id`,
        see [Error codes](#error-codes).

    - err\_msg()

        A convenient way to access `$JSON::Relaxed::err_msg`,
        see [Error codes](#error-codes).

- Parser "is" methods

    The following methods indicate if a token has some specific property, such as
    being a string object or a structural character.

    - is\_string()

        Returns true if the token is a string object, i.e. in the class
        `JSON::Relaxed::Parser::Token::String`.

    - is\_struct\_char()

        Returns true if the token is one of the structural characters of JSON, i.e.
        one of the following:

            { } [ ] : ,

    - is\_unknown\_char()

        Returns true if the token is the
        [unknown character](#json-relaxed-parser-token-unknown).

    - is\_list\_opener()

        Returns true if the token is the opening character for a hash or an array,
        i.e. it is one of the following two characters:

            { [

    - is\_comment\_opener()

        Returns true if the token is the opening character for a comment,
        i.e. it is one of the following two couplets:

            /*
            //

- decode()

    `decode()` is the method that does the work of parsing the RJSON string.
    It returns the data structure that is defined in the RJSON string.
    A typical usage would be as follows.

        my $parser = JSON::Relaxed::Parser->new();
        my $structure = $parser->decode('["hello world"]');

    `decode()` does not take any options.

- parse\_chars()

    `parse_chars()` parses the RJSON string into either individual characters
    or two-character couplets. This method returns an array. The only input is the
    raw RJSON string. So, for example, the following string:

        $raw = qq|/*x*/["y"]|;
        @chars = $parser->parse_chars($raw);

    would be parsed into the following array:

        ( "/*", "x", "*/", "[", "\"", "y", "\""", "]" )

    Most of the elements in the array are single characters. However, comment
    delimiters, escaped characters, and Windows-style newlines are parsed as
    two-character couplets:

    - `\` followed by any character
    - `\r\n`
    - `//`
    - `/*`
    - `*/`

    `parse_chars()` should not produce any fatal errors.

- tokenize()

    `tokenize()` organizes the characters from
    `[parse_chars()](#parse_chars)` into tokens. Those tokens can then be
    organized into a data structure with
    `[structure()](#structure)`.

    Each token represents an item that is recognized by JSON. Those items include
    structural characters such as `{` or `}`, or strings such as
    `"hello world"`. Comments and insignificant whitespace are filtered out
    by `tokenize()`.

    For example, this code:

        $parser = JSON::Relaxed::Parser->new();
        $raw = qq|/*x*/ ["y"]|;
        @chars = $parser->parse_chars($raw);
        @tokens = $parser->tokenize(\@chars);

    would produce an array like this:

        (
            '[',
            JSON::Relaxed::Parser::Token::String::Quoted=HASH(0x20bf0e8),
            ']'
        )

    Strings are tokenized into string objects.  When the parsing is complete they
    are returned as scalar variables, not objects.

    `tokenize()` should not produce any fatal errors.

- structure()

    `$parser-`structure()> organizes the tokens from `[tokenize()](#tokenize)`
    into a data structure.  `$parser-`structure()> returns a single string, single
    array reference, a single hash reference, or (if there are errors) undef.

## JSON::Relaxed::Parser::Structure::Hash

This package parses Relaxed into hash structures. It is a static package, i.e.
it is not instantiated.

- build()

    This static method accepts the array of tokens and works through them building
    the hash reference that they represent. When `build()` reaches the closing
    curly brace (`}`) it returns the hash reference.

- get\_value

    This static method gets the value of a hash element. This method is called
    after a hash key is followed by a colon. A colon must be followed by a value.
    It may not be followed by the end of the tokens, a comma, or a closing brace.

## JSON::Relaxed::Parser::Structure::Array

This package parses Relaxed into array structures. It is a static package, i.e.
it is not instantiated.

- build()

    This static method accepts the array of tokens and works through them building
    the array reference that they represent. When `build()` reaches the closing
    square brace (`]`) it returns the array reference.

- missing\_comma()

    This static method build the `missing-comma-between-array-elements` error
    message.

- invalid\_array\_token()

    This static method build the `unknown-array-token` error message.

## JSON::Relaxed::Parser::Token::String

Base class JSON::Relaxed::Parser::Token::String::Quoted and
JSON::Relaxed::Parser::Token::String::Unquoted.

- decode\_uescape()

    Decodes unicode escapes like \\u201D.
    Handles surrogates.

    Extension: Also handles \\u{1d10e} escapes.

- assemble\_surrogate()

    Assembles a Unicode character out of a high and low surrogate.

## JSON::Relaxed::Parser::Token::String::Quoted

A `JSON::Relaxed::Parser::Token::String::Quoted` object represents a string
in the document that is delimited with single or double quotes.  In the
following example, _Larry_ and _Curly_ would be represented by `Quoted`
objects by _Moe_ would not.

    [
    "Larry",
    'Curly',
    Moe
    ]

`Quoted` objects are created by `$parser->tokenize()` when it works
through the array of characters in the document.

- `new()`

    `new()` instantiates a `JSON::Relaxed::Parser::Token::String::Quoted` object
    and slurps in all the characters in the characters array until it gets to the
    closing quote.

    If the string is followed by optional whitespace, a backslash, a
    newline, optional whitespace and another string, the latter string
    will be appended to the current string. In other words,

        "foo" \
        "bar"

    will produce a single string, `"foobar"`.

    `new()` returns the new `Quoted` object.

    A `Quoted` object has the following two properties:

    `raw`: the string that is inside the quotes.  If the string contained any
    escape characters then the escapes are processed and the unescaped characters
    are in `raw`. So, for example, `\n` would become an actual newline.

    `quote`: the delimiting quote, i.e. either a single quote or a double quote.
    In case of a combined string, the delimeter quote of the _final_
    string part.

- `as_perl()`

    `as_perl()` returns the string that was in quotes (without the quotes).

## JSON::Relaxed::Parser::Token::String::Unquoted

A `JSON::Relaxed::Parser::Token::String::Unquoted` object represents a string
in the document that was not delimited quotes.  In the following example,
_Moe_ would be represented by an `Unquoted` object, but _Larry_ and _Curly_
would not.

    [
    "Larry",
    'Curly',
    Moe
    ]

`Unquoted` objects are created by `$parser->tokenize()` when it works
through the array of characters in the document.

An `Unquoted` object has one property, `raw`, which is the string. Escaped
characters are resolved in `raw`.

- `new()`

    `new()` instantiates a `JSON::Relaxed::Parser::Token::String::Unquoted`
    object and slurps in all the characters in the characters array until it gets
    to a space character, a comment, or one of the structural characters such as
    `{` or `:`.

- `as_perl()`

    `as_perl()` returns the unquoted string or a boolean value, depending on how
    it is called.

    If the string is a boolean value, i.e. _true_, _false_, then the `as_perl`
    return 1 (for true), 0 (for false) or undef (for null), **unless** the
    `always_string` option is sent, in which case the string itself is returned.
    If the string does not represent a boolean value then it is returned as-is.

    `$parser->structure()` sends the `always_string` when the token is a key
    in a hash. The following example should clarify how `always_string` is used:

        {
        // key: the literal string "larry"
        // value: 1
        larry : true,

        // key: the literal string "true"
        // value: 'x'
        true : 'x',

        // key: the literal string "null"
        // value: 'y'
        null : 'y',

        // key: the literal string "z"
        // value: undef
        z : null,
        }

## JSON::Relaxed::Parser::Token::Unknown

This class is just used for development of JSON::Relaxed. It has no use in
production. This class allows testing for when a token is an unknown object.

To implement this class, add the 'unknown' option to JSON::Relaxed->new(). The
value of the option should be the character that creates an unknown object.
For example, the following option sets the tilde (~) as an unknown object.

    my $parser = JSON::Relaxed::Parser->new(unknown=>'~');

The "unknown" character must not be inside quotes or inside an unquoted string.

# COMPATIBILITY WITH PRE-0.05 VERSION

The old static methoc `from_rjson` is renamed to `decode_rjson`,
to conform to many other modules of this kind.
`from_rjson` is kept as a synonym for `decode_rjson`,

For the same reason, the old parser method `parse` is renamed to `decode`.
`parse` is kept as a synonym for `decode`.

# COPYRIGHT

Copyright (c) 2024 by Johan Vromans. All rights reserved. This
program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself. This software comes with **NO
WARRANTY** of any kind.

Original copyright 2014 by Miko O'Sullivan. All rights reserved. This
program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself. This software comes with **NO
WARRANTY** of any kind.

# AUTHOR

Johan Vromans `jv@cpan.org`

Miko O'Sullivan `miko@idocs.com`, original version.
