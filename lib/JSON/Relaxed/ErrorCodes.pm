=head1 JSON::Relaxed Error Codes

When JSON::Relaxed encounters a parsing error it returns C<undef> and sets two
global variables: 

=over 4

=item * $JSON::Relaxed::err_id

C<$err_id> is a unique code for a specific error.

=item * $JSON::Relaxed::err_msg

C<$err_msg> is an English description of the code.  It would be cool to migrate
towards multi-language support for C<$err_msg>.

When using object-oriented mode, these can be easily retrieved using
the parser methods err_id() and err_msg().

=back

Following is a list of all error codes in JSON::Relaxed:

=over 4

=item * C<missing-parameter>

The string to be parsed was not sent to $parser->decode(). For example:

    $parser->decode()

=item * C<undefined-input>

The string to be parsed is undefined. For example:

    $parser->decode(undef)

=item * C<zero-length-input>

The string to be parsed is zero-length. For example:

    $parser->decode('')

=item * C<space-only-input>

The string to be parsed has no content beside space characters. For example:

    $parser->decode('   ')

=item * C<no-content>

The string to be parsed has no content. This error is slightly different than
C<space-only-input> in that it is triggered when the input contains only
comments, like this:

    $parser->decode('/* whatever */')

=item * C<unclosed-inline-comment>

A comment was started with /* but was never closed. For example:

    $parser->decode('/*')

=item * C<invalid-structure-opening-character>

The document opens with an invalid structural character like a comma or colon.
The following examples would trigger this error.

    $parser->decode(':')
    $parser->decode(',')
    $parser->decode('}')
    $parser->decode(']')

=item * C<multiple-structures>

The document has multiple structures. JSON and RJSON only allow a document to
consist of a single hash, a single array, or a single string. The following
examples would trigger this error.

    $parse->decode('{}[]')
    $parse->decode('{} "whatever"')
    $parse->decode('"abc" "def"')

=item * C<unknown-token-after-key>

A hash key may only be followed by the closing hash brace or a colon. Anything
else triggers C<unknown-token-after-key>. So, the following examples would
trigger this error.

    $parse->decode("{a [ }") }
    $parse->decode("{a b") }

=item * C<unknown-token-for-hash-key>

The parser encountered something besides a string where a hash key should be.
The following are examples of code that would trigger this error.

    $parse->decode('{{}}')
    $parse->decode('{[]}')
    $parse->decode('{]}')
    $parse->decode('{:}')

=item * C<unclosed-hash-brace>

A hash has an opening brace but no closing brace. For example:

    $parse->decode('{x:1')

=item * C<unclosed-array-brace>

An array has an opening brace but not a closing brace. For example:

    $parse->decode('["x", "y"')

=item * C<unexpected-token-after-colon>

In a hash, a colon must be followed by a value. Anything else triggers this
error. For example:

    $parse->decode('{"a":,}')
    $parse->decode('{"a":}')

=item * C<missing-comma-between-array-elements>

In an array, a comma must be followed by a value, another comma, or the closing
array brace.  Anything else triggers this error. For example:

    $parse->decode('[ "x" "y" ]')
    $parse->decode('[ "x" : ]')

=item * C<unknown-array-token>

This error exists just in case there's an invalid token in an array that
somehow wasn't caught by C<missing-comma-between-array-elements>. This error
shouldn't ever be triggered.  If it is please L<let me know|JSON::Relaxed/"AUTHOR">.

=item * C<unclosed-quote>

This error is triggered when a quote isn't closed. For example:

    $parse->decode("'whatever")
    $parse->decode('"whatever') }

=back

=cut

#
# error codes
#------------------------------------------------------------------------------
