#! perl

package JSON::Relaxed::Parser;
use v5.26;

use JSON::Relaxed; our $VERSION = $JSON::Relaxed::VERSION;

# Global: error codes.
our $err_id;
our $err_msg;

=head1 NAME

JSON::Relaxed::Parser -- The guts of JSON::Relaxed parser

=head1 SYNOPSIS

    my $parser = JSON::Relaxed::Parser->new(%options);
    $hash = $parser->decode($rjson);

Or, more convenient:

    my $parser = JSON::Relaxed->new(%options);
    $hash = $parser->decode($rjson);

=head1 CONSTRUCTOR OPTIONS

A call to new() may pass options as a hash, i.e. I<key> B<< => >> I<value> pairs.

=head2 extra_tokens_ok

If C<extra_tokens_ok> is true then the
C<multiple-structures> error isn't triggered and the parser returns the first
structure it finds.

For example, the following code returns undef and
sets the C<multiple-structures> error:

    $structure = $parser->decode('{"x":1} []');

However, by setting C<multiple-structures> to true, a hash structure is
returned, the extra code after that first hash is ignored, and no error is set.

=head2 unknown

The C<unknown> option sets the character which creates the
L<unknown object|/"JSON::Relaxed::Parser::Token::Unknown">. The unknown object
exists only for testing this mopdule. It has no purpose in production use.

    my $parser = JSON::Relaxed::Parser->new( unknown => '~' );

=head1 METHODS

=head2 extra_tokens_ok()

C<extra_tokens_ok()> sets/gets the C<extra_tokens_ok> property.

=cut

sub extra_tokens_ok {
    my ($parser) = @_;

    # set value
    if (@_ > 1) {
	$parser->{'extra_tokens_ok'} = $_[1] ? 1 : 0;
    }

    # return
    return $parser->{'extra_tokens_ok'} ? 1 : 0;
}

=head2 err_id()

When JSON::Relaxed encounters a parsing error it returns C<undef> and
sets two global variables: C<$err_id> and C<$err_msg>,
see L<below|/"Error-codes">.

C<err_id()> provides a convenient way to retrieve the global error id.

=head2 err_msg()

When JSON::Relaxed encounters a parsing error it returns C<undef> and
sets two global variables: C<$err_id> and C<$err_msg>,
see L<below|/"Error-codes">.

C<err_msg()> provides a convenient way to retrieve the global error message.

=cut

sub err_id  { $err_id  }
sub err_msg { $err_msg }

# =head1 INTERNALS
# 
# The following documentation is for if you want to edit the code of
# C<JSON::Relaxed> itself.
# 
# =head2 JSON::Relaxed
# 
# C<JSON::Relaxed> is the parent package. Not a lot actually happens in
# C<JSON::Relaxed>, it mostly contains L<from_rjson()|/from_rjson()> and
# definitions of various structures.
# 
# Each error codes is set in only one place in JSON::Relaxed.
#
# =over 4
# 
# =cut
# 
# #
# # JSON::Relaxed POD
# #------------------------------------------------------------------------------
# 
# #------------------------------------------------------------------------------
# # special character and string definitions
# #
# 
# =item Special character and string definitions
# 
# The following hashes provide information about characters and strings that have
# special meaning in RJSON.
# 
# =over 4
# 
# =item * Escape characters
# 
# The C<%esc> hash defines the six escape characters in RJSON that are
# changed to single characters.
# 
# =cut

# Escape characters.
our %esc = (
    'b'   => "\b",    #  Backspace
    'f'   => "\f",    #  Form feed
    'n'   => "\n",    #  New line
    'r'   => "\r",    #  Carriage return
    't'   => "\t",    #  Tab
    'v'   => chr(11), #  Vertical tab
);

# =item * Structural characters
# 
# The C<%structural> hash defines the six characters in RJSON that define
# the structure of the data object.
# 
# =cut

# Structural characters.
our %structural = (
    '[' => 1, # beginning of array
    ']' => 1, # end of array
    '{' => 1, # beginning of hash
    '}' => 1, # end of hash
    ':' => 1, # delimiter between name and value of hash element
    ',' => 1, # separator between elements in hashes and arrays
);

# =item * Quotes
# 
# The C<%quotes> hash defines the types of quotes recognized by RJSON: single
# and double quotes, and backticks.
# JSON only allows the use of double quotes to define strings.
# 
# =cut

# Quotes.
our %quotes = (
    '"' => 1,
    "'" => 1,
    "`" => 1,
);

# =item * End of line characters
# 
# End of line characters are used to detect the end of // comments.
# The C<%newlines> hash defines the three ways a line can end in a RJSON
# document. Lines in Windows text files end with carriage-return newline
# ("\r\n").  Lines in Unixish text files end with newline ("\n").
# Note that an escaped newline also terminates a comment.
# Lines in some operating systems end with just carriage returns ("\n").
# 
# =cut

# Newline tokens.
our %newlines = (
    "\r\n" => 1,
    "\r"   => 1,
    "\n"   => 1,
    "\\\n" => 1,
);

# =item * Boolean
# 
# The C<%boolean> hash defines strings that are boolean values: true, false, and
# null. (OK, 'null' isn't B<just> a boolean value, but I couldn't think of what
# else to call this hash.) 
# 
# =cut

# Boolean values.
our %boolean = (
    'null' => undef,
    'true' => 1,
    'false' => 0,
);

sub new {
    my ($class, %opts) = @_;
    my $parser = bless({}, $class);

    # TESTING
    # println subname(); ##i

    # "unknown" object character
    if ( exists $opts{unknown} ) {
	$parser->{unknown} = delete $opts{unknown};
    }
    # Allow extra tokens.
    if ( exists $opts{extra_tokens_ok} ) {
	$parser->{extra_tokens_ok} = delete $opts{extra_tokens_ok};
    }
    if ( keys %opts ) {
	Carp::croak('Unprocessed options: ' . join(" ", sort keys %opts));
    }

    # return
    return $parser;
}

=head2 error( $id, $msg )

Sets the global error code and message and returns undef.

=cut

sub error {
    my ($parser, $id, $msg) = @_;

    # set errors
    $err_id = $id;
    $err_msg = $msg;

    return undef;
}

=head2 is_error()

Returns true if there is an error, false otherwise.

=cut

sub is_error {
    my ($parser) = @_;

    # return true if there is an error, false otherwise
    return !!$err_id;
}

=head2 is_string($object)

Returns true if the object is a string object, i.e. in the class
C<JSON::Relaxed::Parser::Token::String>.

=cut

sub is_string {
    my ($parser, $object) = @_;
    return $object->isa('JSON::Relaxed::Parser::Token::String');
}

=head2 is_struct_char($object)

Returns true if the token is one of the structural (reserved) characters.

=cut

sub is_struct_char {
    my ($parser, $object) = @_;

    # if it's a reference, it's not a structural character
    if (ref $object) {
	return 0;
    }

    # else if the object is defined
    elsif (defined $object) {
	return $structural{$object};
    }

    # else whatever it is it isn't a structural character
    else {
	return 0;
    }
}

=head2 is_unknown_char($object)

Returns true if the token is the
L<unknown character|/"JSON::Relaxed::Parser::Token::Unknown">.

=cut

sub is_unknown_char {
    my ($parser, $char) = @_;

    # if there even is a "unknown" character
    if (defined $parser->{'unknown'}) {
	if ($char eq $parser->{'unknown'})
	    { return 1 }
    }

    # it's not the "unknown" character
    return 0;
}

=head2 is_list_opener($object)

Returns true if the object is the opening character for a hash or an
array, i.e. it is one of the characters C<{> and C<[>,

=cut

sub is_list_opener {
    my ($parser, $token) = @_;

    # if not defined, return false
    if (! defined $token)
	{ return 0 }

    # if it's an object, return false
    if (ref $token)
	{ return 0 }

    # opening brace for hash
    if ($token eq '{')
	{ return 1 }

    # opening brace for array
    if ($token eq '[')
	{ return 1 }

    # it's not a list opener
    return 0;
}

=head2 is_comment_opener($object)

Returns true if the objectis the opening character for a comment,
i.e. it is one of the following two couplets: C</*> and C<//>.

=cut

sub is_comment_opener {
    my ($parser, $token) = @_;

    # if not defined, return false
    if (! defined $token)
	{ return 0 }

    # if it's an object, return false
    if (ref $token)
	{ return 0 }

    # opening inline comment
    if ($token eq '/*')
	{ return 1 }

    # opening line comment
    if ($token eq '//')
	{ return 1 }

    # it's not a comment opener
    return 0;
}

=head2 decode($data)

C<decode()> is the method that does the work of parsing the RJSON string.
It returns the data structure that is defined in the RJSON string.

C<decode()> does not take any options.

=cut

sub decode {
    my ($parser, $raw) = @_;
    my (@chars, @tokens, $rv);

    # clear global error information
    undef $err_id;
    undef $err_msg;

    # must have at least two params
    if (@_ < 2) {
	return $parser->error(
	    'missing-parameter',
	    'the string to be parsed was not sent to $parser->decode()'
	)
    }

    # $raw must be defined
    if (! defined $raw) {
	return $parser->error(
	    'undefined-input',
	    'the string to be parsed is undefined'
	);
    }

    # $raw must not be an empty string
    if ($raw eq '') {
	return $parser->error(
	    'zero-length-input',
	    'the string to be parsed is zero-length'
	);
    }

    # $raw must have content
    if ($raw !~ m|\S|s) {
	return $parser->error(
	    'space-only-input',
	    'the string to be parsed has no content beside space characters'
	);
    }

    # get characters
    @chars = $parser->parse_chars($raw);

    # get tokens
    @tokens = $parser->tokenize(\@chars);

    # special case: entire structure is a single scalar
    # NOTE: Some versions of JSON do not allow a single scalar as an entire
    # JSON document.
    #if (@tokens == 1) {
    #	# if single scalar is a string
    #	if ( $parser->is_string($tokens[0]) )
    #		{ return $tokens[0]->as_perl() }
    #}

    # must be at least one token
    if (! @tokens) {
	return $parser->error(
	    'no-content',
	    'the string to be parsed has no content'
	)
    }

    # build structure
    $rv = $parser->structure(\@tokens, top=>1);
}

# For compatibility with pre-0.50.
*parse = \&decode;

=head2 parse_chars($srting)

C<parse_chars()> parses the RJSON string into either individual characters
or two-character couplets.

This method returns an array. The only input is the
raw RJSON string. So, for example, the following string:

    $raw = qq|/*x*/["y"]|;
    @chars = $parser->parse_chars($raw);

would be parsed into the following array:

    ( "/*", "x", "*/", "[", "\"", "y", "\""", "]" )

Most of the elements in the array are single characters. However, comment
delimiters, escaped characters, and Windows-style newlines are parsed as
two-character couplets.

C<parse_chars()> should not produce any fatal errors.

=cut

sub parse_chars {
    my ($parser, $raw) = @_;
    my (@rv);

    # clear global error information
    undef $err_id;
    undef $err_msg;

    # split on any of the following couplets, or on single characters
    #   \{any character}
    #   \r\n
    #   //
    #   /*
    #   */
    #   {any character}
    @rv = split( m/(\\.|\r\n|\r|\n|\/\/|\/\*|\*\/|,|:|{|}|\[|\]|\s|.)/s, $raw );

    # remove empty strings
    @rv = grep {length($_)} @rv;

    # return
    return @rv;
}

=head2 tokenize($chars)

C<tokenize()> organizes the characters from
C<L<parse_chars()|/"parse_chars()">> into a list of tokens.
The tokens can then be organized into a data structure with
C<L<structure()|/"structure()">>.

Each token represents an item that is recognized by JSON. Those items include
structural characters such as C<{> or C<}>, or strings such as
C<"hello world">. Comments and insignificant whitespace are filtered out
by C<tokenize()>.

For example:

    $raw = qq|/*x*/ ["y"]|;
    @chars = $parser->parse_chars($raw);
    @tokens = $parser->tokenize(\@chars);

would produce a list of tokens like this:

    (
        '[',
        JSON::Relaxed::Parser::Token::String::Quoted=HASH(0x20bf0e8),
        ']'
    )

Strings are tokenized into string objects.  When the parsing is complete they
are returned as scalar variables, not objects.

C<tokenize()> should not produce any fatal errors.

=cut

sub tokenize {
    my ($parser, $chars_org) = @_;
    my (@chars, @tokens);

    # TESTING
    # println subname(); ##i

    # create own array of characters
    @chars = @$chars_org;

    # TESTING
    # println '[', join('] [', @chars), ']';

    # loop through characters
    CHAR_LOOP:
    while (@chars) {
	my $char = shift(@chars);

	# // - line comment
	# remove everything up to and including the end of line
	if ($char eq '//') {
	    LINE_COMMENT_LOOP:
	    while (@chars) {
		my $next = shift(@chars);

		# if character is any of the end of line strings
		if ($newlines{$next})
		    { last LINE_COMMENT_LOOP }
	    }
	}

	# /* */ - inline comments
	# remove everything until */
	elsif ($char eq '/*') {
	    INLINE_COMMENT_LOOP:
	    while (@chars) {
		my $next = shift(@chars);

		# if character is any of the end of line strings
		if ($next eq '*/')
		    { next CHAR_LOOP }
	    }

	    # if we get this far then the comment was never closed
	    return $parser->error(
		'unclosed-inline-comment',
		'a comment was started with /* but was never closed'
	    );
	}

	# /* */ - inline comments
	# remove everything until */
	elsif ($char eq '/*') {
	    INLINE_COMMENT_LOOP:
	    while (@chars) {
		my $next = shift(@chars);

		# if character is any of the end of line strings
		if ($next eq '*/')
		    { last INLINE_COMMENT_LOOP }
	    }
	}

	# white space: ignore
	elsif ($char =~ m|\s+|) {
	}

	# structural characters
	elsif ( $structural{$char}) {
	    push @tokens, $char;
	}

	# quotes
	# remove everything until next quote of same type
	elsif ($quotes{$char}) {
	    my $str = $parser->quoted_string($char, \@chars);
	    push @tokens, $str;
	}

	# "unknown" object string
	elsif ($parser->is_unknown_char($char)) {
	    my $unknown = $parser->unknown_char($char);
	    push @tokens, $unknown;
	}

	# else it's an unquoted string
	else {
	    my $str = $parser->unquoted_string($char, \@chars);
	    push @tokens, $str;
	}
    }

    # return tokens
    return @tokens;
}

sub quoted_string {
    my ( $self, $char, $chars ) = @_;
    JSON::Relaxed::Parser::Token::String::Quoted->new($self, $char, $chars);
}
sub unquoted_string {
    my ( $self, $char, $chars ) = @_;
    JSON::Relaxed::Parser::Token::String::Unquoted->new($self, $char, $chars);
}
sub unknown_char {
    my ( $self, $char ) = @_;
    JSON::Relaxed::Parser::Token::Unknown->new($char);
}

=head2 structure( $tokens, %options )

C<structure()> organizes the tokens from L<tokenize()|/"tokenize()">
into a data structure.  C<$parser->structure()> returns a single string, single
array reference, a single hash reference, or (if there are errors) undef.

The initial token to be processed is the first token from the list,
unless specified in %options with key C<opener>.

=cut

sub structure {
    my ($parser, $tokens, %opts) = @_;
    my ($rv, $opener);

    # get opening token
    if (defined $opts{'opener'})
	{ $opener = $opts{'opener'} }
    else
	{ $opener = shift(@$tokens) }

    # if no opener that's an error, so we're done
    if (! defined $opener)
	{ return undef }

    # string
    if ($parser->is_string($opener)) {
	$rv = $opener->as_perl();
    }

    # opening of hash
    elsif ($opener eq '{') {
	$rv = $parser->build_hash($tokens);
    }

    # opening of array
    elsif ($opener eq '[') {
	$rv = $parser->build_array($tokens);
    }

    # else invalid opening character
    else {
	return $parser->error(
	    'invalid-structure-opening-character',
	    'expected { or [ but got ' .
	    $parser->invalid_token($opener) . ' ' .
	    'instead'
	);
    }

    # If this is the outer structure, and there are any tokens left, then
    # that's a multiple structure document.  We don't allow that sort of thing
    # around here unless extra_tokens_ok is explicitly set to ok
    if ($opts{'top'}) {
	if (! $parser->is_error) {
	    if (@$tokens) {
		unless ($parser->extra_tokens_ok()) {
		    return $parser->error(
		    	'multiple-structures',
		    	'the string being parsed contains two separate structures, only one is allowed'
		    );
		}
	    }
	}
    }

    # return
    return $rv;
}

sub build_hash {
    my ( $self, $tokens ) = @_;
    JSON::Relaxed::Parser::Structure::Hash->build($self, $tokens);
}
sub build_array {
    my ( $self, $tokens ) = @_;
    JSON::Relaxed::Parser::Structure::Array->build($self, $tokens);
}

sub invalid_token {
    my ($parser, $token) = @_;

    # string
    if ($parser->is_string($token)) {
	return 'string';
    }

    # object
    elsif (ref $token) {
	return ref($token) . ' object';
    }

    # scalar
    else {
	return $token;
    }
}

################################################################

package JSON::Relaxed::Parser::Structure::Hash;
use strict;

=head1 JSON::Relaxed::Parser::Structure::Hash

This package processes parsed tokens into hash structures.

The package is not loaded or instantiated and contains only
static menthods.

=over 4

=item build( $parser, $tokens )

This static method accepts the array of tokens and works through them building
the hash reference that they represent. When C<build()> reaches the closing
curly brace (C<}>) it returns the hash reference.

=cut

sub build {
    my ($class, $parser, $tokens) = @_;
    my $rv = {};

    # build hash
    # work through tokens until closing brace
    TOKENLOOP:
    while (@$tokens) {
	my $next = shift(@$tokens);
	# what is allowed after opening brace:
	#	closing brace
	#	comma
	#	string

	# if closing brace, return
	if ($next eq '}') {
	    return $rv;
	}

	# if comma, do nothing
	elsif ($next eq ',') {
	}

	# string
	# If the token is a string then it is a key. The token after that
	# should be a value.
	elsif ( $parser->is_string($next) ) {
	    my ($key, $value, $t0);
	    $t0 = $tokens->[0];

	    # set key using string
	    $key = $next->as_perl(always_string=>1);

	    # if anything follows the string
	    if (defined $t0) {
		# if next token is a colon then it should be followed by a value
		if ( $t0 eq ':' ) {
		    # remove the colon
		    shift(@$tokens);

		    # if at end of token array, exit loop
		    @$tokens or last TOKENLOOP;

		    # get hash value
		    $value = $class->get_value($parser, $tokens);

		    # if there is a global error, return undef
		    $parser->is_error() and return undef;
		}

		# a comma or closing brace is acceptable after a string
		elsif ($t0 eq ',') {
		}
		elsif ($t0 eq '}') {
		}

		# anything else is an error
		else {
		    return $parser->error(
		    	'unknown-token-after-key',
		    	'expected comma or closing brace after a ' .
		    	'hash key, but got ' .
		    	$parser->invalid_token($t0) . ' ' .
		    	'instead'
		    );
		}
	    }

	    # else nothing followed the string, so break out of token loop
	    else {
		last TOKENLOOP;
	    }

	    # set key and value in return hash
	    $rv->{$key} = $value;
	}

	# anything else is an error
	else {
	    return $parser->error(
		'unknown-token-for-hash-key',
		'expected string, comma, or closing brace in a ' .
		'hash key, but got ' .
		$parser->invalid_token($next) . ' ' .
		'instead'
	    );
	}
    }

    # if we get this far then unclosed brace
    return $parser->error(
	'unclosed-hash-brace',
	'do not find closing brace for hash'
    );
}

=item get_value( $parser, $tokens )

This static method gets the value of a hash element. This method is
called after a hash key is followed by a colon. A colon must be
followed by a value. It may not be followed by the end of the tokens,
a comma, or a closing brace.

=cut

sub get_value {
    my ($class, $parser, $tokens) = @_;
    my ($next);

    # TESTING
    # println subname(); ##i

    # get next token
    $next = shift(@$tokens);

    # next token must be string, array, or hash
    # string
    if ($parser->is_string($next)) {
	return $next->as_perl();
    }

    # token opens a hash
    elsif ($parser->is_list_opener($next)) {
	return $parser->structure($tokens, opener=>$next);
    }

    # at this point it's an illegal token
    return $parser->error(
	'unexpected-token-after-colon',
	'expected a value after a colon in a hash, got ' .
	$parser->invalid_token($next) . ' ' .
	'instead'
    );
}

=back

=cut

################################################################

package JSON::Relaxed::Parser::Structure::Array;
use strict;

=head1 JSON::Relaxed::Parser::Structure::Array

This package parses Relaxed into array structures.

The package is not loaded or instantiated and contains only
static menthods.

=over 4

=item build( $parser, $tokens )

This static method accepts the array of tokens and works through them
building the array reference that they represent. When C<build()>
reaches the closing square brace (C<]>) it returns the array
reference.

=cut

sub build {
    my ($class, $parser, $tokens) = @_;
    my $rv = [];

    # TESTING
    # println subname(); ##i

    # build array
    # work through tokens until closing brace
    while (@$tokens) {
	my $next = shift(@$tokens);

	# closing brace: we're done building this array
	if ($next eq ']') {
	    return $rv;
	}

	# opening of hash or array
	elsif ($parser->is_list_opener($next)) {
	    my $object = $parser->structure($tokens, opener=>$next);
	    defined($object) or return undef;
	    push @$rv, $object;
	}

	# Comma: if we get to a comma at this point, and we have
	# content, do nothing with it
	elsif ( $next eq ',' && @$rv ) {
	}

	# if string, add it to the array
	elsif ($parser->is_string($next)) {
	    # add the string to the array
	    push @$rv, $next->as_perl();

	    # Check following token.
	    if ( @$tokens ) {
		my $n2 = $tokens->[0] || '';

		# Spec say: Commas are optional between objects pairs
		# and array items.
		# The next element must be a comma or the closing brace,
		# or a string or list.
		# Anything else is an error.
		unless ( $n2 eq ','
			 || $n2 eq ']'
			 || $parser->is_string($n2)
			 || $parser->is_list_opener($n2) ) {
		    return missing_comma($parser, $n2);
		}
	    }
	}

	# else unkown object or character, so throw error
	else {
	    return invalid_array_token($parser, $next);
	}
    }

    # if we get this far then unclosed brace
    return $parser->error(
	'unclosed-array-brace',
	'do not find closing brace for array'
    );
}

sub missing_comma {
    my ($parser, $token) = @_;

    # initialize error message
    return $parser->error(
	'missing-comma-between-array-elements',
	'expected comma or closing array brace, got ' .
	$parser->invalid_token($token) . ' ' .
	'instead'
    );
}

sub invalid_array_token {
    my ($parser, $token) = @_;

    # initialize error message
    return $parser->error(
	'unknown-array-token',
	'unexpected item in array: got ' .
	$parser->invalid_token($token)
    );
}

=back

=cut

################################################################

package JSON::Relaxed::Parser::Token::String;
use strict;

=head1 JSON::Relaxed::Parser::Token::String

Base class for JSON::Relaxed::Parser::Token::String::Quoted and
JSON::Relaxed::Parser::Token::String::Unquoted.

=over 4

=item decode_uescape($chars)

Decodes unicode escapes like \u201D.
Handles surrogates.

Extension: Also handles \u{1d10e} escapes.

=cut

sub decode_uescape {
    my ( $self, $chars ) = @_;
    my $next = $chars->[0];

    return unless $next eq '\u' && @$chars >= 5;

    if ( $chars->[1] eq '{' ) { # extended
	my $i = 2;
	my $u = "";
	while ( $i < @$chars ) {
	    if ( $chars->[$i] =~ /[[:xdigit:]]/ ) {
		$u .= $chars->[$i];
		$i++;
		next;
	    }
	    if ( $chars->[$i] eq '}' ) {
		splice( @$chars, 0, $i );
		return chr(hex($u));
	    }
	    last;
	}
	return;
    }

    # Let's not be too relaxed -- require exactly 4 hexits.
    my $u = join('',@$chars[1..4]);
    if ( $u =~ /^[[:xdigit:]]+$/ ) {
	splice( @$chars, 0, 4 );
	if ( $u =~ /^d[89ab][[:xdigit:]]{2}/i # utf-16 HI
	     && @$chars >= 6 && $chars->[1] eq '\u' ) {
	    my $utf16hi = $u;
	    $u = join('',@$chars[2..5]);
	    if ( $u =~ /^d[c-f][[:xdigit:]]{2}/i ) { # utf-16 LO
		splice( @$chars, 0, 5 );
		return $self->assemble_surrogate( $utf16hi, $u );
	    }
	    else {
		return chr(hex($u));
	    }
	}
	else {
	    return chr(hex($u));
	}
    }

    return;
}

sub assemble_surrogate {
    my ( $self, $hi, $lo ) = @_;
    pack('U*', 0x10000 + (hex($hi) - 0xD800) * 0x400 + (hex($lo) - 0xDC00) );
}

=back

=cut

################################################################

package JSON::Relaxed::Parser::Token::String::Quoted;
use strict;

use parent qw( -norequire JSON::Relaxed::Parser::Token::String );

=head1 JSON::Relaxed::Parser::Token::String::Quoted

A C<JSON::Relaxed::Parser::Token::String::Quoted> object represents a string
in the document that is delimited with quotes.

The objects are created by C<< $parser->tokenize() >> when it works
through the array of characters in the document.

A C<Quoted> object has the following properties:

=over 4

=item C<raw>

The string that is inside the quotes.  If the string contained any
escape characters then the escapes are processed and the unescaped characters
are in C<raw>. So, for example, C<\n> would become an actual newline.

=item C<quote>

The delimiting quote, i.e. either a single quote or a double quote.
In case of a combined string, the delimeter quote of the I<final>
string part.

=back

Methods:

=over 4

=item C<new()>

C<new()> instantiates a C<JSON::Relaxed::Parser::Token::String::Quoted> object
and slurps in all the characters in the characters array until it gets to the
closing quote.

If the string is followed by optional whitespace, a backslash, a
newline, optional whitespace and another string, the latter string
will be appended to the current string. In other words,

    "foo" \
    "bar"

will produce a single string, C<"foobar">.

C<new()> returns the new C<Quoted> object.

=cut

sub new {
    my ($class, $parser, $quote, $chars) = @_;
    my $str = bless({}, $class);

    # initialize hash
    $str->{'quote'} = $quote;
    $str->{'raw'} = '';

    # loop through remaining characters until we find another quote
    CHAR_LOOP:
    while (@$chars) {
	my $next = shift(@$chars);

	# if this is the matching quote, we're done
	if ($next eq $str->{'quote'}) {
	    # However, if the quote is followed by [ws] \ \n [ws]
	    # and a new quote, append the new string.
	    # TODO: This probably only works with newline, not CR or CRLF...
	    if ( @$chars > 2 ) {
		my $i = 0;
		while ( $i < @$chars && $chars->[$i] =~ /^\s$/ ) {
		    $i++;
		}
		if ( $chars->[$i] eq "\\\n" ) {
		    $i++;
		    while ( $i < @$chars && $chars->[$i] =~ /^\s$/ ) {
			$i++;
		    }
		    if ( $quotes{$chars->[$i]} ) {
			$str->{quote} = $chars->[$i];
			splice( @$chars, 0, $i+1 );
			next;
		    }
		}
	    }
	    return $str;
	}

	# if leading slash, check if it's a special escape character
	if ($next =~ s|^\\(.)|$1|s) {
	    if ($esc{$next}) {
		$next = $esc{$next};
	    }
	    # \uXXXX escapes.
	    elsif ( $next eq 'u' ) {
		unshift( @$chars, '\u' );
		$next = $str->decode_uescape($chars) // 'u';
		shift(@$chars);
	    }
	}

	# add to raw
	$str->{'raw'} .= $next;
    }

    # if we get this far then we never found the closing quote
    return $parser->error(
	'unclosed-quote',
	'string does not have closing quote before end of file'
    );
}

=item C<as_perl()>

C<as_perl()> returns the string that was in quotes (without the quotes).

=cut

sub as_perl {
    my ($str) = @_;
    return $str->{'raw'};
}

=back

=cut

################################################################

package JSON::Relaxed::Parser::Token::String::Unquoted;
use strict;

use parent qw( -norequire JSON::Relaxed::Parser::Token::String );

=head1 JSON::Relaxed::Parser::Token::String::Unquoted

A C<JSON::Relaxed::Parser::Token::String::Unquoted> object represents a string
in the document that was not delimited with quotes.

C<Unquoted> objects are created by C<< $parser->tokenize() >> when it works
through the array of characters in the document.

An C<Unquoted> object has the following properties:

=over 4

=item  C<raw>

The string. Escaped characters are resolved in C<raw>.

=back

Methods:

=over 4

=item C<new()>

C<new()> instantiates a C<JSON::Relaxed::Parser::Token::String::Unquoted>
object and slurps in all the characters in the characters array until it gets
to a space character, a comment, or one of the structural characters such as
C<{> or C<:>.

=cut

sub new {
    my ($class, $parser, $char, $chars) = @_;
    my $str = bless({}, $class);

    # TESTING
    # println subname(); ##i

    # initialize hash
    $str->{'raw'} = "";
    unshift( @$chars, $char );

    # loop while not space or structural characters
    TOKEN_LOOP:
    while (@$chars) {
	my $next = $chars->[0];
	# if structural character, we're done
	if ($structural{$next})
	    { last TOKEN_LOOP }

	# if space character, we're done
	if ($next =~ m|\s+|s)
	    { last TOKEN_LOOP }

	# if opening of a comment, we're done
	if ($parser->is_comment_opener($next))
	    { last TOKEN_LOOP }

	if ( $next eq '\u' ) {
	    $next = $str->decode_uescape($chars) // 'u';
	}

	# add to raw string
	$str->{'raw'} .= $next;
	shift(@$chars);
    }

    # return
    return $str;
}

=item C<as_perl(%options)>

C<as_perl()> returns the unquoted string or a boolean value, depending on how
it is called.

If the string is a boolean value, i.e. I<true>, I<false>, then the C<as_perl>
return 1 (for true), 0 (for false) or undef (for null), B<unless> the
C<always_string> option is sent, in which case the string itself is returned.
If the string does not represent a boolean value then it is returned as-is.

=cut

sub as_perl {
    my ($str, %opts) = @_;
    my $rv = $str->{'raw'};

    # if string is one of the unquoted boolean values
    # unless options indicate to always return the value as a string, check it
    # the value is one of the boolean string
    unless ($opts{'always_string'}) {
	if (exists $boolean{lc $rv}) {
	    $rv = $boolean{lc $rv};
	}
    }

    # return
    return $rv;
}

=back

=cut

################################################################

package JSON::Relaxed::Parser::Token::Unknown;
use strict;

=head1 JSON::Relaxed::Parser::Token::Unknown

This class is just used for development of JSON::Relaxed. It has no use in
production. This class allows testing for when a token is an unknown object.

To implement this class, add the 'unknown' option to JSON::Relaxed->new(). The
value of the option should be the character that creates an unknown object.
For example, the following option sets the tilde (~) as an unknown object.

    my $parser = JSON::Relaxed::Parser->new(unknown=>'~');

The "unknown" character must not be inside quotes or inside an unquoted string.

Methods:

=over 4

=item C<new()>

=cut

sub new {
    my ($class, $char) = @_;
    my $unknown = bless({}, $class);
    $unknown->{'raw'} = $char;
    return $unknown;
}

=back

=head1 Error codes

When JSON::Relaxed encounters a parsing error it returns C<undef> and sets two
global variables: 

=over 4

=item * $JSON::Relaxed::Parser::err_id

C<$err_id> is a unique code for a specific error.  Every code is set in only
one place in JSON::Relaxed.

=item * $JSON::Relaxed::Parser::err_msg

C<$err_msg> is an English description of the code.  It would be cool to migrate
towards multi-language support for C<$err_msg>.

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
shouldn't ever be triggered.  If it is please L<let me know|/AUTHOR>.

=item * C<unclosed-quote>

This error is triggered when a quote isn't closed. For example:

    $parse->decode("'whatever")
    $parse->decode('"whatever') }

=back

=head1 COMPATIBILITY WITH PRE-0.05 VERSION

The old parser method C<parse> is renamed to C<decode>.
C<parse> is kept as a synonym for C<decode>.

=head1 SEE ALSO

This module is part of L<JSON::Relaxed>.

=cut
