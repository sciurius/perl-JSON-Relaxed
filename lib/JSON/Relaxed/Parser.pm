#! perl

use v5.26;
use Object::Pad;
use utf8;

class JSON::Relaxed::Parser;

# Instance data.
field $data    :mutator;	# RJSON string being parser
field @chars;			# string in character(duplets)
field @tokens;			# string as tokens

# Instance properties.
field $extra_tokens_ok	   :mutator :param = undef;
field $croak_on_error	   :mutator :param = 1;
field $croak_on_error_internal;
field $strict		   :mutator :param = 0;

# Error indicators.
field $err_id		    :accessor;
field $err_msg		    :accessor;

method decode( $str ) {

    $croak_on_error_internal = $croak_on_error;
    $data = $str;
    return $self->error('missing-input')
      unless defined $data && length $data;

    undef $err_id;
    undef $err_msg;

    $self->parse_chars;
    $self->tokenize;
    return $self->error('empty-input') unless @tokens;

    $self->structure( top => 1 );
}

# Legacy.
method parse( $str ) {
    $croak_on_error_internal = 0;
    $self->decode($str);
}

################ Character classifiers ################

# Reserved characters.
#    '['  beginning of array
#    ']'  end of array
#    '{'  beginning of hash
#    '}'  end of hash
#    ':'  delimiter between name and value of hash element
#    ','  separator between elements in hashes and arrays

my $p_reserved = qr/[,:{}\[\]]/;

method is_reserved ($c) {
    $c =~ /^$p_reserved$/;
}

# Newlines. CRLF (Windows), CR (MacOS) and newline (sane systems).

my $p_newlines = qr/(?:\r\n|\r|\n|\\\n)/;

method is_newline ($c) {
    $c =~ /^$p_newlines$/;
}

# Quotes. Single, double and backtick.

my $p_quotes = qr/["'`]/;

method is_quote ($c) {
    $c =~ /^$p_quotes$/;
}

# Numbers. A special case of unquoted strings.
my $p_number = qr/[+-]?\d*.?\d+(?:[Ee][+-]?\d+)?/;

method is_number ($c) {
    $c =~ /^$p_number$/;
}

method parse_chars() {

    @chars = split( m/ (
			   \\.		# escaped char
		       |   $p_newlines	# CRLF, CR, newline
		       |   \/\/		# line comment
		       |   \/\*		# comment start
		       |   \*\/		# comment end
		       |   $p_reserved	# reserved chars
		       |   .		# any char
		       ) /sox, $data );

    # remove empty strings
    @chars = grep {length($_)} @chars;

}

# Accessor for @chars.
method chars() { \@chars }

method tokenize() {

    @tokens = ();
    my $offset = 0;		# token offset in input

    # Loop through characters.
    while ( @chars ) {
	my $char = shift(@chars);

	# // - line comment
	# Remove everything up to and including the end of line.
	if ( $char eq '//' ) {
	    my $comment = "";
	    my $off = $offset;
	    $offset += length($char);
	    while ( @chars ) {
		my $next = shift(@chars);
		$offset += length($next);

		# if character is any of the end of line strings
		last if $self->is_newline($next);
		$comment .= $next;
	    }
	    #$self->addtok( $comment, 'LC', $off );
	}

	# /* */ - inline comments
	# Remove everything until */.
	elsif ( $char eq '/*' ) {
	    my $comment = "";
	    my $off = $offset;
	    my $next = '';
	    while (@chars) {
		$next = shift(@chars);
		$offset += length($next);
		if ( $next eq '*/' ) {
		    #$self->addtok( $comment, 'IC', $off );
		    last;
		}
		else {
		    $comment .= $next;
		}
	    }
	    # If we get this far then the comment was never closed.
	    return $self->error('unclosed-inline-comment')
	      unless $next eq '*/';
	}

	# White space: ignore.
	elsif ( $char !~ /\S/ ) {
	    $offset += length($char);
	}

	# Reserved characters.
	elsif ( $self->is_reserved($char) ) {
	    $self->addtok( $char, 'C', $offset );
	    $offset += length($char);
	}

	# Quotes
	# Remove everything until next quote of same type.
	elsif ( $self->is_quote($char) ) {
	    unshift( @chars, $char );
	    my ( $str, $off ) = $self->quoted_string;
	    $self->error('unclosed-quote') unless defined $str;
	    $self->addtok( $str, 'Q', $offset );
	    $offset = $off;
	}

	# Else it's an unquoted string.
	else {
	    unshift( @chars, $char );
	    my ( $str, $off ) = $self->unquoted_string;
	    $self->addtok( $str,
			   $self->is_number($str->content) ? 'N' : 'U',
			   $offset );
	    $offset = $off;
	}
    }
}

# Accessor for @tokens,
method tokens() { \@tokens }

# Add a new token to @tokens.
method addtok( $tok, $typ, $off ) {

    push( @tokens,
	  JSON::Relaxed::Parser::Token->new( token  => $tok,
					     type   => $typ,
					     offset => $off ) );
}

# Build the result structure out of the tokens.
method structure( %opts ) {

    my $this = shift(@tokens) // return;
    my $rv;

    if ( $this->is_string ) { # (un)quoted string
	$rv = $this->as_perl;
    }
    elsif ( $this->token eq '{' ) {
	$rv = $self->build_hash;
    }
    elsif ( $this->token eq '[' ) {
	$rv = $self->build_array;
    }
    else {
	return $self->error( 'invalid-structure-opening-character',
			     $this );
    }

    # If this is the outer structure, then no tokens should remain.
    if ( $opts{top}
	 && !$self->is_error
	 && @tokens
	 && !$extra_tokens_ok
       ) {
	return $self->error( 'multiple-structures', $tokens[0] );
    }

    return $rv;
}


method error( $id, $aux = undef ) {
    require JSON::Relaxed::ErrorCodes;
    $err_id = $id;
    $err_msg = JSON::Relaxed::ErrorCodes->message( $id, $aux );

    die( $err_msg, "\n" ) if $croak_on_error_internal;
    return;			# undef
}

method is_error() {
    $err_id;
}

# For debugging.
method dump_tokens() {
    my $tokens = \@tokens;
    return unless require DDP;
    if ( -t STDERR ) {
	DDP::p($tokens);
    }
    else {
	warn DDP::np($tokens), "\n";
    }
}

method build_hash() {

    my $rv = {};

    while ( @tokens ) {
	my $this = shift(@tokens);
	# What is allowed after opening brace:
	#	closing brace
	#	comma
	#	string

	# If closing brace, return.
	return $rv if $this->token eq '}';

	# If comma, do nothing.
	next if $this->token eq ',';

	# String
	# If the token is a string then it is a key. The token after that
	# should be a value.
	if ( $this->is_string ) {
	    my ( $key, $value );

	    # Set key using string.
	    $key = $this->as_perl( always_string => 1 );
	    $rv->{$key} = undef;

	    my $next = $tokens[0];
	    # If anything follows the string.
	    last unless defined $next;

	    # A comma or closing brace is acceptable after a string.
	    next if $next->token eq ',' || $next->token eq '}';

	    # If next token is a colon then it should be followed by a value.
	    if ( $next->token eq ':' ) {
		# Step past the colon.
		shift(@tokens);

		# If at end of token array, exit loop.
		last unless @tokens;

		# Get hash value.
		$value = $self->get_value;

		# If there is a global error, return undef.
		return undef if $self->is_error;
	    }

	    # Anything else is an error.
	    else {
		return $self->error('unknown-token-after-key', $next );
	    }

	    # Set key and value in return hash.
	    $rv->{$key} = $value;
	}

	# Anything else is an error.
	else {
	    return $self->error('unknown-token-for-hash-key', $this );
	}
    }

    # If we get this far then unclosed brace.
    return $self->error('unclosed-hash-brace');

}

method get_value() {

    # Get token.
    my $this = shift(@tokens);

    # Token must be string, array, or hash.

    # String.
    if ( $this->is_string ) {
	return $this->as_perl;
    }

    # Token opens a hash or array.
    elsif ( $this->is_list_opener ) {
	unshift( @tokens, $this );
	return $self->structure;
    }

    # At this point it's an illegal token.
    return $self->error('unexpected-token-after-colon', $this );
}

method build_array() {

    my $rv = [];

    # Build array. Work through tokens until closing brace.
    while ( @tokens ) {
	my $this = shift(@tokens);

	# Closing brace: we're done building this array.
	return $rv if $this->token eq ']';

	# Opening brace of hash or array.
	if ( $this->is_list_opener ) {
	    unshift( @tokens, $this );
	    my $object = $self->structure;
	    defined($object) or return undef;
	    push( @$rv, $object );
	}

	# Comma: if we get to a comma at this point, and we have
	# content, do nothing with it.
	elsif ( $this->token eq ',' && @$rv ) {
	}

	# if string, add it to the array
	elsif ( $this->is_string ) {
	    # add the string to the array
	    push( @$rv, $this->as_perl );

	    # Check following token.
	    if ( @tokens ) {
		my $next = $tokens[0] || '';
		# Spec say: Commas are optional between objects pairs
		# and array items.
		# The next element must be a comma or the closing brace,
		# or a string or list.
		# Anything else is an error.
		unless ( $next->token eq ','
			 || $next->token eq ']'
			 || $next->is_string
			 || $next->is_list_opener ) {
		    return $self->error( 'missing_comma-between-array-elements',
					 $next );
		}
	    }
	}

	# Else unkown object or character, so throw error.
	else {
	    return $self->error( 'unknown-array-token', $this );
	}
    }

    # If we get this far then unclosed brace.
    return $self->error('unclosed-array-brace');
}

method quoted_string() {
    my $res = JSON::Relaxed::Parser::Token::String::Quoted->new
      ->build(\@chars);
    return unless defined $res;
    return ( $res, length($data)-length(join('',@chars)) );
}

method unquoted_string() {
    my $res = JSON::Relaxed::Parser::Token::String::Unquoted->new
	->build(\@chars);
    return unless defined $res;
    return ( $res, length($data)-length(join('',@chars)) );
}

method is_comment_opener( $char ) {
    $char eq '//' || $char eq '/*';
}

################ Tokens ################

class JSON::Relaxed::Parser::Token :isa(JSON::Relaxed::Parser);

field $token  :accessor :param;
field $type   :accessor :param;
field $offset :accessor :param;

method is_string() {
    $type =~ /[QUN]/
}

method is_number() {
    $type eq 'N';
}

method is_list_opener() {
    $type eq 'C' && $token =~ /[{\[]/;
}

method is_comment() {
    $type =~ /[LI]C/;
    ...;			# catch accidental use
}

method as_perl( %options ) {	# for values

    return $token->as_perl(%options) if $token->can("as_perl");
    ...;			# reached?
    $token;
}

method _data_printer( $ddp ) {	# for DDP
    my $res = "Token(";
    if ( $self->is_string ) {
	$res .= $token->_data_printer($ddp);
    }
    else {
	$res .= "\"$token\"";
    }
    $res .= ", $type";
    $res . ", $offset)";
}

method as_string {		# for messages
    my $res = "";
    if ( $self->is_string ) {
	$res = '"' . ($token->content =~ s/"/\\"/gr) . '"';
    }
    else {
	$res .= "\"$token\"";
    }
    $res;
}

################ Strings ################

class JSON::Relaxed::Parser::Token::String :isa(JSON::Relaxed::Parser);

method decode_uescape( $chars ) {
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

    # Require exactly 4 hexits.
    my $u = join('',@$chars[1..4]);
    if ( $u =~ /^[[:xdigit:]]+$/ ) {
	splice( @$chars, 0, 4 );
	if ( $u =~ /^d[89ab][[:xdigit:]]{2}/i # utf-16 HI
	     && @$chars >= 6 && $chars->[1] eq '\u' ) {
	    my $hi = $u;
	    $u = join('',@$chars[2..5]);
	    if ( $u =~ /^d[c-f][[:xdigit:]]{2}/i ) { # utf-16 LO
		splice( @$chars, 0, 5 );
		return pack( 'U*',
			     0x10000 + (hex($hi) - 0xD800) * 0x400
			     + (hex($u) - 0xDC00) );
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

################ Quoted Strings ################

class JSON::Relaxed::Parser::Token::String::Quoted
  :isa(JSON::Relaxed::Parser::Token::String);

field $quote :accessor;
field $content   :accessor;

my %esc = (
    'b'   => "\b",    #  Backspace
    'f'   => "\f",    #  Form feed
    'n'   => "\n",    #  New line
    'r'   => "\r",    #  Carriage return
    't'   => "\t",    #  Tab
    'v'   => chr(11), #  Vertical tab
);

method build($chars) {

    $quote = shift(@$chars);
    $content = '';

    # Loop through remaining characters until we find another quote.
    while ( @$chars ) {
	my $next = shift(@$chars);

	# If this is the matching quote, we're done.
	if ( $next eq $quote ) {
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
		    if ( $self->is_quote($chars->[$i]) ) {
			$quote = $chars->[$i];
			splice( @$chars, 0, $i+1 );
			next;
		    }
		}
	    }
	    return $self;
	}

	# Check if it's a special escape character.
	if ( $next =~ s|^\\(.)|$1|s ) {
	    if ( $esc{$next} ) {
		$next = $esc{$next};
	    }
	    # \uXXXX escapes.
	    elsif ( $next eq 'u' ) {
		unshift( @$chars, '\u' );
		$next = $self->decode_uescape($chars) // 'u';
		shift(@$chars);
	    }
	}

	# Add to content.
	$content .= $next;
    }

    # If we get this far then we never found the closing quote.
    return;
}

method as_perl( %options ) {
    $content;
}

method _data_printer( $ddp ) {
    $quote . $content . $quote;
}

################ Unquoted Strings ################

class JSON::Relaxed::Parser::Token::String::Unquoted
  :isa(JSON::Relaxed::Parser::Token::String);

field $content   :accessor;

# Values for reserved strings.
my %boolean = (
    null  => undef,
    true  => 1,
    false => 0,
);

method build($chars) {

    $content = "";

    # Loop while not space or reserved characters.
    while ( @$chars ) {
	my $next = $chars->[0];

	# If reserved character, we're done.
	last if $self->is_reserved($next);

	# If space character, we're done.
	last if $next !~ /\S/;

	# If opening of a comment, we're done.
	last if $self->is_comment_opener($next);

	if ( $next eq '\u' ) {
	    $next = $self->decode_uescape($chars) // 'u';
	}

	# Add to content.
	$content .= $next;
	shift(@$chars);
    }

    # return
    return $self;
}

method as_perl( %options ) {

    return $content if $options{always_string};
    exists( $boolean{lc $content} ) ? $boolean{lc $content} : $content;

}

method _data_printer( $ddp ) {
    "<" . $content . ">";
}

################

1;
