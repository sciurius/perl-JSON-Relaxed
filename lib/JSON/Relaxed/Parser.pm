#! perl

use v5.26;
use Object::Pad;
use utf8;

class JSON::Relaxed::Parser;

field $data    :mutator;
field @chars;
field @tokens;

field $extra_tokens_ok	   :mutator :param = undef;
field $unknown_token_char  :mutator :param = "~";

our $err_id;
our $err_msg;

method decode( $str ) {

    $data = $str;
    return $self->error('undefined-input')   unless defined $data;
    return $self->error('zero-length-input') unless length $data;
    return $self->error('space-only-input')  unless $data =~ /\S/;

    undef $err_id;
    undef $err_msg;

    $self->parse_chars;
    $self->tokenize;
    return $self->error('no-content') unless @tokens;
    $self->structure( top => 1 );
}

# Legacy.
*parse = \&decode;

method parse_chars() {

    @chars = split( m/ (
			   \\.		# escaped char
		       |   \r\n		# CRLF
		       |   \r		# CR
		       |   \n		# newline
		       |   \/\/		# line comment
		       |   \/\*		# comment start
		       |   \*\/		# comment end
		       |   [,:{}\[\]]	# reserved chars
		       |   \s		# whitespace char
		       |   .		# any char
		       ) /sx, $data );

    # remove empty strings
    @chars = grep {length($_)} @chars;

}

method tokenize() {

    # create own array of characters
    my @chars = @chars;
    @tokens = ();
    my $offset = 0;

    # loop through characters
    while ( @chars ) {
	my $char = shift(@chars);

	# // - line comment
	# remove everything up to and including the end of line
	if ( $char eq '//' ) {
	    $offset += length($char);
	    while ( @chars ) {
		my $next = shift(@chars);
		$offset += length($next);

		# if character is any of the end of line strings
		last if $self->is_newline($next);
	    }
	}

	# /* */ - inline comments
	# remove everything until */
	elsif ( $char eq '/*' ) {
	    my $next = '';
	    while (@chars) {
		$next = shift(@chars);
		$offset += length($next);
		# if character is any of the end of line strings
		last if $next eq '*/';
	    }
	    # if we get this far then the comment was never closed
	    return $self->error('unclosed-inline-comment')
	      unless $next eq '*/';
	}

	# white space: ignore
	elsif ( $char !~ /\S/ ) {
	    $offset += length($char);
	}

	# reserved characters
	elsif ( $self->is_reserved($char) ) {
	    $self->newtok( $char, 'C', $offset );
	    $offset += length($char);
	}

	# quotes
	# remove everything until next quote of same type
	elsif ( $self->is_quote($char) ) {
	    my ( $str, $off ) = $self->quoted_string( $char, \@chars );
	    $self->error('unclosed-quote') unless defined $str;
	    $self->newtok( $str, 'Q', $offset );
	    $offset = $off;
	}

	# "unknown" object string
	elsif ( $char eq $unknown_token_char ) {
	    $self->newtok( $unknown_token_char, '?', $offset );
	}

	# else it's an unquoted string
	else {
	    my ( $str, $off ) = $self->unquoted_string( $char, \@chars );
	    $self->newtok( $str, 'U', $offset );
	    $offset = $off;
	}
    }
}

method is_reserved ($c) {

    #    '['  beginning of array
    #    ']'  end of array
    #    '{'  beginning of hash
    #    '}'  end of hash
    #    ':'  delimiter between name and value of hash element
    #    ','  separator between elements in hashes and arrays

    $c =~ /^[[\]{}:,]$/;
}

method is_newline ($c) {
    $c =~ /^(?:\r\n|\r|\n|\\\n)$/;
}

method is_quote ($c) {
    $c =~ /^['"`]$/;
}

method newtok( $tok, $typ, $off ) {

    push( @tokens,
	  JSON::Relaxed::Parser::Token->new( token  => $tok,
					     type   => $typ,
					     offset => $off ) );
}

method structure( %opts ) {

    my $opener = $opts{opener} // shift(@tokens) // return;
    my $rv;

    if ( $opener->is_string ) { # (un)quoted string
	$rv = $opener->as_perl;
    }
    elsif ( $opener->token eq '{' ) {
	$rv = $self->build_hash;
    }
    elsif ( $opener->token eq '[' ) {
	$rv = $self->build_array;
    }
    else {
	return $self->error( 'invalid-structure-opening-character',
			     $opener );
    }

    if ( $opts{top}
	 && !$self->is_error
	 && @tokens
	 && !$extra_tokens_ok
       ) {
	return $self->error('multiple-structures');
    }

    return $rv;
}

method chars() { \@chars }
method tokens() { \@tokens }

method error( $id, $aux = undef ) {
    require JSON::Relaxed::ErrorCodes;
    $err_id = $id;
    $err_msg = JSON::Relaxed::ErrorCodes->message( $id, $aux );

    return;			# undef
}

method is_error() {
    $err_id;
}

method err_id() {
    $err_id;
}

method err_msg() {
    $err_msg;
}

method dump_tokens() {
    use DDP; p(@tokens);
}

method build_hash() {

    my $rv = {};

    while ( @tokens ) {
	my $next = shift(@tokens);
	# what is allowed after opening brace:
	#	closing brace
	#	comma
	#	string

	# if closing brace, return
	return $rv if $next->token eq '}';

	# if comma, do nothing
	next if $next->token eq ',';

	# string
	# If the token is a string then it is a key. The token after that
	# should be a value.
	if ( $next->is_string ) {
	    my ( $key, $value, $t0 );
	    $t0 = $tokens[0];

	    # set key using string
	    $key = $next->as_perl( always_string => 1 );

	    # if anything follows the string
	    last unless defined $t0;

	    # a comma or closing brace is acceptable after a string
	    next if $t0->token eq ','|| $t0->token eq '}';

	    # if next token is a colon then it should be followed by a value
	    if ( $t0->token eq ':' ) {
		# remove the colon
		shift(@tokens);

		# if at end of token array, exit loop
		last unless @tokens;

		# get hash value
		$value = $self->get_value;

		# if there is a global error, return undef
		return undef if $self->is_error;
	    }

	    # anything else is an error
	    else {
		return $self->error('unknown-token-after-key', $t0 );
	    }

	    # set key and value in return hash
	    $rv->{$key} = $value;
	}

	# anything else is an error
	else {
	    return $self->error('unknown-token-for-hash-key', $next );
	}
    }

    # if we get this far then unclosed brace
    return $self->error('unclosed-hash-brace');

}

method get_value() {

    # get next token
    my $next = shift(@tokens);

    # next token must be string, array, or hash
    # string
    if ( $next->is_string ) {
	return $next->as_perl;
    }

    # token opens a hash
    elsif ($next->is_list_opener ) {
	return $self->structure( opener => $next );
    }

    # at this point it's an illegal token
    return $self->error('unexpected-token-after-colon', $next );
}

method build_array() {

    my $rv = [];

    # build array
    # work through tokens until closing brace
    while ( @tokens ) {
	my $next = shift(@tokens);

	# closing brace: we're done building this array
	return $rv if $next->token eq ']';

	# opening of hash or array
	if ( $next->is_list_opener ) {
	    my $object = $self->structure( opener => $next );
	    defined($object) or return undef;
	    push @$rv, $object;
	}

	# Comma: if we get to a comma at this point, and we have
	# content, do nothing with it
	elsif ( $next->token eq ',' && @$rv ) {
	}

	# if string, add it to the array
	elsif ( $next->is_string ) {
	    # add the string to the array
	    push @$rv, $next->as_perl();

	    # Check following token.
	    if ( @tokens > 1 ) {
		my $n2 = $tokens[1] || '';

		# Spec say: Commas are optional between objects pairs
		# and array items.
		# The next element must be a comma or the closing brace,
		# or a string or list.
		# Anything else is an error.
		unless ( $n2->token eq ','
			 || $n2->token eq ']'
			 || $n2->is_string
			 || $n2->is_list_opener ) {
		    return $self->error( 'missing_comma-between-array-elements',
					 $n2 );
		}
	    }
	}

	# else unkown object or character, so throw error
	else {
	    return $self->error( 'unknown-array-token', $next );
	}
    }

    # if we get this far then unclosed brace
    return $self->error('unclosed-array-brace');
}

method quoted_string( $char, $chars ) {
    my $res = JSON::Relaxed::Parser::Token::String::Quoted->new( quote => $char )
      ->build( $char, $chars );
    return unless defined $res;
    return ( $res, length($data)-length(join('',@$chars)) );
}

method unquoted_string( $char, $chars ) {
    my $res = JSON::Relaxed::Parser::Token::String::Unquoted->new
	->build( $char, $chars );
    return unless defined $res;
    return ( $res, length($data)-length(join('',@$chars)) );
}

method is_comment_opener( $char ) {
    $char eq '//' || $char eq '/*';
}

class JSON::Relaxed::Parser::Token
  :isa(JSON::Relaxed::Parser);

field $token  :accessor :param;
field $type   :accessor :param;
field $offset :accessor :param;

method is_string() {
    $type =~ /[QU]/
}

method is_list_opener() {
    $type eq 'C' && $token =~ /[{\[]/;
}

method as_perl( %options ) {
    return $token->as_perl(%options) if $token->can("as_perl");
    $token;
}

method _data_printer( $ddp ) {
    my $res = "Token(\"$token\", $type, $offset)";
    $res;
}

class JSON::Relaxed::Parser::Token::String
  :isa(JSON::Relaxed::Parser);

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

class JSON::Relaxed::Parser::Token::String::Quoted
  :isa(JSON::Relaxed::Parser::Token::String);

field $quote :accessor :param;
field $raw   :accessor;

my %esc = (
    'b'   => "\b",    #  Backspace
    'f'   => "\f",    #  Form feed
    'n'   => "\n",    #  New line
    'r'   => "\r",    #  Carriage return
    't'   => "\t",    #  Tab
    'v'   => chr(11), #  Vertical tab
);

method build( $char, $chars ) {

    $raw = '';

    # loop through remaining characters until we find another quote
    while ( @$chars ) {
	my $next = shift(@$chars);

	# if this is the matching quote, we're done
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

	# add to raw
	$raw .= $next;
    }

    # if we get this far then we never found the closing quote
    return;
}

method as_perl( %options ) {
    $raw;
}

class JSON::Relaxed::Parser::Token::String::Unquoted
  :isa(JSON::Relaxed::Parser::Token::String);

field $raw   :accessor;

# Values for reserved strings.
my %boolean = (
    null  => undef,
    true  => 1,
    false => 0,
);

method build( $char, $chars ) {

    $raw = "";
    unshift( @$chars, $char );

    # loop while not space or reserved characters
    while ( @$chars ) {
	my $next = $chars->[0];
	# if reserved character, we're done
	last if $self->is_reserved($next);

	# if space character, we're done
	last if $next !~ /\S/;

	# if opening of a comment, we're done
	last if $self->is_comment_opener($next);

	if ( $next eq '\u' ) {
	    $next = $self->decode_uescape($chars) // 'u';
	}

	# add to raw string
	$raw .= $next;
	shift(@$chars);
    }

    # return
    return $self;
}

method as_perl( %options ) {

    return $raw if $options{always_string};
    exists( $boolean{lc $raw} ) ? $boolean{lc $raw} : $raw;

}

1;
