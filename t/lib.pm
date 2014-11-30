##i!/usr/bin/perl -w
use strict;
use FileHandle;

# debugging
# use Debug::ShowStuff ':all';
# use Debug::ShowStuff::ShowVar;


#------------------------------------------------------------------------------
# eval_error
#
sub eval_error {
	my ($expected, $code) = @_;
	my ($object);
	
	# TESTING
	# println subname(); ##i
	
	# run code
	$object = &$code();
	
	# should not have a structure at this point
	if (defined $object) {
		set_ok(0, 'should not have defined structure');
	}
	
	# should have error id
	if (! $JSON::Relaxed::err_id) {
		set_ok(0, '$JSON::Relaxed::err_id should be true but is not');
	}
	
	# error should have given id
	if ($JSON::Relaxed::err_id ne $expected) {
		set_ok(
			0,
			'got error ' .
			"\n\t($JSON::Relaxed::err_id): $JSON::Relaxed::err_msg\n" .
			'but expected error:' .
			"\n\t$expected"
		);
	}
	
	# all ok
	set_ok(1);
}
#
# eval_error
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
# error_not_ok
#
sub error_from_rjson {
	if ($JSON::Relaxed::err_id) {
		set_ok(0, $JSON::Relaxed::err_id . ': ' . $JSON::Relaxed::err_msg);
	}
	else {
		set_ok(1);
	}
}
#
# error_not_ok
#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
# comp
#
sub comp {
	my ($is, $shouldbe) = @_;
	
	# TESTING
	# println subname(); ##i
	
	if(! equndef($is, $shouldbe)) {
		showvar $is;
		showvar $shouldbe;
		
		print STDERR 
			"\n",
			"\tis:         ", (defined($is) ?       $is       : '[undef]'), "\n",
			"\tshould be : ", (defined($shouldbe) ? $shouldbe : '[undef]'), "\n\n";
		set_ok(0, 'values do not match');
	}
	
	else {
		set_ok(1);
	}
}
#
# comp
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
# slurp
#
sub slurp {
	my ($path) = @_;
	my $in = FileHandle->new($path);
	$in or die $!;
	return join('', <$in>);
}
#
# slurp
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
# arr_comp
#
sub arr_comp {
	my ($alpha_sent, $beta_sent, %opts) = @_;
	my (@alpha, @beta);
	
	# TESTING
	# println subname(); ##i
	
	# both must be array references
	unless (
		UNIVERSAL::isa($alpha_sent, 'ARRAY') &&
		UNIVERSAL::isa($beta_sent, 'ARRAY')
		)
		{ die 'both params must be array references' }
	
	# if they have different lengths, they're different
	if (@$alpha_sent != @$beta_sent)
		{ set_ok(0) }
	
	# get arrays to use for comparison
	@alpha = @$alpha_sent;
	@beta = @$beta_sent;
	
	# if order insensitive
	if ($opts{'order_insensitive'}) {
		@alpha = sort @alpha;
		@beta = sort @beta;
	}
	
	# if case insensitive
	if ($opts{'case_insensitive'}) {
		grep {$_ = lc($_)} @alpha;
		grep {$_ = lc($_)} @beta;
	}
	
	# loop through array elements
	for (my $i=0; $i<=$#alpha; $i++) { ##i
		# if one is undef but other isn't
		if (
			( (  defined $alpha[$i]) && (! defined $beta[$i]) ) ||
			( (! defined $alpha[$i]) && (  defined $beta[$i]) )
			) {
			set_ok(0);
		}
		
		# if $alpha[$i] is undef then both must be, so they're the same
		elsif (! defined $alpha[$i]) {
		}
		
		# both are defined
		else {
			unless ($alpha[$i] eq $beta[$i])
				{ set_ok(0) }
		}
	}
	
	# if we get this far, they're the same
	set_ok(1);
}
#
# arr_comp
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
# check_isa
#
sub check_isa {
	my ($ob, $class) = @_;
	set_ok(UNIVERSAL::isa $ob, $class);
}
#
# check_isa
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
# equndef
#
sub equndef {
	my ($str1, $str2) = @_;
	
	# if both defined
	if ( defined($str1) && defined($str2) )
		{return $str1 eq $str2}
	
	# if neither are defined 
	if ( (! defined($str1)) && (! defined($str2)) )
		{return 1}
	
	# only one is defined, so return false
	return 0;
}
#
# equndef
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
# token_check
#
sub token_check {
	my ($rjson, $should) = @_;
	my ($parser, @chars, @tokens);
	
	# TESTING
	# println subname(); ##i
	
	# parse
	$parser = JSON::Relaxed::Parser->new();
	@chars = $parser->parse_chars($rjson);
	
	# should not be any errors
	error_from_rjson();
	
	# get tokens
	@tokens = $parser->tokenize(\@chars);
	
	# should not be any errors
	error_from_rjson();
	
	# should only be one token
	set_ok(@tokens == 1);
	
	# should be a JSON::Relaxed::Parser::Token::String object
	check_isa($tokens[0], 'JSON::Relaxed::Parser::Token::String');
	
	# value of str should be xyz
	comp($tokens[0]->{'raw'}, $should);
}
#
# token_check
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
# stringify_tokens
#
sub stringify_tokens {
	my (@orgs) = @_;
	my (@rv);
	
	# loop through original tokens and build array of string versions
	foreach my $org (@orgs) {
		if (UNIVERSAL::isa $org, 'JSON::Relaxed::Parser::Token::String') {
			push @rv, $org->{'raw'};
		}
		else {
			push @rv, $org;
		}
	}
	
	# return new array
	return @rv;
}
#
# stringify_tokens
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
# set_ok
#
sub set_ok {
	my ($ok, $msg) = @_;
	
	# TESTING
	# println subname(); ##i
	
	# development environment
	if ($ENV{'IDOCSDEV'}) {
		if ($ok) {
			return 1;
		}
		
		else {
			if (! defined $msg)
				{ $msg = 'unspecific not-ok' }
			
			die($msg);
		}
	}
	
	# else regular ok
	ok($ok);
}
#
# set_ok
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
# key_count
#
sub key_count {
	my ($hash, $count) = @_;
	
	unless (scalar(keys %$hash) == $count) {
		set_ok(
			0,
			'hash should have ' .
			$count . ' ' .
			'element' .
			( ($count == 1) ? '' : 's' ) . ' ' .
			'but actually has ' .
			scalar(keys %$hash)
		);
	}
	
	set_ok(1);
}
#
# key_count
#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
# el_count
#
sub el_count {
	my ($arr, $count) = @_;
	
	unless (scalar(@$arr) == $count) {
		set_ok(
			0,
			'array should have ' .
			$count . ' ' .
			'element' .
			( ($count == 1) ? '' : 's' ) . ' ' .
			'but actually has ' .
			scalar(@$arr)
		);
	}
	
	set_ok(1);
}
#
# el_count
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
# show_rjson_err
#
sub show_rjson_err {
	if ($JSON::Relaxed::err_id) {
		print
			$JSON::Relaxed::err_id, ': ',
			$JSON::Relaxed::err_msg, "\n\n";
	}
}
#
# show_rjson_err
#------------------------------------------------------------------------------



# return true
1;
