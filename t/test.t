#!/usr/bin/perl -w
use strict;
use JSON::Relaxed ':all';

# go to test directory
BEGIN {
	use File::Spec;
	use File::Basename();
	my $thisf = File::Spec->rel2abs($0);
	my $thisd = File::Basename::dirname($thisf);
	chdir($thisd);
}

# load libraries
require './test-lib.pm';
use Test;
BEGIN { plan tests => 202 };

# debugging
# use Debug::ShowStuff ':all';
# use Debug::ShowStuff::ShowVar;


#------------------------------------------------------------------------------
##= parse_chars
#
if (1) { ##i
	my ($parser, $rjson, @got, @should);
	
	# instantiate parser
	$parser = JSON::Relaxed::Parser->new();
	
	# slurp in raw json
	$rjson = slurp('parse-chars.rjson');
	
	# parse
	@got = $parser->parse_chars($rjson);
	
	# should not be any errors
	error_from_rjson();
	
	# set array we should get
	@should = ("//", "a", "\r\n", "/*", "b", "*/", "\r\n", "[", "\"", "c", "\\d", "e", "\"", "]");
	
	# compare
	set_ok(arr_comp(\@got, \@should));
}
#
# parse_chars
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
##= line comment
#
if (1) { ##i
	my ($parser, $rjson, @chars, @tokens);
	
	$parser = JSON::Relaxed::Parser->new();
	$rjson = qq|//line\r\n|;
	@chars = $parser->parse_chars($rjson);
	
	# should not be any errors
	error_from_rjson();
	
	@tokens = $parser->tokenize(\@chars);
	
	# should not be any errors
	error_from_rjson();
	
	# should be empty array
	arr_comp(\@tokens, []);
}
#
# line comment
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
##= inline comment
#
if (1) { ##i
	my ($parser, $rjson, @chars, @tokens);
	
	$parser = JSON::Relaxed::Parser->new();
	$rjson = qq|/*comment*/|;
	@chars = $parser->parse_chars($rjson);
	
	# should not be any errors
	error_from_rjson();
	
	@tokens = $parser->tokenize(\@chars);
	
	# should not be any errors
	error_from_rjson();
	arr_comp(\@tokens, []);
}
#
# inline comment
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
##= unclosed inline comment
#
if (1) { ##i
	eval_error (
		'unclosed-inline-comment',
		sub { JSON::Relaxed::Parser->new()->parse("/*x\n") }
	);
}
#
# unclosed inline comment
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
##= string tokenization
#
if (1) { ##i
	# single quoted string
	token_check(q|'xyz'|, 'xyz');
	
	# double quoted string
	token_check(q|"xyz"|, 'xyz');
	
	# double quoted string
	token_check(q|"xyz"|, 'xyz');
	
	# special escape chars
	token_check(q|'\b\f\n\r\t\v'|, qq|\b\f\n\r\t| . chr(11));
	
	# misc escaped character
	token_check(q|'\x'|, 'x');
	
	# single quote in single quote
	token_check(q|'\''|, "'");
	
	# double quote in double quote
	token_check(q|"\""|, '"');
	
	# comment-like string in quotes
	token_check(q|"/*x*/"|, '/*x*/');
	
	# unquoted string
	if (1) { ##i
		my ($parser, $rjson, @chars, @tokens);
		
		$parser = JSON::Relaxed::Parser->new();
		$rjson = qq|abc/*whatever*/ \t xyz//aaa\n\n|;
		@chars = $parser->parse_chars($rjson);
		
		# should not be any errors
		error_from_rjson();
		
		@tokens = $parser->tokenize(\@chars);
		
		# should not be any errors
		error_from_rjson();
		
		# should only be one token
		set_ok(@tokens == 2);
		
		# should be a JSON::Relaxed::Parser::Token::String object
		check_isa($tokens[0], 'JSON::Relaxed::Parser::Token::String::Unquoted');
		check_isa($tokens[1], 'JSON::Relaxed::Parser::Token::String::Unquoted');
		
		# value of str should be xyz
		comp($tokens[0]->{'raw'}, 'abc');
		comp($tokens[1]->{'raw'}, 'xyz');
	}
}
#
# string tokenization
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
##= structural tokens
#
if (1) { ##i
	my ($parser, $rjson, @chars, @tokens);
	$parser = JSON::Relaxed::Parser->new();
	$rjson = q| {}  [] ,: |;
	@chars = $parser->parse_chars($rjson);
	
	# should not be any errors
	error_from_rjson();

	@tokens = $parser->tokenize(\@chars);
	
	# should not be any errors
	error_from_rjson();
	
	arr_comp(\@tokens, ['{', '}', '[', ']', ',', ':']);
}
#
# structural tokens
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
# invalid parse input
#
if (1) { ##i
	# no input
	eval_error (
		'missing-parameter',
		sub { JSON::Relaxed::Parser->new()->parse() }
	);
	
	# undefined input
	eval_error (
		'undefined-input',
		sub { JSON::Relaxed::Parser->new()->parse(undef) }
	);
	
	# zero-length input
	eval_error (
		'zero-length-input',
		sub { JSON::Relaxed::Parser->new()->parse('') }
	);
	
	# space-only input
	eval_error (
		'space-only-input',
		sub { JSON::Relaxed::Parser->new()->parse(" \t\r\n ") }
	);
}
#
# invalid parse input
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
##= document is a single string
#
if (1) { ##i
	my ($got);
	
	# single quoted string
	$got = JSON::Relaxed::Parser->new()->parse("'x'");
	error_from_rjson();
	comp($got, 'x');
	
	# double quoted string
	$got = JSON::Relaxed::Parser->new()->parse('"x"');
	error_from_rjson();
	comp($got, 'x');
	
	# unquoted non-boolean string
	$got = JSON::Relaxed::Parser->new()->parse('x');
	error_from_rjson();
	comp($got, 'x');
	
	# null
	$got = JSON::Relaxed::Parser->new()->parse('null');
	error_from_rjson();
	comp($got, undef);
	
	# NuLL
	$got = JSON::Relaxed::Parser->new()->parse('NuLL');
	error_from_rjson();
	comp($got, undef);
	
	# true
	$got = JSON::Relaxed::Parser->new()->parse('true');
	error_from_rjson();
	comp($got, 1);
	
	# TRuE
	$got = JSON::Relaxed::Parser->new()->parse(' TRuE ');
	error_from_rjson();
	comp($got, 1);
	
	# false
	$got = JSON::Relaxed::Parser->new()->parse(' false ');
	error_from_rjson();
	comp($got, 0);
	
	# FaLSE
	$got = JSON::Relaxed::Parser->new()->parse(' FaLSE ');
	error_from_rjson();
	comp($got, 0);
}
#
# document is a single string
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
##= multiple-structures
#
if (1) { ##i
	# two strings
	eval_error (
		'multiple-structures',
		sub { JSON::Relaxed::Parser->new()->parse('"x" "y"') }
	);
	
	# two strings with comma between them
	eval_error (
		'multiple-structures',
		sub { JSON::Relaxed::Parser->new()->parse('"x" , "y"') }
	);
	
	# a string then a structure
	eval_error (
		'multiple-structures',
		sub { JSON::Relaxed::Parser->new()->parse('"x" {}') }
	);
}
#
# multiple-structures
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
##= invalid-structure-opening-character
#
if (1) { ##i
	# colon
	eval_error (
		'invalid-structure-opening-character',
		sub { JSON::Relaxed::Parser->new()->parse(':') }
	);
	
	# comma
	eval_error (
		'invalid-structure-opening-character',
		sub { JSON::Relaxed::Parser->new()->parse(',') }
	);
	
	# closing }
	eval_error (
		'invalid-structure-opening-character',
		sub { JSON::Relaxed::Parser->new()->parse('}') }
	);
	
	# closing ]
	eval_error (
		'invalid-structure-opening-character',
		sub { JSON::Relaxed::Parser->new()->parse(']') }
	);
}
#
# invalid-structure-opening-character
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
##= unclosed brace
#
if (1) { ##i
	# unclosed array brace
	eval_error (
		'unclosed-array-brace',
		sub { JSON::Relaxed::Parser->new()->parse('["x", "y"') }
	);
	
	# unclosed hash brace
	eval_error (
		'unclosed-hash-brace',
		sub { JSON::Relaxed::Parser->new()->parse('{') }
	);
	
	# unclosed hash brace in nested hash
	eval_error (
		'unclosed-hash-brace',
		sub { JSON::Relaxed::Parser->new()->parse('{x:1, y:[]') }
	);
}
#
# unclosed brace
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
##= basic hash and array structures
#
if (1) {
	# hash
	do {
		my $struct = JSON::Relaxed::Parser->new()->parse('{ }');
		error_from_rjson();
		
		# should be hash
		check_isa($struct, 'HASH');
		
		# should be empty hash
		key_count($struct, 0);
	};
	
	# array
	do {
		my $struct = JSON::Relaxed::Parser->new()->parse('[ ]');
		error_from_rjson();
		
		# should be array
		check_isa($struct, 'ARRAY');
		
		# should be empty array
		el_count($struct, 0);
	};
}
#
# basic hash and array structures
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
##= extra comma in array
#
if (1) {
	my $struct = JSON::Relaxed::Parser->new()->parse('[  , , ]');
	error_from_rjson();
	
	# should be hash
	check_isa($struct, 'ARRAY');
	
	# should be empty hash
	comp(scalar(@$struct), 0);
}
#
# extra comma in array
#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
##= array with one element
#
if (1) {
	# quoted string
	do {
		my $array = JSON::Relaxed::Parser->new()->parse('[  "hello world" ]');
		error_from_rjson();
		
		# should be array
		check_isa($array, 'ARRAY');
		
		# should have one element
		comp(scalar(@$array), 1);
		
		# element should be 'hello world'
		comp($array->[0], 'hello world');
	};
	
	# true
	do {
		my $array = JSON::Relaxed::Parser->new()->parse('[  true ]');
		error_from_rjson();
		
		# should be array
		check_isa($array, 'ARRAY');
		
		# should have one element
		comp(scalar(@$array), 1);
		
		# element should be 1
		comp($array->[0], 1);
	};
	
	# false
	do {
		my $array = JSON::Relaxed::Parser->new()->parse('[  false ]');
		error_from_rjson();
		
		# should be array
		check_isa($array, 'ARRAY');
		
		# should have one element
		comp(scalar(@$array), 1);
		
		# element should be 0
		comp($array->[0], 0);
	};
	
	# null
	do {
		my $array = JSON::Relaxed::Parser->new()->parse('[  null ]');
		error_from_rjson();
		
		# should be array
		check_isa($array, 'ARRAY');
		
		# should have one element
		comp(scalar(@$array), 1);
		
		# element should be 0
		comp($array->[0], undef);
	};
	
	# unquoted non-boolean string
	do {
		my $array = JSON::Relaxed::Parser->new()->parse('[x]');
		error_from_rjson();
		
		# should be array
		check_isa($array, 'ARRAY');
		
		# should have one element
		comp(scalar(@$array), 1);
		
		# element should be 0
		comp($array->[0], 'x');
	};
}
#
# array with one element
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
##= unknown object for testing
#
if (1) {
	my ($parser, @chars, @tokens);
	
	# instantiate
	$parser = JSON::Relaxed::Parser->new(unknown=>'~');
	comp($parser->{'unknown'}, '~');
	
	# parse
	@chars = $parser->parse_chars('~');
	error_from_rjson();
	@tokens = $parser->tokenize(\@chars);
	error_from_rjson();
	comp(scalar(@tokens), 1);
	
	# check that the first element is an unknown object
	check_isa($tokens[0], 'JSON::Relaxed::Parser::Token::Unknown')
}
#
# unknown object for testing
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
##= missing comma between array elements
#
if (1) {
	# colon in array
	eval_error (
		'unknown-array-token',
		sub { JSON::Relaxed::Parser->new()->parse('[ : ]') }
	);
	
	# contiguous strings
	eval_error (
		'missing-comma-between-array-elements',
		sub { JSON::Relaxed::Parser->new()->parse('[ "x" "y" ]') }
	);
	
	# invalid character after element
	eval_error (
		'missing-comma-between-array-elements',
		sub { JSON::Relaxed::Parser->new()->parse('[ "x" : ]') }
	);
	
	# unknown token
	eval_error (
		'unknown-array-token',
		sub { JSON::Relaxed::Parser->new(unknown=>'~')->parse('[ ~ ]') }
	);
}
#
# missing comma between array elements
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
##= nested array
#
if (1) {
	my ($array, $nested);
	
	# get array
	$array = JSON::Relaxed::Parser->new()->parse('[ [ null, "x" ] ]');
	error_from_rjson();
	
	# if error, show it
	error_from_rjson();
	
	# array should be defined
	if (! defined $array)
		{ die 'did not get defined array' }
	
	# should have array with one element
	check_isa($array, 'ARRAY');
	comp(scalar(@$array), 1);
	
	# nested array should have two elements
	$nested = $array->[0];
	check_isa($nested, 'ARRAY');
	comp(scalar(@$nested), 2);
	comp($nested->[0], undef);
	comp($nested->[1], 'x');
}
#
# nested array
#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
##= extra comma in hash
#
if (1) {
	my $struct = JSON::Relaxed::Parser->new()->parse('{  , }');
	error_from_rjson();
	
	# should be hash
	check_isa($struct, 'HASH');
	
	# should be empty hash
	key_count($struct, 0);
}
#
# extra comma in hash
#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
##= invalid token after colon
#
if (1) {
	# comma
	do { ##i
		eval_error (
			'unexpected-token-after-colon',
			sub { JSON::Relaxed::Parser->new()->parse('{"a":,}') }
		);
	};
	
	# end of hash
	do { ##i
		eval_error (
			'unexpected-token-after-colon',
			sub { JSON::Relaxed::Parser->new()->parse('{"a":}') }
		);
	};
}
#
# invalid token after colon
#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
##= colon last token
#
if (1) {
	eval_error (
		'unclosed-hash-brace',
		sub { JSON::Relaxed::Parser->new()->parse('{"a":') }
	);
}
#
# colon last token
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
##= no hash value after colon
#
if (1) { ##i
	my $hash = JSON::Relaxed::Parser->new()->parse(q|{"a", 'b':2}|);
	error_from_rjson();
	
	# should be hash with two elements
	check_isa($hash, 'HASH');
	key_count(\%$hash, 2);
	
	# check values
	comp($hash->{'a'}, undef);
	comp($hash->{'b'}, 2);
}
#
# no hash value after colon
#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
##= unclosed quote
#
if (1) { ##i
	# single quote
	eval_error (
		'unclosed-quote',
		sub { JSON::Relaxed::Parser->new()->parse("'whatever") }
	);
	
	# double quote
	eval_error (
		'unclosed-quote',
		sub { JSON::Relaxed::Parser->new()->parse('"whatever') }
	);
}
#
# unclosed quote
#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
##= unexpected token where there should be a hash key
#
if (1) {
	# another hash
	eval_error (
		'unknown-token-for-hash-key',
		sub { JSON::Relaxed::Parser->new()->parse('{{}}') }
	);
	
	# an array
	eval_error (
		'unknown-token-for-hash-key',
		sub { JSON::Relaxed::Parser->new()->parse('{[]}') }
	);
	
	# closing brace for array
	eval_error (
		'unknown-token-for-hash-key',
		sub { JSON::Relaxed::Parser->new()->parse('{]}') }
	);
	
	# colon
	eval_error (
		'unknown-token-for-hash-key',
		sub { JSON::Relaxed::Parser->new()->parse('{:}') }
	);
	
	# end of tokens
	eval_error (
		'unclosed-hash-brace',
		sub { JSON::Relaxed::Parser->new()->parse('{') }
	);
	
	# end of tokens in nested hash
	eval_error (
		'unclosed-hash-brace',
		sub { JSON::Relaxed::Parser->new()->parse('{"x":{') }
	);
}
#
# unexpected token where there should be a hash key
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
##= unexpected token where there should be a hash value
#
if (1) {
	eval_error (
		'unclosed-hash-brace',
		sub { JSON::Relaxed::Parser->new()->parse('{"x":') }
	);
}
#
# unexpected token where there should be a hash value
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
##= multiple-structures
#
if (1) {
	# extra structure
	eval_error (
		'multiple-structures',
		sub { JSON::Relaxed::Parser->new()->parse('{}[]') }
	);
	
	# extra string
	eval_error (
		'multiple-structures',
		sub { JSON::Relaxed::Parser->new()->parse('{}"whatever"') }
	);
}
#
# multiple-structures
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
##= weird space characters
#
if (1) {
	my ($str, $array);
	
	# TESTING
	$str = qq|\b\f\n\r\t| . chr(11);
	
	# get array
	$array = JSON::Relaxed::Parser->new()->parse('[ $str ]');
	error_from_rjson();
	
	# if error, show it
	error_from_rjson();
	
	# should have empty array
	check_isa($array, 'ARRAY');
}
#
# weird space characters
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
##= unquoted boolean strings as keys are strings
#
if (1) {
	my ($hash);
	
	# get hash
	$hash = JSON::Relaxed::Parser->new()->parse('{null:null, true:true, false:false}');
	error_from_rjson();
	
	# "null" should exist and be undef
	set_ok(exists($hash->{'null'}));
	comp($hash->{'null'}, undef);
	
	# "true" and "false" should exist and be 1 and 0
	comp($hash->{'true'}, 1);
	comp($hash->{'false'}, 0);
}
#
# unquoted boolean strings as keys are strings
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
##= no content
#
if (1) {
	# inline comment only
	eval_error (
		'no-content',
		sub { JSON::Relaxed::Parser->new()->parse("/*x*/  ") }
	);
	
	# line comment only
	eval_error (
		'no-content',
		sub { JSON::Relaxed::Parser->new()->parse("//xxx\n\n") }
	);
}
#
# no content
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
##= full test
#
if (1) {
	my $full = JSON::Relaxed::Parser->new()->parse(slurp('full.rjson'));
	error_from_rjson();
	check_isa($full, 'HASH');
	check_isa($full->{'hash'}, 'HASH');
	check_isa($full->{'array'}, 'ARRAY');
	set_ok(exists($full->{'hash-key-with-no-value'}));
	el_count($full->{'array'}, 3);
}
#
# full test
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
##= full test
#
if (1) {
	my $full = from_rjson(slurp('full.rjson'));
	error_from_rjson();
	check_isa($full, 'HASH');
	check_isa($full->{'hash'}, 'HASH');
	check_isa($full->{'array'}, 'ARRAY');
	set_ok(exists($full->{'hash-key-with-no-value'}));
	el_count($full->{'array'}, 3);
}
#
# full test
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
##= unknown-token-after-key
#
if (1) { ##i
	eval_error (
		'unknown-token-after-key',
		sub { JSON::Relaxed::Parser->new()->parse("{a [ }") }
	);
	
	eval_error (
		'unknown-token-after-key',
		sub { JSON::Relaxed::Parser->new()->parse("{a b") }
	);
}
#
# unknown-token-after-key
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
##= from_rjson
#
if (1) {
	my $full = from_rjson(slurp('full.rjson'));
	error_from_rjson();
	check_isa($full, 'HASH');
	check_isa($full->{'hash'}, 'HASH');
	check_isa($full->{'array'}, 'ARRAY');
	set_ok(exists($full->{'hash-key-with-no-value'}));
	el_count($full->{'array'}, 3);
}
#
# from_rjson
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
##= extra_tokens_ok
#
if (1) { ##i
	my ($parser, $struct);
	
	# get parser
	$parser = JSON::Relaxed::Parser->new();
	
	# initial extra_tokens_ok should be 0
	comp($parser->extra_tokens_ok(), 0);
	
	# set extra_tokens_ok to true
	$parser->extra_tokens_ok(1);
	comp($parser->extra_tokens_ok(), 1);
	
	# parse rjson with multiple strings
	$struct = $parser->parse('"abc" "whatever"');
	error_from_rjson();
	comp($struct, 'abc');
	
	# parse rjson with extra hash
	$struct = $parser->parse('{x:11}{j=2}');
	error_from_rjson();
	comp($struct->{'x'}, 11);
	
	# parse rjson with extra string
	$struct = $parser->parse('{x:112}"whatever"');
	error_from_rjson();
	comp($struct->{'x'}, 112);
	
	# set extra_tokens_ok back to false
	$parser->extra_tokens_ok(0);
	comp($parser->extra_tokens_ok(), 0);
	
	# should now get error when parsing with multiple strings
	eval_error (
		'multiple-structures',
		sub { $parser->parse('"abc" "whatever"') }
	);
	
	# should now get error when parsing with extra structures
	eval_error (
		'multiple-structures',
		sub { $parser->parse('{x:112}[]') }
	);
	
	# should now get error when parsing with extra string
	eval_error (
		'multiple-structures',
		sub { $parser->parse('{x:112}"whatever"') }
	);
}
#
# extra_tokens_ok
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
# done
#
if ($ENV{'IDOCSDEV'}) {
	print "[done]\n";
}
#
# done
#------------------------------------------------------------------------------
