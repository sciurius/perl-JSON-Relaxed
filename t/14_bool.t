# perl

use v5.26;

use Test::More tests => 23;
use JSON::Relaxed;
use utf8;
use Scalar::Util qw(reftype);

binmode STDOUT => ':utf8';
binmode STDERR => ':utf8';
my $p = JSON::Relaxed::Parser->new;

my $r = $p->parse( q{true} );
diag( $p->is_error ) if $p->is_error;

isa_ok( $r, 'JSON::Boolean' );
is( $r, $JSON::Boolean::true, "is JSON::Boolean::true" );
is( "".$r, "true", "is true" );
is( 0+$r, 1, "is 1" );

$r = $p->parse( q{false} );
diag( $p->is_error ) if $p->is_error;

isa_ok( $r, 'JSON::Boolean' );
is( $r, $JSON::Boolean::false, "is JSON::Boolean::false" );
is( "".$r, "false", "is false" );
is( 0+$r, 0, "is 0" );

$r = $p->parse( q{"true"} );
diag( $p->is_error ) if $p->is_error;

is( ref($r), '', "\"true\" is not a ref");
is( reftype($r), undef, "\"true\" is not an object");
is( $r, "true", "\"true\" is just \"true\"");

$r = $p->parse( q{"false"} );
diag( $p->is_error ) if $p->is_error;

is( ref($r), '', "\"false\" is not a ref");
is( reftype($r), undef, "\"false\" is not an object");
is( $r, "false", "\"false\" is just \"false\"");

$r = $p->parse( q{[ 0 null "null" false "false" true "true" ]} );
diag( $p->is_error ) if $p->is_error;
is( @$r, 7, "7 values");

is( $r->[0], 0, "0" );
is( $r->[1], undef, "undef" );
is( $r->[2], "null", "null" );
is( $r->[3], "false", "false" );
is( $r->[4], "false", "false" );
is( $r->[5], "true", "true" );
is( $r->[6], "true", "true" );

my $s = $p->encode( data => $r );

is( $s, q{[0,null,"null",false,"false",true,"true"]}, "pretty");
