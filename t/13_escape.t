# perl

use v5.26;

use Test::More tests => 3;
use JSON::Relaxed;
use utf8;

binmode STDOUT => ':utf8';
binmode STDERR => ':utf8';
my $p = JSON::Relaxed::Parser->new;

my $e = chr(92);

is( $p->parse( '"' . $e. 't"' ), "\t", 'TAB' );
diag( $p->is_error ) if $p->is_error;
is( $p->parse( '"' . $e.$e. 't"' ), $e."t", $e.'t' );
diag( $p->is_error ) if $p->is_error;
is( $p->parse( '"' . $e.$e. 'v"' ), $e."v", $e."v" );
diag( $p->is_error ) if $p->is_error;
