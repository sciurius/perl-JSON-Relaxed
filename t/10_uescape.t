# perl

use v5.26;

use Test::More tests => 14;
use JSON::Relaxed;

my $p = JSON::Relaxed::Parser->new;

is( $p->parse( q{ "foo" } ),        "foo",     "simple string" );
is( $p->parse( q{ "fo\uo" } ),      "fouo",    "\\u" );
is( $p->parse( q{ "fo\u0o" } ),     "fou0o",   "\\u0" );
is( $p->parse( q{ "fo\u00o" } ),    "fou00o",  "\\u00" );
is( $p->parse( q{ "fo\u002o" } ),   "fou002o", "\\u002" );
is( $p->parse( q{ "fo\u002fo" } ),  "fo/o",    "\\u002f" );
is( $p->parse( q{  fo\u002fo  } ),  "fo/o",    "\\u002f unquoted" );
is( $p->parse( q{ "fo\u002fao" } ), "fo/ao",   "\\u002fa" );

is( $p->parse( q{ "fo\uoooooo" } ), "fouoooooo",    "\\u..." );
is( $p->parse( q{ "fo\u0ooooo" } ), "fou0ooooo",   "\\u0..." );
is( $p->parse( q{ "fo\u00oooo" } ), "fou00oooo",  "\\u00..." );
is( $p->parse( q{ "fo\u002ooo" } ), "fou002ooo", "\\u002..." );
is( $p->parse( q{ "fo\u002foo" } ), "fo/oo",    "\\u002f..." );
is( $p->parse( q{ "fo\u002fao" } ), "fo/ao",   "\\u002fa..." );
