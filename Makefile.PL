#! perl

use strict;
use warnings;

exit 0 if $ENV{AUTOMATED_TESTING} && $] < 5.026000;

use ExtUtils::MakeMaker 7.54;

# For binaries/scripts.
my @bins; # = qw( prog );

my %makemaker =
  ( 'ExtUtils::MakeMaker' => 7.54 );

my %prereq =
  ( 'Object::Pad'         => 0.78,
  );

my %w =
( NAME                => 'JSON::Relaxed',
  AUTHOR              => 'Johan Vromans <jv@cpan.org>',
  VERSION_FROM        => 'lib/JSON/Relaxed.pm',
  ABSTRACT_FROM       => 'lib/JSON/Relaxed.pm',
  LICENSE             => 'perl_5',
  PL_FILES            => {},
  MIN_PERL_VERSION    => '5.026000',

  @bins == 0 ? () : (
    EXE_FILES         => [ map { "script/$_" } @bins ],
    PL_FILES          => {
			   'script/pp.PL' => [ map { "script/$_" } @bins ]
			 },
  ),

  PREREQ_PM => {
    %makemaker,
    %prereq,
  },

  CONFIGURE_REQUIRES => {
    %makemaker,
  },

#  BUILD_REQUIRES => {
#  },

  TEST_REQUIRES  => {
    'Test::More'	=> 0,
    'FileHandle'	=> 0,
    %prereq,
  },

  META_MERGE => {
    'meta-spec'  => { version => 2 },
    resources    => {
      license    => [ "https://dev.perl.org/licenses/" ],
      homepage   => 'https://github.com/sciurius/perl-JSON-Relaxed',
      repository => {
	url => 'git://github.com/sciurius/perl-JSON-Relaxed.git',
	web => 'https://github.com/sciurius/perl-JSON-Relaxed',
	type => 'git',
      },
      bugtracker => {
        web => "https://github.com/sciurius/perl-JSON-Relaxed/issues",
        mailto => 'jv@cpan.org',
      },
    },
  },
);

WriteMakefile(%w);
