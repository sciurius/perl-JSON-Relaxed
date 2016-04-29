use ExtUtils::MakeMaker;
my ($make_args);

# WriteMakefile params
$make_args = {
               'PL_FILES' => {},
               'AUTHOR' => 'Miko O\'Sullivan <miko@idocs.com>',
               'PREREQ_PM' => {},
               'LICENSE' => 'perl',
               'NAME' => 'JSON::Relaxed',
               'ABSTRACT' => 'JSON::Relaxed -- An extension of JSON that allows for better human-readability.',
               'VERSION_FROM' => 'lib/JSON/Relaxed.pm',
               'EXE_FILES' => []
             };


WriteMakefile(%$make_args);
