use ExtUtils::MakeMaker;
my ($make_args);

# WriteMakefile params
$make_args = {
               'AUTHOR' => 'Miko O\'Sullivan <miko@idocs.com>',
               'EXE_FILES' => [],
               'ABSTRACT' => 'JSON::Relaxed -- An extension of JSON that allows for better human-readability.',
               'VERSION_FROM' => 'lib/JSON/Relaxed.pm',
               'PL_FILES' => {},
               'NAME' => 'JSON::Relaxed',
               'LICENSE' => 'perl',
               'PREREQ_PM' => {
                                'Test' => 0
                              }
             };


WriteMakefile(%$make_args);
