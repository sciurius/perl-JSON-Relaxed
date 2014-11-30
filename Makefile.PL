use ExtUtils::MakeMaker;
my ($make_args);

# WriteMakefile params
$make_args = {
               'AUTHOR' => 'Miko O\'Sullivan <miko@idocs.com>',
               'PREREQ_PM' => {
                                'Test' => 0
                              },
               'LICENSE' => 'perl',
               'EXE_FILES' => [],
               'ABSTRACT' => 'JSON::Relaxed -- An extension of JSON that allows for better human-readability.',
               'VERSION_FROM' => 'lib/JSON/Relaxed.pm',
               'NAME' => 'JSON::Relaxed',
               'PL_FILES' => {}
             };


WriteMakefile(%$make_args);
