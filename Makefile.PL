use ExtUtils::MakeMaker;
my ($make_args);

# WriteMakefile params
$make_args = {
               'PL_FILES' => {},
               'LICENSE' => 'perl',
               'EXE_FILES' => [],
               'NAME' => 'JSON::Relaxed',
               'PREREQ_PM' => {
                                'Test' => 0
                              },
               'VERSION_FROM' => 'lib/JSON/Relaxed.pm',
               'ABSTRACT' => 'JSON::Relaxed -- An extension of JSON that allows for better human-readability.',
               'AUTHOR' => 'Miko O\'Sullivan <miko@idocs.com>'
             };


WriteMakefile(%$make_args);
