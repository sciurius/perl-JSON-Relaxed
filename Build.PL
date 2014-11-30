use strict;
use Module::Build;
my ($build, $params);


$params = {
            'dist_version_from' => 'lib/JSON/Relaxed.pm',
            'license' => 'perl',
            'module_name' => 'JSON::Relaxed',
            'dist_author' => 'Miko O\'Sullivan <miko@idocs.com>',
            'dist_abstract' => 'JSON::Relaxed -- An extension of JSON that allows for better human-readability.',
            'requires' => {
                            'Test' => 0
                          }
          };


$build = Module::Build->new(%$params);
$build->create_build_script();
