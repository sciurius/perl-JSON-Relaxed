use strict;
use Module::Build;
my ($build, $params);


$params = {
            'module_name' => 'JSON::Relaxed',
            'requires' => {
                            'Test' => 0
                          },
            'dist_author' => 'Miko O\'Sullivan <miko@idocs.com>',
            'license' => 'perl',
            'dist_version_from' => 'lib/JSON/Relaxed.pm',
            'dist_abstract' => 'JSON::Relaxed -- An extension of JSON that allows for better human-readability.'
          };


$build = Module::Build->new(%$params);
$build->create_build_script();
