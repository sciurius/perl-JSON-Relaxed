use strict;
use Module::Build;
my ($build, $params);


$params = {
            'requires' => {
                            'Test' => 0
                          },
            'license' => 'perl',
            'dist_abstract' => 'JSON::Relaxed -- An extension of JSON that allows for better human-readability.',
            'module_name' => 'JSON::Relaxed',
            'dist_author' => 'Miko O\'Sullivan <miko@idocs.com>',
            'dist_version_from' => 'lib/JSON/Relaxed.pm'
          };


$build = Module::Build->new(%$params);
$build->create_build_script();
