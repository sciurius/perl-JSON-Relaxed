use strict;
use Module::Build;
my ($build, $params);


$params = {
            'dist_abstract' => 'JSON::Relaxed -- An extension of JSON that allows for better human-readability.',
            'requires' => {},
            'dist_version_from' => 'lib/JSON/Relaxed.pm',
            'license' => 'perl',
            'dist_author' => 'Miko O\'Sullivan <miko@idocs.com>',
            'module_name' => 'JSON::Relaxed'
          };


$build = Module::Build->new(%$params);
$build->create_build_script();
