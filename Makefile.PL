use ExtUtils::MakeMaker;

print <<EOL;

Note: This is a maintenance release, and should be considered beta
quality. There are bugs, and some tests have been commented out so that
this will install successfully. This is being released to get some bug
fixes out. There should be another release soon.

EOL

WriteMakefile(
    'NAME'		=> 'Date::ICal',
    'VERSION_FROM'	=> 'ICal.pm', # finds $VERSION
    'PREREQ_PM'		=> {
        Test::More => 0,
        Pod::Tests => 0,
        Date::Leapyear => '1.03',
        Time::Local => 0,
        Memoize     => 0,
        Time::HiRes => 0,
        Storable    => 0,
    }, # e.g., Module::Name => 1.1
);

open(MANIFEST, "MANIFEST");
# foreach my $file (grep /\.pm$/, <MANIFEST>) {
#     chomp $file;
#     my($module) = $file =~ m|^(.*)\.pm$|;
#     $module =~ s|/|-|g;
#     system("pod2test $file t/embedded-$module.t");
# }

