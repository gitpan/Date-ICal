use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'		=> 'Date::ICal',
    'VERSION_FROM'	=> 'ICal.pm', # finds $VERSION
    'PREREQ_PM'		=> {
        Test::More => 0,
        Pod::Tests => 0,
    }, # e.g., Module::Name => 1.1
);

open(MANIFEST, "MANIFEST");
foreach my $file (grep /\.pm$/, <MANIFEST>) {
    chomp $file;
    my($module) = $file =~ m|^(.*)\.pm$|;
    $module =~ s|/|-|g;
    system("pod2test $file t/embedded-$module.t");
}

