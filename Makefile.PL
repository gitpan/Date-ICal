use ExtUtils::MakeMaker;

print 
"-------------------------------------------------------------------
Thanks for your interest in Date::ICal. You should know that this
is an ALPHA RELEASE. That means that some tests *will fail*. We
know this, and in some cases the failing tests are intentional.
If you want to help us out, please make the tests pass, or 
write tests for existing code, and send the patches to our
developers' mailing list. See the README for more information.

- The Reefknot team (http://reefknot.sf.net)
-------------------------------------------------------------------
";



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

