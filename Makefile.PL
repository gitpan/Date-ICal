use ExtUtils::MakeMaker;

print qq~
Please note that the API for Date::ICal is still possibly in flux, and
you should expect it to change without much warning. If you are actively
using this module in important work, you need to subscribe to the
development mailing list and keep track of what changes. See
http://www.reefknot.org/ for relevant information.

The Reefknot Team
~;

WriteMakefile(
  'NAME'         => 'Date::ICal',
  'VERSION_FROM' => 'lib/Date/ICal.pm',    # finds $VERSION
  'PREREQ_PM'    => {
      Test::Harness => '2.25',
      Test::More    => '0.45',
      #     Test::Inline => 0,
      Date::Leapyear => '1.03',
      Time::Local    => 0,
      Time::HiRes    => 0,
      Storable       => 0,
  },    # e.g., Module::Name => 1.1
);

# Don't currently have any inline tests
# open(MANIFEST, "MANIFEST");
# foreach my $file (grep /\.pm$/, <MANIFEST>) {
#    chomp $file;
#    my($module) = $file =~ m|^(.*)\.pm$|;
#    $module =~ s|/|-|g;
#    system("pod2test $file t/embedded-$module.t");
#}

sub MY::postamble {
    package MY;
    "\npredist: all\n" .
    "\tcvs2cl -r -b -t -f Changes\n".
    "\techo '# vim: filetype=changelog' >> Changes\n".
    "\tpod2text lib/Date/ICal.pm >! README\n".
    "\techo '------------------------------------------------' >> README\n".
    "\tpod2text lib/Date/ICal/Duration.pm >> README\n"
}

