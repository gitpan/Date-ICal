# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

use Test;

BEGIN { plan tests => 16 }

use Date::ICal;

#======================================================================
# BASIC INITIALIZATION TESTS (1-4)
#====================================================================== 

my $t1 = new Date::ICal(epoch => 0);
ok($t1->epoch, '0');

# Make sure epoch time is being handled sanely.
# FIXME: This will only work on unix systems.
ok($t1->ical, '19700101');

# like the tests above, but starting with ical instead of epoch
my $t2 = new Date::ICal(ical => '19700101Z');
ok($t2->ical, '19700101Z');

# NOTE: this will FAIL unless you are in a UTC timezone. 
ok($t2->epoch, '0');    # seems to be failing. hm.



#======================================================================
# ACCESSOR READ TESTS (5-10)
#====================================================================== 

my $t3 = new Date::ICal(ical => "20010203T183020");

ok($t3->year, '2001');
ok($t3->month, '02');
ok($t3->day, '03');
ok($t3->hour, '18');
ok($t3->minute, '30');
ok($t3->second, '20');

# TODO: test the timezone accessor, when there is one

#======================================================================
# ACCESSOR WRITE TESTS (11-16)
#====================================================================== 

ok($t3->year(1870), '1870');
ok($t3->month(10), '10');
ok($t3->day(21), '21');
ok($t3->hour(12), '12');
ok($t3->minute(10), '10');
ok($t3->second(45), '45');


# OTHER TESTS WE NEED, once the code supports them:
# - timezone testing
# - UTC <-> localtime
# - arithmetic, with and without unit rollovers
