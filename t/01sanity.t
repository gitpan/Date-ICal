# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

use Test::More qw(no_plan);

BEGIN { use_ok('Date::ICal') };

#======================================================================
# BASIC INITIALIZATION TESTS (1-7)
#====================================================================== 

my $t1 = new Date::ICal( epoch => 0 );
ok( $t1->epoch eq '0', "Epoch time of 0" );

# Make sure epoch time is being handled sanely.
# FIXME: This will only work on unix systems.
ok( $t1->ical eq '19700101', "When does the epoch start?" );

ok( $t1->year == 1970, "Year accessor, start of epoch" );
ok( $t1->month == 1,   "Month accessor, start of epoch" );
ok( $t1->day == 1,     "Day accessor, start of epoch" );

# like the tests above, but starting with ical instead of epoch
my $t2 = new Date::ICal( ical => '19700101Z' );
ok( $t2->ical eq '19700101Z', "Start of epoch in ICal notation" );

# NOTE: this will FAIL unless you are in a UTC timezone. 
ok( $t2->epoch == '0', "Time should be stored in UTC anyway, right?" );

#======================================================================
# ACCESSOR READ TESTS (8-13)
#====================================================================== 

my $t3 = new Date::ICal( ical => "20010203T183020" );

ok( $t3->year eq '2001', "Year accessor" );
ok( $t3->month eq '02',  "Month accessor" );
ok( $t3->day eq '03',    "Day accessor" );
ok( $t3->hour eq '18',   "Hour accessor" );
ok( $t3->minute eq '30', "Minute accessor" );
ok( $t3->second eq '20', "Second accessor" );

# TODO: test the timezone accessor, when there is one

#======================================================================
# ACCESSOR WRITE TESTS (14-19)
#====================================================================== 

my $t4 = new Date::ICal( ical => "18701021T121045Z" );
ok( $t4->year eq '1870', "Year accessor, outside of the epoch" );
ok( $t4->month eq '10',  "Month accessor, outside the epoch" );
ok( $t4->day eq '21',    "Day accessor, outside the epoch" );
ok( $t4->hour eq '12',   "Hour accessor, outside the epoch" );
ok( $t4->minute eq '10', "Minute accessor, outside the epoch" );
ok( $t4->second eq '45', "Second accessor, outside the epoch" );

# OTHER TESTS WE NEED, once the code supports them:
# - timezone testing
# - UTC <-> localtime
# - arithmetic, with and without unit rollovers

