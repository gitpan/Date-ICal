use Test::More qw(no_plan);

BEGIN { use_ok( 'Date::ICal' ) }

# XXX Can't add months, years yet.

use Date::ICal;

my $t = Date::ICal->new( ical => '19961122T183020' );

# Add 2 months
# $t->add( month => 2);

#test 1 check year rollover works
# ok($t->year,1997);
#test 2 check month set on year rollover
# ok($t->month,1);

# $t->add( week => 2 );

#test 3 & 4 check year/month rollover with attrib setting
# $t->month(14);
# ok($t->year,1998);
# ok($t->month,2);

#test 5 & 6 test subtraction with attrib setting
# $t->month(-2);
# ok($t->year,1997);
# ok($t->month,10);

$t->add( day => 1 );
ok($t->day == 23, 'Add one day');

$t->add( week => 1 );
ok($t->day == 30, 'Add a week' );

$t->add( hour => 3 );
ok($t->hour == 21, 'Add 3 hours' );

$t->add( day => 15 );
ok( $t->month == 12, "2 weeks later, it is December" );
ok( $t->day == 15, "December 15th to be precise" );

