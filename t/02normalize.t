use Test;

BEGIN { plan tests => 6 }

use Date::ICal;

my $t = Date::ICal->new( ical => '19961122T183020' );
$t->add( month => 2);

#test 1 check year rollover works
ok($t->year,1997);
#test 2 check month set on year rollover
ok($t->month,1);

$t->add( week => 2 );

#test 3 & 4 check year/month rollover with attrib setting
$t->month(14);
ok($t->year,1998);
ok($t->month,2);

#test 5 & 6 test subtraction with attrib setting
$t->month(-2);
ok($t->year,1997);
ok($t->month,10);

