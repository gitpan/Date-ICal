use Test::More qw(no_plan);

BEGIN{ use_ok('Date::ICal'); }

my $date1 = Date::ICal->new( ical => '20010510T040302Z' );
my $date2 = Date::ICal->new( ical => '20010612T050723Z' );

my $diff = $date2 - $date1;

is( $diff->as_days, 33, 'Total # of days' );
is( $diff->weeks,   4,  'Weeks' );
is( $diff->days,    5,  'Days' );
is( $diff->hours,   1,  'Hours' );
is( $diff->minutes, 4,  'Min' );
is( $diff->seconds, 21, 'Sec' );
is( $diff->as_ical, 'P4W5DT1H4M21S', 'Duration' );

