use Test::More qw(no_plan);

BEGIN {
    use_ok( 'Date::ICal' );
}

my $d = Date::ICal->new( year => 2001, month => 7, day => 5 , offset => 0);
ok ($d->year == 2001, "Year, creation by components");
ok ($d->month == 7, "Month, creation by components");
ok ($d->day == 5, "Day, creation by components");
ok ($d->hour == 0, "Hour, creation by components");
ok ($d->min == 0, "Min, creation by components");
ok ($d->sec == 0, "Sec, creation by components");
ok ($d->ical eq '20010705Z', "ical, creation by components");

