package Date::ICal;
use strict;
use warnings;

use vars qw($VERSION);
$VERSION = (qw'$Revision: 1.28 $')[1];
use Carp;
use Time::Local;
use Date::Leapyear qw();
use Memoize;

=head1 NAME

Date::ICal - Perl extension for ICalendar date objects.

=head1 SYNOPSIS

    use Date::ICal;

    $ical = Date::ICal->new( ical => '19971024T120000' );
    $ical = Date::ICal->new( epoch => time );
    $ical = Date::ICal->new( year => 1964,
        month => 10, day => 16, hour => 16,
        min => 12, sec => 47, tz => '0530' );

    $hour = $ical->hour;
    $year = $ical->year;

    $ical_string = $ical->ical;
    $epoch_time = $ical->epoch;

=head1 DESCRIPTION

Date::ICal talks the ICal date format, and is intended to be a base class for
other date/calendar modules that know about ICal time format also.

See http://dates.rcbowen.com/unified.txt for details

=head1 METHODS

Date::ICal has the following methods available:

=head2 new

A new Date::ICal object can be created with any valid ICal string:

    my $ical = Date::ICal->new( ical => '19971024T120000' );

Or with any epoch time:

    my $ical = Date::ICal->new( epoch => time );

Or, better still, create it with components

    my $date = Date::ICal->new( 
                           day => 25, 
                           month => 10, 
                           year => 1066,
                           hour => 7,
                           min => 15,
                           sec => 47
                           );

If you call new without any arguments, you'll get a Date::ICal object that is
set to the time right now.

    my $ical = Date::ICal->new();

If you already have an object in Date::ICal, or some other subclass
thereof, you can create a new Date::ICal (or subclass) object using
that object to start with. This is particularly useful for converting
from one calendar to another:

   # Direct conversion from Discordian to ISO dates
   my $disco = Date::Discordian->new( disco => '12 Chaos, YOLD 3177' );
   my $iso = Date::ISO->new( $disco );
   print $iso->iso;

=begin testing

use lib '../blib/lib';
use Date::ICal;

my $t1 = Date::ICal->new(epoch => '0');
ok ($t1->epoch() eq '0', 'creation test from epoch (compare to epoch)');
ok ($t1->ical() eq '19700101Z', 'creation test from epoch (compare to ical)');

$t1 = Date::ICal->new(epoch => '3600');
ok ($t1->epoch == 3600, 'creation test from epoch = 3600 (compare to epoch)');
ok ($t1->ical eq '19700101T010000Z', 'creation test from epoch (compare to ical = 19700101T010000Z)');


=end testing

=cut

#{{{ sub new

sub new {
    my $class = shift;
    my ( %args, $sec, $min, $hour, $day, $month, $year, $tz, $zflag );

    # First argument can be a Date::ICal (or subclass thereof) object
    if ( ref $_[0] ) {
        $args{ical} = $_[0]->ical;
    } else {
        %args = @_;
    }

    # Date is specified as epoch#{{{
    if ( defined( $args{epoch} ) ) {

        ( $sec, $min, $hour, $day, $month, $year ) =
          ( gmtime( $args{epoch} ) )[ 0, 1, 2, 3, 4, 5 ];
        $year += 1900;
        $month++;

    }    #}}}

    # Date is specified as ical string#{{{
    elsif ( defined( $args{ical} ) ) {

        # Timezone, if any
        $args{ical} =~ s/^(?:TZID=([^:]+):)?//;
        $tz = $1;

        # Split up ical string
        ( $year, $month, $day, $hour, $min, $sec, $zflag ) =
          $args{ical} =~ /^(?:(\d{4})(\d\d)(\d\d))
               (?:T(\d\d)?(\d\d)?(\d\d)?)?
                       (Z)?$/x;

        $zflag = $args{ical} =~ /Z$/;

        # XXX Not sure what to do with the time zone information

    }    #}}}

    # Time specified as components#{{{
    elsif ( defined( $args{day} ) ) {

        # Choke if missing arguments
        foreach my $attrib(qw(day month year )) {
            warn "Attribute $attrib required" unless defined $args{$attrib};
        }
        foreach my $attrib( qw( hour min sec ) ) {
            $args{$attrib} = 0 unless defined $args{$attrib};
        }

        # And then just use what was passed in
        ( $sec, $min, $hour, $day, $month, $year ) =
            @args{ 'sec', 'min', 'hour', 'day', 'month', 'year' };
    }    #}}}

    else {    # Just use current gmtime#{{{

        ( $sec, $min, $hour, $day, $month, $year ) =
          ( gmtime( $args{epoch} ) )[ 0, 1, 2, 3, 4, 5 ];
        $year += 1900;
        $month++;
    }    #}}}

    my $jd = 
      [ leapdays_before($year) + days_this_year( $day, $month, $year )
        + $year * 365 ,
      time_as_seconds( $hour, $min, $sec ) ];

    my $self = { jd => $jd };

    bless $self, $class;
    return $self;
}

#}}}

=head2 ical

    $ical_string = $ical->ical;

Retrieves, or sets, the date on the object, using any valid ICal date/time
string.

=cut

#{{{ sub ical

sub ical {
    my $self = shift;
    my $ical;

    if ( $self->hour || $self->min || $self->sec ) {
        $ical =
          sprintf( '%04d%02d%02dT%02d%02d%02dZ', $self->year, $self->month,
          $self->day, $self->hour, $self->minute, $self->second );
    } else {
        $ical =
          sprintf( '%04d%02d%02dZ', $self->year, $self->month, $self->day );
    }
    return $ical;
}

#}}}

=head2 epoch

    $epoch_time = $ical->epoch;
    
    $ical->epoch( 98687431 );

Sets, or retrieves, the epoch time represented by the object, if it is
representable as such. (Dates before 1971 or after 2038 will not have an epoch
representation.)

Internals note: The ICal representation of the date is considered the only
authoritative one. This means that we may need to reconstruct the epoch time
from the ICal representation if we are not sure that they are in synch. We'll
need to do clever things to keep track of when the two may not be in synch.
And, of course, the same will go for any subclasses of this class.

=begin testing

my $epochtest = Date::ICal->new (epoch => '0');

ok($epochtest->epoch() == '0', "Epoch 0 as epoch-time is 0");
ok($epochtest->ical() eq '19700101Z', "Epoch 0 as ical is 19700101");

#poking at internal data structures to make sure it's doing the right thing
ok($epochtest->hour() ==  '0', "Epoch 0 has hour 0 defined");
ok($epochtest->min() == '0', "Epoch 0 has minute 0 defined");
ok($epochtest->sec() == '0', "Epoch 0 has second 0 defined");

=end testing

=cut

#{{{ sub epoch

sub epoch {
    my $self = shift;
    my $class = ref($self);

    my $epoch;

    if ( $epoch = shift ) {    # Passed in a new value

        my $newepoch = $class->new( epoch => $epoch );
        $self->{jd} = $newepoch->jd;

    }

    else {    # Calculate epoch from components, if possible

        $epoch =
          timegm( $self->sec, $self->min, $self->hour, $self->day,
          ( $self->month ) - 1, ( $self->year ) - 1900 );
    }

    return $epoch;
}

=begin testing

$epochtest = Date::ICal->new(epoch => '997122970');
ok ( $epochtest->epoch( 997121000 ) == 997121000,
    "Setting epoch returns correct value");
ok( $epochtest->epoch == 997121000, "And the value stuck" );
ok( $epochtest->hour == 18, "Hour, after setting epoch" );
ok( $epochtest->min == 3, "Min, after setting epoch" );

=end testing

#}}}

=begin testing

#{{{

my $acctest = Date::ICal->new(ical => "19920405T160708Z");

ok($acctest->sec == 8, "second accessor read is correct");
ok($acctest->minute == 7, "minute accessor read is correct");
ok($acctest->hour == 16, "hour accessor read is correct");
ok($acctest->day == 5, "day accessor read is correct");
ok($acctest->month == 4, "month accessor read is correct");
ok($acctest->year == 1992, "year accessor read is correct");

$parsetest = Date::ICal->new(epoch => "0");

ok($parsetest->second == 0, "_parse_ical seconds are correct on epoch 0");
ok($parsetest->minute == 0, "_parse_ical minutes are correct on epoch 0");
ok($parsetest->hour == 0, "_parse_ical hours are correct on epoch 0");
ok($parsetest->day == 1, "_parse_ical days are correct on epoch 0");
ok($parsetest->month == 1, "_parse_ical months are correct on epoch 0");
ok($parsetest->year == 1970, "_parse_ical year is correct on epoch 0");

# ok($parsetest->{timezone} eq "UTC", "_parse_ical timezone is correct for UTC on epoch 0");

# extra-epoch dates?

my $preepoch = Date::ICal->new( ical => '18700523T164702Z' );
ok( $preepoch->year == 1870, 'Pre-epoch year' );
ok( $preepoch->month == 5, 'Pre-epoch month' );
ok( $preepoch->sec == 2, 'Pre-epoch seconds' );

my $postepoch = Date::ICal->new( ical => '23481016T041612Z' );
ok( $postepoch->year == 2348, "Post-epoch year" );
ok( $postepoch->day == 16, "Post-epoch day");
ok( $postepoch->hour == 04, "Post-epoch hour");

=end testing

#}}}

=cut

=head2 add

    $date->add( %hash ); # Hash of day, hour, min, etc, values
    $date->add( ical => $ical_duration_string );

Adds a duration to a Date::ICal object.

Duration should be passed in as either an ical string, or as a hash of
date/time properties.

The result will be normalized. That is, the output time will have
meaningful values, rather than being 48:73 pm on the 34th of 
hexadecember.

   $self->add( month=>2 );
   $self->add( duration =>'P1W' );

=begin testing

#  Pod::Tests testing #{{{

my $t = Date::ICal->new( ical => '19961122T183020' );
$t->add( week => 8);

ok($t->year == 1997, "year rollover");
ok($t->month == 1, "month set on year rollover");

$t->add( week => 2 );

# Now, test the adding of durations
$t = Date::ICal->new (ical => '19860128T163800Z');

$t->add(duration => 'PT1M12S');
ok ($t->ical eq '19860128T163912Z', "Adding durations with minutes and seconds works");

$t = Date::ICal->new (ical => '19860128T163800Z');

$t->add(duration => 'PT30S');
ok ($t->ical eq '19860128T163830Z', "Adding durations with seconds only works");

$t = Date::ICal->new (ical => '19860128T163800Z');

$t->add(duration => 'PT1H10M');
ok ($t->ical eq '19860128T174800Z', "Adding durations with hours and minutes works");


$t = Date::ICal->new (ical => '19860128T163800Z');

$t->add(duration => 'P3D');
# XXX: what's "right" in the following test? should the result
# just be a date, or a date and time?
ok ($t->ical eq '19860131T163800Z', "Adding durations with days only works");


$t = Date::ICal->new (ical => '19860128T163800Z');

$t->add(duration => 'P3DT2H');
ok ($t->ical eq '19860131T183800Z', "Adding durations with days and hours works");


$t = Date::ICal->new (ical => '19860128T163800Z');

$t->add(duration => 'P3DT2H20M15S');
ok ($t->ical eq '19860131T185815Z', "Adding durations with days, hours, minutes, and seconds works");

# Add 15M - this test failing in N::I::Time
$t = Date::ICal->new( ical =>  '20010405T160000Z');
$t->add( duration => 'PT15M' );
ok( $t->ical eq '20010405T161500Z', "Adding minutes to an ical string");

# Subtract a duration
$t->add( duration => '-PT15M' );
ok( $t->ical eq '20010405T160000Z', "Back where we started");

=end testing

#}}}

=cut

# sub add #{{{

sub add {
    my $self = shift;
    my %args = @_;

    carp "Date::ICal::add was called by an undefined object"
      unless defined($self);
    carp "Date::ICal::add was called without an attribute arg"
      unless ( keys %args );

    my $seconds;
    if ( defined $args{duration} ) {

        $seconds = duration_as_sec( $args{duration} );

      } else {

        $seconds =  0;
        $seconds += $args{sec}                      if defined $args{secs};
        $seconds += $args{min}  * 60                if defined $args{min};
        $seconds += $args{hour} * 60 * 60           if defined $args{hour};
        $seconds += $args{day}  * 60 * 60 * 24      if defined $args{day};
        $seconds += $args{week} * 7  * 60 * 60 * 24 if defined $args{week};
    }

    my $jd = $self->jd;

    # How many days?
    my $days = int( $seconds/86400 );
    $seconds = $seconds - ( $days * 86400 );

    $self->jd( [$jd->[0] + $days, $jd->[1] + $seconds ] );

    return $self;
}
#}}}

# sub duration_as_sec #{{{

sub duration_as_sec {
    my $str = shift;

    my @temp = $str =~ m{
            ([\+\-])?   (?# Sign)
            (P)     (?# 'P' for period? This is our magic character)
            (?:
                (?:(\d+)W)? (?# Weeks)
                (?:(\d+)D)? (?# Days)
            )?
            (?:T        (?# Time prefix)
                (?:(\d+)H)? (?# Hours)
                (?:(\d+)M)? (?# Minutes)
                (?:(\d+)S)? (?# Seconds)
            )?
              }x;
    my ( $sign, $magic ) = @temp[ 0 .. 1 ];
    my ( $weeks, $days, $hours, $mins, $secs ) =
      map { defined($_) ? $_ : 0 } @temp[ 2 .. $#temp ];

    unless ( defined($magic) ) {
        carp "Invalid duration: $str";
        return undef;
    }
    $sign = ( ( defined($sign) && $sign eq '-' ) ? -1 : 1 );

    return $sign * ( $secs + ( $mins * 60 ) + ( $hours * 3600 ) +
      ( $days * 86400 ) + ( $weeks * 604800 ) );
}

#}}}

=head2 compare

    $cmp = $date1->compare($date2);

    @dates = sort {$a->compare($b)} @dates;

Compare two Date::ICal objects. Semantics are compatible with
sort; returns -1 if $a < $b, 0 if $a == $b, 1 if $a > $b. 

# Pod::Tests tests #{{{

=begin testing

use Date::ICal; 
my $date1 = Date::ICal->new( ical => '19971024T120000');
my $date2 = Date::ICal->new( ical => '19971024T120000');


# make sure that comparing to itself eq 0
my $identity = $date1->compare($date2);
ok($identity == 0, "Identity comparison");

$date2 = Date::ICal->new( ical => '19971024T120001');
ok($date1->compare($date2) == -1, 'Comparison $a < $b, 1 second diff');

$date2 = Date::ICal->new( ical => '19971024T120100');
ok($date1->compare($date2) == -1, 'Comparison $a < $b, 1 minute diff');

$date2 = Date::ICal->new( ical => '19971024T130000');
ok($date1->compare($date2) == -1, 'Comparison $a < $b, 1 hour diff');

$date2 = Date::ICal->new( ical => '19971025T120000');
ok($date1->compare($date2) == -1, 'Comparison $a < $b, 1 day diff');

$date2 = Date::ICal->new( ical => '19971124T120000');
ok($date1->compare($date2) == -1, 'Comparison $a < $b, 1 month diff');

$date2 = Date::ICal->new( ical => '19981024T120000');
ok($date1->compare($date2) == -1, 'Comparison $a < $b, 1 year diff');

# $a > $b tests

$date2 = Date::ICal->new( ical => '19971024T115959');
ok($date1->compare($date2) == 1, 'Comparison $a > $b, 1 second diff');

$date2 = Date::ICal->new( ical => '19971024T115900');
ok($date1->compare($date2) == 1, 'Comparison $a > $b, 1 minute diff');

$date2 = Date::ICal->new( ical => '19971024T110000');
ok($date1->compare($date2) == 1, 'Comparison $a > $b, 1 hour diff');

$date2 = Date::ICal->new( ical => '19971023T120000');
ok($date1->compare($date2) == 1, 'Comparison $a > $b, 1 day diff');

$date2 = Date::ICal->new( ical => '19970924T120000');
ok($date1->compare($date2) == 1, 'Comparison $a > $b, 1 month diff');

$date2 = Date::ICal->new( ical => '19961024T120000');
ok($date1->compare($date2) == 1, 'Comparison $a > $b, 1 year diff');


=end testing

#}}}

=cut

sub compare {
    my ( $self, $otherdate ) = (@_);

    unless ( defined($otherdate) ) { return undef }

    # One or more days different

    if (($self->jd)->[0] < ($otherdate->jd)->[0]) {
        return -1;
    } elsif (($self->jd)->[0] > ($otherdate->jd)->[0]) {
        return 1;

    # They are the same day
    } elsif ( ($self->jd)->[1] < ($otherdate->jd)->[1]) {
        return -1;
    } elsif ( ($self->jd)->[1] > ($otherdate->jd)->[1]) {
        return 1;
    }

#    # if we got all this way and haven't yet returned, the units are equal.
    return 0;
}

=begin internal

 $days = leapdays_before( 1984 );

Returns the number of leap days occurring before the specified year.
Starts counting in year 0 AD. Assumes (incorrectly, but sufficient for
our purposes, if we are internally consistent) that all years since 0
have obeyed standard leap year rules.

=end internal

=cut

sub leapdays_before {
    my $year = shift;
    my $days = 0;

    my $counter = 0;
    while ( $counter < $year ) {
        $days++ if Date::Leapyear::isleap($counter);
        $counter += 4;
    }

    return $days;
}

=begin internal

 days_this_year( $args{day}, $args{month}, $args{year} )

Returns the number of days so far in the specified year.

=end internal

=cut

sub days_this_year {
    my ( $day, $month, $year ) = @_;

    my @months = months($year);
    return $months[ $month - 1 ] + $day - 1;
}

sub months {
    my $year = shift;

    my @months = ( 0, 31, 59, 90, 120, 151, 181, 212, 243, 274, 305, 335, 366 );
    if ( Date::Leapyear::isleap($year) ) {
        foreach my $mon( 2 .. 11 ) {
            $months[$mon]++;
        }
    }

    return @months;
}

=begin internal

    time_as_seconds( $args{hour}, $args{min}, $args{sec} );

Returns the time of day as the number of seconds in the day.

=end internal

=cut

sub time_as_seconds {
    my ( $hour, $min, $sec ) = @_;

    $hour ||= 0;
    $min ||= 0;
    $sec ||= 0;

    my $secs = $hour * 3600 + $min * 60 + $sec;
    return $secs;
}

=head2 day

    my $day = $date->day;

Returns the day of the month.

Day is in the range 1..31

=cut

sub day {
    my $self = shift;
    return ($self->parsedays)[0];
}

=head2 month

    my $month = $date->month;

Returns the month of the year.

Month is returned as a number in the range 1..12

=cut

sub month {
    my $self = shift;
    return ($self->parsedays)[1];
}

sub mon { return month(@_); }

=head2 year

    my $year = $date->year;

Returns the year.  

=cut

sub year {
    my $self = shift;
    return ($self->parsedays)[2];
}

=begin internal

 ( $day, $month, $year ) = parsedays ( $jd );

Given a modified modified julian day, returns the day, month, year for
the given date.

=end internal

=cut

sub parsedays {
    my $self = shift;
    my $day = ($self->jd)->[0];
    return parsedays_memo( $day );
}

memoize('parsedays_memo');
sub parsedays_memo {
    my $day = shift;

    # What year are we in?
    my $year = 0;
    my $days = 0;
    while ( $days < $day ) {
        $days += ( 365 + 1 * ( Date::Leapyear::isleap($year) ) );
        $year++;
    }
    $year-- unless $days == $day;

    # What month is it?
    $day -= ( $year * 365 + leapdays_before($year) );
    my @months = months($year);

    my $month = 0;
    foreach my $m( 0 .. 12 ) {
        if ( $months[$m] > $day ) {
            $month = $m;
            last;
        }
    }

    $day -= $months[ $month - 1 ];
    $day++;

    return ( $day, $month, $year );
}

=head1 hour

    my $hour = $date->hour

Returns the hour of the day.

Hour is in the range 0..23

=cut

sub hour {
    my $self = shift;
    return ($self->parsetime)[2];
}

=head1 min

    my $min = $date->min;

Returns the minute.

Minute is in the range 0..59

=cut

sub min {
    my $self = shift;
    return ( $self->parsetime )[1];
}

sub minute { return min(@_); }

=head1 sec

    my $sec = $date->sec;

Returns the second.

Second is in the range 0..60. The value of 60 is (maybe) needed for
leap seconds. But I'm not sure if we're going to go there.

=cut

sub sec {
    my $self = shift;
    return ( $self->parsetime )[0];
}

sub second { return sec(@_); }

=begin internal

 ( $sec, $min, $hour ) = parsetime( $seconds );

Given the number of seconds so far today, returns the seconds,
minutes, and hours of the current time.

=end internal

=cut

sub parsetime {
    my $self = shift;
    my $time = $self->jd->[1];
    return parsetime_memo( $time );
}

memoize('parsetime_memo');
sub parsetime_memo {
    my $time = shift;

    my $hour = int( $time / 3600 );
    $time -= $hour * 3600;

    my $min = int( $time / 60 );
    $time -= $min * 60;

    return ( int($time), $min, $hour );
}

# sub julian/jd #{{{

=head1 julian

  my $jd = $date->jd;

Returns a listref, containing two elements. The date as a julian day,
and the time as the number of seconds since midnight. This should not 
be thought of as a real julian day, because it's not. The module is
internally consistent, and that's enough.

This really should be considered an internal method.

See the file INTERNALS for more information about this internal
format.

=cut

sub jd {
    my $self = shift;

    if ( my $jd = shift ) {
        $self->{jd} = $jd;
    }

    return $self->{jd};
}
sub julian { return $_[0]->{jd} }

#}}}

1;

=head1 TODO

=over 4 

=item - add timezone support, including moving between timezones

=item - add gmtime and localtime methods, perhaps?

=item - Find a solution to the 1-second round-off errors. Perhaps move
to two values (date and time) rather than one single value. This
change can be made internally without affecting anything outside.

=head1 INTERNALS

Please see the file INTERNALS for discussion on the internals.

=head1 AUTHOR

Rich Bowen (DrBacchus) rbowen@rcbowen.com

And the rest of the Reefknot team.

=head1 SEE ALSO

datetime@perl.org mailing list

http://reefknot.org/

http://dates.rcbowen.com/

Time::Local

Net::ICal

# CVS History #{{{

=head1 CVS History

  $Log: ICal.pm,v $
  Revision 1.28  2001/08/06 18:45:47  rbowen
  sub epoch was referencing another sub that has gone away. Fixed, and
  added tests.

  Revision 1.27  2001/08/02 04:38:16  srl
  Adjusted the add() method to return a copy of $self instead of the
  return value of $self->jd(). This was important to making
  the Net::ICal tests pass, but it's also the Right Way, I think.

  Revision 1.26  2001/08/02 03:47:59  rbowen
  Handle negative durations correctly.

  Revision 1.25  2001/08/01 02:19:03  rbowen
  Two main changes here.
  1) Split the internal date/time representation into date, time
  integers, so that we don't have any more roundoff error.
  2) Memoized the parsetime and parsedate methods, so that we're not
  doing that three times every time we want three components, which we
  were doing.


#}}}

=cut

