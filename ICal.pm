package Date::ICal;
use strict;

use vars qw($VERSION);
$VERSION = (qw'$Revision: 1.18 $')[1];
use Carp;
use Time::Local;

=head1 NAME

Date::ICal - Perl extension for ICal date objects.

=head1 SYNOPSIS

    use Date::ICal;

    $ical = Date::ICal->new( ical => '19971024T120000' );
    $ical = Date::ICal->new( epoch => time );

    $hour = $ical->hour;
    $year = $ical->year;

    $ical_string = $ical->ical;
    $epoch_time = $ical->epoch;

    $ical->epoch( time+60 ); # Set the time a minute ahead

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

If you call new without any arguments, you'll get a Date::ICal object that is
set to the time right now.

    my $ical = Date::ICal->new();

=begin testing

use lib '../blib/lib';
use Date::ICal;

my $t1 = Date::ICal->new(epoch => '0');
ok ($t1->epoch() eq '0', 'creation test from epoch (compare to epoch)');
ok ($t1->ical() eq '19700101', 'creation test from epoch (compare to ical)');

$t1 = Date::ICal->new(epoch => '3600');
ok ($t1->epoch() eq '3600', 'creation test from epoch = 3600 (compare to epoch)');
ok ($t1->ical() eq '19700101T010000', 'creation test from epoch (compare to ical = 19700101T010000Z)');


=end testing

=cut

#{{{ sub new
sub new {
    my $class = shift;
    my %args  = @_;

    my $self = \%args;
    bless $self, $class;

    if ( defined( $args{epoch} ) ) {
        $self->epoch( $args{epoch} );

    }
    elsif ( defined( $args{ical} ) ) {

        # Don't actually need to do anything, except perhaps verify the
        # validity of the argument
        $self->ical( $args{ical} );

    }
    else {
        $self->epoch(time);

    }

    return $self;
}

#}}}

=head2 ical

    $ical_string = $ical->ical;

    $ical->ical( '19981016' );

Retrieves, or sets, the date on the object, using any valid ICal date/time
string.

The ICal representation is the one authoritative value in the object, and so
if it is changed, it must be able to indicate that the other values are no
longer valid. Or set the correctly. Or something. Comments welcomed.

=cut

#{{{ sub ical
sub ical {
    my $self = shift;

    if ( my $ical = shift ) {

        # TODO: shouldn't there be some validation that this is good iCalendar?
        $self->{ical} = $ical;
        foreach my $attrib(qw(epoch second minute hour day month year)) {
            delete $self->{$attrib};
        }
    }

    return $self->{ical};
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
ok($epochtest->ical() eq '19700101', "Epoch 0 as ical is 19700101");

#poking at internal data structures to make sure it's doing the right thing
ok($epochtest->hour() ==  '0', "Epoch 0 has hour 0 defined");
ok($epochtest->min() == '0', "Epoch 0 has minute 0 defined");
ok($epochtest->sec() == '0', "Epoch 0 has second 0 defined");
=end testing

=cut

#{{{ sub epoch

sub epoch {

    my ( $self, $epoch ) = @_;

    if ( defined($epoch) ) { # Passed in a new value

        my ( $ss, $mm, $hh, $DD, $MM, $YY ) = gmtime($epoch);

        $self->{year}   = $YY + 1900;
        $self->{month}  = $MM + 1;
        $self->{day}    = $DD;
        $self->{hour}   = $hh;
        $self->{minute} = $mm;
        $self->{second} = $ss;

        $self->_format_ical; # Set the definitive value
    }

    unless ( exists $self->{epoch} ) {
        # Recalculate from the authoritative source
        $self->{epoch} = $self->_epoch_from_ical;
    }

    return $self->{epoch};
}

#}}}

=head2 Other Accessors

    second
    minute
    hour
    day
    month
    year

Generic set/get methods.

    $sec = $t->second;

    $t->minute(12);

=begin testing

my $acctest = Date::ICal->new(ical => "19920405T160708Z");

#TODO: clarify whether these accessors should be returning zero-padded numbers or not
ok($acctest->sec() eq "08", "second accessor read is correct");
ok($acctest->sec(22) eq "22", "second accessor read while write is correct");
ok($acctest->sec() eq "22", "second accessor read after write is correct");

ok($acctest->minute() eq "07", "minute accessor read is correct");
ok($acctest->minute(16) eq "16", "minute accessor read while write is correct");
ok($acctest->minute() eq "16", "minute accessor read after write is correct");

ok($acctest->hour() eq "16", "hour accessor read is correct");
ok($acctest->hour(1) eq "1", "hour accessor read while write is correct");
ok($acctest->hour() eq "1", "hour accessor read after write is correct");

# FIXME: why does it have to be "05" (not "5") to pass?
ok($acctest->day() eq "05", "day accessor read is correct");
ok($acctest->day(7) eq "7", "day accessor read while write is correct");
ok($acctest->day() eq "7", "day accessor read after write is correct");

# FIXME: compare to day() above, except passing tests here have no leading 0
ok($acctest->month() eq "4", "month accessor read is correct");
ok($acctest->month(1) eq "1", "month accessor read while write is correct");
ok($acctest->month() eq "1", "month accessor read after write is correct");

ok($acctest->year() eq "1992", "year accessor read is correct");
ok($acctest->year(1990) eq "1990", "year accessor read while write is correct");
ok($acctest->year() eq "1990", "year accessor read after write is correct");

=end testing

=cut

#{{{ Accessor methods
sub second { attrib( 'second', @_ ); }
sub sec { return second(@_); }

sub minute { attrib( 'minute', @_ ); }
sub min { return minute(@_); }

sub hour { attrib( 'hour', @_ ); }

sub day { attrib( 'day', @_ ); }

sub month { attrib( 'month', @_ ) }

sub year { attrib( 'year', @_ ); }

sub attrib {
    my $attrib = shift;
    my $self   = shift;

    if ( my $value = shift ) {

        # $self->{$attrib} = $value;
        $self->_parse_ical;
        $self->_normal( $attrib, $value );
        $self->_format_ical;
    }

    unless ( exists $self->{$attrib} ) {
        $self->_parse_ical;
    }

    return $self->{$attrib};
}

#}}}

=head2 _parse_ical

    $self->_parse_ical;

C<_parse_ical> is an internal method (although I suppose you could call it
externally if you really wanted to) which repopulates other attributes based
on the ical field. This should be called by various methods if an attribute is
undefined.

=begin testing

my $parsetest = Date::ICal->new(ical => "19920405T160708Z");

$parsetest->_parse_ical();

ok($parsetest->{second} eq "08", "_parse_ical seconds are correct on ical time");
ok($parsetest->{minute} eq "07", "_parse_ical minutes are correct on ical time");
ok($parsetest->{hour} eq "16", "_parse_ical hours are correct on ical time");
ok($parsetest->{day} eq "05", "_parse_ical days are correct on ical time");
ok($parsetest->{month} eq "04", "_parse_ical months are correct on ical time");
ok($parsetest->{year} eq "1992", "_parse_ical year is correct on ical time");
ok($parsetest->{floating} eq "0", "_parse_ical floating flag is correct for UTC on ical time");

ok($parsetest->{timezone} eq "UTC", "_parse_ical timezone is correct for UTC on ical time");

$parsetest = Date::ICal->new(epoch => "0");

$parsetest->_parse_ical();

# TODO: should some of these tests be "eq", since we're testing for
# leading zeroes and such occasionally? (Or are we?) --srl
ok($parsetest->{second} == "0", "_parse_ical seconds are correct on epoch 0");
ok($parsetest->{minute} == "0", "_parse_ical minutes are correct on epoch 0");
ok($parsetest->{hour} == "0", "_parse_ical hours are correct on epoch 0");
ok($parsetest->{day} == "01", "_parse_ical days are correct on epoch 0");
ok($parsetest->{month} == "01", "_parse_ical months are correct on epoch 0");
ok($parsetest->{year} == "1970", "_parse_ical year is correct on epoch 0");
ok($parsetest->{floating} == "0", "_parse_ical floating flag is correct for UTC on epoch 0");

ok($parsetest->{timezone} eq "UTC", "_parse_ical timezone is correct for UTC on epoch 0");

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

=cut

#{{{ sub _parse_ical
sub _parse_ical {
    my $self = shift;

    $self->{ical} =~ s/^(?:(?:DTSTAMP|DTSTART|DTEND)[:;])//;
    my $ical = $self->{ical};

    # grab the timezone, if any
    $ical =~ s/^(?:TZID=([^:]+):)?//;
    my $tz = $1;

    my ( $year, $month, $day, $hour, $minute, $second, $zflag ) =
      $ical =~ /^(?:(\d{4})(\d\d)(\d\d))
           (?:T(\d\d)?(\d\d)?(\d\d)?)?
               (Z)?$/x;

    $zflag = $ical =~ /Z$/;

    # DEBUGGING:
    # print "$year $month $day $hour $minute $second $zflag\n";
    unless ( defined($year) ) {
        carp "Invalid DATE-TIME format ($ical)";
        return undef;
    }

    if ( defined($tz) || defined($zflag) ) {
        $self->{floating} = 0;
    }
    else {
        $self->{floating} = 1;
    }

    if ( defined($zflag) && ($tz) ) {
        carp "Invalid DATE-TIME format -- may not include both Z and timezone";
        return undef;
    }

    $self->{timezone} = defined($zflag) ? 'UTC' : $tz;

    $self->{year}  = $year;
    $self->{month} = $month;
    $self->{day}   = $day;

    $self->{hour}   = $hour || 0;
    $self->{minute} = $minute || 0;
    $self->{second} = $second || 0;

    #$self->{epoch} = $self->_epoch_from_ical;

    # TODO: this doesn't set the epoch time properly.
}

#}}}

=head2 _format_ical

    $self->_format_ical;

This is an internal method used to rebuild the ical string when one component,
such as the C<second> or C<month> field has been changed.

=begin testing

my $time = Date::ICal->new (epoch => '0');

# test that this function doesn't return undef
ok($time->_format_ical(), '_format_ical is defined on zero epoch');

ok($time->{ical} eq '19700101', '_format_ical is correct on zero epoch'); 


=end testing

=cut

#{{{ sub _format_ical
sub _format_ical {
    my $self = shift;

    if ( $self->hour || $self->min || $self->sec ) {
        $self->{ical} =
          sprintf( '%04d%02d%02dT%02d%02d%02d', $self->year, $self->month,
          $self->day, $self->hour, $self->minute, $self->second );
    }
    else {
        $self->{ical} =
          sprintf( '%04d%02d%02d', $self->year, $self->month, $self->day );
    }

    if ( $self->{timezone} ) {
        my $tz = $self->{timezone};
        $self->{ical} =
          ( $tz eq 'UTC' ) ? $self->{ical} . 'Z' : "TZID=$tz:" . $self->{ical};
    }

    return $self->{ical};
}

#}}}

=head2 _epoch_from_ical

    $self->_epoch_from_ical;

This is an internal method used to determine the epoch time from the ical value.

=begin testing

$epochtest = Date::ICal->new(epoch => '0');
ok($epochtest->_epoch_from_ical == '0', "_epoch_from_ical from epoch => 0");

# TODO: should this be "ical => '19700101Z'?
$epochtest = {};
$epochtest = Date::ICal->new(ical => '19700101');

ok($epochtest->_epoch_from_ical == '0', 
    "_epoch_from_ical = 0 if ical => 19700101");

ok($preepoch->_epoch_from_ical eq undef, 
    "Dates before the epoch have no epoch time." );

ok($postepoch->_epoch_from_ical eq undef,
    "Dates after the epoch have no epoch time." );

=end testing

=cut

#{{{ sub _epoch_from_ical
sub _epoch_from_ical {
    my $self = shift;

    foreach my $unit (qw(second minute hour day month year)) {
        carp "$unit was not defined" unless defined $self->$unit;
    }

    if ( ( $self->year < 1970 ) || ( $self->year > 2038 ) ) {
        carp "There is no epoch value defined for years outside the epoch";
        return undef;
    }
    
    my $epoch;
    if ( $self->hour != 0 ) {
        $epoch = Time::Local::timegm(
          $self->second, $self->minute, $self->hour, $self->day,
          $self->month - 1,  $self->year - 1900
        );

    }
    else {
        $epoch =
          Time::Local::timegm( 0, 0, 0, $self->day, $self->month - 1,
          $self->year - 1900 );
    }

    return $epoch;
}

#}}}

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

my $t = Date::ICal->new( ical => '19961122T183020' );
$t->add( month => 2);

#test 1 check year rollover works
ok($t->year == 1997, "year rollover");
#test 2 check month set on year rollover
ok($t->month == 1, "month set on year rollover");

$t->add( week => 2 );

#test 3 & 4 check year/month rollover with attrib setting
$t->month(14);
ok($t->year == 1998, "year rollover with attrib setting");
ok($t->month == 2, "month rollover with attrib setting");

#test 5 & 6 test subtraction with attrib setting
$t->month(-2);
ok($t->year == 1997, "subtraction with attrib setting (year)");
ok($t->month == 10, "subtraction with attrib setting (month)");


# Now, test the adding of durations

$t = Date::ICal->new (ical => '19860128T163800Z');

$t->add(duration => 'PT1M12S');
print $t->ical . "\n";
ok ($t->ical eq '19860128T163912Z', "Adding durations with minutes and seconds works");

$t = Date::ICal->new (ical => '19860128T163800Z');

$t->add(duration => 'PT30S');
print $t->ical . "\n";
ok ($t->ical eq '19860128T163830Z', "Adding durations with seconds only works");

$t = Date::ICal->new (ical => '19860128T163800Z');

$t->add(duration => 'PT1H10M');
print $t->ical . "\n";
ok ($t->ical eq '19860128T174800Z', "Adding durations with hours and minutes works");


$t = Date::ICal->new (ical => '19860128T163800Z');

$t->add(duration => 'P3D');
print $t->ical . "\n";
# XXX: what's "right" in the following test? should the result
# just be a date, or a date and time?
ok ($t->ical eq '19860131T163800Z', "Adding durations with days only works");


$t = Date::ICal->new (ical => '19860128T163800Z');

$t->add(duration => 'P3DT2H');
print $t->ical . "\n";
ok ($t->ical eq '19860131T183800Z', "Adding durations with days and hours works");


$t = Date::ICal->new (ical => '19860128T163800Z');

$t->add(duration => 'P3DT2H20M15S');
print $t->ical . "\n";
ok ($t->ical eq '19860131T185815Z', "Adding durations with days, hours, minutes, and seconds works");



=end testing

=cut

sub add {
    my ( $self, $attrib, $arg ) = @_;

    carp "Date::ICal::add was called by an undefined object" unless defined($self);
    carp "Date::ICal::add was called without an attribute" unless defined($attrib);
    carp "Date::ICal::add was called without an attribute arg" unless defined($arg);
    #my $pos = $arg > 0 ? "+" : "-";
    # TODO: this seems not to be able to add anything but weeks or durations.
    # That probably needs fixing.
    if ( $attrib eq 'week' ) { $arg *= 7; $attrib = "day"; }
    if ( $attrib eq 'duration' ) { return $self->add_duration($arg); }
    if ( $attrib =~ /^P/ ) { return $self->add_duration($attrib); }
    $self->$attrib( $self->$attrib() + $arg );
    $self->_alter_period($attrib);
    #$self->{epoch} = $self->_epoch_from_ical;
}

=head2 _normal

  $self->_normal($attrib,$suggestednewvalue);

  This attempts to flatten out of range values to what they should be and adjust
adjcent values accordingly.  For instance passing 'month' and 14 to _normal
would result in the year being incremented and the ical month field being set
to two

=cut

sub _normal {
    my ( $self, $attrib, $newvalue ) = @_;
    if ( $attrib eq 'week' ) { $newvalue *= 7; $attrib = "day"; }
    $self->{$attrib} = $newvalue;
    $self->_alter_period($attrib);
    $self->{epoch} = $self->_epoch_from_ical;
    return $self->{$attrib};
}

=head2 _month_length

  $self->_month_length();

  This utility returns the length of the current ical month.

=cut

sub _month_length {
    my @months = qw(31 28 31 30 31 30 31 31 30 31 30 31);
    my $self   = shift;
    my $m      = $months[ $self->month() - 1 ];
    $m++
      if ( $self->{month} == 2
      && ( ( $self->{year} % 4 ) == 0 && !( $self->{year} / 1000 ) ) );
    return $m;
}

#period prop is used by _alter_period

my %period_prop = (
  'second' => { 'offset' => 0, 'multip'   => 1,    'overflow' => 59 },
  'minute' => { 'offset' => 1, 'overflow' => 59 },
  'hour'   => { 'offset' => 2, 'multip'   => 3600, 'overflow' => 23 },
  'day'   => { 'offset' => 3, 'multip' => 86400, overflow => \&month_length },
  'month' => { 'offset' => 4, overflow => 12,    fix      => 11 },
  'year'  => {
      'offset' => 5,
      overflow => 2038
  }
);

=head2 _alter_period

  $self->_alter_period($attrib);

called by add and _normal to do the hard work of flattening the values

=cut

sub _alter_period {
    my ( $self, $attrib ) = @_;

    my $overflow = $period_prop{$attrib}->{overflow};
    $overflow = $self->_month_length() if ( $attrib eq 'day' );

    my $month_sign = $self->{month} > 0 ? 0 : -1;
    $self->{year} =
      $self->{year} + ( int( $self->{month} / 12 ) + $month_sign );
    $self->{month} = ( ( $self->{month} - 1 ) % 12 ) + 1;

    if ( $self->{$attrib} > $overflow || $self->{$attrib} < 1 ) {
        my $overflow = $period_prop{$attrib}->{fix} || $overflow;
        my @norm =
          ( $self->{second}, $self->{minute}, $self->{hour}, $self->{day},
          $self->{month} - 1, $self->{year} - 1900 );
        $norm[ $period_prop{$attrib}->{offset} ] =
          $self->{$attrib} > 0 ? $overflow : 1;
        my $tempgm = timegm(@norm);

        if ( exists( $period_prop{$attrib}->{multip} ) ) {
            $tempgm +=
              ( $self->{$attrib} - $norm[ $period_prop{$attrib}->{offset} ] ) *
              $period_prop{$attrib}->{multip};
        }
        $self->epoch($tempgm);
    }
    return $self->{$attrib};
}

sub _duration_as_sec {

    #from old Net::ICal::Duration
    my ( $self, $str ) = @_;
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
      map { defined($_) || 0 } @temp[ 2 .. $#temp ];

    unless ( defined($magic) ) {
        carp "Invalid duration: $str";
        return undef;
    }
    $sign = ( ( defined($sign) && $sign eq '-' ) ? -1 : 1 );
    return $sign * ( $mins * 60 ) + ( $hours * 3600 ) + ( $days * 86400 ) +
      ( $weeks * 604800 );
}

#}}}

=head2 add_duration

   $self->add_duration('P2W');

Adds a rfc2445 duration to current $self->{ical}

=cut

sub add_duration {
    my ( $self, $addon ) = @_;
    $self->epoch( $self->_epoch_from_ical + $self->_duration_as_sec($addon) );
    $self->{epoch} = $self->_epoch_from_ical;
}

=head2 compare

    $cmp = $date1->compare($date2);

    @dates = sort {$a->compare($b)} @dates;

Compare two Date::ICal objects. Semantics are compatible with
sort; returns -1 if $a < $b, 0 if $a == $b, 1 if $a > $b. 

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

=cut

sub compare {
    my ($self, $otherdate) = (@_);

    unless (defined($otherdate)) {return undef};
    
    # here, we walk through units from largest to smallest;
    # if we find a difference, then it's reflective of the difference
    # between the units as a whole. 
    my @units = qw(year month day hour minute second);

    foreach my $unit (@units) {
        if ($self->$unit < $otherdate->$unit) {
            return -1;
        } elsif ($self->$unit > $otherdate->$unit) {
            return 1;
        } 
        # if they're equal for this unit, fall through to the next smaller unit.
    }
    # if we got all this way and haven't yet returned, the units are equal.
    return 0;
}

1;

=head1 TODO

=over 4 

=item - IMPORTANT: rework internal storage to Julian dates and times

=item - add support for initializing dates 

=item - add timezone support, including moving between timezones

=item - add arithmetic methods: add, subtract, ...

=item - add gmtime and localtime methods, perhaps?

=head1 AUTHOR

Rich Bowen (DrBacchus) rbowen@rcbowen.com

And the rest of the Reefknot team.

=head1 SEE ALSO

datetime@perl.org mailing list

http://reefknot.org/

http://dates.rcbowen.com/

Time::Local

Net::ICal

=cut
