package Date::ICal;
use strict;

use vars qw($VERSION);
$VERSION = (qw'$Revision: 1.14 $')[1];
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

The ICal represenatation is the one authoritative value in the object, and so
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

=cut

#{{{ sub epoch
sub epoch {
    my ( $self, $epoch ) = @_;

    if ( defined($epoch) ) {

        my ( $ss, $mm, $hh, $DD, $MM, $YY ) = gmtime($epoch);

        $self->{year}   = $YY + 1900;
        $self->{month}  = $MM + 1;
        $self->{day}    = $DD;
        $self->{hour}   = $hh;
        $self->{minute} = $mm;
        $self->{second} = $ss;

        $self->_format_ical;
    }

    unless ( exists $self->{epoch} ) {
        $self->_parse_ical;
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

    $self->{timezone} = $zflag ? 'UTC' : $tz;

    $self->{year}  = $year;
    $self->{month} = $month;
    $self->{day}   = $day;

    $self->{hour}   = $hour || 0;
    $self->{minute} = $minute || 0;
    $self->{second} = $second || 0;

    $self->{epoch} = $self->_epoch_from_ical;

    # TODO: this doesn't set the epoch time properly.
}

#}}}

=head2 _format_ical

    $self->_format_ical;

This is an internal method used to rebuild the ical string when one component,
such as the C<second> or C<month> field has been changed.

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
}

#}}}

=head2 _epoch_from_ical

    $self->_epoch_from_ical;

This is an internal method used to determine the epoch time from the ical value.

=cut

#{{{ sub _epoch_from_ical
sub _epoch_from_ical {
    my $self = shift;

    my $epoch;
    if ( $self->{hour} != 0 ) {
        $epoch = Time::Local::timegm(
          $self->{second}, $self->{minute}, $self->{hour}, $self->{day},
          $self->{month},  $self->{year}
        );

    }
    else {
        $epoch =
          Time::Local::timegm( 0, 0, 0, $self->{day}, $self->{month} - 1,
          $self->{year} - 1900 );
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

=cut

=head2 add

   $self->add( month=>2 );
   $self->add( duration =>'P1W' );

=begin testing


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

=end testing

=cut

sub add {
    my ( $self, $attrib, $arg ) = @_;
    my $pos = $arg > 0 ? "+" : "-";
    if ( $attrib eq 'week' ) { $arg *= 7; $attrib = "day"; }
    if ( $attrib eq 'duration' ) { return $self->add_duration($arg); }
    if ( $attrib =~ /^P/ ) { return $self->add_duration($attrib); }
    $self->$attrib( $self->$attrib() + $arg );
    $self->_alter_period($attrib);
    $self->{epoch} = $self->_epoch_from_ical;
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

=head2

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

=head _alter_period

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

    if ( !defined($magic) ) {
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

    $cmp = $date2 Date::ICal::compare $date2;

    @dates = sort {$a Date::ICal::compare $b} @dates;

Compare two Date::ICal objects. Symantics should be compatible with
sort.

=cut

sub compare {

}

1;

=head1 TODO

=over 4 

=item - add support for initializing dates 

=item - add timezone support, including moving between timezones

=item - add arithmetic methods: add, subtract, ...

=item - add gmtime and localtime methods, perhaps?

=head1 AUTHOR

Rich Bowen (DrBacchus) rbowen@rcbowen.com

=head1 SEE ALSO

datetime@perl.org mailing list

http://reefknot.org/

http://dates.rcbowen.com/

Time::Local

Net::ICal

=cut
