package Date::ICal;
use strict;

use vars qw($VERSION $localzone $localoffset @months @leapmonths);
$VERSION = (qw'$Revision: 1.49 $')[1];
use Carp;
use Time::Local;
use Date::Leapyear qw();
use Memoize;
use overload
   '<=>' => 'compare',
   'fallback' => 1;

$localzone   = $ENV{TZ} || 0;
$localoffset = _calc_local_offset();

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
    # will default to the timezone specified in $TZ, see below

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

new() handles timezones. It defaults times to UTC (Greenwich
Mean Time, also called Zulu). If you want to set up a time
that's in the US "Pacific" timezone, which is GMT-8, use something
like:
    
    my $ical = Date::ICal->new( ical => '19971024T120000',
                                offset => "-0800");
   
Note that as of version 1.44, new() tries to be intelligent
about figuring out your local time zone. If you enter
a time that's not *explicitly* in UTC, it looks at
the environment variable $TZ, if it exists, to determine
your local offset. If $TZ isn't set, new() will complain.

=cut

#{{{ sub new

sub new {
    my $class = shift;
    my ( $self, %args, $sec, $min, $hour, $day, $month, $year, $tz);

    # $zflag indicates whether or not this time is natively in UTC    
    my $zflag = 0;

    # First argument can be a Date::ICal (or subclass thereof) object
    if ( ref $_[0] ) {
        $args{ical} = $_[0]->ical;
    } else {
        %args = @_;
    }
    
    $self = {}; 

    # Date is specified as epoch#{{{
    if ( defined( $args{epoch} ) ) {

        ( $sec, $min, $hour, $day, $month, $year ) =
          ( gmtime( $args{epoch} ) )[ 0, 1, 2, 3, 4, 5 ];
        $year += 1900;
        $month++;

        # TODO: is this what we really want here? I think so --srl
        $zflag = 1;     # epoch times are by definition in GMT
        
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

        
        # TODO: figure out what to do if we get a TZID. 
        # I'd suggest we store it for use by modules that care
        # about TZID names. But we don't want this module
        # to deal with timezone names, only offsets, I think.
        # --srl
        
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

		# Since we are defaulting, this qualifies as UTC
		$zflag = 1;

        ( $sec, $min, $hour, $day, $month, $year ) =
          ( gmtime( time ))[ 0 .. 5 ];
        $year += 1900;
        $month++;
    }    #}}}
    
    $self->{julian} = leapdays_before($year) + days_this_year( $day, $month, $year )
						+ $year * 365 ;
    $self->{julsec} = time_as_seconds( $hour, $min, $sec );
    bless $self, $class; 
 
	if (exists($args{offset})) {
        if ($zflag) {
			carp "Time had conflicting offset and UTC info. Using UTC"
				unless $ENV{HARNESS_ACTIVE};
        } else {
            # Set up the offset for this datetime.
            $self->offset($args{offset} || 0);
        }
	} elsif (! $zflag) {
        unless (defined $ENV{TZ}) {
            carp 'the environment variable $TZ is not set'
                unless $ENV{HARNESS_ACTIVE};
        }
        my $tz = $ENV{TZ} || '0';
		my $loc = $tz eq $localzone ?  $localoffset : _calc_local_offset();
		carp "saw no offset; setting offset to $loc"
			unless $ENV{HARNESS_ACTIVE};
		$self->offset($loc) if defined $self;
    }
 
    return $self;
}
#}}}

#{{{ sub ical

=head2 ical

    $ical_string = $ical->ical;

Retrieves, or sets, the date on the object, using any valid ICal date/time
string. Output is in UTC (ends with a "Z") by default. To get
output in localtime relative to the current machine, do:
    
    $ical_string = $ical->ical( localtime => 1 );

To get output relative to an arbitrary offset, do:

    $ical_string = $ical->ical( offset => '+0545' );

=cut

sub ical {
    my $self = shift;
    if (1&@_) {			# odd number of parameters?
	carp "Bad args: expected named parameter list";
	shift;			# avoid warning from %args=@_ assignment
    }
    my %args = @_;
    my $ical;
    
    if (exists $args{localtime}) {
		carp "can't have localtime and offset together, using localtime offset"
			if exists $args{offset};
        # make output in localtime format by setting $args{offset}
		$args{offset} = $self->offset;
	}

    if (exists $args{offset}) {
        # make output based on an arbitrary offset
		# No Z on the end!
		my $julian = $self->{julian};
		my $julsec = $self->{julsec};
		my $adjust =  _offset_to_seconds($args{offset});
		$self->add(seconds => -$adjust);
	    $ical = sprintf( '%04d%02d%02dT%02d%02d%02d',
					$self->year, $self->month, $self->day,
					$self->hour, $self->minute, $self->second,
					);
		$self->{julian} = $julian;
		$self->{julsec} = $julsec;
    } else {
        # make output in UTC by default
	    # if we were originally given this time in offset
	    # form, we'll need to adjust it for output
	    if ( $self->hour || $self->min || $self->sec ) {
            $ical =
                sprintf( '%04d%02d%02dT%02d%02d%02dZ', 
                    $self->year, $self->month, $self->day, 
                    $self->hour, $self->minute, $self->second );
        } else {
            $ical =
                sprintf( '%04d%02d%02dZ', 
                    $self->year, $self->month, $self->day );
        }
    } 
    
    return $ical;
}

#}}}

#{{{ sub epoch

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

sub epoch {
    my $self = shift;
    my $class = ref($self);

    my $epoch;

    if ( $epoch = shift ) {    # Passed in a new value

        my $newepoch = $class->new( epoch => $epoch );
        $self->{julian} = $newepoch->{julian};
        $self->{julsec} = $newepoch->{julsec};

    }

    else {    # Calculate epoch from components, if possible

        $epoch =
          timegm( $self->sec, $self->min, $self->hour, $self->day,
          ( $self->month ) - 1, ( $self->year ) - 1900 );
    }

    return $epoch;
}

#}}}

#{{{ sub _offset_to_seconds

=head2 _offset_to_seconds

    $seconds_plus_or_minus = offset_to_seconds($offset);
   
Changes -0600 to -18000. Not object method, no side-effects.

=cut

sub _offset_to_seconds {
	my $offset = shift;

	# Relocated from offset for re-use
	my $newoffset;

	if ($offset eq '0') {
		$newoffset = 0;
	}
	elsif ($offset =~ /^([+-])(\d\d)(\d\d)\z/) {
		my ($sign, $hours, $minutes) = ($1, $2, $3);
		# convert to seconds, ignoring the possibility of leap seconds
		# or daylight-savings-time shifts
		$newoffset = $hours*60*60 + $minutes*60;   
		$newoffset = $sign . $newoffset;      # this is probably inefficient

	}
	else {
		carp("You gave an offset, $offset, that makes no sense");
		return undef;
	}
	return $newoffset;
}

#}}}

#{{{ sub _offset_from_seconds

=head2 _offset_from_seconds

    $seconds_plus_or_minus = offset_from_seconds($offset_in_seconds);
   
Changes -18000 (seconds) to -0600 (hours, minutes). 
Not object method, no side-effects.

=cut

sub _offset_from_seconds {
	my $secoffset = shift;
    my $hhmmoffset = 0;
    
    if ($secoffset ne '0') {
        my ($sign, $secs) = ("", "");
        ($sign, $secs) = $secoffset =~ /([+-])?(\d+)/;
        # throw in a + to make this look like an offset if positive
        $sign = "+" unless $sign;   
       
        # NOTE: the following code will return "+0000" if you give it a number
        # of seconds that are a multiple of a day. However, for speed reasons
        # I'm not going to write in a comparison to reformat that back to 0.
        # 
        my $hours = $secs / (60 * 60);
        $hours = $hours % 24;
        my $mins = ($secs % (60 * 60))/60;
        $hhmmoffset = sprintf('%s%02d%02d', $sign, $hours, $mins);
        
    }
    
	return $hhmmoffset;
}

#}}}


#{{{ sub offset

=head2 offset 

    $offset = $ical->offset;
   
    # We need tests for these.  
    $ical->offset( '+1100' ); # a number of hours and minutes: UTC+11
    $ical->offset( 0 );       # reset to UTC

Sets or retrieves the offset from UTC for this time. This allows
timezone support, assuming you know what your local (or non-local)
UTC offset is. Defaults to 0. 

Internals note: all times are internally stored in UTC, even though they
may have some offset information. Offsets are internally stored in
signed integer seconds. 

BE CAREFUL about using this function on objects that were initialized
with an offset. If you started an object with:
    
    my $d = new(ical=>'19700101120000', offset=>'+0100'); 

and you then call: 

    $d->offset('+0200'); 
    
you'll be saying "Yeah, I know I *said* it was in +0100, but really I
want it to be in +0200 now and forever." Which may be your intention,
if you're trying to transpose a whole set of dates to another timezone---
but you can also do that at the presentation level, with
the ical() method. Either way will work.

=cut

sub offset {
    my ($self, $offset) = @_;
    my $newoffset = undef;

    if ( defined($offset) ) {    # Passed in a new value
        $newoffset = _offset_to_seconds($offset);
	
        unless (defined $newoffset) { return undef; }

        # since we're internally storing in GMT, we need to
        # adjust the time we were given by the offset so that
        # the internal date/time will be right.

        if ($self->{offset}) {
            # figure out whether there's a difference between
            # the existing offset and the offset we were given.
            # If so, adjust appropriately.
            my $offsetdiff = $self->{offset} - $newoffset;
            if ($offsetdiff) {
                $self->{offset} = $newoffset;
                $self->add(seconds => $offsetdiff);  
            } else {
                # leave the offset the way it is
            }
        } else {
            $self->add(seconds => -$newoffset);
            $self->{offset} = $newoffset;
        }

    } else {    
        if ($self->{offset}) {
            $offset = _offset_from_seconds($self->{offset});
        } else {
            $offset = 0;
        }
    }


    return $offset;
}

#}}}

=cut

# sub add #{{{

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

=cut

sub add {
    my $self = shift;
    my %args = @_;

    carp "Date::ICal::add was called by an undefined object"
      unless defined($self);
    carp "Date::ICal::add was called without an attribute arg"
      unless ( keys %args );

    my ($seconds, $days);

    if ( defined $args{duration} ) {

        ($days, $seconds) = duration_value( $args{duration} );
    
    } elsif (defined $args{seconds} ) {
        $days = 0;
        $seconds = $args{seconds};
    
    } else {

        $seconds =  0;
        $seconds += $args{sec}             if defined $args{sec};
        $seconds += $args{min}  * 60       if defined $args{min};
        $seconds += $args{hour} * 60 * 60  if defined $args{hour};

	$days = 0;
        $days += $args{day}       if defined $args{day};
        $days += $args{week} * 7  if defined $args{week};

	if ($args{month}) {
	    my @months = months($self->year);
	    my $start = $months[$self->month - 1];
	    my $end = $self->month + $args{month};
	    my $add = 0;
	    if ($end > 12) {
		$end -= 12;
		$add = $months[12];
		@months = months($self->year + 1);
	    }
	    $end = $months[$end - 1] + $add;

	    $days += $end - $start;
	}
    }

    # ick, we really don't want this.
    my $daycount = int( $seconds/86400 );
    $seconds -= ( $daycount * 86400 );
    $days += $daycount;

    $self->{julian} += $days;
	$self->{julsec} += $seconds;

    return $self;
}
#}}}

# sub duration_value #{{{

sub duration_value {
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

    my $s = $sign * ( $secs + ( $mins * 60 ) + ( $hours * 3600 ) );
    my $d = $sign * ( $days + ( $weeks * 7 ) );
    return ($d, $s);
}

#}}}

# sub compare #{{{

=head2 compare

    $cmp = $date1->compare($date2);

    @dates = sort {$a->compare($b)} @dates;

Compare two Date::ICal objects. Semantics are compatible with
sort; returns -1 if $a < $b, 0 if $a == $b, 1 if $a > $b. 

=cut

sub compare {
    my ( $self, $otherdate ) = (@_);

    unless ( defined($otherdate) ) { return undef }

    # One or more days different

    if      ($self->{julian} < $otherdate->{julian}) {
        return -1;
    } elsif ($self->{julian} > $otherdate->{julian}) {
        return 1;

    # They are the same day
    } elsif ( $self->{julsec} < $otherdate->{julsec}) {
        return -1;
    } elsif ( $self->{julsec} > $otherdate->{julsec}) {
        return 1;
    }

#    # if we got all this way and haven't yet returned, the units are equal.
    return 0;
}

#}}}

=begin internal

 @months = months($year);

Returns the Julian day at the end of a month, correct for that year.

=end internal 

=cut

# precalculate these values at module load time so that we don't
# have to do it repeatedly during runtime. 
# 
BEGIN {
    #           +  31, 28, 31, 30,  31,  30,  31,  31,  30,  31,  30,  31
	@months = ( 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365 );
	@leapmonths = @months;

	for ( 2 .. 12 ) {
		$leapmonths[$_] = $months[$_] + 1;
	}
}

sub months {
    return Date::Leapyear::isleap(shift) ? @leapmonths : @months;
}

=begin internal

 $days = leapdays_before( 1984 );

Returns the number of leap days occurring before the specified year.
Starts counting in year 0 AD. Assumes (incorrectly, but sufficient for
our purposes, if we are internally consistent) that all years since 0
have obeyed standard leap year rules.

Cheats on years between 1900 and 2099 by using a pre-calculated,
hardcoded cheat array. This gives some speed increases.

=end internal

=cut

=begin internal

Routine to produce leapcheat array:

	use Date::Leapyear;

	my @cheat;
	for(1900 .. 2099) {
		push @cheat, leapdays_before($_);
	}

	my $string = 'my @leapcheat = (' . "\n";
	for( $i = 0; $i += 10) {
		$string .= "\t";
		$string .= join ",", @cheat[$i .. $i + 9];
		$string .= ",\n";
	}
	$string =~ s/[\s,]+$//;
	$string .= "\n);";

	print $string;

Might be nice to make it a run-time option.

=end internal

=cut 

{
my @leapcheat = (
	461,461,461,461,461,462,462,462,462,463,
	463,463,463,464,464,464,464,465,465,465,
	465,466,466,466,466,467,467,467,467,468,
	468,468,468,469,469,469,469,470,470,470,
	470,471,471,471,471,472,472,472,472,473,
	473,473,473,474,474,474,474,475,475,475,
	475,476,476,476,476,477,477,477,477,478,
	478,478,478,479,479,479,479,480,480,480,
	480,481,481,481,481,482,482,482,482,483,
	483,483,483,484,484,484,484,485,485,485,
	485,486,486,486,486,487,487,487,487,488,
	488,488,488,489,489,489,489,490,490,490,
	490,491,491,491,491,492,492,492,492,493,
	493,493,493,494,494,494,494,495,495,495,
	495,496,496,496,496,497,497,497,497,498,
	498,498,498,499,499,499,499,500,500,500,
	500,501,501,501,501,502,502,502,502,503,
	503,503,503,504,504,504,504,505,505,505,
	505,506,506,506,506,507,507,507,507,508,
	508,508,508,509,509,509,509,510,510,510
);

sub leapdays_before {
    my $year = shift;
	return $leapcheat[$year - 1900] if defined $leapcheat[$year - 1900];

    my $days = 0;

    my $counter = 0;

    while ( $counter < $year ) {
        $days++ if Date::Leapyear::isleap($counter);
        $counter += 4;
    }

    return $days;
}

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

 ( $day, $month, $year ) = parsedays ( $julian, $secs_since_midnight );

Given a modified modified julian day, returns the day, month, year for
the given date.

=end internal

=cut

sub parsedays {
    my $self = shift;
    my $day = $self->{julian};
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
    my $time = $self->{julsec};
    return parsetime_memo( $time );
}

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

This method really only is here for compatibility with previous
versions, as the jd method is now thrown over for plain hash references.

See the file INTERNALS for more information about this internal
format.

=cut

sub jd {
    my $self = shift;

    if ( my $jd = shift ) {
        ($self->{julian}, $self->{julsec}) = @$jd;
    }

    return [ $self->{julian}, $self->{julsec} ];
}

sub julian { return jd(@_) }



# INTERNAL ONLY: figures out what the UTC offset (in HHMM) is
# is for the current machine.
sub _calc_local_offset {

    use Time::Local;
	my @t = gmtime;

    my $local =  timelocal(@t);
    my $gm    = timegm(@t);

    my $secdiff = $gm - $local;
    return _offset_from_seconds($secdiff); 
}




#}}}

1;

=head1 TODO

=over 4 

=item - add timezone support, including moving between timezones

=item - add gmtime and localtime methods, perhaps?

=head1 INTERNALS

Please see the file INTERNALS for discussion on the internals.

=head1 AUTHOR

Rich Bowen (DrBacchus) rbowen@rcbowen.com

And the rest of the Reefknot team. See the source for a full
list of patch contributors and version-by-version notes.

=head1 SEE ALSO

datetime@perl.org mailing list

http://reefknot.org/

http://dates.rcbowen.com/

Time::Local

Net::ICal

# CVS History #{{{

=head1 CVS History

  $Log: ICal.pm,v $
  Revision 1.49  2001/11/22 10:56:23  srl
  This version incorporates a patch by Yitzchak Scott-Thoennes to
  adjust the offset() API. It no longer takes integer seconds
  as a parameter, because there's no programmatic way to tell the
  difference between, say, +3600 (UTC+1 in seconds) and +3600 (UTC+36,
  if you interpret that as an HHMM value).

  I also refactored things a bit, creating an _offset_from_seconds
  method to match _offset_to_seconds; this should eliminate some
  duplication. There's also new POD to clear up some confusion about
  new(offset => foo) used together with offset().

  Revision 1.48  2001/11/22 09:22:24  srl
  API-consistency patch from Yitzchak Scott-Thoennes <sthoenna@efn.org>;
  Makes the ical() method take a hash of parameters, not a hashref,
  so that ical() is like the other methods. This crept in around 1.44,
  and it shouldn't have. My mistake.

  Revision 1.47  2001/11/22 09:02:34  srl
  Fixed some 5.6isms; patch contributed by Yitzchak Scott-Thoennes
  <sthoenna@efn.org>.

  Revision 1.46  2001/11/15 13:25:04  srl
  Minor patches to tests; another piece of optimization from Mike Heins.

  Revision 1.45  2001/11/15 05:32:17  srl
  Added benchmark.pl to help developers in optimizing the module.
  Also modified new() to warn more clearly if the $TZ environment
  variable isn't set, and not to utterly fail tests if $TZ isn't there.

  Revision 1.44  2001/11/15 05:11:32  srl
  Further patches from Mike Heins, plus some documentation from me:
  	- added localtime argument to ical() for output in localtime.
  	  Added documentation about the localtime argument. Note
            that $ENV{$TZ} is now relevant to some of the module's behavior.
  	- removed a memoize() that wasn't providing significant speed
  	  improvements.
  	- minor optimization of _calc_local_offset

  Revision 1.43  2001/11/15 04:20:38  srl
  Committed another small patch by Mike Heins, which precalculates
  the values returned by the months() function so that the module
  is faster at runtime.

  Revision 1.42  2001/11/15 04:11:13  srl
  Another patch from Mike Heins (mheins@minivend.com); an optimizing
  cheat for leapyears; uses a precalculated table of values instead
  of always calculating leapyear values. I edited Mike's patch slightly
  so that @leapcheat isn't a package global.

  Revision 1.41  2001/11/15 03:58:34  srl
  Incorporated part of a patch by Mike Heins (mheins@minivend.com);
  an optimization. Internal storage of julian times is now in
  $self->{julian} and $self->{julsec}, instead of using an array.
  This gives us slightly better speed. Also, made some of the UTC
  behaviors slightly more consistent.

  Revision 1.40  2001/10/16 10:33:44  srl
  Further fixes to the offset() method. This code isn't as well-tested
  as I'd like it to be, but it seems to do the right thing for all the
  tests that are there. I had to revise many of the tests, because
  the API semantics have changed. Times must now be explicitly
  specified with a Z in order to be handled as UTC.

  Revision 1.39  2001/10/10 02:58:29  srl
  Added some tests, reorganized some code to prepare spaces for
  offset/timezone-aware output. Added at least one test that's
  known to fail for purposes of knowing when we succeed. :)

  Revision 1.38  2001/10/09 04:28:58  srl
  Started working on code to properly handle times with offsets from GMT.
  added a new _calc_local_offset method to figure out what the
  current machine's UTC offset is. We need tests for this that will
  work in any timezone; patches welcome.

  Revision 1.37  2001/09/30 13:19:14  lotr
  * Oops, forgot some bits when I added month to add()
  * use overload for compare

  Revision 1.36  2001/09/29 11:01:55  lotr
  Add the ability to add months to a date. Needed for Net::ICal::Recurrence

  Revision 1.35  2001/09/26 15:26:09  lotr
  * fix off-by-one error in months() and add tests for that

  Revision 1.34  2001/09/12 03:26:23  rbowen
  There's no particular reason to have Date::ICal be 5.6 dependant.

  Revision 1.33  2001/08/25 12:20:30  rbowen
  Fixed bug reported by Chris Jones. In sub add, I was checking one
  attribute and using another. Added tests for this bug, and for adding
  durations by attribute.

  Revision 1.32  2001/08/10 03:27:47  srl
  Started adding timezone support by making an offset() method and an offset
  property. This still needs to be wired into the new() method and the
  output methods, but we have to resolve some interface details first.

  Revision 1.31  2001/08/07 02:41:11  rbowen
  Test::More gets angry if there are no tests.

  Revision 1.30  2001/08/07 02:30:01  rbowen
  Moved the inline tests into t/ for the sake of making the module more
  readable. Please don't let this discorage you from writing inline
  tests.

  Revision 1.29  2001/08/06 19:32:39  rbowen
  Creating an object without args was calling gmtime( $args{epoch} ).
  Fixed and added tests. Also added Time::HiRes to PREREQ list.

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


=cut

#}}}


