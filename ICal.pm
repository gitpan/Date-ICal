package Date::ICal;
use strict;

use vars qw($VERSION);
$VERSION = (qw'$Revision: 1.11 $')[1];
use Carp;

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
    my %args = @_;

    my $self = \%args;
    bless $self, $class;

    if (defined ($args{epoch}) ) {
        $self->epoch( $args{epoch} );
    
    } elsif (defined ($args{ical}) ) {
        # Don't actually need to do anything, except perhaps verify the
        # validity of the argument
        $self->ical($args{ical} );

    } else {
        $self->epoch( time );
    
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
        foreach my $attrib (qw(epoch second minute hour day month year)) {
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
    my ($self, $epoch) = @_;


    if (defined($epoch)) {

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
    my $self = shift;

    if ( my $value = shift ) {
        $self->{$attrib} = $value;
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

    print "i see ical $ical \n";
    $zflag = $ical =~ /Z$/;

    # DEBUGGING:
    # print "$year $month $day $hour $minute $second $zflag\n";
    unless ( defined($year) ) {
        carp "Invalid DATE-TIME format ($ical)";
        return undef;
    }

    if ( defined($tz) || defined($zflag) ) {
        $self->{floating} = 0;
    } else {
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

    $self->{hour} = $hour || 0;
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

    if ( $self->{hour} != 0 ) {
        $self->{ical} =
          sprintf( '%04d%02d%02dT%02d%02d%02d', $self->{year}, $self->{month},
          $self->{day}, $self->{hour}, $self->{minute}, $self->{second} );
    } else {
        $self->{ical} =
          sprintf( '%04d%02d%02d', $self->{year}, $self->{month}, $self->{day} );
    }

    if ($self->{timezone} ) {
        my $tz = $self->{timezone};
        $self->{ical} = ( $tz eq 'UTC' ) ? 
            $self->{ical}. 'Z' : 
            "TZID=$tz:" . $self->{ical};
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
    
    use Date::Manip;

    my $date_string;
    if ( $self->{hour} != 0 ) {
        $date_string = sprintf( '%04d%02d%02d%02d%02d%02d', $self->{year}, $self->{month},
            $self->{day}, $self->{hour}, $self->{minute}, $self->{second} );

    } else {
        $date_string = sprintf( '%04d%02d%02d%02d%02d%02d', $self->{year}, $self->{month},
            $self->{day}, 0 , 0, 0 );
    }

    $date_string .= ' GMT' unless $self->{floating};

    # This measures seconds since Jan 1 1970 00:00:00 GMT. If you're not in GMT,
    # it'll return a not-quite-right date. 
    my $epoch = &UnixDate($date_string, '%s');
    
    return $epoch;
}
#}}}


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

=cut

