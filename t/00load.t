# $Header: /home/cvs/date-ical/t/00load.t,v 1.2 2001/06/15 04:21:19 rbowen Exp $

# Check to see if it loads

BEGIN { $| = 1; print "1..1\n"; }
END {print "not ok 1\n" unless $loaded;}
use Date::ICal;
$loaded = 1;
print "ok 1\n";

