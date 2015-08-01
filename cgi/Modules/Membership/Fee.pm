#!/usr/bin/perl
#-----------------------------------------------------------------------
#
#   Copyright 2001-2007 Exware Solutions, Inc.  http://www.exware.com
#
#   This file is part of ExSite WebWare (ExSite, for short).
#
#   ExSite is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#   ExSite is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with ExSite; if not, write to the Free Software
#   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
#   Users requiring warranty coverage and/or support may arrange alternate
#   commercial licensing for ExSite, by contacting Exware Solutions
#   via the website noted above.
#
#----------------------------------------------------------------------------

package Modules::Membership::Fee;

use strict;
use ExSite::Config;
use ExSite::Base;
use ExSite::Form;
use ExSite::Misc;
use ExSite::ML;
use ExSite::Object;

use vars qw(@ISA);
@ISA = qw(ExSite::Object);

sub my_type { return "member_fee"; }

sub name {
    my $this = shift;
    if ($this->defined()) {
        my $name  = $this->getdata("type");
        my $descr = $this->getdata("description");
        return $descr ? "$name ($descr)" : $name;
    }
    return "n/a";
}

sub description {
    my $this  = shift;
    my $descr = $this->getdata("description");
    my $ml    = &get_obj("ML");
    my $br    = $ml->br;
    $descr =~ s/\r?\n/$br/g;
    return $descr;
}

sub update_recurring_dates {
    my ($this) = @_;
    my $date = new ExSite::Time;
    if ($this->close_date() < $date) {
        my $open  = $this->open_date();
        my $close = $this->close_date();
        next if (!$open || !$close);
        $open->add_approximate(1, "year");
        $close->add_approximate(1, "year");
        $this->setdata("open",  $open->write("sql_date"));
        $this->setdata("close", $close->write("sql_date"));
        $this->force_save();
    }
}

sub open_date {
    my $this = shift;
    if ($this->loaded()) {
        my $open = $this->{data}{open};
        if ($open && $open !~ /^0/) {
            my $time = new ExSite::Time($open, "sql_date");
            return $time;
        }
    }
    return;
}

sub close_date {
    my $this = shift;
    if ($this->loaded()) {
        my $close = $this->{data}{close};
        if ($close && $close !~ /^0/) {
            my $time = new ExSite::Time($close, "sql_date");
            return $time;
        }
    }
    return;
}

# is_open - are we between the start and end dates?
sub is_open {
    my ($this, $date) = @_;
    my $open  = $this->open_date();
    my $close = $this->close_date();
    $date or $date = new ExSite::Time();
    if ($open && $close) {
        if ($date->compare($open) <= 0) {
            if ($date->compare($close) >= 0) {

                # close reg date is in the future
                # that means we are in the registration window
                return 1;
            }
        }
        return 0;
    }
    return 1;
}

# date = date to check against in ExSite::Time format
sub allow {
    my ($this, %opt) = @_;
    my $stat = $this->run_handler("allow_member_fee");
    return $stat if (defined $stat);

    # registration dates - check if this is one of the allowed fees
    if (!$this->is_open($opt{date})) {
        $this->warn($msg{"This registration type is not available at this time."});
        return 0;
    }
    return 1;
}

#------------------------------------------------------------------------
#
# Get cost for this fee
# member => member object
# type => type of member may not match member_type if renewing under a different type
# purchase_type => Renew|New
#
#------------------------------------------------------------------------

sub cost {
    my ($this, %opt) = @_;
    my $stat = $this->run_handler("Membership_renewal_fee", %opt);
    return $stat if (defined $stat);
    my $member = $opt{member};
    my ($cost, $mterm);
    if (ref($member) =~ /Member/) {
        $mterm = &preference("mterm", "member", $member->id);
    }
    if ($opt{purchase_type} eq "Renew" && $this->getdata("renew_cost") > 0) {
        $cost = $this->getdata("renew_cost");
    } else {
        $cost = $this->getdata("cost");
    }
    $cost = $mterm ? $mterm * $cost : $cost;
    return sprintf("%.2f", $cost) || 0;
}

sub show_cost {
    my ($this, $date) = @_;
    my $cost = $this->cost();
    if ($cost > 0) {
        return $share{DB}->show_data_noauth("member_fee", "cost", $cost, $this->id, "money");
    } else {
        return $msg{$config{Membership}{Fee}{free}} || $msg{"FREE"};
    }
}

sub showdata {
    my ($this, $key) = @_;
    my $data = $this->getdata($key);
    if ($key =~ /^cost/ && !$data) {
        return "n/a";
    }
    return $this->SUPER::showdata($key);
}

sub option {
    my ($this, $setting) = @_;
    return ($this->getdata("options") =~ /$setting/) ? 1 : 0;
}

1;
