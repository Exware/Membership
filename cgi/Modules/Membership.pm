package Modules::Membership;

#----------------------------------------------------------------------------
#
#   Copyright (C) 2009 - Exware Solutions, Inc.  http://www.exware.com
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

use strict;

# useful kernel libs; you can/should remove any of these that are not needed

use ExSite::Captcha;
use ExSite::Config;    # required
use ExSite::Form;
use ExSite::FormBuilder;
use ExSite::HTML;
use ExSite::Input;
use ExSite::Mail;
use ExSite::Misc;
use ExSite::ML;
use ExSite::ReportBuilder;
use ExSite::Tree;
use ExSite::Util;
use JSON;
use Modules::ID::Contact;
use Modules::Membership::Base;
use Modules::Membership::Fee;
use Modules::Membership::Import;
use Modules::Membership::Member;
use Modules::Membership::MemberMeta;

# recommended base class for plug-in modules

use Modules::BaseDCD;

# declare package globals

use vars qw(@ISA $ml $profile);

# define our class inheritance

@ISA = qw(Modules::Membership::Base Modules::BaseDCD);    # inherit from this base class

$ml = &get_obj("ML");

sub read {
    my ($this, $opt) = @_;

    # fetch overlayed query and form input
    my $in = new ExSite::Input;
    $this->{input} = $in->combine;
    $this->{post}  = $in->post;
    $this->{query} = $in->query;

    # add path info to input hash as "path"
    if (!$this->{input}{uid}) {
        $in->path_info();
        my @path = $in->path;
        $this->{input}{path} = \@path;
    }
}

# write method (builds output for regular web pages)

sub write {
    my ($this, $options) = @_;

    # global for handlers
    $profile = $this;

    # declare a markup generator
    $ml = &get_obj("ML");
    $this->{section} =
      new ExSite::Section(section => $share{Page}->get("section"));
    my %opt = &DecodeString($options);
    $this->{type} = $this->{input}{type} || $opt{type};

    my $in     = $this->{input};
    my $cmd    = $share{Membership}{cmd} = $in->{pro} || $opt{pro};
    my $uid    = $in->{uid} || $this->find() || $share{DB}->my_uid;
    my $loaded = $this->setup_member($uid);
    if ($uid && !$loaded) {
        return $this->error("UID $uid not found.");
    }
    $this->setup_queries();

    # retain the contact_type in our session for service pages
    $session{Membership_contact_type} = $opt{contact_type}
      if ($opt{contact_type});
    my $out;    # our output buffer

    $out .= $ml->link(
        undef,
        {
            rel  => "stylesheet",
            type => "text/css",
            href => "$config{server}{HTMLpath}/_Modules/Membership/Membership.css"
        }
    ) if ($config{Membership}{stylesheet});

    # always on page functions
    if ($opt{pro} eq "feature") {
        $out .= $this->feature();
        return $this->wrap($out);
    } elsif ($opt{pro} eq "list_new_member") {
        $out .= $this->list_new_member();
        return $this->wrap($out);
    }

    if ($cmd eq "dir") {
        $out .= $this->directory_menu(type=>$this->{type});
        $out .= $this->directory($opt{attr}, $this->{type});
        return $this->wrap($out);
    } elsif ($cmd =~ /member_directory_module/) {
        $out .= $this->directory_menu(type=>$this->{type});
        $out .= $this->member_directory_module();
        return $this->wrap($out);
    } elsif ($cmd =~ /keyword_search/) {
        $out .= $this->directory_menu(type=>$this->{type});
        $out .= $this->keyword_search();
        return $this->wrap($out);
    } elsif ($cmd =~ /advanced_search/) {
        my $search_only = $this->{input}{search_only} || $opt{search_only};
        $out .= $this->directory_menu(type=>$this->{type}, search_only => $search_only);
        $out .= $this->advanced_search(
            contact_type => $opt{contact_type},
            search_only  => $opt{search_only}
        );
        return $this->wrap($out);
    } elsif ($cmd eq "status") {
        $out .= $this->show_my_status();
        return $this->wrap($out);
    } elsif ($cmd eq "name") {
        $out .= $this->my_name();
        return $out;
    } elsif ($cmd =~ /new_member/) {
        $out .=
          $this->new_member_form(type => $opt{type}, goto => $opt{goto});
        return $this->wrap($out);
    } elsif ($cmd =~ /public_new_secondary/) {
        $out .= $this->public_new_secondary();
        return $this->wrap($out);
    } elsif ($cmd =~ /^renew_group$/) {
        $out .= $this->renew_group();
        return $this->wrap($out);
    } elsif ($cmd =~ /renew/) {
        $out .= $this->renewal_form();
        return $this->wrap($out);
    } elsif ($cmd =~ /show_secondary_members/) {
        $out .= $this->show_secondary_members($this->member());
        return $this->wrap($out);
    }

    # remaining functions cannot be placed on static pages
    return if ($share{Page} && $share{Page}->is_publishing);

    if ($cmd eq "status") {
        $out .= $this->show_my_status();
        return $this->wrap($out);
    } elsif ($cmd =~ /^new_secondary/) {
        if (scalar keys %{$this->{post}} == 0) {
            $out .= $this->edit_profile_menu();
        }
        $out .= $this->new_secondary_member();
        return $this->wrap($out);
    } elsif ($this->member->allow_edit()) {

        # allow primaries to edit secondary data
        $share{DB}->handler("user_owns", \&primary_user_owns);
        if ($cmd =~ /^remove_member$/) {
            $out .= $this->wrap($this->remove_member());
            $out .= &ExSite::HTML::Button(
                label => $msg{"Continue"},
                url   => $this->link(pro => undef, uid => $share{DB}->my_uid)
            );
            return $out;
        }
        if ($cmd =~ /^upgrade$/) {
            $out .= $this->upgrade();
            return $this->wrap($out);
        }

        # add profile toolbar to all proceeding functions
        if ($this->member->status() ne "incomplete") {
            $out .= $this->edit_profile_menu();
        }
        if ($cmd =~ /new_contact/) {
            $out .= $this->new_contact();
            return $this->wrap($out);
        } elsif ($cmd eq "login") {
            my $pre_auth;
            if ($this->{input}{user}) {
                $pre_auth = 1;
                my $crypt = new ExSite::Crypt;
                $uid = $crypt->decrypt($this->{input}{user});
            }
            return $this->login($uid, $pre_auth);
        } elsif ($cmd =~ /edit_contacts/) {
            $out .= $this->edit_contacts();
            return $this->wrap($out);
        } elsif ($cmd =~ /edit_contact/) {
            $out .= $this->edit_contact();
            return $this->wrap($out);
        } elsif ($cmd =~ /del_contact/) {
            $out .= $this->delete_contact();
            return $this->wrap($out);
        } elsif ($cmd =~ /^edit_password$/) {
            my $passtype = $this->member->passtype();
            if (!$passtype) {
                $out .= $this->member->change_password();
            } else {
                if ($this->member->is_incomplete()) {
                    $out .= $ml->info(
                        &substitute(
                            $msg{&preference("Membership.message.create_password")},
                            {login => $this->member->getdata("login")}
                        )
                    );
                } else {
                    $out .= $ml->info($msg{&preference("Membership.message.set_password")});
                }
                $out .= $ml->h1(&substitute($msg{"Set password for [[name]]"}, {name => $this->member->name()}));
                $session{Membership}{skip_password_change} = 0;
                my $reply = $this->link(pro => undef);
                my $version =
                  $share{Page} ? $share{Page}->get_my("version") : "";

                # return user to membership "home" page
                my $filename = &preference("Membership.home_page_filename");
                my $pid      = &preference("Membership.home_page_id")
                  || &preference("Membership.change_password.redirect_page_id");
                my $p;
                if (!$pid) {
                    my $pages = $share{DB}->fetch_match(
                        "page",
                        {
                            section_id => $this->get_section_id(),
                            filename   => $filename
                        }
                    );
                    if (scalar @$pages) {
                        $p = new ExSite::Page(id => $pages->[0]->{page_id});
                        $p->set_version($version);
                        $reply = $p->get_url_dynamic_full();
                    }
                } elsif ($pid) {
                    $p = new ExSite::Page(id => $pid);
                    $p->set_version($version);
                    $reply = $p->get_url_dynamic_full();
                }
                $out .= $share{DB}->set_password_form(undef, $reply);
            }
            return $this->wrap($out);
        } elsif ($cmd =~ /^mailing_lists$/) {
            $out .= $this->mailing_lists();
            return $this->wrap($out);
        } elsif ($cmd =~ /^reinstate$/) {
            $out .= $this->reinstate();
            return $this->wrap($out);
        } elsif ($cmd) {
            if ($cmd =~ /^edit_photo$/) {
                $out .= $this->member->edit_photo();
            } elsif ($cmd =~ /^edit_profile$/) {
                $out .= $this->edit_profile();
            } elsif ($cmd =~ /^editmeta$/) {
                if ($this->{input}{meta}) {
                    $opt{meta} = $this->{input}{meta};
                }
                if ($this->{input}{template}) {
                    $opt{template} = $this->{input}{template};
                }
                $out .= $this->member->editmeta(%opt);
            } elsif ($cmd =~ /^my_events$/) {
                $out .= $this->my_events();
            } elsif ($cmd =~ /^acct$/) {
                $out .= $this->show_account();
            }
            return $this->wrap($out);
        }
    }

    if ($uid) {
        $out .= $this->show_member();
    } else {
        $out .= $this->new_member_form(type => $opt{type});
    }
    return $this->wrap($out);
}

sub wrap {
    my $this = shift;
    $_ = shift;
    return $ml->div($_, {class => "Membership"});
}

# ioctl method (used by ExSite to query the module for its functionality)

sub ioctl {
    my $this = shift;
    $_ = shift;    # $_ is the ioctl request

    if (/isRestricted/) {
        return 0;
    } elsif (/isService/) {
        return 1;
    } elsif (/ModuleName/) {
        return "Membership Management";
    } elsif (/ControlPanel/) {
        return \&ctrl_panel;
    } elsif (/Publish/) {
        return \&publish;
    } elsif (/Unpublish/) {
        return \&unpublish;
    } elsif (/Search/) {
        return \&search_index;
    } elsif (/Cron/) {
        return \&cron;
    } elsif (/ToDo/) {
        return \&todo;
    } elsif (/Summary/) {
        return \&summary;
    } elsif (/Dependencies/) {
        return ["Finance", "AddrBook", "MemberDirectory"];
    } elsif (/Category/) {
        return ["Social", "Applications"];
    } elsif (/Tips/) {
        return \&tips;
    } elsif (/Size/) {
        return [1200, 800];
    }
}

#----------------------------------------------------------------------------
# Everything after this point consists of private methods.

# ctrl_panel() generates the contents of the administrator control panel

sub ctrl_panel {
    my $this = shift;

    # global for handlers
    $profile = $this;

    my $sid = $this->get_section_id;

    # declare a markup generator
    $ml = &get_obj("ML");

    my $out;
    $out .= &ExSite::HTML::js();
    $out .= $ml->link(
        undef,
        {
            rel  => "stylesheet",
            href => "//ajax.googleapis.com/ajax/libs/jqueryui/1.7.1/themes/base/jquery-ui.css",
            type => "text/css"
        }
    );

    if (!$sid) { return $this->set_section_id; }
    $this->{section} = new ExSite::Section(id => $sid);
    $this->{admin} = $share{Membership}{admin} = 1;
    if (!$share{DB}->is_manager) {
        return $this->error("Permission denied. You do not have a key for this website.");
    }

    my $uid = $this->{input}{uid};
    if ($uid) {
        $this->setup_member($uid);
    }
    $this->setup_queries();

    # build a block of HTML to display the control panel

    my $cmd = $this->{input}{pro};
    if (($cmd || $sid || $this->{input}{uid})) {
        if (!$this->{input}{_bare}) {
            $out .= $this->top_menu();
            $out .= $this->site_menu();
        }
    }
    if (!$cmd && $this->{input}{uid}) {
        $cmd = "member";
    }
    if ($cmd) {
        if ($cmd eq "roster") {
            my @type = split /; /, $this->{input}{type};
            my @stat = split /; /, $this->{input}{status};
            $out .= $this->roster(
                type           => \@type,
                status         => \@stat,
                contact        => $this->{input}{contact},
                excl_secondary => $this->{input}{excl_secondary}
            );
        } elsif ($cmd eq "fees") {
            $out .= $this->manage_fees();
        } elsif ($cmd eq "search") {
            $out .= $this->search();
        } elsif ($cmd eq "bulk_status_update") {
            $out .= $this->bulk_status_update();
        } elsif ($cmd eq "preview_auto_emails") {
            $out .= $this->preview_auto_emails();
        } elsif ($cmd eq "welcome_email") {
            my $message = $this->welcome_email();
            if ($message !~ /<\w+( [^>]*)?>/) {
                my $br = $ml->br;
                $message =~ s/\n/$br\n/g;
            }
            if ($message) {
                $out .= $ml->info("Message sent.");
                $out .= &ExSite::HTML::BasicBox(pane => $message);
            } else {
                my $error = &ExSite::Config::show_diagnostics();
                $out .= $ml->error("Message could not be sent." . $error);
            }
            $out .= &ExSite::HTML::Button(label => "Return to profile", url => $this->link(pro => "member"));
        } elsif ($cmd =~ /^(import|export_i|do_import1)$/) {
            $out .= $this->import();
        } elsif ($cmd eq "new_member") {
            $out .= $this->new_member_form();
        } elsif ($cmd eq "advanced_search") {
            $out .= $this->advanced_search();
        } elsif ($cmd eq "summary") {
            $out .= $this->summary($sid, 1);
        } elsif ($cmd eq "applicant_report") {
            $out .= $this->sales_report(undef, "applicants");
        } elsif ($cmd eq "renewal_report") {
            $out .= $this->sales_report(undef, "renewals");
        } elsif ($cmd eq "revenues") {
            $out .= $ml->h1("Membership Revenues");
            $out .= $this->report_revenue();
        } else {

            # remaining functions are for managing a specific member
            if ($uid) {
                if ($cmd eq "renew") {
                    $out .= $this->renewal_form();
                    return $out;
                } elsif ($cmd eq "login") {
                    return $this->login($uid);
                } elsif ($this->member->allow_edit()) {
                    if ($cmd eq "new_contact") {
                        $out .= $this->new_contact();
                    } elsif ($cmd eq "admin_bill_member") {
                        $out .= $this->admin_bill_member();
                        return $out;
                    } elsif ($cmd =~ /^(approve|reject)$/) {
                        $out .= $this->moderate_status($cmd, $uid);
                        return $out;
                    } elsif ($cmd =~ /delete/) {
                        my $delout = $this->delete_member();
                        if ($delout) {
                            $out .= $delout;
                        } else {
                            $ml->location(
                                $this->link(
                                    pro         => undef,
                                    uid         => undef,
                                    __plaintext => 1
                                )
                            );
                        }
                    } elsif ($cmd eq "undel_member") {
                        $this->undel_member($uid);
                        $ml->location(
                            $this->link(
                                pro         => undef,
                                __plaintext => 1
                            )
                        );
                    } elsif ($cmd =~ /del_contact/) {
                        $out .= $this->delete_contact();
                        return $out;
                    } elsif ($cmd =~ /new_secondary/) {
                        $out .= $this->new_secondary_member();
                    } elsif ($cmd =~ /^promote$/) {
                        $out .= $this->promote();
                    } else {

                        # update profile options
                        $out .= $this->edit_profile_menu();
                        if ($cmd eq "member") {
                            $out .= $this->show_member($uid);
                        } elsif ($cmd eq "enable") {
                            $this->member->enable();
                            $out .= $this->show_member($uid);
                        } elsif ($cmd eq "disable") {
                            $this->member->disable();
                            $out .= $this->show_member($uid);
                        } elsif ($cmd =~ /^edit_profile$/) {
                            $out .= $this->edit_profile();
                        } elsif ($cmd eq "edit_contact") {
                            $out .= $this->edit_contact();
                        } elsif ($cmd eq "edit_contacts") {
                            $out .= $this->edit_contacts();
                        } elsif ($cmd =~ /^edit_photo$/) {
                            $out .= $this->member->edit_photo();
                        } elsif ($cmd eq "member_status") {
                            $out .= $this->show_status_history($uid);
                        } elsif ($cmd =~ /^acct$/) {
                            $out .= $this->show_account();
                        } elsif ($cmd =~ /^edit_password$/) {
                            $out .= $this->member->change_password();
                        }
                    }
                    return $ml->div($out, {class => "Membership"});
                } else {
                    $out .= $this->error("Unknown command: " . $cmd);
                }
            } elsif ($this->{input}{pid} && $cmd =~ /^new_secondary$/) {
                $out .= $this->new_secondary_member();
            } else {
                $out .= $this->error("Invalid user ID");
            }
        }
    } else {
        $out .= $this->membership_overview();
    }
    return $ml->div($out, {class => "Membership"});
}

sub membership_overview {
    my ($this) = @_;
    my $out;
    my $sid     = $this->get_section_id;
    my $title   = $this->{section}->get_my("title");
    my $summary = $this->summary($sid, $sid);
    $out .= $ml->script(undef, {type => "text/javascript", src => "//www.google.com/jsapi"});
    $out .= $ml->script(
        undef,
        {
            src  => "$config{server}{HTMLpath}/_Modules/Membership/jquery.qtip-1.0.0-rc3.min.js",
            type => "text/javascript"
        }
    );
    $out .= $ml->script("google.load('visualization', '1', {packages: ['corechart']});", {type => "text/javascript"});
    $out .= $ml->h1("$title Membership Reports");
    $out .=
      $ml->table($ml->tr($ml->td($summary) . $ml->td($this->sales_summary())), {class => "well Overview"});
    $out .= $ml->script(
        undef,
        {
            src  => "$config{server}{HTMLpath}/_Modules/Membership/jquery.qtip-1.0.0-rc3.min.js",
            type => "text/javascript"
        }
    );
    $out .= $ml->script(
        "\$(function() {
	\$('a.tooltip').each(function() {
      \$(this).qtip({
      content: \$(this).next('.tip').html(),
	  position: { corner: { target: 'rightMiddle', tooltip: 'leftTop' }},
	  style: {
		  border: {
		     width: 5,
		     radius: 10
		  },
		  width: 460,
		  padding: 5,
		  textAlign: 'left',
		  tip: true,
		  name: 'dark'
	   }
	  });
	})})", {type => "text/javascript"}
    );
    if ($share{DB}->count("member_fee", {section_id => $sid}) > 0) {
        $out .= $ml->table(
            $ml->tr(
                $ml->td($ml->h5("Membership breakdown by type and status") . $ml->div(undef, {id => "chart_memtype"}))
                  . $ml->td($this->application_graph())
            ),
            {class => "well Overview"}
        );
    }
    return $out;
}

sub sales_report {
    my ($this, $section_id, $type) = @_;
    $section_id or $section_id = $this->get_section_id;
    my $in                  = $this->{input};
    my $yearmonth           = $in->{yearmonth};
    my @applicants_by_month = $share{DB}->get_query("$type by month", $section_id);
    my @members;
    my $r = new ExSite::ReportBuilder(class => "Report Roster");
    $r->title($yearmonth . " " . ucfirst($type) . " Report");
    my $format  = $this->{input}{fmt};
    my $columns = [];
    my $cols    = [
        "member_id",    "honorific", "title", "name",      "first_name", "last_name",
        "organization", "type",      "email", "subscribe", "expirydate"
    ];

    foreach my $col (@$cols) {
        push @$columns, $col
          if ($share{DB}->{map}->is_mapped("member", $col)
            || ($col eq "name" && !$format));
    }
    my @head = map { $this->member_column_label($_) } @$columns;
    push(@head, ("Invoice", "Receivable", "Paid"));
    $r->headers(\@head);
    foreach my $record (@applicants_by_month) {
        if ($record->{yearmonth} eq $in->{yearmonth}) {
            my $inv_id = $record->{receivable_id};
            my $receivable = new Modules::Finance::Receivable(id => $inv_id);
            my $is_paid;
            if ($receivable->is_paid) {
                $is_paid = "Paid";
            }
            my $paylink = $ml->a(
                $inv_id,
                {
                    href =>
"$config{server}{server}$config{server}{CGIpath}/$config{prog}{ctrlpanel}/Pay?section_id=$section_id&inv=$inv_id"
                }
            );
            my @mdata;
            push(@mdata, $this->roster_row_data($columns, $record, $format));
            $r->push(@mdata, $paylink, $record->{cost}, $is_paid);
        }
    }
    $r->foot($this->sales_footer($r));
    return $r->make;
}

sub applicants_by_month {
    my ($this, $section_id, $months) = @_;
    my @applicants_by_month = $share{DB}->get_query("applicants by month", $section_id);
    my $applicants_by_month;
    my $today = new ExSite::Time;
    my $cutoff;
    if ($months) {
        $cutoff = new ExSite::Time;
        $cutoff->add(-$months, 'months');
    }
    foreach my $r (@applicants_by_month) {
        next if ($r->{yearmonth} eq "0000-00");
        my $t              = new ExSite::Time($r->{yearmonth} . "-01", "sql_date");
        my $sec            = $t->diff($today);
        my $seconds_in_day = (24 * 60 * 60);
        my $diff           = int($sec / $seconds_in_day);
        next if ($cutoff && $t <= $cutoff);
        $applicants_by_month->{$r->{yearmonth}}{count} += 1;
        $applicants_by_month->{$r->{yearmonth}}{sales} += $r->{cost};
    }
    return $applicants_by_month;
}

sub renewals_by_month {
    my ($this, $section_id, $months) = @_;
    my @applicants_by_month = $share{DB}->get_query("renewals by month", $section_id);
    my $applicants_by_month;
    my $today = new ExSite::Time;
    my $cutoff;
    if ($months) {
        $cutoff = new ExSite::Time;
        $cutoff->add(-$months, 'months');
    }
    foreach my $r (@applicants_by_month) {
        next if ($r->{yearmonth} eq "0000-00");
        my $t              = new ExSite::Time($r->{yearmonth} . "-01", "sql_date");
        my $sec            = $t->diff($today);
        my $seconds_in_day = (24 * 60 * 60);
        my $diff           = int($sec / $seconds_in_day);
        next if ($cutoff && $t <= $cutoff);
        $applicants_by_month->{$r->{yearmonth}}{count} += 1;
        $applicants_by_month->{$r->{yearmonth}}{sales} += $r->{cost};
    }
    return $applicants_by_month;
}

sub sales_summary {
    my ($this, $section_id) = @_;
    $section_id or $section_id = $this->get_section_id;
    my $months;
    $months = 13 if (!$this->{input}{fmt});
    my $applicants_by_month = $this->applicants_by_month($section_id, $months);
    my $renewals_by_month = $this->renewals_by_month($section_id, $months);
    my @keys = keys %{{map { $_ => 1 } (keys %$applicants_by_month, keys %$renewals_by_month)}};
    return if (!scalar @keys);
    my $r = new ExSite::ReportBuilder();
    $r->title("Sales Summary");
    my @head = ("Month", "Applications", "Renewals", "Application Sales", "Renewal Sales", "Total Sales");
    $r->headers(\@head);

    foreach my $yearmonth (sort @keys) {
        my $t = new ExSite::Time;
        $t->set($yearmonth, "year-month");
        my $a_sales =
          sprintf("%.2f", $applicants_by_month->{$yearmonth}{sales});
        my $r_sales =
          sprintf("%.2f", $renewals_by_month->{$yearmonth}{sales});
        $r->push(
            $t->write("month") . " " . $t->write("year"),
            $ml->a(
                $applicants_by_month->{$yearmonth}{count},
                {
                    href => $this->link(
                        pro       => "applicant_report",
                        yearmonth => $yearmonth
                    )
                }
            ),
            $ml->a(
                $renewals_by_month->{$yearmonth}{count},
                {
                    href => $this->link(
                        pro       => "renewal_report",
                        yearmonth => $yearmonth
                    )
                }
            ),
            $this->{input}{fmt} ? $a_sales
            : $share{DB}->show_data_nomap("money", $a_sales),
            $this->{input}{fmt} ? $r_sales
            : $share{DB}->show_data_nomap("money", $r_sales),
            $this->{input}{fmt} ? sprintf("%.2f", $a_sales + $r_sales)
            : $share{DB}->show_data_nomap("money", sprintf("%.2f", $a_sales + $r_sales)),
        );
    }
    $r->foot($this->sales_footer($r));
    return $r->make;
}

sub sales_footer {
    my ($this, $r) = @_;
    if ($this->{input}{fmt} eq "xls") {
        $ml->cancel;
        print "Content-type: application/vnd.ms-excel\ncontent-disposition: attachment; filename=sales.xls\n\n";
        $r->export("excel");
    } elsif ($this->{input}{fmt} eq "csv") {
        $ml->cancel;
        print "Content-type: text/comma-separated-values\n\n";
        print $r->export("csv");
    } else {
        my $foot .= "Report generated " . localtime;
        $foot .= " &bull; ";
        $foot .= $ml->a("Excel", {href => $this->link(fmt => "xls"), class => "button_sm"});
        $foot .= " &bull; ";
        $foot .= $ml->a("CSV",   {href => $this->link(fmt => "csv"), class => "button_sm"});
        $r->foot($foot);
    }
}

sub application_graph {
    my ($this, $section_id) = @_;
    $section_id or $section_id = $this->get_section_id;
    my $applicants_by_month = $this->applicants_by_month($section_id, 36) || {};
    my $renewals_by_month = $this->renewals_by_month($section_id, 36) || {};
    my %merge = (%$applicants_by_month, %$renewals_by_month);
    my @yearmonths = sort keys %merge;
    return if (!scalar keys %$applicants_by_month);
    my $n;
    my $js;

    foreach my $yearmonth (@yearmonths) {
        $n++;
        $n = 0 if ($n == 3);
        my $a = $applicants_by_month->{$yearmonth}{count} || 0;
        my $r = $renewals_by_month->{$yearmonth}{count}   || 0;
        $js .= "data.addRow([\"$yearmonth\",$a,$r]);\n";
    }
    my @month_short      = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
    my $applicants_graph = <<END;
	<h5>Membership applications/renewals by month</h5>
    <div id="visualization"></div>
    <script type="text/javascript">
      var line = null;
      function drawVisualization() {
        // Create and populate the data table.
        var data = new google.visualization.DataTable();
        data.addColumn('string', 'x');
        data.addColumn('number', 'Applications');
        data.addColumn('number', 'Renewals');
		$js

        // Create and draw the visualization.
        line = new google.visualization.LineChart(document.getElementById('visualization')).
            draw(data, {curveType: "function",
            			chartArea:{left:20,top:10,width:"89%"},
                        width: 400, height: 200,
                        legend: 'bottom',
						vAxes: {
						   0: { minValue: 0, maxValue: 5},
						   1: { minValue: 0, maxValue: 10}
						},
						series: {
						   0: {targetAxisIndex: 0},
						   1: {targetAxisIndex: 1},
						}}
                );
      }
    drawVisualization();
    </script>
END
    return $applicants_graph;
}

sub site_menu {
    my ($this, $evt) = @_;
    my $path  = "$config{server}{HTMLpath}/_Modules/Membership";
    my %reset = (
        type             => undef,
        status           => undef,
        uid              => undef,
        rpt_format       => undef,
        use_column_names => undef,
        searchterm       => undef
    );
    my $out = &ExSite::HTML::IconBar(
        links => [
            {
                label => "Overview",
                img   => "$path/overview.png",
                url   => $this->link(pro => undef, %reset)
            },
            {
                label => "Fees",
                img   => "$path/fees.png",
                url   => $this->link(pro => "fees", %reset)
            },
            {
                label => "Roster",
                img   => "$path/roster.png",
                url   => $this->link(
                    pro => "roster",
                    %reset,
                    contact        => undef,
                    excl_secondary => undef
                )
            },
            {
                label => "Search",
                img   => "$path/lookup.png",
                url   => $this->link(pro => "search", %reset)
            },
            {
                label => "Emails",
                img   => "$path/email.png",
                url   => $this->link(
                    pro       => "preview_auto_emails",
                    send_date => undef,
                    %reset
                )
            },
            {
                label => "Revenues",
                img   => "$path/revenues.png",
                url   => $this->link(pro => "revenues", %reset)
            },
            {
                label => "New&nbsp;Member",
                img   => "$path/group_add.png",
                url   => $this->link(pro => "new_member", %reset)
            },
            {
                label => "Import",
                img   => "$path/import.png",
                url   => $this->link(pro => "import", %reset)
            },
        ]
    );
    return $out;
}

sub import {
    my ($this) = @_;
    my $i = new Modules::Membership::Import;
    return $i->topview();
}

# menu appears in administrator control panel
sub user_menu {
    my ($this, $uid) = @_;
    return undef if (!$this->{admin});
    if ($uid) {
        $this->member->setup(id => $uid);
    } else {
        $uid = $this->member->id;
    }
    my $acct_id = $this->member->account->id;
    my $name    = $this->member->name;
    my @menu    = (
        $ml->td($this->quick_status($uid), {colspan => 2}),
        $ml->td("Member ID") . $ml->td($this->member->id()),
        $ml->td("Membership Type") . $ml->td($this->member->member_type()),
        $ml->td("E-mail") . $ml->td($this->member->showdata("email")),
    );
    my $login;
    if ($this->member->enabled()) {
        $login = $ml->span("Enabled", {class => "info"});
        $login .= " " . $ml->a("Disable", {href => $this->link(pro => "disable"), class => "button_sm"});
    } else {
        $login = $ml->span("Disabled", {class => "error"});
        $login .= " " . $ml->a("Enable", {href => $this->link(pro => "enable"), class => "button_sm"});
    }
    push(@menu, $ml->td("Login") . $ml->td($login));
    if ($this->member->use_status()) {
        push(@menu, $ml->td("Expiry Date") . $ml->td($this->member->expirydate()));
    }

    my $tout = $ml->tr($ml->th($name, {colspan => 2}));
    foreach my $item (@menu) { $tout .= $ml->tr($item); }
    return $ml->table($tout, {class => "Report MembershipMemberMenu"});
}

sub status_help {
    my $is_popup = shift;
    my ($message, $attributes);
    if ($is_popup) {
        $message = $ml->a("Membership Definitions", {href => "#", class => "tooltip", position => "top"});
        $attributes = {class => "tip", style => "display: none"};
    }
    my ($pending, $expired);
    if (&preference("Membership.pending_are_members")) {
        $pending .= $ml->span(" Pending members have membership privileges/are considered members.");
    }
    if (&preference("Membership.expired_are_members")) {
        $expired .= $ml->span(
" Expired members have membership privileges/are considered members for the duration of the grace period but do not have access to member only event rates."
        );
    }
    $message .= $ml->ul(
        [
            $ml->strong("Members")
              . " have (or once had) legal status with the organization:"
              . $ml->ul(
                [
                    $ml->strong("Active") . " members are paid up members in good standing at the current time.",
                    $ml->strong("Incomplete") . " members have not completed their application.",
                    $ml->strong("Pending")
                      . " members have signed up, but their application has not been confirmed because they have not yet submitted payment or have not yet been accepted in cases where manual approval is required.$pending",
                    $ml->strong("Expired")
                      . " memberships have recently lapsed, but the member can still sign in to renew.$expired",
                    $ml->strong("Archived") . " members have fully lapsed, and no longer have any membership benefits.",
                    $ml->strong("Other") . " members have no membership history."
                ]
              ),
            $ml->strong("Secondary members")
              . " are users who are affiliated with another membership but do not pay membership dues themselves.",
            $ml->strong("Guest users") . " are not members, but can nevertheless log in to the private areas.",
            $ml->strong("Staff")
              . " are users who have full access as members but do not pay membership dues themselves."
        ],
        $attributes
    );
    return $message;
}

# menu appears on user's profile
sub edit_profile_menu {
    my $this = shift;
    my $stat = $this->run_handler("edit_profile_menu");
    return $stat if ($stat);
    my $out;
    my $is_me   = $this->member->is_me();
    my $heading = &preference("Membership.heading.update_profile");
    $out .= $ml->h1($msg{$heading}) if $heading;
    my $uid = $this->{input}{uid} || $this->member->id;
    my $options = [
        $ml->a(
            $msg{&preference("Membership.label.view_profile") || "View Profile"},
            {
                href => $this->link(pro => undef, uid => $uid, _bare => undef),
                id   => "Membership_view_profile_link"
            }
        ),
        $ml->a(
            $msg{&preference("Membership.label.edit_profile")},
            {
                href => $this->link(
                    pro   => "edit_profile",
                    uid   => $uid,
                    _bare => undef
                ),
                id => "Membership_update_profile_link"
            }
        )
    ];

    if (&preference("Membership.label.edit_contacts")) {
        push(
            @$options,
            $ml->a(
                $msg{&preference("Membership.label.edit_contacts")},
                {
                    href => $this->link(
                        pro   => "edit_contacts",
                        uid   => $uid,
                        _bare => undef
                    ),
                    id => "Membership_update_contact_link"
                }
            )
        );
    }
    if (&preference("Membership.label.edit_photo")) {
        push(
            @$options,
            $ml->a(
                $msg{&preference("Membership.label.edit_photo")},
                {
                    href => $this->link(
                        pro   => "edit_photo",
                        uid   => $uid,
                        _bare => undef
                    ),
                    id => "Membership_update_photo_link"
                }
            )
        );
    }
    if ($is_me || $this->{admin}) {
        push @$options,
          $ml->a(
            $msg{&preference("Membership.label.edit_password")},
            {
                href => $this->link(pro => "edit_password", uid => $uid, _bare => undef),
                id   => "Membership_change_password_link"
            }
          ),
          ;
    }
    my $events = $msg{&preference("Membership.label.my_events")};
    if ($events && $share{Page}) {
        push @$options,
          $ml->a(
            $events,
            {
                href => $this->link(pro => "my_events", _bare => undef),
                id   => "Membership_my_events_link"
            }
          ),
          ;
    }
    my $type = $this->member->member_type();
    if (   &preference("Membership.secondary.add")
        && $this->allow_add_secondary($type)
        && !$this->member->parent)
    {
        my $label = $msg{&preference("Membership.label.add_secondary")};
        if ($label) {
            push @$options,
              $ml->a(
                $label,
                {
                    href => $this->link(
                        pro   => "new_secondary",
                        type  => undef,
                        pid   => $uid,
                        uid   => undef,
                        _bare => undef
                    ),
                    id => "Membership_new_secondary_link"
                }
              ),
              ;
        }
    }
    my $acct = &preference("Membership.label.account_statement");
    if ($acct) {
        if ($is_me) {
            push @$options,
              $ml->a(
                $msg{$acct},
                {
                    href => $this->link(pro => "acct", _bare => undef),
                    id   => "Membership_account_link"
                }
              );
        }
    }
    if (&preference("Membership.constant_contact.enabled") && $is_me) {
        push @$options,
          $ml->a(
            $msg{&preference("Membership.constant_contact.input_label")} || $msg{"Mailing Lists"},
            {
                href => $this->link(pro => "mailing_lists"),
                id   => "Membership_mailing_list_link"
            }
          );
    }

    my $account = $this->member->account();
    my $name    = $this->member->name;
    if ($this->{admin} && $account) {
        my @admin_options =
          ($ml->a("Login As", {href => $this->link(pro => "login"), uid => $uid, target => "_blank"}));
        push(@admin_options,
            $ml->a("Status History", {href => $this->link(pro => "member_status", uid => $uid, _bare => undef)}));
        my $acct_id = $account->id();
        push(
            @admin_options,
            $ml->a(
                $acct || "Account Statement",
                {
                        href => "javascript:popup_large('"
                      . "$config{server}{CGIpath}/$config{prog}{ctrlpanel}/Pay?acct=$acct_id&section_id="
                      . $this->{input}{section_id} . "')"
                }
            )
        );
        if ($this->member->parent) {
            push @admin_options, $ml->a("Promote", {href => $this->link(pro => "promote", _bare => undef)});
        }
        if (&preference("Membership.label.member_log")) {
            push @admin_options,
              $ml->a(
                &preference("Membership.label.member_log"),
                {
                        href => "javascript:popup_large('"
                      . "$config{server}{CGIpath}/$config{prog}{ctrlpanel}/MemberLog?uid=$uid&log=member_report&section_id="
                      . $this->{input}{section_id} . "')"
                }
              ),
              ;
        } elsif ($share{DB}->{map}->is_mapped("member", "location_id")
            && &preference("Membership.label.org_module"))
        {
            push @admin_options,
              $ml->a(
                &preference("Membership.label.org_module"),
                {
                        href => "javascript:popup_large('"
                      . "$config{server}{CGIpath}/$config{prog}{ctrlpanel}/Organization?uid=$uid&section_id="
                      . $this->{input}{section_id} . "')"
                }
              );
        }
        $name =~ s/'/\\'/g;
        my $uri     = new ExSite::URI;
        my $new_uri = "$config{server}{CGIpath}/$config{prog}{ctrlpanel}/Membership";
        $uri->setup($new_uri);
        my %query = (pro => "delete", uid => $this->member->id);
        $query{section_id} = $this->{input}{section_id}
          if ($this->{input}{section_id});
        $uri->query(%query);
        my $link = $uri->write;
        push @admin_options,
          $ml->a(
            "DELETE",
            {
                href =>
"javascript:confirm_custom('Are you sure you want to delete $name, and all related history, financial data, and secondary users?','"
                  . $link . "')"
            }
          );
        $out .= &ExSite::HTML::ToolBar(tools => [join(" ", @$options), join(" ", @admin_options)]);
    } else {
        $out .= $ml->ul($options, {class => "Membership_update_profile_menu"});
    }
    return $out;
}

sub edit_profile {
    my ($this, %opt) = @_;
    my $stat = $this->run_handler("edit_profile", %opt);
    return $stat if $stat;
    my $db  = $share{DB};
    my $mem = $this->member;
    if ($mem->ok) {
        if ($mem->allow_edit()) {
            if (scalar keys %{$this->{post}} > 0) {
                return $this->do_membership_form(
                    data => $this->{post},
                    fee  => 0
                );
            } else {
                my $out = $ml->h1(&substitute($msg{preference("Membership.heading.edit_profile")}, {name => $mem->name}));
                $mem->account;
                $out .= $this->membership_form(
                    data  => $mem->get(),
                    other => {member_id => $this->member->id}
                );
                return $ml->div($out, {class => "edit_profile"});
            }
        } else {
            return $mem->error("Permission denied.");
        }
    }
    return $mem->show_diagnostics("error", "html");
}

sub my_events {
    my $this = shift;
    if (my $heading = $msg{preference("Membership.label.my_events")}) {
        if ($share{Page}) {
            my $out;
            $out .= $ml->h1($heading);
            $out .= $ml->p($msg{"Post information regarding upcoming events you are hosting on this site's calendar."});
            $out .= $ml->div("<!--&EvtCal(evtcmd=myevt)-->\n", {class => "my_events"});
            return $out;
        }
    }
}

sub show_account {
    my $this = shift;
    if (   $share{Page}
        && $msg{preference("Membership.label.account_statement")})
    {
        return $ml->div("<!--&Pay(cart=myacct)-->\n", {class => "account_history"});
    } else {
        return $this->member->account->show_statement()
          . $ml->p(
            $ml->a(
                "open full account (payments module)",
                {
                        href => "$config{server}{CGIpath}/$config{prog}{ctrlpanel}/Pay?acct="
                      . $this->member->account->id
                      . "&section_id="
                      . $this->{input}{section_id}
                }
            )
          );
    }
}

sub report_settings {
    my ($this, %settings) = @_;
    my $out;
    my $table;
    foreach my $s (keys %settings) {
        $table .= $ml->tr($ml->td($s) . $ml->td($settings{$s}));
    }
    $out .= $ml->table($table, {class => "Report"});
    return $out;
}

sub tips {
    my $this = shift;
    my $out .= $ml->h2("Membership");
    my $expiry;
    my $t = new ExSite::Time;
    if (my $roll = &preference("Membership.rollover_date")) {
        $t->set($t->write("raw_year") . '-' . $roll, 'sql_date');
        my $date = $t->write("%MMMM %D");
        $expiry = $date;
    } else {
        $expiry = "anniversary date";
    }
    my $rdays = &preference("Membership.renewal_email.days");
    my @rdays = split(/,/, $rdays);
    my $rdays_readable;
    foreach my $rday (@rdays) {
        if ($rday < 0) {
            $rdays_readable .= $ml->div(abs($rday) . " days before expiry");
        } else {
            $rdays_readable .= $ml->div(abs($rday) . " days after expiry");
        }
    }
    $rdays_readable ||= "N/A";

    my $approval = &preference("Membership.approval");
    my %settings = (
        "Members expire on"                  => $expiry,
        "Members receive a renewal reminder" => $rdays_readable,
        "Key Contacts"
          . $ml->br
          . $ml->small(
            "Key contacts can login on behalf of a related member and initiate actions such as membership renewals") =>
          $this->{handler}->{"member_is_key_contact"} ? "enabled" : "disabled",
        "Members can renew "
          . $ml->br
          . $ml->small(
"Early renewal is only available to active members. Once early renewal has taken place the ability to renew is disabled until the following year."
          ) => &preference("Membership.early_renewal_period")
          . " days before expiry",
        "Members are archived (grace period)" => &preference("Membership.late_grace_period") . " days after expiry",
        "From email address of membership notifications" => &preference("Membership.owner_email"),
        "Where administrator notifications such as application notifications are sent" =>
          &preference("Membership.owner_email") || &preference("Membership.notification_email"),
        "Membership approval" => $approval,
        "Expired members"     => &preference("Membership.expired_are_members")
        ? "retain membership benefits until archival"
        : "must renew to access member rates and pages"
    );
    if (my $fee = $this->reinstatement_fee()) {
        $settings{"Membership reinstatement fee for archived members"} = "\$" . $fee;
    }
    if (&preference("Membership.prorated_application")) {
        my $label =
            "Prorated membership applications"
          . $ml->br()
          . $ml->small("Gives a prorated discount on membership dues based on # of months remaining");
        $settings{$label} = "enabled";
    }
    if (&preference("Membership.prorated_renewal")) {
        $settings{"Prorated membership renewals"} = "enabled";
    }
    $out .= $this->report_settings(%settings);
    my $fees = $this->get_allowed_fees($this->get_section_id);
    $share{Module} = undef;
    my $columns = ["type", "cost"];
    my $open_close;
    while (my $fee = $fees->next()) {
        $open_close = 1 if (!$fee->is_open);
    }
    push(@$columns, qw(open close)) if ($open_close);
    if (&preference("Membership.tips.show_fees")) {
        $out .= $fees->report(
            rclass  => "Report MembershipFees",
            columns => $columns
        );
    }
    my $dupe_checks =
      join(', ', keys %{&preference("Membership.dupe_check")});
    my $approval = "Once payment is received (either via e-commerce or offline), ";
    if (&preference("Membership.approval") ne "auto") {
        if (&preference("Membership.extended_application")) {
            $approval .= "and the extended application forms are completed by the user, ";
        }
        $approval .= "the membership is sent to the membership administrator for approval.";
    } else {
        $approval .= "the membership is automatically approved.";
    }
    $out .= $ml->h3("Membership Sign-up");
    if (my $t = $this->get_template("Membership_signup_tips")) {
        $out .= $t;
    } else {
        $out .= <<END;
<ol>
<li>Potential members applying for membership can choose to apply for any of the currently
active membership types.</li>
<li>Users are first asked to choose their member type and then fill in the sign-up form associated with that member type.</li>
<li>At this time the system will check that the member is not a duplicate by checking the entered $dupe_checks against
existing records in the database.</li>
<li>If the user has correctly filled out the form, the system adds their membership to a shopping cart and the user can continue shopping or checkout.</li>
<li>Once the regular checkout process is completed the system will finalize membership.</li>
<li>In cases where the user is mailing in their payment the administrator must add the payment in the Payment module when the cheque is received.</li>
<li>$approval</li>
<li>Membership approval triggers the system to set the member's status to active and send
them a welcome email containing their login instructions. In some situations when an administrator
is manually applying a payment, the system will display a 'Send welcome email' button instead.</li>
</ol>

<p>* Administrators adding new members on behalf of users are encouraged to sign-up from the website
as if they were the user.  The "New Member" tool in the membership control panel is a short-cut for forcing a membership
into the system (e-commerce is not available in the control panel).</p>
END
    }
    $out .= $ml->h3("Membership Renewal");
    my $force_type = "Members are able to change their membership type prior to renewal.";
    if (&preference("Membership.force_renewal_type")) {
        $force_type = "Members that wish to change their membership type must contact a website administrator.";
    }
    $out .= <<END;
<ol>
<li>Members must login in order to renew their membership. The member's status with the association is displayed throughout the year.</li>
<li>$force_type</li>
<li>Members who choose to pay offline will remain in a pending state until their payment has been added by an administrator.</li>
<li>Admins can renew on behalf of a member by finding the members profile in the membership module and clicking on the renew button on the right sidebar or by using the ‘login as member’ feature.</li>
</ol>
END
    $out .= $ml->h3("Membership Status Definitions");
    $out .= &status_help(0);
    $out .= <<END;
<h3>Automated Expiration</h3>
<p>Members expire on the morning of their expiry date.  A members expiry date can be manually changed
from the control panel by editing the member's profile.  Lifetime members should be
given an expiry date in the distant future.  Members who do not renew within the grace period as noted above are automatically archived.
If there is a reinstatement fee for archived members(see Membership settings), the fee is added as a separate item when the archived
members logs in to renew.
END
    $out .= $ml->h3("Directories");
    my $write_visible =
      $share{DB}->{map}->get_column_attr("member", "visibility", "write");
    my $write_privacy =
      $share{DB}->{map}->get_column_attr("member", "privacy", "write");
    my $label_visible =
      $share{DB}->{map}->get_column_attr("member", "visibility", "label");
    my $label_privacy =
      $share{DB}->{map}->get_column_attr("member", "privacy", "label");
    my $visible_status = &preference("Membership.visible_status");
    my $d              = "Member profiles may be listed on the various directories on your website.";
    $d .= " Members must have the following statuses to be visible: $visible_status.";
    $d .= " Additionally, there are settings within each profile which will determine whether profiles are listed.";
    $d .= $ml->div("\"$label_visible\":");

    if ($write_visible == 1) {
        $d .= " This setting can be changed by logged in users.";
    } elsif ($write_visible <= 3) {
        $d .= " This setting can be changed by administrators only.";
    }
    $d .= $ml->div("\"$label_privacy\":");
    if ($write_privacy == 1) {
        $d .= " This setting can be changed by logged in users.";
    } elsif ($write_privacy <= 3) {
        $d .= " This setting can be changed by administrators only.";
    }
    $out .= $ml->p($d);
    return $out;
}

sub summary {
    my ($this, $section_id, $link) = @_;
    my $out;
    $section_id =
      $section_id || $this->get_section_id() || $share{DB}->my_gid();
    if (!$this->{section}) {
        $this->{section} = new ExSite::Section(id => $section_id);
    }
    $this->setup_queries();
    $ml or $ml = &get_obj("ML");

    # scan the membership data to get counts

    # scan the data in groups of 1000 records, just in case the DB is so
    # large that we run into memory problems
    $share{DB}->set("max_select", 1000);
    my $members = $share{DB}->get_query("all members with status", $section_id);
    my %count;

    while (scalar @$members > 0) {
        foreach my $m (@$members) {
            my $stat = $m->{status} || "other";
            $stat = "other" if ($m->{type} !~ /^member\//);
            $count{$m->{type}}{$stat}++;
        }
        $members = $share{DB}->fetchrows();
    }
    $share{DB}->set("max_select", 0);

    my $r = new ExSite::ReportBuilder();
    $r->title($this->{section}->get_my("title") . " Membership Overview");
    my @head = ("Membership Type", "Active", "Pending", "Expired", "Archived", "Other");
    $r->headers(\@head);
    $r->foot(&status_help(1));
    my $total = 0;

    my %mtype;
    foreach my $type (keys %count) { $mtype{$type} = 1; }
    foreach my $type ($this->membership_types($this->{section}->id)) {
        $mtype{$type} = 1;
    }
    foreach my $type (sort keys %count) {
        next if ($type !~ /^member/);
        my $typename = $type || "unknown";
        my @data =
          $link
          ? (
            $ml->a(
                $typename,
                {
                    href => $this->link(
                        pro    => "roster",
                        type   => $type,
                        status => undef
                    )
                }
            )
          )
          : ($typename);
        foreach my $stat (qw(active pending expired archived other)) {
            if ($count{$type}{$stat}) {
                push @data,
                  $link
                  ? (
                    $ml->a(
                        $count{$type}{$stat},
                        {
                            href => $this->link(
                                pro    => "roster",
                                type   => $type,
                                status => $stat,
                                page   => undef
                            )
                        }
                    )
                  )
                  : ($count{$type}{$stat});
            } else {
                push @data, undef;
            }
        }
        $r->push(\@data);
        $total = $count{$type}{active} + $count{$type}{pending} + $count{$type}{expired} + $count{$type}{archived};
    }
    foreach my $type (sort keys %count) {
        next if ($type =~ /^member/);
        my $typename = $type || "unknown";
        my @data =
          $link
          ? ($ml->a($typename, {href => $this->link(pro => "roster", type => $type)}))
          : ($typename);
        foreach my $stat (qw(active pending expired archived other)) {
            if ($count{$type}{$stat}) {
                push @data,
                  $link
                  ? (
                    $ml->a(
                        $count{$type}{$stat},
                        {
                            href => $this->link(
                                pro    => "roster",
                                type   => $type,
                                status => $type =~ /^member\// ? $stat : undef,
                                page   => undef
                            )
                        }
                    )
                  )
                  : ($count{$type}{$stat});
            } else {
                push @data, undef;
            }
        }
        $r->push(\@data);
        $total = $count{$type}{active} + $count{$type}{pending} + $count{$type}{expired} + $count{$type}{archived};
    }
    $out .= $r->make();
    my $url = "$config{server}{server}$config{server}{CGIpath}/$config{prog}{dcd}/Membership?pro=json&type=memtype";
    my $rows;
    foreach my $type (keys %count) {
        next if ($type =~ /^(staff|guest|secondary)$/);
        my $row;
        my $type_simple = $type;
        $type_simple =~ s/member\///g;
        push @$row, {v => $type_simple};
        foreach my $status (qw(active pending expired archived)) {
            push @$row, {v => $count{$type}{$status}};
        }
        push @$rows, {c => $row};
    }
    my $data = {
        "cols" => [
            {"label" => "Type",     "pattern" => "", "type" => "string"},
            {"label" => "Active",   "pattern" => "", "type" => "number"},
            {"label" => "Pending",  "pattern" => "", "type" => "number"},
            {"label" => "Expired",  "pattern" => "", "type" => "number"},
            {"label" => "Archived", "pattern" => "", "type" => "number"}
        ],
        "rows" => $rows
    };
    my $json_data = JSON::to_json($data);
    my $chart     = <<END;
    // Set a callback to run when the Google Visualization API is loaded.
    google.setOnLoadCallback(drawChart);

    function drawChart() {
      var jsonData = $json_data;

      // Create our data table out of JSON data.
      var data = new google.visualization.DataTable(jsonData);

      // Instantiate and draw our chart, passing in some options.
        var chart = new google.visualization.ColumnChart(
            document.getElementById('chart_memtype'));
        chart.draw(data, {
  			chartArea:{left:10,top:10,width:"100%"},
            width: 400, height: 200,
        	isStacked: 1, 'legend': 'bottom',
            'vAxis': {'title': 'Number of members'}});
	}
END
    if (!$share{Page}) {
        $out .= $ml->script($chart, {type => "text/javascript"});
    }
    return $out;
}

#------------------------------------------------------------------------
# link - optionally reference members using pathinfo

sub link {
    my ($this, %opt) = @_;

    # remove keys that should not persist
    if (!exists $opt{_auth}) {
        $opt{_auth} = undef;
    }
    if (!$this->{admin} && &preference("page.dynamic_method") eq "path") {
        $this->link_by_path(%opt);
    } else {
        if ($opt{_member}) {
            $opt{uid} = $opt{_member}->id;
            delete $opt{_member};
        }
        $this->SUPER::link(%opt);
    }
}

sub link_by_path {
    my ($this, %opt) = @_;

    my $in = new ExSite::Input;
    my $q  = $in->query();

    # %opt2 will contain the new query options in the link
    my %opt2 = (%$q, %opt);

    my $cmd = $opt2{pro} || "member";
    if ($cmd eq "member" && ($opt{uid} || $opt{_member})) {

        # this appears to be a simple link to view the member
        # so we will use path-linking
        my $member;
        if ($opt{_member}) {
            $member = $opt{_member};
        } elsif ($opt{uid}) {
            $member = new Modules::Membership::Member(id => $opt{uid});
        }
        my $path = $this->path($member);    # path to member
        if ($path) {
            $opt{__path}  = {"Membership" => $path};
            $opt{uid}     = undef;                     # remove member ID from query
            $opt{_member} = undef;                     # remove member object from query
            $opt{pro}     = undef;                     # remove membership cmd from query
        }
    } else {

        # this is an administrative link, not a view link
        $in->path("Membership", undef);
    }
    return $this->SUPER::link(%opt);
}

sub setup_member {
    my ($this, $uid) = @_;
    my $m = $this->member();
    $m->setup(id => $uid);

    #	for performance reasons we will not do a full load
    return $m;

    #    $m->load();
    #    return $m->loaded();
}

sub top_menu {
    my $this = shift;
    my @link;
    my %opt = (
        pro              => undef,
        section_id       => undef,
        uid              => undef,
        type             => undef,
        status           => undef,
        rpt_format       => undef,
        use_column_names => undef,
        searchterm       => undef,
        _bare            => undef
    );
    push @link, $ml->a("Top", {href => $this->link(%opt)});
    if ($this->{section}) {
        push @link,
          $ml->a($this->{section}->get_my("title"), {href => $this->link(%opt, section_id => $this->get_section_id())});
    }
    if ($this->{input}{uid}) {
        push @link, $ml->a($this->member->name, {href => $this->link(pro => "member", _bare => undef)});
    }
    my $uri = new ExSite::URI();
    $uri->setup("$config{server}{CGIpath}/ctrl-panel.cgi/Membership?section_id=" . $this->get_section_id);
    $uri->query(pro => "search");
    push(
        @link,
        $ml->span("Member Search")
          . $ml->form(
            $ml->input(undef,
                {name => "searchterm", type => "text", placeholder => "name, organization, email or id", size => 30})
              . $ml->input(undef, {name => "pro", value => "search", type => "hidden"})
              . $ml->input(undef, {name => "section_id", value => $this->get_section_id(), type => "hidden"}),
            {method => "get", action => $uri->write_full()}
          )
    );
    return &ExSite::HTML::PathBar(links => \@link);
    return $ml->p(join(" &gt; ", @link));
}

sub show_member {
    my ($this, $uid) = @_;
    my $out;
    if ($this->{admin}) {
        if ($uid) {
            $this->member->setup(id => $uid);
        } else {
            $uid = $this->member->id;
        }
        return $ml->warn("Invalid member $uid.") if (!$this->member->defined);
        my $acct_id = $this->member->account->id;
        my $name    = $this->member->name;
    }

    # display profile
    $out .= $this->user_menu($uid);
    $out .= $this->member->show();

    # change our page metadata to reflect the current member
    if ($share{Page}) {
        my $page_title = $this->run_handler("profile_page_title")
          || $this->member->name;
        $share{Page}->set_metadata("title", $page_title);
    } elsif ($this->{admin}) {

        # administrator profile displays auxillary information about the member
        # event history
        if (   $share{DB}->{map}->is_mapped("evt_reg")
            && $share{DB}->count("evt_reg"))
        {
            my $url = "$config{server}{CGIpath}/$config{prog}{dcd}/EvtReg?_opt=regcmd=user_registrations%26uid=$uid";
            my $ajax = $ml->a("registrations", {class => "load_ajax", href => $url});
            $out .= $ml->div($ajax, {id => "event_registrations"});
        }
    }

    # index of secondary members
    if ($this->member->allow && !$this->{input}{_bare}) {
        $out .= $this->show_secondary_members($this->member);
    }

    return $out;
}

# --- manage secondary members

sub show_secondary_members {
    my ($this, $member) = @_;
    my $out = $this->run_handler("show_secondary_members", $member);
    return $out if defined $out;

    # are we a secondary?
    my $parent = $member->parent();
    if ($parent && $parent->loaded()) {
        $out .= $ml->p(
            &substitute(
                $msg{"Associated with <a href='[[url]]'>[[name]]</a>."},
                {
                    name => $parent->name,
                    url  => $this->link(_bare => undef, uid => $parent->id)
                }
            )
        );
        if ($parent->is_me() && $member->allow_edit()) {
            $out .= $ml->span(
                $ml->a(
                    &substitute($msg{"Remove [[name]] From My Account"}, {name => $member->name()}),
                    {class => "button", href => $this->link(pro => "remove_member", uid => $member->id)}
                ),
                {style => "float:right"}
            );
        }
    }

    # check for secondary members
    my $secondary = $member->get_child("member");
    $secondary->sort("last_name");
    if ($secondary && $secondary->count > 0) {
        my $fmt = &preference("Membership.secondary.format");
        my $list;
        while (my $sec = $secondary->next()) {
            last
              if (!$share{DB}->authorize
                && &preference("Membership.secondary.hidden_to_public"));
            $list .= $this->show_secondary_member($sec, $fmt);
        }
        if ($list) {
            my $label = &preference("Membership.secondary.plural_label")
              || &preference("Membership.secondary.label");
            $out .= $ml->h2($msg{$label});
            if (&preference("Membership.secondary.container") eq "li") {
                $out .= $ml->ul($list);
            } else {
                $out .= $list;
            }
        }
    }
    return $ml->div($out, {id => "secondary_m"});
}

sub show_secondary_member {
    my ($this, $secondary, $fmt) = @_;
    my $out = $this->run_handler("show_secondary_member", $secondary, $fmt);
    return $out if defined $out;
    return
      if (!$share{DB}->is_manager()
        && $secondary->getdata("type") =~ /^member/
        && !$secondary->is_valid_member);
    if ($secondary->is_visible) {
        if ($fmt eq "link_to_profile") {
            my $title;
            if ($title = $secondary->getdata("title")) {
                $title = ", " . $title;
            }
            my $container = &preference("Membership.secondary.container") || "p";
            $out .= $ml->$container(
                $ml->a(
                    $secondary->name,
                    {
                        href => $this->link(
                            _bare => undef,
                            uid   => $secondary->id,
                            pro   => undef
                        )
                    }
                  )
                  . $title
            );
        } elsif ($fmt eq "mailto") {
            $out .= $ml->p(&ExSite::HTML::MailTo($secondary->email, $secondary->name));
        } elsif ($secondary->allow() && $fmt eq "contact_info") {
            my $contact = $secondary->contacts->next;
            my $info    = $secondary->name() . $ml->br;
            $info .= $secondary->getdata("title") . $ml->br
              if ($secondary->getdata("title"));
            $info .= &ExSite::HTML::MailTo($secondary->email) . $ml->br;
            if ($contact) {
                $info .= $contact->getdata("phone1");
            }
            $out .= $ml->p($info, {class => "MembershipSecondaryContact"});
        } elsif ($secondary->allow() && $fmt eq "mini_profile") {
            my $sec = $secondary->show_mini_profile();
            if ($secondary->is_child) {

                # viewer is primary - add a view link
                $sec .= $ml->a($msg{"(view profile)"}, $this->link(_bare => undef, uid => $sec->id));
            }
            $out .= $ml->div($secondary->show_mini_profile(), {class => "MembershipMiniProfile"});
        } elsif ($fmt eq "linked_mini_profile") {
            $out .= $ml->div($secondary->show_mini_profile($this->link(_bare => undef, uid => $secondary->id)),
                {class => "MembershipMiniProfile"});
        } else {
            my $sec = $secondary->name;
            if ($secondary->is_child) {

                # viewer is primary - add a view link
                $sec .=
                  $ml->span(" " . $ml->a($msg{"(view profile)"}, {href => $this->link(uid => $secondary->id)}));
            }
            $out .= $ml->p($sec);
        }
    }
    return $out;
}

sub remove_member {
    my ($this) = @_;
    my $out;
    my $name = $this->member->name();
    if (!$this->member->owns()) {
        return $this->error($msg{"Permission denied."});
    }
    $this->member->setdata("parent_id",    0);
    $this->member->setdata("organization", "");
    if ($share{DB}->{map}->is_mapped("member", "location_id")) {
        $this->member->setdata("location_id", 0);
    }
    $this->member->force_save();
    $out .= $ml->p(&substitute($msg{"[[name]] has been removed from your organization."}, {name => $name}));
    return $out;
}

sub promote {
    my ($this) = @_;
    my $out = $ml->h1("Promote Member");
    if ($this->{input}{confirm}) {
        $out .= $this->member->promote();
    } else {
        my $name   = $this->member->name;
        my $parent = $this->member->parent;
        if ($parent) {
            my $pname = $parent->name;
            $out .= $ml->p(
                "Promoting this member will make them the primary member/user for this
group.  The existing primary user ($pname) will be demoted to become a
secondary user."
            );
            $out .= $ml->p(
                $ml->a(
                    "View current primary ($pname)",
                    {
                        href   => $this->link(pro => undef, uid => $parent->uid),
                        target => "_blank"
                    }
                )
            );
            $out .= $ml->p($ml->a("Confirm promotion of $name", {href => $this->link(confirm => 1)}));
        } else {
            $out .= $ml->p("This member is already the primary user for this group.");
        }
    }
    return $out;
}

# --- show membership status

sub show_status_history {
    my ($this, $uid) = @_;
    my $out;

    $out .= $ml->h1("Status History of " . $this->member->name);
    if ($uid) {
        $this->member->setup(id => $uid);
    } else {
        $uid = $this->member->id;
    }

    if ($this->{post}{status} =~ /^(active|pending|expired|archived)$/) {
        my $note =
          $this->{input}{note} || "updated by " . $share{DB}->my_name();
        my $stat = $this->member->set_status($this->{post}{status}, $note);
        if (!$stat) {
            $out .= $this->error("Could not set status for this member.");
        }
    }
    $out .= $ml->p($this->quick_status($uid));

    my $f = new ExSite::FormBuilder(method => "post");
    $f->template($ml->p("[[status:prompt]] [[status:input]] [[note:prompt]] [[note:input]] [[buttons]]"));
    $f->set("buttons", $ml->input(undef, {type => "submit", value => "Change status"}));
    $f->input(
        name      => "status",
        type      => "select",
        prompt    => "Change status to:",
        options   => ["active", "pending", "expired", "archived"],
        nullvalue => "== status ==",
        required  => 1,
    );
    $f->input(
        name   => "note",
        type   => "text",
        prompt => "Note:",
        size   => 30,
    );
    $out .= $f->make();
    $out .= $ml->p(
        $ml->a(
            "Account Statement",
            {
                href => "javascript:popup_large('"
                  . "$config{server}{CGIpath}/$config{prog}{ctrlpanel}/Pay?acct="
                  . $this->member->account->id
                  . "&section_id="
                  . $this->{input}{section_id} . "')",
                class => "button"
            }
        )
    );

    my $r = new ExSite::ReportBuilder(title => "Membership History of " . $this->member->name());
    $r->headers(["Date", "Changed to", "Type", "Note", "Expiry Date"]);
    my @status = $share{DB}->fetch_match("member_status", {member_id => $uid}, "member_status_id desc");
    foreach my $stat (@status) {
        $r->push(
            $stat->{date}, $stat->{status}, $stat->{member_type} || "n/a",
            $stat->{note}, $stat->{expirydate} || "n/a"
        );
    }
    $out .= $r->make();
    return $out;
}

sub quick_status {
    my ($this, $uid) = @_;
    my %flags = (active => "info", expired => "info", pending => "warning", archived => "error");
    my $current_status =
      $ml->span($ml->strong(uc($this->member->status())), {class => $flags{$this->member->status()}});
    my $out;
    if ($this->member->member_type() eq "staff") {
        return $ml->div("Staff members do not have a member status.");
    } else {
        if ($this->member->parent() && !$this->member->use_status()) {
            $out .= $ml->div(
                &substitute(
                    "Membership status via [[name]]: [[status]]",
                    {status => $current_status, name => $this->member->parent->name()}
                )
            );
        } elsif ($this->member->is_orphan()) {
            $out .= $ml->div("This member is an orphan (unattached secondary member)", {class => "error"});
        } else {
            $out .= $ml->div($current_status);
        }
    }
    my $account = $this->member->account;
    if ($account->balance > 0.01) {
        $out .= $ml->div("This member has a balance owing of: " . $ml->strong("\$" . $account->balance),
            {class => "balance_owing"});
    }
    if ($this->member->status() eq "pending") {
        my $approve_link = $this->link(pro => "approve", status => undef, _bare => undef);
        my $reject_link  = $this->link(pro => "reject",  status => undef, _bare => undef);
        $out .= $ml->div(
            $ml->a("approve", {href => $approve_link, class => "button"}) . " "
              . $ml->a("reject", {href => $reject_link, class => "button"}),
            {class => "buttons"}
        );
    } elsif ($this->member->is_renewable) {
        $out .= $ml->div(
            $ml->a(
                "Renew",
                {
                    href  => $this->link(pro => "renew", type => undef, _bare => undef),
                    class => "button"
                }
            ),
            {style => "margin: 5px"}
        );
    }
    return $out;
}

sub moderate_status {
    my ($this, $cmd, $uid) = @_;
    my $out;
    if ($uid) {
        $this->member->setup(id => $uid);
    } else {
        $uid = $this->member->id;
    }
    if (!$this->member->is_pending) {
        return $this->error($msg{"You cannot $cmd this membership because it is not pending."});
    }
    if ($cmd eq "approve") {
        $out .= $this->approve_application();
    } elsif ($cmd eq "reject") {
        $out .= $this->member->status_reject();
    }
    return $out if ($out);
    $ml->location($this->link(pro => "member_status", __plaintext => 1));
}

sub search {
    my ($this, $term) = @_;
    $term or $term = $this->{input}{searchterm};

    # requires a fulltext index:
    # alter table member add fulltext(organization,first_name,middle_name,last_name);
    my $out;
    if ($term && length($term) > 3) {
        my @member =
          $share{DB}->get_query("member fulltext search", $term . '*', $this->{section}->id, $term . '*', $term);
        $out .= $this->roster(data => \@member);
    } elsif ($term) {
        my @member =
          $share{DB}->get_query("member search not fulltext", $this->{section}->id, '%' . $term . '%', $term);
        $out .= $this->roster(data => \@member);
    }
    $out .= $this->advanced_search();
    $out .= $ml->h3("Search for members by name");
    $out .= $ml->p("You can search for multiple names by typing them in separated by a space");
    $out .= $ml->p("The * character can be appended to the end of your terms to find partial matches.");

    $out .= $ml->form(
        $ml->input(undef, {type => "text", name => "searchterm", value => $term})
          . $ml->input(undef, {type => "hidden", name => "pro", value => "search"})
          . $ml->input(
            undef,
            {
                type  => "hidden",
                name  => "section_id",
                value => $this->{section}->id
            }
          )
          . $ml->input(undef, {type => "submit", value => "Search"}),
        {method => "get"}
    );
    return $out;
}

sub manage_fees {
    my $this = shift;

    # declare a markup generator
    $ml = new ExSite::ML;
    my $out;
    my $sid = $this->{section}->id();
    my $section = $share{DB}->fetch("section", $sid);
    $this->get_fees($sid);

    if (keys %{$this->{post}} > 0) {
        $out .= $this->update_fees($sid, $this->{post});
        $this->get_fees($sid);    # refresh
    }

    my $tbody;
    while (my $f = $this->{fees}->next()) {
        my $id         = $f->id();
        my $type       = $f->getdata("type");
        my $cost       = $f->getdata("cost");
        my $renew_cost = $f->getdata("renew_cost");
        my $open_date  = $f->open_date();
        my $close_date = $f->close_date();
        my ($open, $close);
        $open  = $f->open_date->write("sql_date")  if ($open_date);
        $close = $f->close_date->write("sql_date") if ($close_date);
        $tbody .= $ml->tr(
                $ml->td($ml->input(undef, {type => "checkbox", name => "del_" . $f->id}), {align => "center"})
              . $ml->td($type)
              . $ml->td(
                $ml->input(
                    undef,
                    {
                        type  => "text",
                        size  => 5,
                        name  => "fee${id}_cost",
                        value => $cost
                    }
                )
              )
              . $ml->td(
                $ml->input(
                    undef,
                    {
                        type  => "text",
                        size  => 5,
                        name  => "fee${id}_renew_cost",
                        value => $renew_cost
                    }
                )
              )
              . $ml->td(
                $ml->textarea($f->getdata("description"), {rows => 2, cols => 24, name => "fee${id}_description"})
              )
              . $ml->td(
                $share{DB}->input_exsite(
                    name     => "fee${id}_visibility",
                    datatype => "list:member_visibility",
                    value    => $f->getdata("visibility")
                )
              )
              . $ml->td(
                $share{DB}->input_date(
                    name              => "fee${id}_open",
                    date_input_format => "picker",
                    value             => $open
                )
              )
              . $ml->td(
                $share{DB}->input_date(
                    name              => "fee${id}_close",
                    date_input_format => "picker",
                    value             => $close
                )
              )
              . $ml->td(
                $ml->input(
                    undef,
                    {
                        type  => "text",
                        size  => 3,
                        name  => "fee${id}_sortkey",
                        value => $f->getdata("sortkey")
                    }
                )
              )
        );
    }
    my $tfoot = $ml->tr(
            $ml->td($msg{"New Type"})
          . $ml->td($ml->input(undef, {type => "text", size => 20, name => "new_type"}))
          . $ml->td($ml->input(undef, {type => "text", size => 5,  name => "new_cost"}))
          . $ml->td($ml->input(undef, {type => "text", size => 5,  name => "new_renew_cost"}))
          . $ml->td($ml->input(undef, {type => "text", size => 30, name => "new_description"}))
          . $ml->td(
            $share{DB}->input_exsite(
                name     => "new_visibility",
                datatype => "list:member_visibility"
            )
          )
          . $ml->td($share{DB}->input_date(name => "new_open", date_input_format => "picker"))
          . $ml->td(
            $share{DB}->input_date(
                name              => "new_close",
                date_input_format => "picker"
            )
          )
          . $ml->td($ml->input(undef, {type => "text", size => 3, name => "new_sortkey"}))
    );
    $out .= $ml->form(
        $ml->table(
            {
                thead => $ml->tr(
                        $ml->th("Delete")
                      . $ml->th("Membership Type")
                      . $ml->th("Fee")
                      . $ml->th("Renewal Fee" . $ml->br . "(optional)")
                      . $ml->th("Description")
                      . $ml->th("Visibility")
                      . $ml->th("Open Date")
                      . $ml->th("Close Date")
                      . $ml->th("Order")
                ),
                tbody   => $tbody,
                tfoot   => $tfoot,
                caption => "$section->{title} Membership Fees",
            },
            {class => "Report"}
          )
          . $ml->input(undef, {type => "submit", value => "Submit"}),
        {method => "post"}
    );
    return $out;
}

sub update_fees {
    my ($this, $section_id, $data) = @_;
    my $out;
    my $db = $share{DB};
    if ($db->is_manager($section_id)) {

        # delete
        my %deleted;
        foreach my $key (keys %$data) {
            if ($key =~ /^del_(.+)$/) {
                my $id   = $1;
                my $f    = new Modules::Membership::Fee(id => $id);
                my $type = $f->getdata("type");
                if ($f->id() eq $id) {
                    $f->delete();
                    $out .= $ml->p("Deleted $type fee [$id].");
                    $deleted{$id} = 1;
                }
            }
        }

        # parse fees
        my %parse;
        foreach my $key (keys %$data) {
            if ($key =~ /^fee(\d+)_(\w+)#?/) {
                my $id  = $1;
                my $col = $2;
                next if ($deleted{$id});
                $parse{$id}{$2} = $data->{$key};
            }
        }

        # save fees
        foreach my $id (keys %parse) {
            next if ($id !~ /\d+/);
            my $f = new Modules::Membership::Fee(id => $id);
            my $type = $f->getdata("type");
            foreach my $col (keys %{$parse{$id}}) {
                my $value = $parse{$id}{$col};
                if ($value ne $f->getdata($col)) {
                    $f->setdata($col, $parse{$id}{$col});
                }
            }
            if (!$f->save()) {
                $out .= $this->error("Failed to update fee $id: " . $f->show_diagnostics("error", "html"));
            }
        }

        # new fee
        if ($data->{new_cost} && $data->{new_type}) {
            if ($data->{new_cost} =~ /^\d+(\.\d\d)?$/) {
                my $sepchar    = $config{form}{sepchar};
                my $renew_cost = undef;
                $renew_cost = $data->{renew_cost}
                  if ($data->{renew_cost} =~ /^\d+(\.\d\d)?/);
                my $fee = new Modules::Membership::Fee(
                    data => {
                        type        => $data->{new_type},
                        cost        => $data->{new_cost},
                        renew_cost  => $renew_cost,
                        description => $data->{new_description},
                        visibility  => $data->{new_visibility},
                        open        => $data->{"new_open${sepchar}date"},
                        close       => $data->{"new_close${sepchar}date"},
                        sortkey     => $data->{new_sortkey},
                        section_id  => $section_id
                    }
                );
                if ($fee->save()) {
                    $out .= $ml->p("Added new $data->{new_type} fee.");
                } else {
                    $out .=
                      $this->error(
                        "Failed to add new $data->{new_type} fee: " . $fee->show_diagnostics("error", "html"));
                }
            } else {
                $out .= $this->error("Invalid $data->{new_type} fee: $data->{new_fee} - fee not added.");
            }
        }
    } else {
        $out .= $this->error("Permission denied - invalid website.");
    }
    return $out;
}

# publish() can be used to write out any files that will benefit from
# being available statically

sub publish {
    my $this = shift;
    my $ml   = &get_obj("ML");
    my $out  = $ml->h2("Publishing Membership");
    my $q    = &DecodeQuery($ENV{QUERY_STRING});
    my $sid  = $q->{section};
    my $members;
    if (!$sid) {
        $members = $share{DB}->custom_query("select * from member where length(photo) > 1");
    } else {
        $members = $share{DB}
          ->custom_query("select * from member where length(photo) > 1 and (section_id = 0 or section_id = ?)", $sid);
    }
    foreach my $m (@$members) {
        $out .= $this->publish_member($m);
    }
    return $out;
}

sub publish_member {
    my ($this, $m) = @_;
    my $out;
    my $member = new Modules::Membership::Member(data => $m);
    my $path = $this->diskpath . "/" . $member->id();

    if ($member->getdata("photo")) {
        my $c     = new ExSite::ContentData;
        my $mkdir = $c->mkdir($path);
        if (!$mkdir) {
            $out .= $this->show_diagnostics("error", "html");
        }
        if (my $photo = $member->getdata("photo")) {
            my $img      = new ExSite::Image($photo);
            my $filename = &clean_filename($img->name);
            $out .= $this->publish_file("$path/$filename", $img);
        }
        if (my $thumb = $member->getdata("thumbnail")) {
            my $img      = new ExSite::Image($thumb);
            my $filename = &clean_filename($img->name);
            $out .= $this->publish_file("${path}/th_${filename}", $img);
        }
    }
    return $out;
}

# diskpath to published files

sub diskpath {
    my $this = shift;
    return $config{server}{HTMLroot} . $this->htmlpath;
}

sub htmlpath {
    my $this = shift;
    my $path = "$config{server}{HTMLpath}/_Modules/Membership";
    return $path;
}

sub publish_file {
    my ($this, $file, $img) = @_;
    my $out;
    my $ml = &get_obj("ML");
    $file =~ /^(((\/[\w-]+)+)\/([\w-]+\.[\w]+))$/;
    my $secure_filename = $1;
    my $imgdata         = $img->get;
    if (!-e $secure_filename
        || length $imgdata != -s "$secure_filename")
    {
        if (open my $OUTFILE, q{>}, "$secure_filename") {
            print $OUTFILE $imgdata;
            close $OUTFILE;
            $out .= "Writing $secure_filename... succeeded." . $ml->br;
        } else {
            $this->error("Membership::publish: publish $secure_filename failed: $!");
            $out .= $ml->span("FAILED: $!", {class => "error"}) . $ml->br;
        }
    }
    return $out;
}

sub unpublish {
    my $this = shift;

    # It can optionally return some status messages.
    my $out;

    return $out;
}

# search_index() allows this plug-in to manipulate the system search index.

sub search_index {
    my ($this, $search, $site, $page) = @_;

    # $search is an ExSite::Search object.  Use this to modify the
    # search index.

    # $site is an ExSite::Section object for the current section.  It is
    # normally used to identify information that should be indexed in
    # this call.

    # $page is an ExSite::Page object for the service page that
    # all search hits will be directed to.

    ### index on first_name, last_name, organization, bio/description
    ### set url privacy to match the privacy setting for the profile
    ### only index visible profiles

    # You can optionally return some status messages
    my $out;

    # manipulate the search index here.

    return $out;
}

# cron() allows this plug-in to respond to scheduling events.

sub cron {
    my ($this, $command, $type, $id) = @_;
    $this->setup_queries();
    $ml = new ExSite::ML;

    # $command, $type, $id are message parameters passed to us by the
    # scheduler.  They have no predefined meaning, so you can do as you
    # please with them.

    # You can optionally return some status messages
    my $out;

    # perform your scheduled tasks here.

    #### expire memberships
    if ($command eq "expire_members") {
        if ($type eq "section" && $id) {
            $out .= "running $command on section $id\n";
            $out .= $this->bulk_status_update($id);
        }
    }

    #### notify of pending memberships

    #### notify potential renewals

    # options are passed as encoded string
    if ($command eq "notify_renewal") {
        if ($type eq "section" && $id) {
            $out .= "running $command on section $id\n";
            $out .= $this->notify_renewal(section => $id, cron => 1);
        }
    }
    return $out;
}

sub preview_auto_emails {
    my ($this) = @_;
    my $out;
    my (%out, @tabs, @panes, $thistab, $sid);
    my $sid = $this->get_section_id;
    my $t   = new ExSite::Time;
    if (!$this->{input}{send_date}) {
        my $fb = new ExSite::FormBuilder(method => "get", action => $this->link());
        $out{preview} .= $ml->h3("Preview Automated Emails");
        $out{preview} .= $ml->p(
"This tool will show you what emails are scheduled to go out as of the current system setup on any particular day."
        );
        $out{preview} .= $ml->p("Enter a date for which you want to preview membership renewal emails.");
        $fb->input(
            type  => "hidden",
            name  => "pro",
            value => "preview_auto_emails"
        );
        $fb->input(type => "hidden", name => "section_id", value => $sid);
        $fb->input(
            name   => "send_date",
            prompt => "Date",
            value  => $t->write("sql_date")
        );
        $fb->input(type => "submit", value => "Go");
        $out{preview} .= $fb->make();
    } else {
        my ($titles, $panes, $states, $links);
        my $crontask = $this->DB->fetch_match(
            "crontask",
            {
                module  => "Membership",
                command => "notify_renewal",
                type    => "section",
                id      => $sid
            }
        );
        foreach my $task (@$crontask) {
            my $days = &preference("Membership.renewal_email.days");
            foreach my $day (split(/,/, $days)) {
                my $pane;
                $t->set($this->{input}{send_date}, "sql_date");
                my %opt     = (time => $t, days => $day, section => $sid);
                my $command = $task->{command};
                my $label   = abs($day) . " days";
                if ($day < 0) {
                    push(@$titles, "emails sent $label before expiry");
                } elsif ($day > 0) {
                    push(@$titles, "emails sent $label after expiry");
                } else {
                    push(@$titles, "emails sent on expiry");
                }
                $pane .= $this->notify_renewal(%opt);
                push(@$panes,  $pane);
                push(@$states, 1);
            }
        }
        $out{preview} .= $ml->p($ml->a("preview another date", {href => $this->link(send_date => undef)}));
        $out{preview} .= <<END;
<script>
// this tells jquery to run the function below once the DOM is ready
\$(document).ready(function() {

// choose text for the show/hide link
var showText="show preview";
var hideText="hide preview";

// create the toggle link
\$(".email_m").before("<a href='#' class='button_sm toggle_link'>"+showText+"</a>");

// hide the content
\$('.email_m').hide();

// capture clicks on the newly created link
\$('a.toggle_link').click(function() {
// change the link text
if (\$(this).text()==showText) {
\$(this).text(hideText);
}
else {
\$(this).text(showText);
}

// toggle the display
\$(this).next().toggle();

// return false so any link destination is not followed
return false;
});
});
</script>
END
        $out{preview} .= &ExSite::HTML::AccordionBox(
            titles => $titles,
            panes  => $panes,
            states => $states,
            links  => $links
        );
    }

    $out{edit} = $ml->h3("Edit Automated Emails");
    my $days = &preference("Membership.renewal_email.days");
    my @days = split(/,/, $days);
    my @notfound;
    my $loc = new ExSite::Section(id => $sid);
    foreach my $days (@days) {
        my $name;
        my $label;
        if ($days < 0) {
            $name  = "email_expiring_" . abs($days);
            $label = "emails sent " . abs($days) . " days before expiry";
        } elsif ($days >= 0) {
            $name  = "email_expired_" . abs($days);
            $label = "emails sent " . abs($days) . " days after expiry";
        }
        if ($loc) {

            # always get the newest revision of templates
            $loc->set_revision("newest");
            my $ctemplate = $loc->find($name);
            my $content   = $share{DB}->fetch("content", $ctemplate->id);
            my $cdata     = $share{DB}->fetch_match("content_data", {content_id => $ctemplate->id}, "content_data_id");
            my $path      = "/" . $sid . "/" . $content->{page_id} . "/" . $ctemplate->id;
            if ($ctemplate->get_html) {
                my $preview_path =
                  "/" . $sid . "/" . $content->{page_id} . "/" . $ctemplate->id . "/" . $cdata->[-1]->{content_data_id};
                my $preview = $ml->a(
                    "[preview]",
                    {
                        href   => "$config{server}{CGIpath}/ctrl-panel.cgi/CMS?previewmode=full&path=$preview_path",
                        target => "_blank"
                    }
                );
                $out{edit} .= $ml->p(
                    $ml->a(
                        $label,
                        {
                            href   => "$config{server}{CGIpath}/ctrl-panel.cgi/CMS?previewmode=full&path=$path",
                            target => "_blank"
                        }
                      )
                      . "&nbsp;"
                      . $preview
                );
            } else {
                push(@notfound, $days);
            }
        }
    }
    if (scalar @notfound) {
        my @before;
        foreach my $n (@notfound) {
            push(@before, abs($n)) if ($n < 0);
        }
        my @after;
        foreach my $n (@notfound) {
            push(@after, $n) if ($n >= 0);
        }
        my $ctemplate = $loc->find("email_renewal");
        my $content   = $share{DB}->fetch("content", $ctemplate->id);
        my $cdata     = $share{DB}->fetch_match("content_data", {content_id => $ctemplate->id}, "content_data_id");
        my $path      = "/" . $sid . "/" . $content->{page_id} . "/" . $ctemplate->id;
        if ($ctemplate->get_html) {
            my $label = "emails sent";
            if (scalar @before) {
                $label .= " " . join(",", @before) . " days before expiry";
            }
            if (scalar @after) {
                $label .= " " . join(",", @after) . " days after expiry";
            }
            my $preview_path =
              "/" . $sid . "/" . $content->{page_id} . "/" . $ctemplate->id . "/" . $cdata->[-1]->{content_data_id};
            my $preview = $ml->a(
                "[preview]",
                {
                    href   => "$config{server}{CGIpath}/ctrl-panel.cgi/CMS?path=$preview_path",
                    target => "_blank"
                }
            );
            $out{edit} .= $ml->p(
                $ml->a(
                    $label,
                    {
                        href   => "$config{server}{CGIpath}/ctrl-panel.cgi/CMS?previewmode=full&path=$path",
                        target => "_blank"
                    }
                  )
                  . "&nbsp;"
                  . $preview
            );
        }
    }
    my $ntab   = 0;
    my $tabnum = 0;
    foreach my $tab (qw(preview edit)) {
        if ($out{$tab}) {
            push @panes, $out{$tab};
            push @tabs,  ucfirst $tab;
            $ntab++;
        }
    }
    if (!$tabnum && $ntab > 1) { $tabnum = 1; }

    return &ExSite::HTML::DynTabBox(
        tabs    => \@tabs,
        panes   => \@panes,
        thistab => 0,
        width   => "100%",
    );
    return $out;
}

# return body of renewal reminder email
# name = name of content object (structured as email_[expiring|expired]_[days])
# section_id = working section
# member = member object
sub renewal_reminder_email {
    my ($this, $name, $section_id, $member) = @_;
    my $stat = $this->run_handler("renewal_reminder_email", $name, $section_id, $member);
    return $stat if ($stat);
    if ($share{Membership}{renewal_reminder_email}{$name}{$section_id}) {
        return $share{Membership}{renewal_reminder_email}{$name}{$section_id};
    }
    my $template = $this->get_template($name, $section_id);
    if (!$template) {
        $template = $this->get_template("email_renewal", $section_id)
          || $msg{
            &get_file(
                "$config{server}{HTMLroot}$config{server}{HTMLpath}/_Modules/Membership/notify-renewal-email.txt")
          };
    }
    $share{Membership}{renewal_reminder_email}{$name}{$section_id} = $template;
    return $template;
}

# notify members that they will be expiring or have expired
# days = days before(-) or after(+) expiry
# section = section_id of members to notify
# cron = are we running via cron or are we in test mode
# time = preset time at which emails will go out; for testing purposes
sub notify_renewal {
    my ($this, %opt) = @_;
    my $db         = new ExSite::Form;
    my $section_id = $opt{section};
    my $days =
      defined $opt{days}
      ? $opt{days}
      : &preference("Membership.renewal_email.days", "section", $section_id);
    my @days = split(/,/, $days);
    my $section = new ExSite::Section(id => $section_id);
    return if (!$section);
    $this->{section} = $share{Section} = $section;
    my ($out, $n);
    my $members = $db->get_query("all association members with status", $section_id);

    foreach my $days (@days) {
        my $today = $opt{time} || new ExSite::Time;
        my $email_name;
        if ($days < 0) {
            $email_name = "email_expiring_" . abs($days);
        } else {
            $email_name = "email_expired_" . abs($days);
        }
        foreach my $m (@$members) {
            my $member = new Modules::Membership::Member(data => $m);
            my $template = $this->renewal_reminder_email($email_name, $section_id, $member);
            next if (!$template);
            $this->{Member} = $member;
            my $expiry = $member->expirydate;
            my $window = &preference("Membership.early_renewal_period");
            $window = 700 if (!$opt{cron});
            next if ($member->is_archived || !$expiry);
            next if (!$member->is_renewable(1, $window));
            my $t = new ExSite::Time;
            $t->set($expiry, "sql_date");
            $t->add($days, "days");
            my $merge      = &Modules::Membership::Base::get_merge($member);
            my $membertype = $m->{type};

            if ($membertype =~ /^member/) {
                $membertype =~ s/^member\///;
            }
            if ($days < 0) {
                $merge->{expiring_or_expired} = "expires shortly";
            } else {
                $merge->{expiring_or_expired} = "has expired";
            }

            # reinstantiate section to expand again
            my $section = new ExSite::Section(id => $section_id);

            # test if email should go out
            if ($opt{cron}) {
                $out .= "date:" . $t->write("sql_date") . " today:" . $today->write("sql_date") . "\n";
            }
            if ($t->write("sql_date") eq $today->write("sql_date")) {
                next
                  if ( &preference("Membership.renewal_email.exclude_secondary")
                    && $member->getdata("parent_id"));
                if ($template =~ /\[\[renewal_fee\]\]/) {
                    $merge->{renewal_fee} = $this->membership_fee(
                        member => $member,
                        type   => $membertype
                    );
                }
                my $message = &substitute($template, $merge);

                # expand cms references
                my $tmp = $config{page}{dynamic_url_type};

                # force full dynamic_url_type for email
                $config{page}{dynamic_url_type} = "full";
                $message = $section->expand(html => $message);
                $config{page}{dynamic_url_type} = $tmp;

                my $title = $section->get_my("title");
                my $owner =
                     &preference("Membership.renewal_email.from", "section", $section_id)
                  || &preference("Membership.owner_email", "section", $section_id)
                  || $share{DB}->owner_email($section->get());
                my $email;
                $email = $this->run_handler("Membership_notify_renewal_email", $member);
                $email = $member->getdata("email") if (!$email);

                # comma seperated list of emails to cc
                $email .= "," . &preference("Membership.renewal_email.cc", "section", $section_id)
                  if (&preference("Membership.renewal_email.cc", "section", $section_id));
                my $subject = $msg{&preference("Membership.renewal_email.subject.$days", "section", $section_id)};
                if (  !$subject
                    && &preference("Membership.renewal_email.subject", "section", $section_id))
                {
                    $subject = $msg{&preference("Membership.renewal_email.subject", "section", $section_id)};
                } elsif (!$subject) {
                    $subject = $msg{"$title Membership Renewal"};
                }
                if (!$opt{cron}) {
                    my $summary = $ml->a(
                        $member->name(),
                        {
                            href => $this->link(uid => $member->id, pro => undef)
                        }
                    );
                    $summary .= "&nbsp";
                    $summary .= $ml->div($message, {class => "email_m"});
                    $summary .= $ml->br() . $ml->br();
                    $out     .= $summary;
                } else {
                    $out .= "sending renewal email to $email -- member " . $member->id . "\n\n";
                    &ExSite::Mail::send(
                        to      => $email,
                        from    => $owner,
                        subject => $subject,
                        body    => $message,
                    );
                }
            }
        }
    }
    $out = &html_to_plaintext($out) if ($opt{cron});
    return $out;
}

sub date_range_inputs {
    my $this = shift;
    my $in   = $this->{input};

    my $ml  = &get_obj("ML");
    my $out = $ml->script(
        undef,
        {
            type => "text/javascript",
            src  => "$config{server}{HTMLpath}/_ExSite/js/date.js"
        }
    );
    $out .= $ml->script(
        undef,
        {
            type => "text/javascript",
            src  => "$config{server}{HTMLpath}/_ExSite/js/jquery.datePicker.js"
        }
    );
    $out .= $ml->link(
        undef,
        {
            rel   => "stylesheet",
            type  => "text/css",
            media => "screen",
            href  => "$config{server}{HTMLpath}/_ExSite/css/datePicker.css"
        }
    );
    $out .= $ml->script(
"Date.firstDayOfWeek = 0;\nDate.format = 'yyyy-mm-dd';\n\$(function() {\n\$('.date-pick').datePicker({startDate:'$config{form}{date_picker_start_date}'});\n});\n",
        {type => "text/javascript", charset => "utf-8"}
    );

    my ($date_start, $date_end);
    if ($in->{date_start} && $in->{date_end}) {
        $date_start = $in->{date_start};
        $date_end   = $in->{date_end};
    } else {
        my $t = new ExSite::Time;
        $date_end = $t->write("sql_date");
        $t->add(-1, "years");
        $date_start = $t->write("sql_date");
    }
    my $f = new ExSite::FormBuilder(method => "get");
    $f->input(
        name  => "section_id",
        type  => "hidden",
        value => $this->get_section_id
    );
    $f->input(name => "pro", type => "hidden", value => $this->{input}{pro});
    $f->input(name => "rev", type => "hidden", value => $this->{input}{rev});
    $f->input(
        name   => "date_start",
        prompt => "Start Date",
        value  => $date_start,
        size   => 12,
        class  => "date-pick"
    );
    $f->input(
        name   => "date_end",
        prompt => "End Date",
        value  => $date_end,
        size   => 12,
        class  => "date-pick"
    );
    $f->set("buttons", $ml->input(undef, {type => "submit", value => "Go"}));
    $f->template("[[date_start:input]] TO [[date_end:input]]");
    $out .= $ml->p($f->make());
    return $out;
}

sub report_revenue {
    my $this = shift;
    my $report_type = shift || $this->{input}{rev} || "by_type";
    my ($itab, $pane, $report);
    if ($report_type eq "by_purchase") {
        $pane = $this->report_revenue_by_purchase();
        $itab = 0;
    } elsif ($report_type eq "by_type") {
        $pane = $this->report_revenue_by_type();
        $itab = 1;
    }

    my $out;
    $out .= $this->date_range_inputs();
    $out .= &ExSite::HTML::TabBox(
        tabs => [
            {
                label => "By purchase",
                url   => $this->link(rev => "by_purchase")
            },
            {
                label => "By member type",
                url   => $this->link(rev => "by_type")
            }
        ],
        thistab => $itab,
        pane    => $pane,
    );
    return $out;
}

sub report_revenue_by_purchase {
    my $this = shift;
    my $out;
    my $db = $share{DB};
    my $r  = new ExSite::ReportBuilder(
        title => "Revenues by purchase",
        width => "100%"
    );
    $r->nodata("No membership dues payments received.");
    my $headers = [
        "Date",   "Member ID", "Fee",    "First Name", "Last Name",  "Type",
        "E-mail", "Account",   "Expiry", "Status",     "Receivable", "Paid",
    ];
    my @report_meta;
    if (my $report_meta = &preference("Membership.roster.report_meta")) {

        if (ref($report_meta) eq "ARRAY") {
            @report_meta = @$report_meta;
        } else {
            @report_meta = split(/,/, $report_meta);
        }
    }
    if ($this->{input}{fmt}) {
        splice @$headers, 6, 0, "Address", "City", "Prov/State", "Postal/Zip Code", "Country", "Phone",
          "Date Paid/Fulfilled";
        if (scalar @report_meta) {
            foreach my $meta (@report_meta) {
                push(@$headers, $this->member->meta->get_map_info($meta, "label"));
            }
        }
    }
    $r->headers($headers);

    my (%inv, $total_r, $total_p, $total_o);
    my @row;
    my @dues;
    my $t = new ExSite::Time($this->{input}{date_end}, "sql_date");
    $t->add(1, "days");
    my $date_end = $t->write("sql_date");
    $t->add(-1, "years");
    my $date_start = $this->{input}{date_start} || $t->write("sql_date");
    if ($date_start && $date_end) {
        @dues = $db->get_query("dues revenues by date", $this->get_section_id, $date_start, $date_end);
    } else {
        @dues = $db->get_query("dues revenues", $this->get_section_id);
    }
    if ($share{DB}{map}->get_column_attr("member", "organization", "display") ne "no") {
        splice @$headers, 6, 0, "Organization";
    }
    my $payments = $share{DB}->fetch_all("payment");
    my %payment_by_r = &keywise("receivable_id", $payments);
    foreach my $regrev (@dues) {
        my $inv = new Modules::Finance::Receivable(id => $regrev->{receivable_id}, data => $regrev);
        my $acct = $inv->account();
        &ExSite::Module::read_conf("MembershipReports");
        my $paid = ($payment_by_r{$regrev->{receivable_id}} || $inv->is_paid) ? $regrev->{cost} : "";

        #       my $regsubtotal = $inv->subtotal(&preference("Membership.membership.acctcode"));
        my $type = $regrev->{type};
        $type =~ s/member\///;
        my $fee = $regrev->{item};
        $fee .= " - $1" if ($regrev->{note} =~ /^((Re)?new)/i);
        my $data = [
            $regrev->{date},    # receivable date
            $regrev->{member_id},
            $fee,
            $regrev->{first_name},
            $regrev->{last_name},
            $type
        ];
        if ($share{DB}{map}->get_column_attr("member", "organization", "display") ne "no") {
            push(@$data, $regrev->{organization});
        }
        if ($this->{input}{fmt}) {
            my $contact_precedence = &preference("MembershipReports.pref_contact")
              || &preference("Membership.contact_form.type");
            my $contact = $acct->get_contact($contact_precedence);
            my $cdata = $contact ? $contact->get() : {};
            push(@$data,
                $cdata->{address}, $cdata->{city},   $cdata->{provstate}, $cdata->{pcode},
                $cdata->{country}, $cdata->{phone1}, $regrev->{fulfilled_on});
        }
        push(
            @$data,
            $regrev->{email},
            $ml->a(
                $regrev->{account_id},
                {
                    href =>
"javascript:popup_large('$config{server}{CGIpath}/$config{prog}{ctrlpanel}/Pay?section_id=$this->{input}{section_id}&amp;acct="
                      . $inv->getdata("account_id")
                      . "&amp;inv="
                      . $inv->id . "')"
                }
            ),
            $regrev->{expirydate},
            $regrev->{status},
            $ml->div($regrev->{cost}, {class => "num"}),
            $ml->div($paid,           {class => "num"})
        );
        if ($this->{input}{fmt} && scalar @report_meta) {
            my $member = new Modules::Membership::Member(id => $regrev->{member_id});
            if ($member) {
                foreach my $meta (@report_meta) {
                    push(@$data, $member->meta_get($meta));
                }
            }
        }
        push @row, $data;
        $inv{$regrev->{receivable_id}} = 1;

        # totals
        $total_r += $regrev->{cost};
        $total_p += $paid;
    }

    # add rows to report, in time order
    foreach my $row (sort { $a->[0] cmp $b->[0] } @row) {
        $r->push($row);
    }

    # add totals to report
    if (!$this->{input}{fmt}) {
        my $empty_columns = 9;
        $empty_columns++
          if ($share{DB}{map}->get_column_attr("member", "organization", "display") ne "no");
        $empty_columns += 6 if ($this->{input}{fmt});
        my @undef = (("") x $empty_columns);
        $r->push(
            @undef,
            $ml->span("TOTAL", {class => "total"}),
            $ml->div(sprintf("%.02f", $total_r), {class => "num total"}),
            $ml->div(sprintf("%.02f", $total_p), {class => "num total"})
        );
    }

    $r->foot($this->sales_footer($r));
    $out .= $r->make;
    return $out;
}

sub report_revenue_by_type {
    my $this = shift;
    my $out;
    my $db = $share{DB};
    my $r  = new ExSite::ReportBuilder(
        title => "Revenues by member type",
        width => "100%"
    );
    $r->nodata("No membership dues payments received.");
    $r->headers(["Type", "Applications", "Renewals", "Receivable", "Paid"]);

    my (%inv, $total_r, $total_p, $total_app, $total_renew, $total_o);
    my @row;
    my @dues;
    my $t = new ExSite::Time($this->{input}{date_end}, "sql_date");
    $t->add(1, "days");
    my $date_end = $t->write("sql_date");
    $t->add(-1, "years");
    my $date_start = $this->{input}{date_start} || $t->write("sql_date");
    if ($date_start && $date_end) {
        @dues = $db->get_query("dues revenues by date", $this->get_section_id, $date_start, $date_end);
    } else {
        @dues = $db->get_query("dues revenues", $this->get_section_id);
    }
    my %index;
    my @types_allowed = sort $this->membership_types();
    foreach my $regrev (@dues) {
        my $inv         = new Modules::Finance::Receivable(id => $regrev->{receivable_id});
        my $regsubtotal = $inv->subtotal(&preference("Membership.membership.acctcode"));
        my $paid        = $inv->is_paid ? $regrev->{cost} : "";
        my $app         = ($regrev->{note} =~ /^New:(.+)/) ? $regrev->{cost} : 0;
        my $renew       = ($regrev->{note} =~ /^Renew:(.+)/) ? $regrev->{cost} : 0;
        my $type;
        if ($regrev->{note} =~ /^(New|Renew):(.+)/) {
            $type = $2;
        } else {
            $type = $regrev->{type};
            $type =~ s/^member\///;
        }

        #       my $other = $inv{$regrev->{receivable_id}} ? "-" : $inv->total - $regsubtotal;
        if (!grep(/$type/, @types_allowed)) {
            $type = "unknown";
        }
        $index{$type}{app}        += $app;
        $index{$type}{renew}      += $renew;
        $index{$type}{receivable} += $regrev->{cost};
        $index{$type}{paid}       += $paid;

        # totals
        $total_app   += $app;
        $total_renew += $renew;
        $total_r     += $regrev->{cost};
        $total_p     += $paid;
    }

    push(@types_allowed, "Other") if ($index{Other});
    foreach my $type (@types_allowed) {
        push @row,
          [
            $ml->div($type),
            $ml->div(sprintf("%.02f", $index{$type}{app}),        {class => "num"}),
            $ml->div(sprintf("%.02f", $index{$type}{renew}),      {class => "num"}),
            $ml->div(sprintf("%.02f", $index{$type}{receivable}), {class => "num"}),
            $ml->div(sprintf("%.02f", $index{$type}{paid}),       {class => "num"}),
          ];
    }

    # add rows to report, in time order
    foreach my $row (sort { $a->[0] cmp $b->[0] } @row) {
        $r->push($row);
    }

    # add totals to report1
    $r->push(
        $ml->span("TOTAL", {class => "total"}),
        $ml->div(sprintf("%.02f", $total_app),   {class => "num total"}),
        $ml->div(sprintf("%.02f", $total_renew), {class => "num total"}),
        $ml->div(sprintf("%.02f", $total_r),     {class => "num total"}),
        $ml->div(sprintf("%.02f", $total_p),     {class => "num total"}),
    );

    $r->foot($this->sales_footer($r));
    $out .= $r->make;
    return $out;
}

sub bulk_status_update {
    my ($this, $section_id) = @_;
    $section_id = $section_id || $this->get_section_id();
    $this->{section} = $share{Section} =
      new ExSite::Section(id => $section_id);
    my $out;
    $out .= $this->expire_members($section_id);
    $out .= $this->archive_members($section_id);
    $out .= $this->trash_members($section_id);
    return $out;
}

sub expire_members {
    my ($this, $section_id) = @_;
    my $stat = $this->run_handler("Membership_expire_members", $section_id);
    return $stat if ($stat);
    my $n = 0;

    # expire active members
    my $members = $share{DB}->get_query("members to expire", $section_id);
    foreach my $m (@$members) {
        my $member = new Modules::Membership::Member(data => $m);
        if (   $member->is_active()
            && $member->set_status_expired("automatic expiry of active membership"))
        {
            $n++;
        } elsif ($member->is_pending()
            && $member->days_to_expiry() < -60
            && $member->set_status_expired("automatic expiry of pending membership"))
        {
            $n++;
        }
    }
    return "expired $n members\n";
}

sub archive_members {
    my ($this, $section_id) = @_;
    my $stat = $this->run_handler("Membership_archive_members", $section_id);
    return $stat if ($stat);
    my $n = 0;
    my $t = new ExSite::Time;

    # grace period is number of days a member has to pay for an expired membership before
    # becoming archived
    my $days = &preference("Membership.late_grace_period");
    $t->add(-$days, "days");
    my $date = $t->write("sql_date");
    my $members = $share{DB}->get_query("members to archive", $section_id, $date);
    foreach my $m (@$members) {
        my $member = new Modules::Membership::Member(data => $m);
        if (   $member->is_expired()
            && $member->status_archive("automatic archival of expired membership"))
        {
            $n++;
        }
    }
    return "archived $n members\n";
}

sub trash_members {
    my ($this, $section_id) = @_;
    my $stat = $this->run_handler("Membership_trash_members", $section_id);
    return $stat if ($stat);
    my $out;
    my $n = 0;

    # trash abandoned applications requiring payment without active invoice
    my $members = $share{DB}->get_query("members to trash", $section_id);
    my $t = scalar @$members;
    foreach my $m (@$members) {
        my $member = new Modules::Membership::Member(data => $m);
        my $fee = $this->membership_fee(
            member => $member,
            type   => $member->member_type
        ) || 0;
        my $account  = $member->account;
        my %children = $account->get_children();
        my @recv =
          $children{receivable} ? $children{receivable}->getlist() : ();
        my $receivable_count = 0;
        if (@recv > 0) {
            foreach my $r (@recv) {
                my $receivable = new Modules::Finance::Receivable(data => $r);
                if ($receivable->is_active()) {
                    $receivable_count++;
                }
            }
        }
        $out .= &substitute(
            "member [[member_id]] has [[receivable_count]] unpaid invoices for \$[[fee]]\n\n",
            {
                membertype       => $member->member_type(),
                member_id        => $member->id,
                fee              => $fee,
                receivable_count => $receivable_count
            }
        );
        if ($receivable_count == 0) {
            $n++;
            $member->delete;
        }
    }
    $out .= "trashed $n members out of $t\n";
    return $out;
}

# todo() allows this plug-in to advise the administrator of work that needs
#        attention.

sub todo {
    my $this = shift;

    # the todo list is analogous to an RSS feed of tasks
    my @todo;

    # determine your list of tasks, and add them to the list, eg.
    #push @todo, { link=> $url, title=> $title, description=> $description },

    #### notify of pending memberships

    return @todo;
}

sub list_new_member {
    my ($this)  = @_;
    my $sort    = $this->sort_field();
    my $members = $this->fetch_match_member(
        member => {visibility => "visible", type => "member/%"},
        sort   => "ctime desc",
        limit  => 100
    );
    my $out;
    my $count;
    foreach my $m (@$members) {
        my $type = $m->{type};
        $type =~ s/member\///;
        my $p = new Modules::Membership::Member(data => $m);
        my $ctime = new ExSite::Time($p->getdata("ctime"), "sql_date");
        my $t = new ExSite::Time;
        next if ($ctime->diff($t) < 86400);

        # share data is intialized by Modules::Membership::Member
        my $tmp =
          $share{Membership}{Config}->{config}{mini_profile}{template}{$type};
        $share{Membership}{Config}->{config}{mini_profile}{template}{$type} =
          "list_new_member";
        $out .= $ml->div($p->show_mini_profile($this->link(pro => "member", char => undef, uid => $p->uid), %$m),
            {class => "MembershipMiniProfile"});
        $share{Membership}{Config}->{config}{mini_profile}{template}{$type} =
          $tmp;
        $count++;
        last
          if ($count >= &preference("Membership.directory.new_member_limit"));
    }
    return $out;
}

# type = member type
sub directory_menu {
    my ($this, %opt) = @_;

    my $stat = $this->run_handler("Membership_directory_menu", \%opt);
    return $stat if (defined $stat);

    my $template;
    my $menu;
    my $pos = 1;
    my $ml  = &get_obj("ML");
    my $out = $ml->a(undef, {name => "top"});
    my $type = $opt{type};
    my @types = split(/,/,$opt{type});
    $type = $types[0];
    $type =~ s/^member\///;
    while (my %menuitem = &DecodeString(Modules::Membership::Config::confrule("directory.menu.$pos", "membertype", $type))) {
        last if ($menuitem{label} eq "undef");
        last if ($opt{search_only});
        my $pro = $menuitem{pro} || "dir";
        my $uri = new ExSite::URI;
        $uri->setup($share{Page}->get_url_dynamic);
        $uri->service_page("Membership");
        $uri->query(pro => $pro, attr => $menuitem{attr}, type => $opt{type});
        my $url = $uri->write();
        my $attr = {href => $url};
        $attr->{class} = "active"
          if ( $share{Membership}{cmd} eq $pro
            && $this->{input}{attr} eq $menuitem{attr});
        $menu .= $ml->li($ml->a($msg{$menuitem{label}}, $attr)) . "\n";
        $pos++;
    }
    my $title = Modules::Membership::Config::confrule("directory.title", "membertype", $type);
    $title = $ml->h1($msg{$title}) if ($title);

    # don't display menu with 1 item
    return $title if ($pos == 1);

    my $menu =
      $ml->ul($menu, {class => "MembershipDirectoryMenu"}) . $ml->hr();
    my $template = $this->get_template("membership_directory_menu") || "[[title]] [[menu]]";
    $out .= &substitute($template, {title => $title, menu => $menu});
    return $out;
}

sub member_directory_module {
    my ($this) = @_;
    my $page = $share{Page};
    if ($share{DB}{map}->is_mapped("member_category")) {
        return $page->get_dynamic_content("MemberDirectory");
    }
    return;
}

sub keyword_search {
    my ($this) = @_;
    my $out;
    my $term;
    my $search_meta = &preference("Membership.keyword_search.meta")
      || &preference("Membership.keyword_search.member_fields");
    if (!$search_meta) {
        return $this->error($msg{"Preferences for this feature have not been set."});
    }
    if ($term = $this->{input}{searchterm}) {
        $out .= $ml->h3($msg{"Search Results"});
        if (length($term) < 4) {
            $out .= $ml->p("Sorry, your search term must be at least 4 characters.");

        } else {

            # array of member attribute names to search
            my @search_meta = map { "'" . $_ . "'" } @$search_meta;

            # comma seperated list of fields in member table to search
            my $search_fields = &preference("Membership.keyword_search.member_fields");
            if (ref $search_fields eq "ARRAY") {
                $search_fields = join(',', @$search_fields);
            }
            $search_meta = join(',', @search_meta);

            # members by keyword search on aggregate member data
            # limit to 40 results

            # add attribute fields
            my @meta = $this->get_visible_meta;
            my (@extracol, $join);
            my $nattr = 1;
            my $join  = "member m left join member_attribute a0 on a0.member_id=m.member_id";
            foreach my $meta (@meta) {
                push @extracol, "a$nattr.value $meta";
                if ($join) {
                    $join =
"($join) left join member_attribute a$nattr on a$nattr.member_id=m.member_id and a$nattr.name='$meta'";
                }
                $nattr++;
            }
            my $select = "m.*" . "," . join(",", @extracol);

            # add contact columns
            $join .=
              " left join account a on a.member_id = m.member_id left join contact c on c.account_id = a.account_id";
            $select .=
",c.address contact_address,c.city contact_city,c.provstate contact_provstate,c.country contact_country,c.pcode contact_pcode,c.phone1 contact_phone1,c.phone2 contact_phone2,c.fax contact_fax,c.email contact_email,c.web contact_web,c.privacy contact_privacy";
            my $sort = $this->sort_field();

            # limit results to 200 for performance reasons
            my $where =
"m.visibility = 'visible' and ((a0.name in ($search_meta) and a0.value like ?) or concat_ws('; ',$search_fields) like ?)";
            $where .= " and m.type in " . $this->fetch_member_type_sql()
              if ($this->fetch_member_type_sql);
            my $contact_type = $this->contact_type();
            $where .= " and (c.type = '$contact_type' or c.type is null)"
              if ($contact_type);
            if ($this->scope() eq "local") {
                $where .= " and m.section_id = ?";
            }
            my @params = ('%' . $term . '%', '%' . $term . '%');
            if ($this->scope() eq "local") {
                push(@params, $this->get_section_id);
            }

            my @visible_status = $this->visible_status();
            @visible_status = map { "'$_'" } @visible_status;
            my $status_list = join(",", @visible_status);
            if ($status_list) {
                $where .=
" and (select status from member_status force index (member_id) where member_status.member_id=m.member_id order by member_status_id desc limit 1) in ($status_list)";
            }
            my $members =
              $share{DB}
              ->custom_query("select $select from $join where $where group by m.member_id order by $sort limit 200",
                @params);
            my $m_keyword_result = &preference("Membership.message.keyword_search_result")
              || "Looking for members that match \"[[term]]\"";
            $out .= $ml->p(&substitute($msg{$m_keyword_result}, {term => $term}));
            $out .= $this->index($members);
        }
    }
    my $term    = $this->{input}{searchterm};
    my $heading = &preference("Membership.keyword_search.heading");
    $out .= $ml->h3($msg{$heading}) if ($heading);
    my $fb = new ExSite::FormBuilder(
        method => "post",
        action => $this->link(pro => "keyword_search")
    );
    $fb->input(name => "searchterm", prompt => " ",  type  => "text");
    $fb->input(type => "hidden",     name   => "md", value => "search");
    $fb->input(type => "submit",     value  => "Search");

    my $loc = $share{Page} || $share{Section};
    if ($loc) {
        my $ctemplate = $loc->find("member_keyword_template");
        my $template  = $ctemplate->get_html();
        if ($template && !$term) {
            $fb->template($template);
        } else {
            $fb->template("", "[[input]]", "");
        }
    }
    $fb->{buttons} = $ml->input(undef, {type => "submit", value => $msg{"Search"}, class => "formButton"});
    $out .= $ml->div($fb->make());
    return $out;
}

sub filter_members_by_category {
    my ($this, $members, $cid) = @_;
    my $index      = $this->members_in_category;
    my $tree       = $this->member_category_tree;
    my @categories = $tree->collapse($cid);
    push(@categories, $tree->getnode_data($cid));
    my %result;
    my @list_of_members;
    foreach my $cat (@categories) {
        push(@list_of_members, keys %{$index->{$cat->{member_category_id}}});
    }
    foreach my $m (@$members) {
        $result{$m->{member_id}} = $m
          if (grep(/^$m->{member_id}$/, @list_of_members));
    }
    my @result = values %result;
    return \@result;
}

# members indexed by category

sub members_in_category {
    my ($this) = @_;
    my $out;
    my @categories = $share{DB}->get_query("all member categories");
    my $index;
    foreach my $cat (@categories) {
        $index->{$cat->{member_category_id}}{$cat->{member_id}} = 1;
    }
    return $index;
}

sub member_category_tree {
    my ($this) = @_;
    return $this->{member_category_tree} if ($this->{member_category_tree});
    my @cat = $share{DB}->fetch_match("member_category", {section_id => $this->get_section_id});
    push(@cat, $share{DB}->fetch_match("member_category", {section_id => 0}));
    return $this->{member_category_tree} = new ExSite::Tree("member_category_id", "parent_id", @cat);
}

sub category_input {
    my ($this, $tree) = @_;
    $tree = $this->member_category_tree() if (!$tree);
    my @options;
    push @options, ["", "== select =="];
    my @topcat = $tree->get_topnodes_data();
    foreach my $cat (@topcat) {
        &add_member_category($this, 0, $tree, $cat, \@options);
    }
    my $f = new ExSite::FormBuilder();
    my $n = $this->{input}{n};
    return $f->input(
        name    => "member_category",
        options => \@options,
        type    => "select",
        value   => $this->{input}{catid}
    );
}

sub add_member_category {
    my ($this, $level, $tree, $cat, $options) = @_;
    my $label = ". . " x $level;
    $label .= $cat->{category};
    push @$options, [$cat->{"member_category_id"}, $label];
    my @subcat = $tree->get_child_data($cat->{member_category_id});
    $level++;
    return if ($level > 2);
    foreach my $cat (@subcat) {
        &add_member_category($this, $level, $tree, $cat, $options);
    }
}

sub advanced_search {
    my ($this, %opt) = @_;
    my $out;
    my $cmd = "index";
    my $db  = $share{DB};
    $db->get_data;
    my $data = $db->{form};
    my %parameters_output;
    if (keys %$data >= 1) {
        my %member_match;
        foreach my $col (keys %{$data->{member}{_}}) {
            if ($data->{member}{_}{$col}) {
                $member_match{$col} = $data->{member}{_}{$col};
                my $label = $db->{map}->get_column_attr("member", $col, "label");
                $parameters_output{$label} = $data->{member}{_}{$col};
            }
        }
        foreach my $n (keys %{$data->{member_attribute}}) {
            my $name  = $data->{member_attribute}{$n}{name};
            my $value = $data->{member_attribute}{$n}{value};
            if ($value) {
                $member_match{$name} = $value;
                my $label = $this->member->meta->get_map_info($name, "label");
                if ($label ne $value) {
                    $parameters_output{$label} = $value;
                } else {

                    # summarize output when label is same as value
                    $parameters_output{$label} = "yes";
                }
            }
        }
        %member_match = $this->wildcards_to_match(\%member_match);
        $member_match{visibility} = "visible"
          if (!$db->is_manager || !$this->{admin});
        my %contact_match;
        foreach my $col (keys %{$data->{contact}{_}}) {
            if (my $value = $data->{contact}{_}{$col}) {
                $contact_match{$col} = '%' . $value . '%';
                my $label = $db->{map}->get_column_attr("contact", $col, "label");
                $parameters_output{$label} = $value;
            }
        }
        my $members;
        if (   $share{DB}{map}->is_mapped("member_category")
            && $this->{input}{member_category})
        {
            if (keys %parameters_output) {
                $members = $this->fetch_match_member(
                    member  => \%member_match,
                    contact => \%contact_match,
                    nolimit => 1
                );
            } else {
                my @params;
                push(@params, $this->get_section_id)
                  if ($this->scope eq "local");
                $members = $share{DB}->get_query("all visible members", @params);
            }
            @$members = grep { $_->{visibility} eq "visible" } @$members;
            my $catid = $this->{input}{member_category};
            $members = $this->filter_members_by_category($members, $catid);
            my $cat = $this->member_category_tree()->getnode_data($catid);
            $parameters_output{Category} = $cat->{category};
        } else {
            $members = $this->fetch_match_member(
                member  => \%member_match,
                contact => \%contact_match,
                nolimit => 1
            );
        }
        $out .= $ml->div(join($ml->br, map { "$msg{$_}: $msg{$parameters_output{$_}}" } keys %parameters_output),
            {class => "searchterms"});
        %contact_match = $this->wildcards_to_match(\%contact_match);
        if (!$share{Page}) {
            $out .= $this->roster(
                data     => $members,
                prefetch => $share{Membership}{prefetch},
                contact  => $this->{input}{contact}
            );
        } else {

            # FIXME: pagination not implemented for advanced search
            my $per_page        = &preference("Membership.records_per_page") || 400;
            my $count           = scalar @$members;
            my $results_message = "Displaying [[count]] results...";
            $out .= $ml->p(&substitute($msg{$results_message}, {count => scalar @$members}));
            $out .= $this->index($members);
        }
    }
    my $fb = $db->form();
    $fb->init();

    my $search_keys = $this->run_handler("Membership_search_keys");
    if (!$search_keys) {
        if (   $this->{admin}
            && &preference("Membership.advanced_search.admin_columns"))
        {
            $search_keys = &preference("Membership.advanced_search.admin_columns");
        } else {
            $search_keys = &preference("Membership.advanced_search.columns");
        }
    }
    if (ref $search_keys ne "ARRAY") {
        my @keys = split(/,/, $search_keys);
        $search_keys = \@keys;
    }
    my $section_id = $this->get_section_id();
    $db->set_action("search");
    $config{form}->{switch_search_inputs} = 1;
    my $member = new Modules::Membership::Member;
    foreach my $key (@$search_keys) {
        if (   $db->{map}->is_mapped("member", $key)
            && $db->{map}->get_column_attr("member", $key, "read") <= $db->authorize)
        {
            $db->input_column(
                table    => "member",
                column   => $key,
                required => 0,
                value    => $this->{input}{$key}
            );
        } elsif ($member->meta()->is_allowed($key)) {
            $member->meta()->input(undef, $key, $this->{input}{"meta_$key"});
        } elsif ($db->{map}->is_mapped("contact", $key)) {
            $db->input_column(
                table    => "contact",
                column   => $key,
                required => 0,
                value    => $this->{input}{"contact_$key"}
            );
        }
    }

    my $loc = $share{Page} || $share{Section};
    if ($loc) {
        my $ctemplate = $loc->find("member_search_template");
        my $template  = $ctemplate->get_html();
        $fb->template($template) if ($template);
    }

    my $action = $this->link(pro => "advanced_search");
    $fb->input(
        name   => "contact",
        prompt => "include Contact information",
        type   => "checkbox"
    ) if ($this->{admin});
    $db->input_html(
        name  => "pro",
        type  => "hidden",
        value => "advanced_search"
    );
    my $page = &ExSite::Module::service_page("Membership", $this->get_section_id);
    $db->input_html(name => "_id", type => "hidden", value => $page->id)
      if ($page);
    $db->input_html(
        name  => "section_id",
        type  => "hidden",
        value => $this->{section}->id()
    );
    $db->input_html(
        name  => "search_only",
        type  => "hidden",
        value => $opt{search_only}
    );

    if ($opt{contact_type}) {
        $db->input_html(
            name  => "contact##type",
            type  => "hidden",
            value => $opt{contact_type}
        );
    }
    if ($share{DB}{map}->is_mapped("member_category")) {
        my $tree;
        my $value = $this->{input}{catid};
        if ($value) {
            $tree = $this->member_category_tree;
            $tree = $tree->subtree($this->{input}{catid});
        }
        $fb->input(
            type     => "formatted",
            name     => "member_category",
            prompt   => "Category",
            input    => $this->category_input($tree),
            required => $value ? 1 : 0
        );
    }

    $fb->action($action);
    if (!$this->{admin}) {
        $fb->method("post");
    } else {
        $fb->method("get");
    }
    my $heading = &preference("Membership.advanced_search.heading") || "Advanced Search";
    $out .= $ml->h3($msg{$heading}, {id => "advsearch"}) if ($heading);
    $out .= $fb->make();
    $fb->{template} = undef;
    return $out;
}

# index of members
# if attr given, display listing of possible values
# if attr + value given, display listing of members with specific attribute value
# otherwise display alphalist
# type = single or comma delimited list of member types
sub directory {
    my ($this, $attr, $type) = @_;
    if ($this->{input}{uid} && $this->member->allow()) {
        return $this->member->show();
    }
    my $out;
    my $members;
    my $member = new Modules::Membership::Member;
    if (my $attr = $this->{input}{attr} || $attr) {
        $this->{attr} = $attr;
        if (my $value = $this->{input}{value}) {
            my $member_match = {$attr => $value, visibility => "visible"};
            my $total = $this->fetch_match_member(
                member  => $member_match,
                nolimit => 1,
                type    => $type
            );
            $members = $this->fetch_match_member(member => $member_match, type => $type);
            my $count = scalar @$total;
            my $label = $member->meta->get_map_info($attr, "label") || $attr;
            $out .= $ml->h2(
                $ml->span(ucfirst($msg{$label}) . ": ", {id => "label"})
                  . $ml->span($msg{$value} . " " . $msg{"directory"}, {id => "value"}) . " "
                  . $ml->span(" ($count " . $msg{members} . ")",      {id => "count"}),
                {id => "dheader"}
            );
            $out .= $this->index($members, $count);
        } else {
            $out .= $this->browse_attr($attr, $type);
        }
    } else {
        $out .= $this->alphalist(undef, $type);
    }
    return $out;
}

sub real_members {
    my ($this, $members) = @_;
    my @real_members;
    foreach my $m (@$members) {
        my $p = new Modules::Membership::Member(data => $m);
        next if (!$p->is_member());
        push(@real_members, $p);
    }
    return wantarray ? @real_members : \@real_members;
}

sub alphalist {
    my ($this, $members, $type) = @_;
    my $char         = $this->{input}{char} || "A";
    my $sort         = $this->sort_field($type);
    my $member_match = {visibility => "visible", section_id => $this->get_section_id};
    my $dbsize       = $share{DB}->count("member", $member_match);
    my $charsize;
    if (!$members) {
        $member_match->{parent_id} = 0
          if (!&preference("Membership.directory.show_secondary"));
        if ($dbsize > 300) {
            if ($char ne "Other") {
                $member_match->{$sort} = $char . '%';
            } else {
                $member_match->{$sort} = $char;
            }
        }
        $charsize = $this->fetch_match_member(
            member  => $member_match,
            nolimit => 1,
            type    => $type,
            sort    => $sort
        );
        $members = $this->fetch_match_member(member => $member_match, type => $type, sort => $sort);
    }
    my %alphabin;
    my $out;
    foreach my $m (@$members) {
        my $p     = new Modules::Membership::Member(data => $m);
        my $value = $p->getdata($sort);
        my $c     = uc(substr($value, 0, 1));
        $c = $char if ($char eq "Other");
        if (!$alphabin{$c}) {
            $alphabin{$c} = [$m];
        } else {
            push @{$alphabin{$c}}, $m;
        }
    }
    my $bin = $alphabin{a};
    my $out;
    return $this->index($members, scalar @$members) if ($dbsize < 300);

    # alphabetical index
    my $alpha;
    foreach my $c (('A' .. 'Z', "Other")) {
        if ($dbsize > 300 || $alphabin{$c}) {
            my $link_params = {href => $this->link(type => $type, pro => "dir", char => $c, page => undef)};
            $link_params->{class} = "selected_char" if ($c eq $char);

            $alpha .= $ml->a($c eq "Other" ? $msg{$c} : $c, $link_params);
        } else {
            $alpha .= $ml->span($c, {class => "MembershipNoMembers"});
        }
    }
    $out .= $ml->div($alpha, {class => "MembershipDirectoryAlpha"});
    my $count      = eval { scalar @{$alphabin{$char}} } || 0;
    my $whatami    = $this->whatami(1);
    my $sort_label = $this->member_column_label($sort);
    $out .= $ml->h3(
        &substitute(
            $msg{"Displaying [[count]] $whatami whose $sort_label starts with '[[char]]'"},
            {char => $char, count => $count}
        )
    );
    $out .= $this->index($alphabin{$char}, scalar @$charsize);
    return $out;
}

sub browse_attr {
    my ($this, $attr, $type) = @_;
    $attr = $attr || $this->{input}{attr};
    if ($attr eq "organization") {
        return $this->browse_organization($type);
    }
    my $ml       = &get_obj("ML");
    my $member   = new Modules::Membership::Member();
    my $datatype = $member->meta()->get_datatype($attr);
    my $regexp   = $share{DB}->{map}->get_datatype_attr($datatype, "regexp");
    my @set      = split /\|/, $regexp;
    my $out;
    my $label = $member->meta()->get_map_info($attr, "label");
    $out .= $ml->h3(&substitute($msg{"Browsing by '[[label]]'"}, {label => $msg{$label}}), {id => "bheader"});
    my @params = ($attr);
    push(@params, $this->get_section_id) if ($this->scope eq "local");
    my $counts = $share{DB}->get_query("count of visible members by meta value", @params);
    my $value_to_count;

    foreach my $count (@$counts) {
        my @values = split('; ', $count->{value});
        map { $value_to_count->{$_} += $count->{count} } @values;
    }
    my $set_count = scalar @set;
    my $num_cols  = 3;
    my ($i, $rows);
    my @cols;
    my $col_width = sprintf("%.2f", 100 / $num_cols);
    if (scalar @set != keys %$value_to_count && $datatype =~ /olist/) {
        push(@set, "Other");
    }
    foreach my $value (keys %$value_to_count) {
        if (!grep(/^$value$/, @set)) {
            $value_to_count->{"Other"} += $value_to_count->{$value};
            delete $value_to_count->{$value};
        }
    }
    foreach my $item (sort @set) {
        $i++;
        my $count = $value_to_count->{$item} || 0;
        my $link;
        if ($count) {
            $link = $ml->a(
                $msg{$item},
                {
                    href => $this->link(
                        pro   => "dir",
                        attr  => $attr,
                        value => $item,
                        type  => $type
                    )
                }
            );
        } else {
            $link = $item;
        }
        if (&preference("Membership.directory.show_count")) {
            $link .= " " . $ml->span("($count)", {class => "attr_count"});
        }
        push @cols, $ml->td($link, {width => $col_width . '%'});
        if ($i % $num_cols == 0 || $i == scalar @set) {
            $rows .= $ml->tr(join(/\n/, @cols));
            @cols = ();
        }
    }
    $out .= $ml->table($rows, {width => "100%", class => "MembershipDirectoryTable"});
    return $out;
}

sub browse_organization {
    my ($this, $type) = @_;
    my $ml = &get_obj("ML");
    $share{Membership}{directory_org} = 1;
    my $member = new Modules::Membership::Member();
    my $out;
    my $label =
      $share{DB}{map}->get_column_attr("member", "organization", "label");
    $out .= $this->run_handler("Membership_browse_organization_header")
      || $ml->h3(&substitute($msg{"Browsing by [[label]]"}, {label => $msg{$label}}), {id => "bheader"});
    my @mcol = map { "m." . $_ } $this->get_member_columns();
    my $mcol = join ",", @mcol;
    my $member_match = {visibility => "visible", parent_id => 0};
    $member_match->{type} = $type if ($type);
    my $default_match = $this->run_handler("Membership_default_match")
      || &preference("Membership.directory.default_member_match");

    if ($default_match && keys %$default_match) {
        my %match = (%$default_match, %$member_match);
        $member_match = \%match;
    }
    my $char  = $this->{input}{char} || "A";
    my $sort  = "organization";
    my $count = $share{DB}->count("member", $member_match);

    if ($count > 300) {
        if ($char ne $msg{"Other"}) {
            $member_match->{$sort} = $char . '%';
        } else {
            $member_match->{$sort} = $char;
        }
    }
    my $full = $this->fetch_match_member(member => $member_match, sort => $sort, nolimit => 1);
    my $members = $this->fetch_match_member(member => $member_match, sort => $sort);

    # filter members that are not classified as organizations in this context
    my @filtered;
    foreach my $m (@$members) {
        if ($this->is_organization($m, "directory")) {
            push(@filtered, $m);
        }
    }

    # alphabetical index
    if ($count > 300) {
        my $alpha;
        foreach my $c (('A' .. 'Z', $msg{'Other'})) {
            $alpha .= $ml->a(
                $c,
                {
                    href => $this->link(
                        pro  => "dir",
                        char => $c,
                        attr => "organization",
                        page => undef,
                        type => $type
                    )
                }
            );
        }
        $out .= $ml->div($alpha, {class => "MembershipDirectoryAlpha"});
    }
    $out .= $this->index(\@filtered, scalar @$full);
    return $out;
}

sub index {
    my ($this, $members, $count) = @_;
    my $stat = $this->run_handler("membership_index", $members);
    return $stat if ($stat);
    my $out;
    my $nmem = 0;
    $ml = &get_obj("ML");
    $out .= $this->paginate($count) if ($count);
    foreach my $member (@$members) {
        my $p = new Modules::Membership::Member(data => $member);
        my %profile = %$member;
        if ($share{Membership}{prefetch} != 1) {
            my $metadata = $p->meta_get();
            map { $member->{$_} = $metadata->{$_} } keys %$metadata;
        }
        if (&preference("Membership.directory.use_show_data")) {
            foreach my $key (keys %$member) {
                my $datatype = $this->get_datatype($key);
                if ($datatype) {
                    $profile{$key} =
                      $share{DB}->show_data_nomap($datatype, $member->{$key});
                }
            }
        }
        $this->{Member} = $p;
        $nmem++;
        $out .= $ml->div($p->show_mini_profile($this->link(pro => undef, char => undef, _member => $p), %profile),
            {class => "MembershipMiniProfile"});
        my $per_page = &preference("Membership.records_per_page") || 400;
        last if ($count && $nmem > $per_page);
    }
    if ($nmem > 25) {
        $out .= $ml->p($ml->a("&uarr; " . $msg{"Top"}, {href => "#top"}), {class => "MembershipTopLink"});
    }
    if (!$nmem) {
        my $message = $msg{&preference("Membership.message.no_members")}
          || $msg{"No members found."};
        $out .= $ml->p($message);
    }
    if ($count && &preference("Membership.show_paginate.bottom")) {
        $out .= $ml->div($this->paginate($count), {class => "BottomPageIndex"});
    }
    return $out;
}

sub my_status {
    my $this = shift;
    return $this->member ? $this->member->status() : undef;
}

sub my_type {
    my $this = shift;
    return $this->member ? $this->member->getdata("type") : undef;
}

sub my_name {
    my $this = shift;
    return $this->member ? $this->member->name() : "unknown user";
}

sub is_member {
    my $this = shift;
    return $this->member ? $this->member->is_member : undef;
}

sub show_my_summary {
    my ($this) = @_;
    my $member = $this->member;
    my $type   = $member->member_type();
    my $out;
    if ($member->parent) {
        my $parent = $member->parent->name;
        $out .= $ml->p(
            &substitute(
                $msg{"You are a secondary contact for [[name]] who has a [[type]] membership."},
                {name => $parent, type => $type}
            )
        );
    } else {
        $out .= $ml->p(&substitute($msg{"You are a primary contact with a $type membership."}, {type => $type}));
    }
    if ($member->is_member) {
        my $status     = $member->status || "undefined";
        my $expirydate = $member->expirydate;
        my $t          = new ExSite::Time($expirydate, "sql_date");
        my $e          = $t->write("date_long");
        $out .= $ml->p(
            &substitute(
                $msg{"Your current status is [[status]] and your expiry date on file is $e."},
                {status => $status}
            )
        );
    }
    return $out;
}

sub show_my_card {
    my $this = shift;
    my $pane;
    if ($this->is_member() && $this->member->is_valid_member()) {
        if ($this->member->is_me()) {
            my $t    = new ExSite::Time;
            my $data = $this->member->get_profile_data();
            $data->{join_date} = $this->member->showdata("ctime");
            $data->{status}    = ucfirst($this->member->status());
            $data->{year}      = new ExSite::Time->write("year");
            my $template = $this->get_template("membership_card")
              || $ml->div(
                $ml->h3("[[first_name]] [[last_name]]", {id => "name"})
                  . $ml->div("[[organization]]",          {id => "organization"})
                  . $ml->div("[[status]] Member",         {id => "status"})
                  . $ml->div("Good Thru: [[expirydate]]", {id => "expirydate"})
                  . $ml->a("Print", {href => "#", id => "print"}),
                {id => "MembershipCard"}
              );
            $pane .= &substitute($template, $data);
        }
        my $out = &ExSite::HTML::Overlay(
            pane   => $pane,
            label  => $msg{"My Membership Card"},
            width  => 500,
            height => 200
        );
        $out .= $ml->script(
            "\$(function () {
\$('#print').click(function () {
	w = window.open();
	\$('.popup_close').hide();
	\$('#print').hide();
	w.document.write(\$('.overlay').html());
	w.print();
	\$('.popup_close').show();
	\$('#print').show();
})
});"
        );
        return $out;
    }
    return;
}

sub show_my_status {
    my $this = shift;
    my $out;

    # user did not set own password / force set
    if (   $this->member->passtype()
        && !$share{DB}->is_manager()
        && !$session{Membership}{skip_password_change}
        && !$session{key_contact_uid})
    {
        my $url = $this->link(pro => "edit_password");
        my $f = new ExSite::FormBuilder(method => "POST", action => $url);
        $f->name("Membership_redirect");
        $f->buttons(submit => 0, cancel => 0, reset => 0);

        # alternative submit for clients with javascript disabled
        my $submit = $ml->noscript(
            $f->inputtag(
                type  => "submit",
                name  => "submit_action",
                value => $msg{"Setup my password"}
            )
        );
        $f->input(
            type  => "preformatted",
            name  => "noscript",
            input => $submit
        );

        # javascript to autosubmit the form
        my $autosubmit = $ml->script("document.Membership_redirect.submit()", {type => "text/javascript"});
        return $f->make() . $autosubmit;
    }

    my ($main_email, $main_name, $main_organization);
    if ($this->member->parent()) {
        $main_email = $this->member->parent->email();
        $main_name  = $this->member->parent->getdata("first_name") . " " . $this->member->parent->getdata("last_name");
        $main_organization = $this->member->parent->getdata("organization");
    }
    if ($this->is_member) {
        if ($this->member->is_me && $this->member->is_member()) {
            my $expirydate = $this->member->expirydate();

            #  use epoch date if expirydate is undefined
            if (!$expirydate || $expirydate =~ /^(0+\-0+\-0+)$/) {
                $expirydate = "2001-01-01";
            }
            my $status = $this->my_status;
            if ($status eq "active") {
                my $renewal_window = &preference("Membership.early_renewal_period") || 0;
                my $active_msg = $msg{&preference("Membership.message.good_status")};
                if ($this->member->is_renewable()) {
                    my $t = new ExSite::Time;
                    my $diff = int($t->diff($expirydate, "sql_date") / 86400);
                    if ($diff <= $renewal_window) {
                        my $message =
                            $diff < 0
                          ? $this->{config}{message}{expired}
                          : $this->{config}{message}{expiring};
                        $t->set($expirydate, "sql_date");
                        $out .= &substitute(
                            $msg{$message},
                            {
                                ndays => abs($diff),
                                date  => $expirydate,
                            }
                        ) . " ";
                        if (!&preference("Membership.disable_renewal_form")) {
                            $out .= $ml->a(
                                $msg{"Renew Now"},
                                {
                                    class => "renew button",
                                    href  => $this->link(pro => "renew")
                                }
                            );
                        }
                    } else {
                        $out .= $active_msg;
                    }
                } else {
                    $out .= $active_msg;
                    if ($this->is_upgradable()) {
                        $out .= $ml->a($msg{"Upgrade Membership"},
                            {class => "upgrade button", href => $this->link(pro => "upgrade")});
                    }
                }
                $out = $ml->div($out, {class => "MembershipStatusGood"});
            } elsif ($status eq "pending") {
                my $account = $this->member->account;
                if ($account->balance > 0.01) {
                    my $pending_msg = $msg{&preference("Membership.message.pending_payment")};
                    $out .= $msg{$pending_msg};
                    $out = $ml->div($out, {class => "MembershipStatusGood"});
                } else {
                    my $pending_msg = $msg{&preference("Membership.message.pending_status")};
                    $out .= $msg{$pending_msg};
                    $out = $ml->div($out, {class => "MembershipStatusGood"});
                }
            } elsif ($status eq "expired") {
                my $t = new ExSite::Time($expirydate, "sql_date");
                my $ago = $t->write("ago");
                $out .= &substitute(
                    $msg{$this->{config}{message}{expired}},
                    {
                        date => $t->write("date"),
                        ago  => $ago
                    }
                ) . " ";
                if ($this->member->is_renewable()) {
                    if (!&preference("Membership.disable_renewal_form")) {
                        $out .= " "
                          . $ml->a(
                            $msg{"Renew Now"},
                            {
                                class => "renew button",
                                href  => $this->link(pro => "renew")
                            }
                          );
                    }
                } elsif ($this->member->parent()
                    && $this->member->parent->exists()
                    && $this->member->parent->is_renewable()
                    && !$this->member->is_key_contact())
                {
                    # secondary members can be given permission to switch to primary account
                    $out .= $ml->p(
                        &substitute(
                            $msg{"Your Primary Member ([[name]] - [[email]]) must renew the membership."},
                            {name => $main_name, email => $main_email}
                        )
                    );
                }
                $out = $ml->div($out, {class => "MembershipStatusBad"});
            } elsif ($status eq "archived") {
                $out .= $msg{&preference("Membership.message.archived_status")};
                if ($this->is_reinstate()) {
                    $out .= $ml->p(
                        &substitute(
                            $msg{"A reinstatement fee of \$[[fee]] will be applied upon renewal."},
                            {fee => $this->reinstatement_fee()}
                        )
                    );
                }
                if ($this->member->is_renewable()) {
                    if (!&preference("Membership.disable_renewal_form")) {
                        $out .= " "
                          . $ml->a(
                            $msg{"Renew Now"},
                            {
                                class => "renew button",
                                href  => $this->link(pro => "renew")
                            }
                          );
                    }
                } elsif ($this->member->parent()
                    && $this->member->parent->exists()
                    && $this->member->parent->is_renewable()
                    && !$this->member->is_key_contact())
                {
                    $out .= $ml->p(
                        &substitute(
                            $msg{"Your Primary Member ([[name]] - [[email]]) must renew the membership."},
                            {name => $main_name, email => $main_email}
                        )
                    );
                }
                $out = $ml->div($out, {class => "MembershipStatusBad"});
            } else {
                $out .= $msg{&preference("Membership.message.incomplete_status")};
            }
        } else {
            $out .= $msg{"This is not you."};
        }
    } else {
        my $template = $this->get_template("membership_status_guest");
        if ($template) {
            $out .= $template;
        } else {
            $out .= $msg{&preference("Membership.message.no_membership")};
            if ($this->is_upgradable) {
                $out .= " "
                  . $ml->a(
                    $msg{"Become a Member"},
                    {
                        class => "renew button",
                        href  => $this->link(pro => "renew")
                    }
                  );
            }
        }
    }
    if (&preference("Membership.show_membership_card")) {
        $out .= $this->show_my_card();
    }
    if ($this->member->parent() && $this->member->is_key_contact()) {
        my $text        = $msg{&preference("Membership.message.key_contact_description")};
        my $login_label = $msg{&preference("Membership.message.key_contact_login_label")};
        $login_label = &substitute($login_label, {name => $main_name, organization => $main_organization});

        $out .= $ml->div(
                $text
              . $ml->br()
              . &ExSite::HTML::Button(
                label => $login_label,
                url   => $this->link(pro => "login", old_uid => $this->member->id(), uid => $this->member->parent->id())
              )
        );
    } elsif ($session{key_contact_uid}) {
        my $text = $msg{&preference("Membership.message.key_contact_message")};
        $out .= $ml->div(
                $text
              . $ml->br()
              . &ExSite::HTML::Button(
                label => $msg{"Switch back to your own account"},
                url   => $this->link(pro => "login", uid => $session{key_contact_uid})
              )
        );
    }
    return $ml->div($out, {class => "MembershipStatus"});
}

sub reinstatement_fee {
    my $this = shift;
    my $stat = $this->run_handler("Membership_reinstatement_fee");
    return $stat if ($stat);
    return &preference("Membership.reinstatement_fee");
}

sub is_reinstate {
    my ($this, $member) = @_;
    $member = $member || $this->member();
    my $stat = $this->run_handler("Membership_is_reinstate");
    return $stat if (defined $stat);
    my $fee = $this->reinstatement_fee();
    return if (!$fee);
    return if (!$member->is_archived);
    my $out;
    my $inv = new Modules::Finance::Receivable(id => $session{invoice});
    my $itemlist = $inv->loaditems();
    my $is_reinstated;

    if ($itemlist && $itemlist->count()) {
        while (my $item = $itemlist->next()) {
            if (   $item->getdata("objtype") eq "member"
                && $item->getdata("objid") == $member->id
                && $item->getdata("note") =~ /^Reinstate/)
            {
                $is_reinstated = 1;
            }
        }
    }
    return if ($is_reinstated);
    return 1;
}

sub reinstate {
    my ($this, $member) = @_;
    $member = $member || $this->member();
    my $fee = $this->reinstatement_fee();
    my %pay = (
        cart        => "add",
        label       => $msg{"Add reinstatement fee"},
        item        => $msg{"Membership Reinstatement"},
        description => $member->name(),
        cost        => $fee,
        acctcode_id => &preference("Membership.membership.acctcode") || 0,
        member_id   => $member->id(),
        objid       => $member->id(),
        objtype     => "member",
        note        => "Reinstate",
        silent      => 1
    );
    my $payargs = &EncodeHash(%pay);
    $session{continue_shopping_url} = -1;
    return "<!--&Pay($payargs)-->";
}

sub is_renewable {
    my ($this, $m) = @_;
    $m = $m || $this->member();
    my $stat = $this->run_handler("Membership_is_renewable", $m);
    return $stat if (defined $stat);
    if ($m && $m->is_member() && $m->member_type() ne "secondary") {
        if (   $m->is_me
            || $m->is_child
            || $m->is_parent
            || $m->is_sibling
            || $share{DB}->is_manager($m->gid()))
        {
            my $status = $m->status;
            if ($status eq "active") {
                my $renewal_window = &preference("Membership.early_renewal_period") || 0;
                my $t              = new ExSite::Time;
                my $diff           = int($t->diff($m->expirydate, "sql_date") / 86400);
                return ($diff <= $renewal_window) ? 1 : 0;
            } elsif ($status =~ /^(expired|archived)$/) {
                return 1;
            } elsif ($status) {
                return 0;
            } else {

                # no status history?
                return 1;
            }
        }
    }
    return 0;
}

# membership upgrade
sub upgrade {
    my ($this, $m) = @_;
    $m = $m || $this->member();
    my $out;
    my %fees = $this->membership_fees(member => $m, purchase_type => "Renew");
    if (my $newtype = $this->{query}{type}) {
        my $fee;
        $fee = $fees{$newtype} - $fees{$m->member_type()};
        my $days_left = $m->days_to_expiry();
        my $t         = new ExSite::Time;
        $t->add(&preference("Membership.anniversary.length"), &preference("Membership.anniversary.period"));
        my $today     = new ExSite::Time;
        my $totaldays = $today->diffdays($t);
        my $prorated  = $fee * ($days_left / $totaldays);
        my %pay       = (
            cart        => "add",
            item        => "Membership Upgrade",
            description => $m->member_type() . " -> " . $newtype,
            cost        => $prorated,
            acctcode_id => &preference("Membership.membership.acctcode") || 0,
            member_id   => $m->id(),
            objid       => $m->id(),
            objtype     => "member",
            note        => "Upgrade:" . $newtype,
        );
        my $payargs = &EncodeHash(%pay);
        $out .= "<!--&Pay($payargs)-->";
    } else {
        $out .= $ml->p(
            &substitute(
                "Your membership type is currently [[type]]. Thank you for your support!",
                {type => $m->member_type()}
            )
        );
        if ($this->is_upgradable($m)) {
            $out .= $ml->p("You may upgrade your membership by selecting your new membership type below:");
            $out .= $this->membership_menu("upgrade");
        } else {

        }
    }
    return $out;
}

# guests can become members
# members can upgrade to a 'higher' membership type
sub is_upgradable {
    my ($this, $m) = @_;
    $m = $m || $this->member();
    return 0 if (!$m);
    my $stat = $this->run_handler("Membership_is_upgradable", $m);
    return $stat if (defined $stat);
    return 0 unless (&preference("Membership.allow_upgrades"));
    my $out;
    if (!$m->is_member() && $m->member_type() =~ /^guest/) {

        if (   $m->is_me
            || $share{DB}->is_manager($m->gid()))
        {
            my $renewal_window = &preference("Membership.early_renewal_period") || 0;
            my $t              = new ExSite::Time;
            my $diff           = int($t->diff($m->expirydate, "sql_date") / 86400);
            return ($diff <= $renewal_window) ? 1 : 0;
        }
    } elsif ($m->is_active()) {
        my %fees = $this->membership_fees(member => $m, purchase_type => "Renew");
        foreach my $type (keys %fees) {
            if ($fees{$type} >= $fees{$m->member_type()}) {
                return 1;
            }
        }
    }
    return 0;
}

sub membership_types {
    my $this = shift;
    return $this->allowed_membership_types;
}

sub user_types {
    my ($this, $section_id) = @_;
    $section_id or $section_id = $this->gid;
    my @type  = qw(staff guest secondary);
    my @mtype = $this->membership_types();
    if (@mtype > 0) {
        foreach my $mtype ($this->membership_types($section_id)) {
            push @type, "member/$mtype";
        }
    } else {
        push @type, "member";
    }
    return wantarray ? @type : \@type;
}

# menu_type = join|renew|upgrade
sub membership_menu {
    my ($this, $menu_type) = @_;

    my @type     = $this->user_types();
    my $fees     = $this->get_allowed_fees($this->{section}->id());
    my $mytype   = $this->my_type();
    my $out      = $ml->h2($msg{"Select Membership Type"});
    my $showcost = &preference("Membership.show_cost_in_menu");

    my $fee_description;
    $fees->sort("sortkey");
    my $count;
    my $last_fee;
    my $current_dues;
    while (my $fee = $fees->next()) {
        my $type       = $fee->getdata("type");
        my $visibility = $fee->getdata("visibility");
        if ($type eq $this->member->member_type()) {
            $current_dues = $fee->cost(member => $this->member());
        }
        if (!$this->{admin}) {
            next if ($visibility eq "hidden");
            next if (!$fee->allow());
        }
        $count++;
        $last_fee = $fee;
        my $item = $ml->a(
            $msg{$type},
            {
                href => $this->link(type => $type, pro => "new_member"),
                id   => $type
            }
        );
        if ($showcost) {
            my $cost = $fee->cost(member => $this->member);
            $item .= " (\$$cost)";
        }
        $item .= $ml->div($msg{$fee->description()});
        $fee_description .= $ml->li($item);
    }

    if (($share{Page} && $share{Page}->is_publishing) || !$mytype) {

        # not a member, use hyperlinks so all choices are clearly visible
        if ($count == 1) {
            return $this->new_member_form(type => $last_fee->getdata("type"));
        }
        $out .= $ml->ul($fee_description, {class => "MembershipType"});
        my $opt;
        if ($this->{admin}) {
            $out .= $ml->p("Non-member types:");
            $opt = $ml->li($ml->a($msg{secondary}, {href => $this->link(type => "secondary")})
                  . " (non-paying, affiliated with an existing member)");
            $opt .= $ml->li($ml->a($msg{staff}, {href => $this->link(type => "staff")})
                  . " (non-paying, but full membership privileges)");
            $opt .= $ml->li($ml->a($msg{guest}, {href => $this->link(type => "guest")})
                  . " (non-member, with some login privileges)");
            $out .= $ml->ul($opt, {class => "MembershipType"});
        }
    } else {

        # already a member; use a select menu so we can preselect the default
        if ($menu_type ne "upgrade" && $mytype =~ /^member/) {
            $out .= $ml->p($msg{"Your current membership type is preselected as the default choice."});
        }
        my $options;
        while (my $fee = $fees->next()) {
            my $type = $fee->getdata("type");
            my $cost = $fee->cost(member => $this->member);
            if ($menu_type eq "upgrade") {

                # do not allow downgrades
                next if ($cost - $current_dues <= 0.01);
            }
            my $visibility = $fee->getdata("visibility");
            next if ($visibility eq "hidden" && !$this->{admin});
            my %attr = (value => $type);
            if ($this->type_value($type) eq $mytype) {
                $attr{selected} = undef;
            }
            my $item = $type;
            if ($showcost) {
                $item .= " (\$$cost)";
            }
            $options .= $ml->option($item, \%attr);
        }
        (tied %msg)->nohighlight;
        my $ctrl_vars;
        if ($this->{admin}) {
            $ctrl_vars = $ml->input(
                undef,
                {
                    type  => "hidden",
                    name  => "pro",
                    value => $this->{input}{pro}
                }
              )
              . $ml->input(
                undef,
                {
                    type  => "hidden",
                    name  => "uid",
                    value => $this->{input}{uid}
                }
              )
              . $ml->input(
                undef,
                {
                    type  => "hidden",
                    name  => "section_id",
                    value => $this->{input}{section_id}
                }
              );
        } else {
            if ($this->{input}{pro}) {
                $ctrl_vars .= $ml->input(
                    undef,
                    {
                        type  => "hidden",
                        name  => "pro",
                        value => $this->{input}{pro}
                    }
                );
            }
            if ($this->{input}{_id}) {
                $ctrl_vars .= $ml->input(
                    undef,
                    {
                        type  => "hidden",
                        name  => "_id",
                        value => $this->{input}{_id}
                    }
                );
            }
        }

        $out .= $ml->form(
            $ml->p($ml->select($options, {name => "type"}))
              . $ctrl_vars
              . $ml->p(
                    $ml->input(undef, {type => "reset"}) . " "
                  . $ml->input(undef, {type => "submit", value => $msg{"Continue &gt;"}})
              ),
            {method => "get"}
        );
    }
    $out .= "<!--content(membership_menu_footer)-->\n";
    return $out;
}

sub new_member_form {
    my ($this, %opt) = @_;
    if (my $goto = &preference("Membership.custom_application_url")) {
        return &redirect($goto) if (!$this->{admin});
    }
    my $type = $this->{input}{type} || $opt{type};
    my $conf = Modules::Membership::Config::confrule("heading.new_member", "membertype", $type);
    my $title = &substitute($msg{$conf}, {type => $msg{$type}});
    if ($type =~ /^guest/) {
        $title = $msg{&preference("Membership.heading.new_guest")};
    }
    my $out = $ml->h1($title);
    if (scalar keys %{$this->{post}} > 2) {
        delete $this->{post}{submit_action};
        if (!$share{DB}->level) {

            # check captcha
            my $captcha;
            if (&preference("Membership.captcha.mode") eq "passive") {
                $captcha = new ExSite::Captcha(
                    mode => "passive",
                    name => "member_comment"
                );
            } else {
                $captcha = new ExSite::Captcha(textmode => $this->{input}{captchamode});
            }
            if (!$captcha->pass()) {
                return $this->error($msg{"Wrong answer to anti-robot question."}) . ExSite::HTML::BackButton();
            }
            delete $this->{post}{member_comment};
            delete $this->{post}{captcha};
            delete $this->{post}{captcha_solution};
        }

        # clear cart if current session has account that is attached to existing member
        if (my $invid = $session{invoice}) {
            my $invoice = new Modules::Finance::Receivable(id => $invid);
            if ($invoice->getdata("account_id")) {
                if ($invoice->account()->get_member()) {
                    $session{invoice} = 0;
                }
            }
        }

        return $this->do_membership_form(
            data => $this->{post},
            fee  => 1,
            goto => $opt{goto}
        );
    } elsif ($share{DB}->my_uid()
        && $share{Page}
        && !$share{Page}->is_publishing)
    {
        my $name = $share{DB}->my_name();
        if (&preference("Membership.disable_renewal_form")) {
            $out .= $ml->p(
                &substitute(
                    $msg{
"WARNING: you may be using the wrong form. You are currently logged in as [[name]]. If you want to sign up someone who is not [[name]], please log out first."
                    },
                    {name => $name}
                )
            );
        } else {
            $out .= $ml->p(
                &substitute(
                    $msg{
"WARNING: you may be using the wrong membership form. You are currently logged in as [[name]].  If you want to renew your membership, please use the <a href='[[renew_url]]'>membership renewal form</a> instead. If you want to sign up someone who is not [[name]], please log out first."
                    },
                    {
                        name      => $name,
                        renew_url => $this->link(pro => "renew")
                    }
                )
            );
        }
    } elsif ($type) {
        $out .= $this->membership_form(
            data => {
                section_id   => $this->get_section_id(),
                type         => $type,
                parent_id    => $this->{input}{pid},
                organization => $this->{post}{organization},
            },
            captcha       => 1,
            purchase_type => "New",
        );
    } else {
        return $this->membership_menu();
    }
    return $out;
}

sub renewal_form {
    my ($this, %opt) = @_;
    $session{Membership_renewal_form} = 1;
    my $stat = $this->run_handler("Membership_renewal_form", %opt);
    return $stat if ($stat);
    my $out;
    if ($this->my_type() =~ /^guest/) {
        $out = $ml->h1($msg{"Become a Member"});
    } else {
        $out = $ml->h1($msg{"Renew Membership"});
    }
    if (  !$share{DB}->is_manager()
        && &preference("Membership.disable_renewal_form"))
    {
        return $ml->p($msg{"Membership renewal is currently disabled.  Please try again later."});
    }
    if (  !$share{DB}->is_manager
        && $this->get_section_id != $this->member->gid)
    {
        my $section = new ExSite::Section(id => $this->member->gid);
        my $page = &ExSite::Module::service_page("Membership", $this->member->gid);
        my $uri = new ExSite::URI;
        $uri->setup($page->get_url_dynamic);
        $uri->query(pro => "renew");
        my $title = $section->title();
        my $link = $ml->a($uri->write, {href => $uri->write()});
        return $ml->p(
            &substitute($msg{"Please renew your membership at [[link]] on the $title website."}, {link => $link}));
    }
    if ($this->member->is_renewable || $this->is_upgradable) {
        if (scalar keys %{$this->{post}} > 1) {
            delete $this->{post}{submit_action};
            my $out;
            if ($this->is_reinstate()) {
                $out .= $this->reinstate();
            }
            $out .= $this->do_membership_form(data => $this->{post}, fee => 1);
            return $out;
        } elsif ($this->member) {
            my $type = $this->{input}{type};
            if (!$type && &preference("Membership.force_renewal_type")) {
                my $types_allowed = $this->membership_types();
                my $current_type  = $this->member->member_type();
                if (scalar grep(/$current_type/, @$types_allowed) > 0) {
                    $type = $this->member->member_type();
                }
            }
            if ($type) {
                my $data     = $this->member->get();
                my $metadata = $this->member->meta->get();
                foreach my $meta (%$metadata) {
                    $data->{"meta_$meta"} = $metadata->{meta};
                }
                if ($type !~ /^(staff|guest|secondary)$/) {
                    $type = "member/$type";
                }
                $out .= $this->membership_form(
                    data          => $data,
                    type          => $type,
                    other         => {member_id => $this->member->id},
                    purchase_type => "Renew"
                );
            } else {
                $out .= $this->membership_menu();
            }
        }
    } else {
        $out .= $msg{"Your membership is not renewable at this time, because: "};
        $out .= $this->show_my_status;
    }
    return $ml->p($out);
}

sub mailing_lists {
    my $this = shift;
    if (&preference("Membership.constant_contact.enabled")) {
        require Email::ConstantContact;

        my $apikey   = &preference("Membership.constant_contact.api_key");
        my $username = &preference("Membership.constant_contact.username");
        my $password = &preference("Membership.constant_contact.password");
        my $cc       = new Email::ConstantContact($apikey, $username, $password);

        my $member  = $this->member();
        my $contact = $cc->getContact($member->email());

        my @all_lists = $cc->lists();
        @all_lists =
          sort { lc($a->{SortOrder}) <=> lc($b->{SortOrder}) } @all_lists;
        my @valid_lists;
        foreach my $list (@all_lists) {
            push(@valid_lists, $list)
              if ($list->{DisplayOnSignup} eq "Yes");
        }

        my $save_message;
        if ($this->{input}{action} eq "doform") {
            my @mailing_lists =
              split("; ", $this->{input}{constant_contact_mailing_lists});

            if ($contact) {
                foreach my $list (@valid_lists) {
                    my $newcls;
                    foreach my $cl (@{$contact->{ContactLists}}) {
                        push(@$newcls, $cl) if ($cl ne $list->{id});
                    }
                    $contact->{ContactLists} = $newcls;
                }

                foreach my $list (@mailing_lists) {
                    $contact->addToList($list);
                }

                $contact->save();
            } else {
                $cc->newContact(
                    $member->email(),
                    {
                        FirstName    => $member->getdata("first_name"),
                        LastName     => $member->getdata("last_name"),
                        CompanyName  => $member->getdata("organization"),
                        ContactLists => \@mailing_lists
                    }
                );
            }

            if ($member->getdata("subscribe") eq "N" && scalar @mailing_lists) {
                $member->setdata("subscribe", "Y");
                $member->save();
            }

            $save_message = $share{ML}->p($share{ML}->b($msg{"Your settings have been saved."}));
        }

        my $list_ids;
        foreach my $listid (@{$contact->{ContactLists}}) {
            $list_ids->{$listid} = 1;
        }

        foreach my $list (@valid_lists) {
            my $opt = {
                type  => "checkbox",
                name  => "constant_contact_mailing_lists",
                id    => "constant_contact_mailing_lists",
                value => $list->{id}
            };

            $opt->{checked} = "yes" if ($list_ids->{$list->{id}});

            require Encode;
            my $checkbox =
                $ml->input(undef, $opt) . " "
              . Encode::encode("utf8", Encode::decode("ISO-8859-1", $list->{Name}))
              . $ml->br();
            $share{DB}->form()->input(
                type   => "preformatted",
                name   => "constant_contact_mailing_lists",
                id     => "constant_contact_mailing_lists",
                input  => $checkbox,
                prompt => $msg{&preference("Membership.constant_contact.input_label")} || $msg{"Mailing Lists"}
            );
        }

        $share{DB}->input_html(
            type  => "hidden",
            name  => "action",
            value => "doform"
        );

        my $template = <<END;
[[constant_contact_mailing_lists:input]]
<br/>
END

        $share{DB}->form()->template($template);

        return
            $share{ML}->h2($msg{"Mailing Lists"})
          . $save_message
          . $share{ML}->p(
            $msg{"Please select the areas of interest for which you would like to receive an occasional email from us."}
          ) . $share{DB}->form()->make();
    } else {
        return "Constant Contact not enabled.";
    }
}

sub keywise($$) {
    my ($key, $ref_list_datahash) = @_;
    my %datahash = ();
    foreach my $rhash (@{$ref_list_datahash}) {
        $datahash{$rhash->{lc($key)}} = $rhash;
    }
    return wantarray ? %datahash : \%datahash;
}

# interface for allowing the public to join a pre-existing organization

sub public_new_secondary {
    my ($this) = @_;
    my $db     = $share{DB};
    my $out    = $ml->h1($msg{"New Member Sign-up"});
    my $name   = $share{DB}->my_name();
    if (   $share{DB}->my_uid()
        && $share{Page}
        && !$share{Page}->is_publishing)
    {
        $out .= $ml->p(
            &substitute(
                $msg{
"WARNING: you may be using the wrong membership form. You are currently logged in as [[name]].  If you want to renew your membership, please use the <a href='[[renew_url]]'>membership renewal form</a> instead. If you want to sign up someone who is not [[name]], please log out first."
                },
                {
                    name      => $name,
                    renew_url => $this->link(pro => "renew")
                }
            )
        );
        return $out;
    }
    my $label =
      $share{DB}{map}->get_column_attr("member", "organization", "label");
    if (my $input = $this->{input}{organization}) {
        my $section_id = $this->{section}->id;
        my $members = $db->get_query("all active primary members", $section_id);

        # filter types that allow secondary members
        # more in-depth validation is done on next screen to account for limits etc
        my @filtered;
        my @types_allowed = $this->types_allow_secondary();
        if (scalar @types_allowed) {
            foreach my $m (@$members) {
                my $type = $m->{type};
                $type =~ s/^member\///;
                if (scalar grep(/$type/, @types_allowed) > 0) {
                    push(@filtered, $m);
                }
            }
        } else {
            @filtered = @$members;
        }
        my $organizations = &keywise("organization", \@filtered);
        my %scores;
        foreach my $org (keys %$organizations) {
            my $score = &ExSite::Misc::compare($input, $org);
            next if ($score < 0.2);
            $scores{$org} = $score;
        }
        my @matches = sort { $scores{$b} <=> $scores{$a} } keys %scores;
        if (scalar @matches) {
            $out .=
              $ml->h3(&substitute($msg{"The following [[label]](s) are a close match:"}, {label => $label}));
            $out .= $ml->p(
                &substitute(
                    $msg{"If your [[label]] is listed in our existing database please select it below:"},
                    {label => $label}
                )
            );
            my $i;
            foreach my $o (@matches[0 .. $#matches]) {
                $i++;
                my $mid  = $organizations->{$o}->{member_id};
                my $type = $organizations->{$o}->{type};
                $type =~ s/member\///;
                my $link = $ml->a(
                    $o,
                    {
                        href => $this->link(
                            pro  => "new_member_form",
                            pid  => $mid,
                            type => $type
                        )
                    }
                );
                $out .= $ml->div($link);
                last if ($i > 10);
            }
        } else {
            return $this->new_member_form();
        }
        my %linkopt = (pro => "new_member_form");
        $linkopt{type} = $this->{input}{type} if ($this->{input}{type});
        $out .= $ml->p(
            $ml->span($msg{"My $label is not listed"} . ", ")
              . $ml->span(
                $ml->a(
                    $msg{"create a new entry for it."},
                    {
                        href => $this->link(%linkopt)
                    }
                )
              )
        );
        return $out;
    }
    my $section_id = $this->{section}->id;
    my $members = $db->get_query("all primary members", $section_id);
    my @options;
    foreach my $m (@$members) {
        push @options, [$m->{member_id}, $m->{organization}];
    }
    my $f = new ExSite::FormBuilder(method => "post");
    $out .= $this->get_template("public_new_secondary_header");
    $f->input(
        name     => "organization",
        type     => "text",
        prompt   => $msg{$label},
        required => 1,
    );
    $f->input(
        type  => "hidden",
        name  => "type",
        value => $this->{input}{type}
    );
    $f->buttons(submit => 1, cancel => 1);
    $out .= $f->make();
    return $out;
}

sub new_secondary_member {
    my ($this, %opt) = @_;
    my $label = &preference("Membership.secondary.label");
    my $out   = $ml->h1($msg{"New $label"});
    my $type  = $this->member->member_type();
    if (!$this->{admin}) {
        if (!$this->{Member}) {
            return $this->error($msg{"No member found."});
        }

        # check if type is allowed to add secondary members
        if ($type eq "staff") {
            return $this->error($msg{"Secondary members can not be attached to staff accounts."});
        }
        if (&preference("Membership.secondary.fee")
            && !$this->member->is_active())
        {
            return $this->error(
                $msg{"You cannot add a secondary member at this time because your membership is not active."});
        }
        if (!$this->allow_add_secondary($type)) {
            return $this->error($msg{"You do not have permission to add a secondary member."});
        }
    }
    $share{DB}->handler("user_owns", \&primary_user_owns);

    my $uid = $this->member ? $this->member->uid : 0;
    my $pid = $this->{post}{parent_id} = $this->{input}{pid} || $uid;
    my $parent = new Modules::Membership::Member(id => $pid);
    my $secondary_type;
    my @types = split(/,/, &preference("Membership.secondary.type"));
    if (&preference("Membership.secondary.use_parent_type")) {
        $secondary_type = $parent->getdata("type");
    } else {
        $secondary_type = $opt{type} || $types[0];
    }
    if (scalar keys %{$this->{post}} > 1) {
        delete $this->{post}{submit_action};
        $this->{post}{parent_id} = $pid;

        # set secondary member section to be same as parent in multiple website configurations with global logins
        if ($parent && $parent->gid) {
            $this->{post}{section_id} = $parent->gid || $uid;
            $this->{post}{organization} = $parent->getdata("organization")
              if (!$this->{post}{organization});
        }
        $out .= $this->do_membership_form(
            data => $this->{post},
            fee  => &preference("Membership.secondary.fee")
        );
        return $out if (!$this->member->id);
        if (   !&preference("Membership.secondary.fee")
            && !$this->member->getdata("access"))
        {
            $out .= $this->finalize_application();
        }
        if ($this->{input}{pid} && $this->allow_add_secondary() || $share{DB}->my_uid == $pid) {
            $out .= $ml->p(
                $ml->a(
                    $msg{"Back to primary user"},
                    {
                        href => $this->link(
                            uid => $this->{input}{pid},
                            pid => undef,
                            pro => undef
                        )
                    }
                )
            );
        }
    } elsif ($secondary_type) {
        ### change $this->{Member} to be a blank member to get better
        ### form prepopulation
        my $data = {
            section_id => $this->get_section_id(),
            type       => $secondary_type,
            parent_id  => $pid
        };
        $this->{Member} = new Modules::Membership::Member(data => $data);
        $out .= $this->membership_form(
            data    => $data,
            captcha => 0,
        );
    }
    return $out;
}

sub membership_form {
    my ($this, %opt) = @_;
    my $data = $opt{data} || {};
    my $type = $opt{type}
      || $data->{type};

    # force a type for regular users
    if (!$share{DB}->is_manager && !$type) {
        $type = $this->membership_types()->[0];
    }
    my $other = $opt{other} || {};

    # add referer to other inputs
    $other->{_ref} = $this->{input}{reply} || $ENV{HTTP_REFERER};

    # handler for membership type field
    $share{DB}->handler("input_column", \&profile_input_exsite);

    my $out;

    $share{DB}->form(method => "POST", action => $this->link);

    # custom membership form template?
    my $template;
    my $loc = $share{Page} || $share{Section};
    if ($loc) {
        my $template_name;
        my $stat = $this->run_handler("Membership_form_template", %opt);
        if ($stat) {
            $template_name = $stat;
        } else {
            $template_name =
              exists $other->{member_id}
              ? "update_membership_form_template"
              : "create_membership_form_template";
        }
        my $ctemplate = $loc->find($template_name);
        $template = $ctemplate->get_html();
        if (!$template) {
            $ctemplate = $loc->find("membership_form_template");
            $template  = $ctemplate->get_html();
        }
    }
    if (!$template) {
        $template = "<table class='MembershipForm'>
<tr>
<td width='33%'>[[first_name:prompt]]<br>[[first_name:input]]</td>
<td width='33%'>[[middle_name:prompt]]<br>[[middle_name:input]]</td>
<td width='33%'>[[last_name:prompt]]<br>[[last_name:input]]</td>
</tr>
<tr>
<td>[[email:prompt]]<br>[[email:input]]</td>
<td colspan=2>[[subscribe:prompt]]<br>[[subscribe:input]]</td>
</tr>
</table>";
    }
    $share{DB}->form()->template($template);

    my $hide = $this->hidden_member_columns($type);
    if (!$this->member->defined) {

        # set some dummy values as a hint to get_allowed attributes
        $this->member->setdata("section_id", $this->get_section_id());
        $this->member->setdata("type",       $this->type_value($type));
        $this->member->setdata("parent_id",  $this->{input}{pid});
    }

    my $map      = $share{DB}{map};
    my $ncols    = $map->get_ncol("member");
    my $typeflag = 0;
    for (my $icolumn = 0 ; $icolumn < $ncols ; $icolumn++) {
        my %column = $map->get_column("member", $icolumn);
        next if ($hide->{$column{column}});
        if (!$column{label}) { $column{label} = $column{column}; }
        my $allow = ($column{write} == 1);
        if (!$allow && $share{DB}->is_manager() && $this->{admin}) {
            $allow = ($column{write} <= $share{DB}->level());
        }
        if ($allow) {
            $typeflag = 1 if ($column{column} eq "type");

            # emphasize required fields
            my $validate = $this->run_handler("Membership_column_validate", $column{column}, $type)
              || $column{validate};
            my $required = ($validate =~ /soft|hard/);
            my $value    = $data->{$column{column}};
            if ($column{column} eq "type") {
                $value = $this->type_value($type);
            }
            if (  !$data->{member_id}
                && $column{column} =~ /^(login|password|access)$/)
            {
                # do not prompt for login or password on new members
                next;
            }
            if (  !$data->{member_id}
                && $column{column} eq "parent_id"
                && $this->{input}{pid})
            {
                # do not prompt for parent if it can be deduced from context
                next;
            }
            my $pid =
              $this->{input}{pid} || $this->member->getdata("parent_id");
            my $p = new Modules::Membership::Member(id => $pid);
            if (   $pid
                && !$this->allow_add_secondary(undef, $p, 1)
                && $opt{purchase_type} eq "New")
            {
                my $org      = $p->getdata("organization");
                my $email    = $p->email();
                my $section  = new ExSite::Section(id => $this->get_section_id());
                my $sitename = $section->get_my("title");
                return $this->error(
                    &substitute(
                        $msg{
                            "Please contact [[organization]] at [[email]] to be added as a sub-member for [[sitename]]."
                        },
                        {
                            organization => $org,
                            email        => $email,
                            sitename     => $sitename
                        }
                    )
                );
            }
            if ($column{column} eq "organization" && $pid) {

                # do not prompt for organization if it can be deduced from context
                next;
            }
            if (   $data->{member_id}
                && $column{column} =~ /^(photo|thumbnail|password)$/)
            {
                # do not prompt for photo or password on existing members
                next;
            }
            $value = "0000-00-00"
              if ($column{column} eq "expirydate" && !$value);
            $share{DB}->input_column(
                table    => "member",
                name     => $column{column},
                column   => $column{column},
                size     => $column{size},
                datatype => $column{datatype},
                required => $required,
                value    => $value,
            );
        }
    }

    # temporarily setup member with renewing type
    my $tmp        = $this->member->getdata("type");
    my $type_value = $this->type_value($type);
    $this->member->setdata("type", $type_value);
    my @metafields = $this->member->meta()->get_allowed();
    $this->member->setdata("type", $tmp);
    if (@metafields == 0) {
        my %meta = $this->member->meta()->get_all();
        @metafields = keys %meta;
    }
    foreach my $key (sort @metafields) {
        my $write_access = $this->member->meta()->get_map_info($key, "write");
        if (!defined $write_access
            || $write_access <= $share{DB}->level())
        {
            $this->member->meta()->input("meta_$key", $key, $data->{"meta_$key"});
        }
    }

    # embedded contact form
    if (!exists $other->{member_id}
        && &preference("Membership.contact_form.enabled"))
    {
        my $prepop;
        if ($this->{input}{pid}) {
            my $parent =
              new Modules::Membership::Member(id => $this->{input}{pid});
            my $contact_type = &preference("Membership.contact_form.type")
              || &preference("Membership.directory.contacts_type");
            my $c = $parent->account()->get_contact($contact_type);
            $prepop = $c->get() if ($c);
        }
        my $f = $share{DB}->form();
        my %fields;
        map { $fields{$_} = 1 }
          split(/,/, &preference("Membership.contact_form.fields"));
        my %required_contact;
        map { $required_contact{$_} = 1 }
          split(/,/, &preference("Membership.contact_form.required"));
        if ($fields{privacy}) {
            $f->input(
                prompt  => $msg{"Privacy"},
                name    => "contact_privacy",
                type    => "select",
                value   => $prepop->{privacy},
                options => $share{DB}{map}->regexp("list:contact_privacy")
            );
        }
        if ($fields{address}) {
            $f->input(
                prompt   => $msg{$share{DB}{map}->get_column_attr("contact", "address", "label")},
                name     => "contact_address",
                type     => "textarea",
                value    => $prepop->{address},
                size     => 120,
                required => $required_contact{address}
            );
        }
        if ($fields{city}) {
            $f->input(
                prompt   => $msg{"City"},
                name     => "contact_city",
                type     => "text",
                value    => $prepop->{city},
                size     => 40,
                required => $required_contact{city}
            );
        }
        if ($fields{provstate}) {
            $f->input(
                prompt => $msg{$share{DB}{map}->get_column_attr("contact", "provstate", "label")},
                name   => "contact_provstate",
                type   => "select",
                value => $prepop->{provstate} || $share{DB}{map}->get_column_attr("contact", "provstate", "default"),
                options =>
                  $share{DB}{map}->regexp($share{DB}{map}->get_column_attr("contact", "provstate", "datatype")),
                nullvalue => "== select ==",
                required  => $required_contact{provstate}
            );
        }
        if ($fields{country}) {
            $f->input(
                prompt => $msg{"Country"},
                name   => "contact_country",
                type   => "select",
                value  => $prepop->{country} || $share{DB}{map}->get_column_attr("contact", "country", "default"),
                options => $share{DB}{map}->regexp($share{DB}{map}->get_column_attr("contact", "country", "datatype")),
                nullvalue => "== select ==",
                value     => $share{DB}{map}->get_column_attr("contact", "country", "default"),
                required  => $required_contact{country}
            );
        }
        if ($fields{pcode}) {
            $f->input(
                prompt   => $msg{$share{DB}{map}->get_column_attr("contact", "pcode", "label")},
                name     => "contact_pcode",
                type     => "text",
                value    => $prepop->{pcode},
                size     => 10,
                required => $required_contact{pcode}
            );
        }
        if ($fields{phone1}) {
            $f->input(
                prompt   => $msg{$share{DB}{map}->get_column_attr("contact", "phone1", "label")},
                name     => "contact_phone1",
                type     => "text",
                value    => $prepop->{phone1},
                size     => 15,
                required => $required_contact{phone1}
            );
        }
        if ($fields{phone2}) {
            $f->input(
                prompt   => $msg{$share{DB}{map}->get_column_attr("contact", "phone2", "label")},
                name     => "contact_phone2",
                type     => "text",
                value    => $prepop->{phone2},
                size     => 15,
                required => $required_contact{phone2}
            );
        }
        if ($fields{fax}) {
            $f->input(
                prompt   => $msg{$share{DB}{map}->get_column_attr("contact", "fax", "label")},
                name     => "contact_fax",
                type     => "text",
                value    => $prepop->{fax},
                size     => 15,
                required => $required_contact{fax}
            );
        }
        if ($fields{email}) {
            $f->input(
                prompt   => $msg{"E-mail"},
                name     => "contact_email",
                type     => "text",
                value    => $prepop->{email},
                size     => 40,
                required => $required_contact{email}
            );
        }
        if ($fields{web}) {
            $f->input(
                prompt   => $msg{"Website"},
                name     => "contact_web",
                type     => "text",
                value    => $prepop->{web},
                size     => 40,
                required => $required_contact{web}
            );
        }
    }

    # constant contact
    if (&preference("Membership.constant_contact.enabled")
        && !$other->{member_id})
    {
        require Email::ConstantContact;

        my $apikey   = &preference("Membership.constant_contact.api_key");
        my $username = &preference("Membership.constant_contact.username");
        my $password = &preference("Membership.constant_contact.password");
        my $cc       = new Email::ConstantContact($apikey, $username, $password);

        my @all_lists = $cc->lists();
        @all_lists =
          sort { lc($a->{SortOrder}) <=> lc($b->{SortOrder}) } @all_lists;
        foreach my $list (@all_lists) {
            next unless ($list->{DisplayOnSignup} eq "Yes");

            my $opt = {
                type  => "checkbox",
                name  => "constant_contact_mailing_lists",
                id    => "constant_contact_mailing_lists",
                value => $list->{id}
            };

            require Encode;
            my $checkbox =
                $ml->input(undef, $opt) . " "
              . Encode::encode("utf8", Encode::decode("ISO-8859-1", $list->{Name}))
              . $ml->br();
            $share{DB}->form()->input(
                type   => "preformatted",
                name   => "constant_contact_mailing_lists",
                id     => "constant_contact_mailing_lists",
                input  => $checkbox,
                prompt => $msg{&preference("Membership.constant_contact.input_label")} || $msg{"Mailing Lists"}
            );
        }
    }

    $this->term_select($type_value, $opt{purchase_type});

    # extra hidden inputs
    foreach my $key (keys %$other) {
        $share{DB}->form()->input(type => "hidden", name => $key, value => $other->{$key});
    }
    if ($this->{input}{pid}) {
        $share{DB}->form()->input(
            type  => "hidden",
            name  => "parent_id",
            value => $this->{input}{pid}
        );
    }
    if (!$share{DB}->form()->has_input("type")) {
        $share{DB}->form()->input(
            type  => "hidden",
            name  => "type",
            value => $this->type_value($type)
        ) if (!$typeflag);
    }

    if ($opt{captcha} && $share{DB}->level < 1) {
        my $captcha;
        if (&preference("Membership.captcha.mode") eq "passive") {
            $captcha = new ExSite::Captcha(
                mode => "passive",
                name => "member_comment"
            );
        } else {
            $captcha =
              new ExSite::Captcha(textmode => $this->{input}{captchamode});
        }
        $share{DB}->form()->input(
            type  => "preformatted",
            name  => "captcha",
            input => $captcha->make()
        );
    }

    # admin is not required to fill in required fields
    $share{DB}->form()->{required_id} = {} if ($this->{admin});
    $out .= $share{DB}->form()->make();
    return $out;
}

# return term selection input for membership_form
# type = proposed member_type
# purchase_type = New|Renew
sub term_select {
    my ($this, $type, $purchase_type) = @_;
    return undef if ($type !~ /^member\//);
    $type =~ s/^member\///g;
    return if (!$purchase_type);
    my $stat = $this->run_handler("Membership_term_select", $type, $purchase_type);
    return $stat if ($stat);
    if (my $options = Modules::Membership::Config::confrule("selectable_terms", "membertype", $type)) {
        my @lengths = split(/,/, $options);
        my $unit = $msg{ucfirst($msg{&preference("Membership.anniversary.period")})};
        my @options;
        foreach my $length (@lengths) {
            if ($length == 1) {
                push(@options, [$length, "$length $unit"]);
            } elsif ($length < 20) {
                push(@options, [$length, "$length ${unit}s"]);
            } else {
                push(@options, [$length, "Lifetime"]);
            }
        }
        $share{DB}->form()->input(
            name     => "_mterm",
            type     => "select",
            prompt   => $msg{"Choose your membership term"},
            options  => \@options,
            required => 1
        );
    }
    return;
}

sub hidden_member_columns {
    my ($this, $membertype) = @_;
    my $stat = $this->run_handler("hidden_member_columns");
    return $stat if ($stat);
    my $list = &preference("Membership.typehide.$membertype");
    if (!$list) {
        $list = &preference("Membership.hide");
    }
    my %hide = map { $_ => 1 } split /,/, $list;
    return \%hide;
}

sub renew_group {
    my ($this) = @_;
    my $is_renewing = $this->is_renewing();
    return $ml->p($msg{"Please renew your own membership before proceeding."})
      if (!$is_renewing);
    my @members = split(/; /, $this->{input}{renew});
    my $out;
    my $i;
    foreach my $uid (@members) {
        my $silent;
        $silent = 1 if ($i < $#members);
        $out .= $this->setup_bill(
            uid           => $uid,
            purchase_type => "Renew",
            silent        => $silent
        );
        $i++;
    }
    $session{continue_shopping_url} = -1;
    $out .= "<!--&Pay()-->" if (!$out);
    return $out;
}

sub setup_bill {
    my ($this, %opt) = @_;
    my $tmp = $this->member->id;
    if ($opt{uid}) {
        $this->setup_member($opt{uid});
    }
    my $membertype = $opt{membertype} || $this->member->member_type();
    my $purchase_type = $opt{purchase_type};
    my $out;
    my $fee = $this->membership_fee(
        member        => $this->member(),
        type          => $membertype,
        purchase_type => $purchase_type
    );
    if ($fee >= 0) {
        (tied %msg)->nohighlight();
        my $item = &preference("Membership.receivable_item_name");
        my $info = $this->receivable_info(
            purchase_type => $purchase_type,
            membertype    => $membertype,
            fee           => $fee
        );
        $item = &substitute($msg{$item}, $info);
        (tied %msg)->restore();
        $out .=
          $this->bill_member($item, $info->{description}, $fee, $info->{note},
            $opt{goto}, $info->{start_time}, $info->{end_time}, $opt{silent});
    }
    $this->setup_member($tmp);
    return $out;
}

sub do_membership_form {
    my ($this, %opt) = @_;

    my $out;
    my %data = %{$opt{data}};
    if (scalar keys %data > 2) {

        # get the formdata, parsed into columns
        my %pdata = $share{DB}->parse_parts(%data);

        # disable account change function in Pay
        $session{disable_change_account} = 1;

        # store selected membership term if defined
        my $mterm;
        if ($mterm = $data{_mterm}) {
            delete $pdata{_mterm};
        }

        # set redirect url for multi-step forms
        # handler can be used to do form pre-processing
        $opt{goto} =
             $opt{goto}
          || $session{Membership_redirect}
          || $this->run_handler("do_membership_goto", \%pdata);
        if ($opt{goto}) {
            my $url;
            if ($opt{goto} =~ /^\d+$/) {
                my $page = new ExSite::Page(id => $opt{goto});
                $url = $page->get_url;
            } else {
                $url = $opt{goto};
                delete $session{Membership_redirect};
            }

            # form based re-direction
            my $f = new ExSite::FormBuilder(method => "POST", action => $url);
            $f->name("Membership_redirect");
            $f->buttons(submit => 0, cancel => 0, reset => 0);

            # alternative submit for clients with javascript disabled
            my $submit = $ml->noscript(
                $f->inputtag(
                    type  => "submit",
                    name  => "submit_action",
                    value => $msg{"Proceed to next step"}
                )
            );
            $f->input(
                type  => "preformatted",
                name  => "noscript",
                input => $submit
            );

            # javascript to autosubmit the form
            my $autosubmit = $ml->script("document.Membership_redirect.submit()", {type => "text/javascript"});
            $out .= $f->make() . $autosubmit;
        }

        # process constant contact subscriptions first and then remove the key
        if (&preference("Membership.constant_contact.enabled")) {
            require Email::ConstantContact;

            my $apikey   = &preference("Membership.constant_contact.api_key");
            my $username = &preference("Membership.constant_contact.username");
            my $password = &preference("Membership.constant_contact.password");
            my $cc       = new Email::ConstantContact($apikey, $username, $password);

            my $existing_contact = $cc->getContact($pdata{email});

            if (defined $pdata{constant_contact_mailing_lists}
                && $pdata{subscribe} ne "N")
            {
                my @mailing_lists =
                  split("; ", $pdata{constant_contact_mailing_lists});

                # check to see if there is an existing constant contact record
                if ($existing_contact) {
                    $existing_contact->{FirstName}   = $pdata{first_name};
                    $existing_contact->{LastName}    = $pdata{last_name};
                    $existing_contact->{CompanyName} = $pdata{organization};

                    foreach my $list_id (@mailing_lists) {
                        $existing_contact->addToList($list_id);
                    }

                    $existing_contact->save();
                } else {
                    $cc->newContact(
                        $pdata{email},
                        {
                            FirstName    => $pdata{first_name},
                            LastName     => $pdata{last_name},
                            CompanyName  => $pdata{organization},
                            ContactLists => \@mailing_lists
                        }
                    );
                }
            }

            if ($pdata{subscribe} eq "N" && $existing_contact) {
                $existing_contact->clearAllLists();
                $existing_contact->save();
            }

            delete $pdata{constant_contact_mailing_lists};
        }

        # extract the metadata from the form
        my %metadata;
        foreach my $key (keys %pdata) {
            if ($key =~ /^meta_(.+)$/) {
                $metadata{$1} = $pdata{$key};
                delete $pdata{$key};
            }
        }

        # extract the contact data from the form
        my %contact;
        foreach my $key (keys %pdata) {
            if ($key =~ /^contact_(.+)$/) {
                $contact{$1} = $pdata{$key};
                delete $pdata{$key};
            }
        }

        # extract control variables
        my %other;
        foreach my $key (keys %pdata) {
            if ($key =~ /^_(.+)$/) {
                $other{$1} = $pdata{$key};
                delete $pdata{$key};
            }
        }

        my $purchase_type;

        # are we processing an existing UID?
        my $uid = $pdata{member_id} || $this->{input}{uid};
        my $pid;
        if ($uid) {
            $this->member->setup(id => $uid);
            if ($this->member->allow_edit()) {
                delete $pdata{member_id};
            } else {
                $uid = 0;
                return $this->error("permission denied");
            }
        }

        # validate permission to assign access level if it is being changed
        if (   $pdata{access}
            && $pdata{access} != $this->member->getdata("access")
            && $pdata{access} > $share{DB}->level())
        {
            return $this->error(
                &substitute(
                    "Sorry, you are not authorized to assign access level [[level]] to this user.",
                    {level => $pdata{access}}
                )
            ) . ExSite::HTML::BackButton();
        }

        if ($uid) {

            # processing of existing UID
            if ($this->member->getdata("type") !~ /^guest/) {
                $purchase_type = "Renew";
            } else {

                # upgrading guest account
                $purchase_type = "New";
                $session{renewal_type} = $this->{input}{type} || $this->my_type();
            }

            if ($this->member->uid != $uid) {
                $this->warn("membership renewal: switching to uid $uid");
                $this->member->setup(id => $uid);
            }

            # update regular data
            my $olddata   = $this->member->get();
            my $old_login = $olddata->{$config{auth}{user_login_column}};
            my $old_email = $olddata->{$config{auth}{user_email_column}};

            my $access = $share{DB}->level;
            foreach my $key (keys %pdata) {
                next
                  if ( $key eq "type"
                    && !$this->{admin}
                    && &preference("Membership.force_renewal_type"));

                if (!$share{DB}{map}->is_mapped("member", $key)) {
                    $this->warn("$key: invalid member attribute");
                } elsif ($share{DB}{map}->get_column_attr("member", $key, "write") > $access) {
                    $this->warn("$key: permission denied");
                } elsif ($pdata{$key} ne $olddata->{$key}) {
                    if ($key eq "parent_id" && $pdata{$key}) {
                        $this->move_secondary($pdata{$key}, $olddata->{$key});
                    }
                    $this->member->setdata($key, $pdata{$key});
                }
            }

            # sync email and login if necessary
            my $new_email =
              $this->member->getdata($config{auth}{user_email_column});
            my $new_login =
              $this->member->getdata($config{auth}{user_login_column});
            if (   $old_login eq $old_email
                && $old_login eq $new_login
                && $old_email ne $new_email)
            {
                my @r =
                  $share{DB}->fetch_match("member", {login => $new_email});
                if (!scalar @r) {
                    $this->member->setdata($config{auth}{user_login_column}, $new_email);
                    $this->member->save();
                    if (  !$this->{admin} && !$share{DB}->level
                        || $share{DB}->my_uid == $this->member->id)
                    {
                        # kill session because we are logging in and shopping cart is now invalid
                        $session{invoice} = 0;
                        delete $session{prelim_identity};
                        $share{DB}->do_login($this->member->get());
                    }
                }
            }

            # update contact emails if preferred email has been updated and they were previously a match
            &ExSite::Module::read_conf("Pay");
            my $contacts = $this->member->account()->get_contacts();
            $contacts->reset();
            while (my $c = $contacts->next) {
                if ($c->getdata("email") eq $old_email) {
                    $c->setdata("email", $new_email);
                    $c->force_save();
                }
            }

            # update metadata
            $olddata = $this->member->meta_get();
            my @meta_err;
            foreach my $key (keys %metadata) {
                if (!$this->member->meta_allowed($key)) {
                    $this->warn("$key: invalid member metadata");
                } elsif ($metadata{$key} ne $olddata->{$key}) {
                    if (!$this->member->meta_set($key, $metadata{$key})) {
                        push(@meta_err, $this->member->meta()->show_diagnostics("error", "html"));
                    }
                }
            }
            if (scalar @meta_err) {
                return join("\n", @meta_err) . ExSite::HTML::BackButton();
            }
        } else {

            # setup a new UID
            my @err;
            $purchase_type = "New";
            $this->{Member} = new Modules::Membership::Member();

            # setup regular data
            foreach my $key (keys %pdata) {
                if (!$share{DB}{map}->is_mapped("member", $key)) {
                    push(@err, "$key: invalid member attribute");
                }
                $this->member->setdata($key, $pdata{$key});
            }
            my @dupes = $share{DB}->get_query("email dupe check", $this->get_section_id(), $pdata{email});
            if (
                &preference("Membership.dupe_check.email")
                &&

                # upgrades are allowed
                !$share{DB}->authorize() && scalar @dupes
              )
            {
                my $forgot = $ml->a(
                    $msg{"retrieve your password"},
                    {
                        href => "$config{server}{CGIpath}/$config{prog}{login}?login_action=forgot"
                    }
                );
                push(
                    @err,
                    &substitute(
                        $msg{
"[[email]] has already been registered. Please login with your existing account or [[forgot]] if you have forgotten it."
                        },
                        {email => $pdata{email}, forgot => $forgot}
                    )
                );
            }

            # set default preferences if not already set
            my $privacy = &preference("Membership.contact_privacy")
              || &preference("Membership.privacy");
            $this->member->setdata("privacy", $privacy)
              if (!$this->member->getdata("privacy"));

            # set restricted data
            $this->member->setdata("section_id", $this->gid);
            $this->member->setdata("ctime",      undef);

            # automatically set the organization for this secondary member
            if ($pid = $this->member->getdata("parent_id")) {
                my $p = $this->member->parent;
                $this->member->setdata("organization", $p->getdata("organization"));
            }
            if (exists $pdata{"photo"}) {
                my $in       = new ExSite::Input;
                my $filedata = $pdata{photo};
                my $img      = new ExSite::Image($filedata);
                if ($img->web_ready || $img->jpeg) {
                    if (
                        $img->scale(
                            &preference("Membership.photo.shrinksize") || 400,
                            &preference("Membership.photo.shrinksize") || 400,
                            -1, "-quality 95"
                        )
                      )
                    {
                        my %data;
                        $this->member->setdata("photo", $img->encode);
                        $img->thumb($config{thumbnail_size}, "-quality 100");
                        $this->member->setdata("thumbnail", $img->encode);
                    }
                }
            }

            # update metadata
            foreach my $key (keys %metadata) {

                if (!$this->member->meta_allowed($key)) {
                    push(@err, "$key: " . $msg{"invalid member metadata"});
                } elsif ($metadata{$key}) {
                    if (!$this->member->meta_set($key, $metadata{$key})) {
                        my @meta_err = $this->member->meta()->fetch_diagnostics("error");
                        push(@err, @meta_err);
                    }
                } elsif ($this->member->meta->required($key) && !$this->{admin}) {
                    my $label = $this->member->meta->label($key);
                    push(@err, &substitute($msg{"You must provide a value for [[label]]."}, {label => $label}));
                }
            }

            # update contact data
            if (&preference("Membership.contact_form.enabled")) {
                $this->{Contact} = new Modules::ID::Contact();
                $this->{Contact}->setdata("type",
                    &preference("Membership.contact_form.type") || &preference("Membership.directory.contacts_type"));
                my %required_contact;
                map { $required_contact{$_} = 1 }
                  split(/,/, &preference("Membership.contact_form.required"));
                foreach my $key (keys %contact) {
                    if ($contact{$key}) {
                        $this->{Contact}->setdata($key, $contact{$key});
                    } elsif ($required_contact{$key}) {
                        push(@err, "You must provide a value for $key.");
                    }
                }
                $this->{Contact}->setdata("email", $pdata{email})
                  if (!$contact{email});
                my $privacy = &preference("Membership.contact_privacy")
                  || &preference("Membership.privacy");
                $privacy =~ s/\sonly//;
                $this->{Contact}->setdata("privacy", $privacy)
                  if (!$contact{privacy});
                if (!$this->{Contact}->save()) {
                    my @contact_err = $this->{Contact}->fetch_diagnostics("error");
                    push(@err, @contact_err);
                }
            }

            if (scalar @err) {
                my $back = $ml->input(
                    undef,
                    {
                        type    => "button",
                        value   => $msg{"Back"},
                        onclick => "history.back()"
                    }
                );
                return $ml->p(
                    $msg{
"There was an issue while trying to process your form. Please correct the following errors and try again."
                    }
                  )
                  . $ml->div(join("\n", map { $ml->p($_, {class => "error"}) } @err), {class => "errors"})
                  . $back;
            }
        }
        $this->run_handler("pre_process_membership", $purchase_type);

        # fees
        if ($opt{fee}) {
            my $type = $this->{input}{type} || $this->my_type();
            my $membertype = $type;
            if ($membertype =~ /^member/) {
                $membertype =~ s/^member\///;
            }
            if ($type =~ /^(guest|staff)/ && $this->{admin}) {

                # only admins can setup non-member user types
                $this->member->setdata("type", $type);
            } else {

                # VALIDATE!
                if ($purchase_type eq "New" && $this->my_type() ne "guest") {
                    $this->member->setdata("type", $type);
                } elsif ($purchase_type eq "Renew") {
                    $session{renewal_type} = $membertype;
                }
            }
            if (
                $membertype
                && (!grep(/$membertype/, $this->membership_types())
                    && $type !~ /^(guest|staff|secondary)/)
              )
            {
                $out .= $this->error("unknown membership type: $type");
            } elsif ($this->member->save()) {

                # record selected membership term
                if ($mterm) {
                    my $obj = new ExSite::ObjectMeta(
                        type => "member",
                        id   => $this->member->id()
                    );
                    $obj->meta_set("mterm", $mterm);
                    $obj->force_save();
                }

                $share{DB}->handler("user_owns", \&primary_user_owns);
                if (!$this->member->is_me() && !$this->member->is_child) {

                    # store a preliminary identity that we can use for
                    # pre-authentication before the user is fully enabled
                    $session{prelim_identity} = {
                        access   => 1,
                        name     => $this->member->name,
                        password => undef,
                        uid      => $this->member->id,
                        lookup   => $this->member->get(),
                    };
                    $share{use_prelim_identity} = $this->member->id;
                }

                # create member account
                if ($this->member->account && $this->{Contact}) {
                    $share{DB}->update(
                        "contact",
                        {account_id => $this->member->account->id},
                        {contact_id => $this->{Contact}->id}
                    );
                }

                # initialize status for new applicants
                my $mid = $this->member->id;
                if (!$uid && $this->member->use_status()) {
                    if (!$this->member->set_status_incomplete("application received from $ENV{REMOTE_ADDR}")) {
                        $out .= $this->warn($msg{"failed to set membership to incomplete $mid"});
                    }
                }
                my $fee = $this->membership_fee(
                    member        => $this->member(),
                    type          => $membertype,
                    purchase_type => $purchase_type
                );
                if (   $fee > 0
                    || &preference("Membership.secondary.must_checkout"))
                {
                    (tied %msg)->nohighlight();
                    my $item = &preference("Membership.receivable_item_name");
                    my $info = $this->receivable_info(
                        purchase_type => $purchase_type,
                        membertype    => $membertype,
                        fee           => $fee
                    );
                    $item = &substitute($msg{$item}, $info);
                    (tied %msg)->restore();
                    $out .=
                      $this->bill_member($item, $info->{description}, $fee,
                        $info->{note}, $opt{goto}, $info->{start_time}, $info->{end_time});
                } else {
                    if ($purchase_type eq "New") {
                        $out .= $this->finalize_application();
                        if (   $this->is_extended_application
                            && $this->member->is_incomplete)
                        {
                            my $owner = &preference("Membership.welcome_email.from")
                              || &preference("Membership.owner_email");
                            &ExSite::Mail::send(
                                to      => $this->member->email,
                                from    => $owner,
                                subject => &substitute(
                                    $msg{"Login instructions for [[sitename]]"},
                                    &Modules::Membership::Base::get_merge($this->member)
                                ),
                                body => &substitute(
                                    $msg{&preference("Membership.message.email_extended")},
                                    &Modules::Membership::Base::get_merge($this->member)
                                )
                            );
                        }
                    } else {
                        $out .= $this->renew_member();
                        $out .= "renewed " . $this->member()->name();
                    }
                    my $loc = $share{Page} || $share{Section};
                    if ($loc) {
                        my $ctemplate    = $loc->find("comp_membership_receipt");
                        my $receipt_html = $ctemplate->get_html()
                          || $ml->p($msg{"This membership type is complimentary (no cost)."});
                        $out .= $share{Section}->expand(html => $receipt_html);
                    }
                    if (!$this->{admin} && !$share{DB}->level()) {

                        # kill session because we are logging in and shopping cart is now invalid
                        $session{invoice} = 0;
                        delete $session{prelim_identity};
                        $share{DB}->do_login($this->member->get());
                    }
                    $out .= $this->edit_profile_menu();
                    $out .= $this->member->show();
                }

                # tie the account to this member - must do this using low-level
                # tools because the member is not yet activated and does not have
                # the security clearance to make such changes
                my $acctdata = $this->member->account->get();
                $acctdata->{member_id} = $this->member->id;
                $share{DB}->update("account", $acctdata);

                # tie the member to a primary membership
                # must force save to bypass security check
                if ($pid && !$share{DB}->authorize()) {
                    my $parent = $share{DB}->fetch("member", $pid);
                    $this->member->setdata("parent_id",    $pid);
                    $this->member->setdata("organization", $parent->{organization});
                    $this->member->force_save();
                }
            } else {
                $out .= $this->member->show_diagnostics("error", "html");
            }
        } else {

            # no fee collection - simple profile edit
            if ($this->member->save()) {

                # create member account
                if ($this->member->account && $this->{Contact}) {
                    $share{DB}->update(
                        "contact",
                        {account_id => $this->member->account->id},
                        {contact_id => $this->{Contact}->id}
                    );
                }
                $out .= $ml->p($msg{"Your profile has been updated."});
                $out .= $this->user_menu($uid);
                $out .= $this->member->show();
                if ($this->{input}{reply}) {
                    $out .= &ExSite::HTML::Button(label => $msg{"Continue"}, url => $this->{input}{reply});
                }
            } else {
                $out .= $this->member->show_diagnostics("error", "html");
            }
        }
    } else {
        $out .= $this->error($msg{"Insufficient data to process membership form."});
    }
    if (&AtError()) {
        return $ml->p(
            $msg{
"There was an issue while trying to process your form. Please correct the following errors and try again."
            }
          )
          . &ExSite::Config::show_diagnostics()
          . $ml->p(ExSite::HTML::BackButton());
    }
    return $out;
}

sub receivable_info {
    my ($this, %opt) = @_;
    my $current_expiry_date = $this->member->expirydate;
    my $start = new ExSite::Time($current_expiry_date, "sql_date");
    if ($opt{purchase_type} =~ /Renew/) {
        $session{renewal_type} = $opt{membertype};
    }
    my $next_expiry_date = $this->member->set_expiry(&preference("Membership.rollover_date"),
        &preference("Membership.early_renewal_period") || 0)
      || $current_expiry_date;
    my $end = new ExSite::Time($next_expiry_date, "sql_date");
    my $diffdays = $start->diffdays($end);

    # reset expirydate, it will be set later once receivable finalized
    $this->member->setdata("expirydate", $current_expiry_date);
    my $note = "$opt{purchase_type}:$opt{membertype}";
    my $description;
    my $merge = {
        start         => $start->write("date"),
        end           => $end->write("date"),
        start_time    => $start,
        end_time      => $end,
        fee           => $opt{fee},
        purchase_type => $opt{purchase_type},
        type          => $opt{membertype},
        name          => $this->member->name(),
        note          => $note
    };
    if ($this->run_handler("Membership_receivable_item_description", $merge)) {
        $description = $this->run_handler("Membership_receivable_item_description", $merge);
    } elsif ($current_expiry_date >= $next_expiry_date) {
        $description = $msg{"[[purchase_type]] membership ([[type]]) for [[name]] until [[end]]"};

        # any period over 20 years is considered a lifetime membership
    } elsif ($diffdays > 7300) {
        $description = $msg{"[[purchase_type]] Lifetime membership ([[type]] for [[name]])"};
    } else {
        $description = $msg{&preference("Membership.receivable_item_description")};
    }
    $description = &substitute($description, $merge);
    $merge->{description} = $description;
    return $merge;
}

# data synchronization when secondary is moved between parent records
sub move_secondary {
    my ($this, $new_pid, $old_pid) = @_;
    my $whatami = &preference("Membership.whatami") || "member";
    if ($whatami eq "organization") {
        my $new_parent = new Modules::Membership::Member(id => $new_pid);
        $this->member->setdata("organization", $new_parent->getdata("organization"));
    }
}

sub bill_member {
    my ($this, $item, $description, $cost, $note, $goto, $start, $end, $silent) = @_;
    my $out;

    # disable account switching
    $session{disable_change_account} = 1;

    # create an invoice for the membership fee
    require Modules::Finance::Receivable;
    my $inv = new Modules::Finance::Receivable();
    my $invid = $share{Page} ? $session{invoice} : 0;
    my $acct;
    if ($invid) {

        # we already have a shopping cart going
        $inv->setup(id => $invid);
        if (!$inv->is_editable()) {
            $session{invoice} = 0;
            return $this->error($msg{"Invalid invoice: starting new invoice."});
        }
        $acct = $inv->account();

        # set member attached to account if not defined
        if (!$acct->getdata("member_id")) {
            $acct->setdata("member_id", $this->self->uid);
            $acct->force_save();
        }
    } else {

        # setup a new cart
        if ($share{Page}) {
            $acct = $this->self->account();
        } else {
            $acct = $this->member->account();
        }
        $inv->setdata("account_id", $acct->id);
        my $date = new ExSite::Time;
        $inv->setdata("date",   $date->write("sql_timestamp"));
        $inv->setdata("type",   "receivable");
        $inv->setdata("status", "inactive");
        if ($share{DB}{map}->is_mapped("receivable", "member_id")) {
            $inv->setdata("member_id", $session{receivable_uid} || $share{DB}->my_uid);
        }
        $invid = $inv->save();

        if ($invid) {
            $session{invoice} = $invid;
        } else {
            $out .= $inv->show_diagnostics("error", "html");
            $out .= $share{DB}->show_diagnostics("error", "html");
            return $out;
        }
    }

    # prorate fees based on full membership term
    # determine days remaining using anniversary settings
    my $pro;
    if (
        (!$this->member->is_archived && &preference("Membership.prorated_renewal") && $note =~ /^Renew/)
        || (   &preference("Membership.prorated_application")
            && $note =~ /^New/)
      )
    {
        my $days_left   = int(($end - $start) / 86400);
        my $months_left = int($days_left / 30);
        my $t           = new ExSite::Time;
        $t->add(&preference("Membership.anniversary.length"), &preference("Membership.anniversary.period"));
        my $today          = new ExSite::Time;
        my $sec            = $today->diff($t);
        my $seconds_in_day = (24 * 60 * 60);
        my $total_days     = int($sec / $seconds_in_day);
        my $total_months   = int($total_days / 30);
        my $discount       = $cost * (($total_months - $months_left) / $total_months);
        my $descr =
          &substitute($msg{"[[months_left]] month(s) remaining in membership term"}, {months_left => $months_left});

        if ($discount && ($days_left < $total_days)) {
            $pro = {
                item        => $msg{"Prorated Discount"},
                description => $descr,
                cost        => -sprintf("%.2f", $discount),
                acctcode_id => &preference("Membership.membership.acctcode")
                  || 0,
                objtype => "member",

                # undefined so we don't finalize this member twice
                objid => undef,
                note  => "Prorated"
            };
        }
    }

    my $app;

    # application fees
    if (&preference("Membership.application_fee") && $note =~ /^New/) {
        $app = {
            item        => $msg{"Membership Application Fee"},
            description => $this->member->name(),
            cost        => &preference("Membership.application_fee"),
            acctcode_id => &preference("Membership.membership.acctcode") || 0,
            objtype     => "member",

            # undefined so we don't finalize this member twice
            objid => undef,
            note  => "Application Fee"
        };
    }

    my $cart = &preference("Membership.quickpay") ? "quickpay" : "add";
    my %item = (
        item        => $item,
        description => $description,
        cost        => $cost,
        acctcode_id => &preference("Membership.membership.acctcode") || 0,
        objtype     => "member",
        objid       => $this->member->id,
        note        => $note
    );

    if ($this->member->is_me) {

        # check for related members
        my $p = $this->member->parent;
        my $secondary;
        if (!$p) {
            $secondary = $this->member->get_child("member");
        } else {
            $secondary = $p->get_child("member");
        }
        $secondary->sort("last_name");
        if ($p) {
            $p->load;
            if (   $p->is_renewable
                && !$share{Membership}{renew_self}
                && $share{DB}->my_uid != $p->id)
            {
                $out .= $this->setup_bill(
                    uid           => $p->id,
                    purchase_type => "Renew",
                    silent        => 1
                );
                $out .= $ml->p(
                    $msg{
"In order to renew/add your own membership, the primary membership for your organization must also be renewed.  As such the fee has been added to your shopping cart."
                    }
                );
                if ($this->is_reinstate($p)) {
                    $out .= $this->reinstate($p);
                }
                $share{Membership}{renew_self} = 1;
            }
        }
        if (  !$this->{input}{renew}
            && $secondary->count > 0
            && $note =~ /^Renew/
            && &preference("Membership.renew_group"))
        {
            $silent = 1;
            my ($table, $total, $i);
            my $table .=
                $ml->colgroup(undef, {align => "right"})
              . $ml->colgroup()
              . $ml->colgroup(undef, {span => 2, align => "right"});
            my $thead =
              $ml->thead($ml->tr($ml->th(undef) . $ml->th($msg{"Name"}) . $ml->th($msg{"Expiry Date"})));
            my $tbody;
            my $input = $ml->input(
                undef,
                {
                    type     => "checkbox",
                    checked  => 1,
                    disabled => 1,
                    name     => "renew",
                    value    => $this->member->id
                }
            );
            $tbody .= $ml->tr(
                [
                    $input,
                    $ml->div($this->member->name(verbose => 1)),
                    $ml->div($this->member->expirydate() || $this->member->status())
                ]
            );
            while (my $sec = $secondary->next()) {
                next if ($sec->is_me());
                if ($sec->is_renewable()) {
                    my $input = $ml->input(
                        undef,
                        {
                            type  => "checkbox",
                            name  => "renew",
                            value => $sec->id()
                        }
                    );
                    $tbody .= $ml->tr([$input, $ml->div($sec->name), $ml->div($sec->expirydate() || $sec->status())]);
                } else {
                    my $input = "&nbsp;x";
                    $tbody .= $ml->tr([$input, $ml->div($sec->name), $ml->div($sec->status())]);
                }
            }
            if ($tbody) {
                my $select_js = $ml->a(
                    $msg{"select all"},
                    {
                        href  => "#",
                        id    => "renew_all",
                        class => "button"
                    }
                  )
                  . "&nbsp;&nbsp;"
                  . $ml->a(
                    $msg{"select none"},
                    {
                        href  => "#",
                        id    => "renew_none",
                        class => "button"
                    }
                  );
                $select_js .= $ml->script(
                    "\$(function () {
\$('#renew_all').click(function () {
var name = 'renew';
var checkBoxes = \$('input[name=' + name + ']');
checkBoxes.prop('checked', true);
})
\$('#renew_none').click(function () {
var name = 'renew';
var checkBoxes = \$('input:not(:disabled)[name=' + name + ']');
checkBoxes.prop('checked', false);
})});", {type => "text/javascript"}
                );
                $tbody = $ml->tbody($select_js . $tbody);
                my $label = $share{DB}{map}->get_column_attr("member", "organization", "label");
                $out .= $this->get_template("renew_group", $this->get_section_id)
                  || &substitute($msg{&preference("Membership.message.renew_group")}, {label => $label});

                my $form = $ml->table($table . $thead . $tbody, {class => "Receivable", cellpadding => 5});
                $form .= $ml->input(undef, {type => "submit", value => $msg{"Renew"}});
                my $action = $this->link(pro => "renew_group");
                $out .= $ml->form(
                    $form,
                    {
                        method => "post",
                        url    => $this->link(),
                        action => $action
                    }
                );
            }
        }
    }
    if ($app && (!$inv->can("has_item") || !$inv->has_item($app))) {
        my $i = $inv->add_item($app);
        if (!$i->id) {
            $out .= $i->show_diagnostics("error", "html");
        }
    }
    my $i;
    if ((!$inv->can("has_item") || !$inv->has_item(\%item))) {
        $i = $inv->add_item(\%item);
        if (!$i->id) {
            $out .= $i->show_diagnostics("error", "html");
        }
    }
    if ($pro && (!$inv->can("has_item") || !$inv->has_item($pro))) {
        my $i = $inv->add_item($pro);
        if (!$i->id) {
            $out .= $i->show_diagnostics("error", "html");
        }
    }
    $out .= "<!--&Pay()-->\n" unless ($silent);

    my $stat = eval "require Modules::Organization";
    if (  !$silent
        && $stat
        && &ExSite::Module::get_module("Organization")
        && &preference("Organization.continue_application"))
    {
        $out .= "<!--&Organization(cmd=continue_application)-->";
    }

    $this->run_handler("update_membership_invoice", $inv);

    if ($share{Page}) {

        # store any useful identity info in our session for
        # the shopping cart to make use of later
        if (!$acct->get_contact() && !exists $session{customer_info}) {
            $session{customer_info} = {
                name  => $this->member->name(),
                email => $this->member->getdata("email"),
            };
        }
    } else {
        $out .= $this->admin_bill_member($inv);
    }
    $session{continue_shopping_url} = -1;
    delete $session{Membership_renewal_form};
    return $out;
}

sub admin_bill_member {
    my ($this, $inv) = @_;
    my $out;
    my $acct = $this->member->account;
    if (!$inv) {
        $inv = new Modules::Finance::Receivable(id => $this->{input}{inv});
    }
    if (   !&preference("Membership.contact_form.enabled")
        && !$acct->get_contact)
    {
        $out .= $this->new_contact(
            action => $this->link(
                pro => "admin_bill_member",
                uid => $this->member->id,
                inv => $inv->id
            )
        );
        return $out if ($this->{input}{form} ne "contact");
    }
    my $sc_count = $inv->add_surcharges();
    $out .= $ml->p(
        "Created membership invoice. ("
          . $ml->a(
            "view",
            {
                    href => "javascript:popup_large('"
                  . "$config{server}{CGIpath}/$config{prog}{ctrlpanel}/Pay?inv="
                  . $inv->id
                  . "&section_id="
                  . $this->get_section_id . "')"
            }
          )
          . ")"
    );
    $out .=
      $ml->p("You may want to add contact info or make other profile updates at this time, using the links below.");

    # activate
    if (!$inv->activate) {
        $out .= &ExSite::Config::show_diagnostics();
        $out .= $this->error("Failed to activate invoice.");
    } else {

        # finish session
        delete $session{invoice};
    }

    # extra editing/info
    $out .= $this->edit_profile_menu();

    # if admin has not already set an expirydate
    if (!$this->member->expirydate) {

        # set expiry date
        $this->member->set_expiry(&preference("Membership.rollover_date"),
            &preference("Membership.early_renewal_period") || 0);
        $this->member->save();
    }
    return $out;
}

# edit contact info

sub edit_contact {    # process an edit contact form
    my $this = shift;
    my $stat = $this->run_handler("Membership_edit_contact");
    return $stat if ($stat);
    my $out;
    if (keys %{$this->{post}} > 0) {
        delete $this->{post}{form};
        delete $this->{post}{submit_action};
        my $c = new Modules::ID::Contact(id => $this->{post}{contact_id});
        my $errcnt = &AtError();
        $out .= $c->do_edit();
        if (!&AtError($errcnt)) {

            # no problems in update - redirect back to the edit_contacts page
            my $p = $share{Page} || $share{ML};
            if ($p) {
                $p->redirect($this->link(pro => "edit_contacts", __plaintext => 1));
            }
        } else {
            $out .= &ExSite::Config::show_diagnostics();
            $out .= $ml->p($ml->a("Try again", {href => $this->link(pro => "edit_contacts")}));
        }
    }
    return $out;
}

sub edit_contacts {    # display edit contact form(s)
    my ($this, %opt) = @_;
    my $ml  = &get_obj("ML");
    my $out = $ml->script(
        undef,
        {
            src  => "$config{server}{HTMLpath}/_ExSite/js/misc.js",
            type => "text/javascript"
        }
    );
    my $member = $this->member;
    if ($member->ok) {
        if ($member->allow_edit()) {
            $out .= $ml->h1(&substitute($msg{preference("Membership.heading.address_cards")}, {name => $member->name}));
            my $contacts = $member->contacts();
            my ($outshow, $outedit);
            if ($contacts) {
                if ($contacts->count) {
                    my $edit_instructions = "Click on 'Click to update' to edit an address card.";
                    $outshow .= $ml->p($msg{$edit_instructions});
                } else {
                    $outshow .= $ml->p($msg{"No contact information on file."});
                }
                while (my $c = $contacts->next()) {

                    my $privacy = $c->showdata("privacy") || "administrators";
                    my $type    = ucfirst $msg{$c->getdata("type")};
                    my $uri     = new ExSite::URI();
                    $uri->setup(
                        "$config{server}{CGIpath}/ctrl-panel.cgi/Membership?section_id=" . $this->get_section_id);
                    $uri->query(
                        pro => "del_contact",
                        cid => $c->id,
                        uid => $member->id
                    );
                    my $del_link = $uri->write_full();
                    $outshow .= $ml->div(
                        $ml->h2(
                            &substitute($msg{"[[type]] (shown to [[privacy]])"}, {type => $type, privacy => $privacy})
                          )
                          . $c->show()
                          . $ml->a(
                            $msg{"Click to update"},
                            {href => "#", class => "ContactEditTip", onclick => "showcontactform(" . $c->id() . ")"}
                          ),
                        {
                            class => "ContactPreview",
                            id    => "Contact" . $c->id(),
                        }
                    );

                    my $contact_form_tips = &preference("Membership.contact_form.custom_tips") || $ml->ul(
                        [
                            $msg{"Most fields are optional.  Enter only what you need to."},
                            $msg{
"You can provide multiple address cards - for example, one for home and one for office.  You do not have to put all your contact information into one form."
                            },
                            $msg{
"Privacy settings determine who is allowed to see the information on this address card:  website administrators, fellow members of your organization, or members of the general public."
                            },
                            $msg{
"You can have multiple address cards with different privacy settings.  For example, one entry for administrators with full contact information, and another entry for the public, with only your city/country and website."
                            }
                        ]
                    );

                    my @hide =
                      split(/,/, &preference("Membership.contact_form.hide"))
                      || ("account_id", "location_id", "section_id");
                    $outedit .= $ml->div(
                        &ExSite::HTML::HelpPopup(
                            label   => $msg{"Tips"},
                            message => $contact_form_tips
                          )
                          . $ml->h2($msg{"Update Contact Information"})
                          . $c->edit(
                            action => $this->link(pro => "edit_contact"),
                            hide   => \@hide
                          )
                          . $ml->p(
                            $ml->a(
                                $msg{"Delete this contact record"},
                                {
                                    class => "ContactDeleteLink",
                                    href  => "javascript:confirm_custom('"
                                      . &substitute($msg{"Are you sure you want to delete [[type]] address card?"},
                                        {type => $type})
                                      . "','"
                                      . $del_link . "')"
                                }
                            )
                          )
                          . $ml->p(
                            $ml->a(
                                "&lt;&lt; " . $msg{"Back to address cards"},
                                {
                                    class => "ContactBackLink",
                                    href  => "javascript:hidecontactform(" . $c->id() . ")"
                                }
                            )
                          ),
                        {
                            id    => "ContactEdit" . $c->id(),
                            class => "ContactEdit",
                            style => "display:none"
                        }
                    );
                }
            } else {
                $out .= $ml->p($msg{"No contact information on file."});
            }
            $outshow .= $ml->p(
                $ml->a(
                    $msg{"Add new address card"},
                    {
                        href  => $this->link(pro => "new_contact"),
                        class => "ContactAddLink"
                    }
                )
            );
            $out .= $ml->div($outshow, {id => "ContactPreviewBlock"}) . $ml->div($outedit, {id => "ContactEditBlock"});
            $out .= $ml->script(
                "function showcontactform(id) {
var loc = document.getElementById(\"ContactEdit\" + id);
loc.style.display = \"block\";
var loc = document.getElementById(\"ContactPreviewBlock\");
loc.style.display = \"none\";
}
function hidecontactform(id) {
var loc = document.getElementById(\"ContactEdit\" + id);
loc.style.display = \"none\";
var loc = document.getElementById(\"ContactPreviewBlock\");
loc.style.display = \"block\";
}
"
            );
            $out .= $ml->noscript($msg{"Please enable Javascript to edit your contacts."});
        } else {
            $out .= $this->error($msg{"Permission denied."});
        }
    } else {
        $out .= $this->show_diagnostics("error", "html");
    }
    if (!$share{Page}) {
        return &ExSite::HTML::BasicBox(pane => $ml->div($out, {class => "edit_contacts"}));
    }
    return $ml->div($out, {class => "edit_contacts"});
}

sub new_contact {
    my ($this, %opt) = @_;
    my $ml = &get_obj("ML");
    my $out;
    my $member = $this->member;
    if ($member->ok) {
        if ($member->allow_edit()) {
            my $c = new Modules::ID::Contact();
            if (keys %{$this->{post}} > 0
                && $this->{post}{form} eq "contact")
            {
                delete $this->{post}{form};
                delete $this->{post}{submit_action};
                my $acct = $this->member->account;
                if ($acct) {
                    $out .= $c->do_make(account_id => $acct->id);
                } else {
                    $out .=
                      $this->error($msg{"No account is defined for this member - cannot add contact information."});
                }
                $out .= $ml->p($ml->a($msg{"Add another contact card."}, {href => $this->link()}));
            } else {
                my $contact_form_tips = &preference("Membership.contact_form.custom_tips") || $ml->ul(
                    [
                        $msg{"Most fields are optional.  Enter only what you need to."},
                        $msg{
"You can provide multiple address cards - for example, one for home and one for office.  You do not have to put all your contact information into one form."
                        },
                        $msg{
"Privacy settings determine who is allowed to see the information on this address card:  website administrators, fellow members of your organization, or members of the general public."
                        },
                        $msg{
"You can have multiple address cards with different privacy settings.  For example, one entry for administrators with full contact information, and another entry for the public, with only your city/country and website."
                        }
                    ]
                );

                $out = $ml->h1(&substitute($msg{"New address card for [[name]]"}, {name => $member->name}));
                $out .= &ExSite::HTML::HelpPopup(
                    label   => $msg{"Tips"},
                    message => $contact_form_tips
                );
                my $action = $opt{action} || $this->link();
                if ($c->approve("insert")) {
                    $out .= $c->contact_form(
                        action => $action,
                        hide   => ["account_id", "location_id", "section_id", "web"],
                        extra => {form => "contact"}
                    );
                    if ($opt{help}) {
                        $out .= $opt{help};
                    }
                }
            }
        } else {
            $out .= $this->error($msg{"Permission denied."});
        }
    } else {
        $out .= $this->show_diagnostics("error", "html");
    }
    return $ml->div($out, {class => "new_contact"});
}

sub delete_contact {
    my ($this, %opt) = @_;
    my $ml = &get_obj("ML");
    my $out;
    my $member = $this->member;
    if ($member->ok) {
        if ($member->allow_edit()) {
            my $cid = $this->{input}{cid};
            my $c = new Modules::ID::Contact(id => $cid);
            if ($c->getdata("account_id") == $member->account->id) {
                $c->delete();
                my $page = $share{Page} || $share{ML};
                $page->redirect(
                    $this->link(
                        pro         => "edit_contacts",
                        cid         => undef,
                        __plaintext => 1
                    )
                );
            } else {
                $out .= $this->error($msg{"Permission denied."});
            }
        } else {
            $out .= $this->error($msg{"Permission denied."});
        }
    } else {
        $out .= $member->show_diagnostics("error", "html");
    }
    return $out;
}

sub delete_member {
    my ($this) = @_;
    my $out;
    my $member = $this->member;
    if ($member->ok) {
        if ($member->allow_edit()) {
            my $stat = $member->delete();
            my $name = $member->name();
            $out .= &ExSite::HTML::ErrorBox(pane => $ml->p("$name has been deleted."));
            $out .= &ExSite::HTML::Button(label => "Continue", url => $this->link(pro => undef, uid => undef));
            $out .= &ExSite::HTML::Button(
                label => "Undo deletion",
                url   => $this->link(pro => "undel_member", undel => $member->id())
            );
        } else {
            $out .= $this->error($msg{"Permission denied."});
        }
    } else {
        $out .= $member->show_diagnostics("error", "html");
    }
    return $out;
}

sub undel_member {
    my ($this, $uid) = @_;
    my $out;
    $share{DB}->undelete("member", $uid);
    return;
}

# feature a random member from a list of eligible members

sub feature {
    my ($this)     = @_;
    my $section_id = $this->get_section_id;
    my $members    = $this->run_handler("Membership_featured_members")
      || $share{DB}->get_query("all members with status", $section_id);
    my $template = $this->get_template("membership_feature", $section_id)
      || "[[profile_name]]";
    return if (!scalar @$members);
    my $i;
    while (scalar @$members) {
        $i++;
        my $randomIndex = int(rand(scalar @$members - 1));
        my $member = new Modules::Membership::Member(id => $members->[$randomIndex]->{member_id});
        if (!$member->is_valid_member || !$member->is_visible) {
            splice(@$members, $randomIndex, 1);
            next;
        }
        my %profile = $member->get_profile_data();
        $profile{profile_name} = $member->name;
        $profile{profile_link} =
          $ml->a($member->name, {href => $this->link(pro => "member", uid => $member->id)});
        my $section = new ExSite::Section(id => $section_id);
        my $out = &substitute($template, \%profile);
        return $section->expand(html => $out);
    }
    return;
}

# handler for membership type inputs

sub profile_input_exsite {
    my ($db, %opt) = @_;
    if ($opt{table} eq "member" && $opt{column} eq "type") {
        my @value        = $profile->user_types($profile->get_section_id());
        my $hidden_types = &preference("Membership.hidden_member_types");
        if (ref $hidden_types eq "ARRAY") {
            push(@value, @$hidden_types);
        }
        if (@value > 0) {
            $opt{type}      = "select";
            $opt{options}   = \@value;
            $opt{nullvalue} = "== select ==";
            $opt{prompt}    = "Membership Type";
            return $db->input_html(%opt);
        }
    }
    return;
}

# handler for allowing primaries to edit secondary profile info

sub primary_user_owns {
    my ($db, $table, $record) = @_;
    return if ($db->is_manager());
    if ($table =~ /^(member|contact|account)$/) {
        foreach my $type (("secondary", "parent")) {
            my $method;
            $method = "is_child"  if ($type eq "secondary");
            $method = "is_parent" if ($type eq "parent");
            if (&preference("Membership.$type.edit")) {
                my $m;
                if ($table eq "contact") {

                    # primary members can manage secondaries' contact info
                    my $c =
                      ref $record
                      ? new Modules::ID::Contact(data => $record)
                      : new Modules::ID::Contact(id   => $record);
                    my $uid = $c->uid();
                    if ($uid) {
                        $m = new Modules::Membership::Member(id => $uid);
                        return ($m->$method || $m->is_me());
                    }
                } elsif ($table eq "account") {

                    # primary members can manage secondaries' account info
                    my $a =
                      ref $record
                      ? new Modules::Finance::Account(data => $record)
                      : new Modules::Finance::Account(id   => $record);
                    my $uid = $a->getdata("member_id");
                    if ($uid) {
                        $m = new Modules::Membership::Member(id => $uid);
                        return ($m->$method || $m->is_me());
                    }
                } else {
                    $m =
                      ref $record
                      ? new Modules::Membership::Member(data => $record)
                      : new Modules::Membership::Member(id   => $record);
                    return ($m->$method || $m->is_me());
                }
            }
        }
    }
    return;
}

sub setup_queries {
    my $this = shift;
    my $db   = $share{DB};
    my @mcol = map { "m." . $_ } $this->get_member_columns(1);
    my $sort = $this->sort_field();
    my $mcol = join ",", @mcol;

    my $section_sql;
    if ($this->scope() eq "local") {
        $section_sql = "and m.section_id = ?";
    }

    # members with ctime who have been or are active
    $db->set_query(
        "applicants by month",
        (
            sql => &preference("Membership.queries.applicants_by_month")
              || "select $mcol,max(ri.receivable_id) as receivable_id,ri.item,sum(ri.cost) as cost,substr(r.date from 1 for 7) yearmonth,(select status from member_status force index (member_id) where member_status.member_id=m.member_id order by member_status_id desc limit 1) status from receivable_item ri, receivable r, member m where section_id = ? and ri.objid = m.member_id and ri.receivable_id = r.receivable_id and ri.note like 'New:%' and r.status = 'active' group by ri.receivable_id,ri.objid order by ri.receivable_id desc",
            nparam => 1,
            mode   => "r",
            keys   => ["member"],
        )
    );

    $db->set_query(
        "renewals by month",
        (
            sql => &preference("Membership.queries.renewals_by_month")
              || "select $mcol,ri.receivable_id,ri.item,sum(ri.cost) as cost,substr(r.date from 1 for 7) yearmonth,(select status from member_status force index (member_id) where member_status.member_id=m.member_id order by member_status_id desc limit 1) status from receivable_item ri, receivable r, member m where section_id = ? and ri.objid = m.member_id and ri.receivable_id = r.receivable_id and ri.note like 'Renew:%' and r.status = 'active' group by ri.receivable_id,ri.objid order by ri.receivable_id desc",
            nparam => 1,
            mode   => "r",
            keys   => ["member"],
        )
    );

    # all member records
    $db->set_query(
        "all members",
        (
            sql    => "select $mcol from member m where m.section_id=? order by $sort",
            nparam => 1,
            mode   => "r",
            keys   => ["member"],
        )
    );

    # all primary member records
    $db->set_query(
        "all primary members",
        (
            sql    => "select $mcol from member m where m.section_id=? and m.parent_id = 0 order by $sort",
            nparam => 1,
            mode   => "r",
            keys   => ["member"],
        )
    );

    # all primary member records
    $db->set_query(
        "all visible members",
        (
            sql    => "select $mcol from member m where m.parent_id = 0 $section_sql order by $sort",
            nparam => $this->scope() eq "local" ? 1 : 0,
            mode   => "r",
            keys   => ["member"],
        )
    );

    $db->set_query(
        "all active primary members",
        (
            sql =>
"select $mcol from member m where (select status from member_status force index (member_id) where member_status.member_id=m.member_id order by member_status_id desc limit 1) = 'active' and m.parent_id = 0 $section_sql order by $sort",
            nparam => $this->scope() eq "local" ? 1 : 0,
            mode   => "r",
            keys   => ["member"],
        )
    );

    # all member records with status
    $db->set_query(
        "all members with status",
        (
            sql =>
"select $mcol,member_status.status from member m left join member_status on member_status.member_status_id=(select max(ms.member_status_id) from member_status ms where ms.member_id=m.member_id) where m.section_id = ? order by $sort",
            nparam => 1,
            mode   => "r",
            keys   => ["member", "member_status"],
        )
    );

    # all member records by type with status
    $db->set_query(
        "all members by type with status",
        (
            sql =>
"select $mcol,member_status.status from member m left join member_status on member_status.member_status_id=(select max(ms.member_status_id) from member_status ms where ms.member_id=m.member_id) where m.section_id = ? and m.type = ? order by $sort",
            nparam => 2,
            mode   => "r",
            keys   => ["member", "member_status"],
        )
    );

    # all association member records with status
    $db->set_query(
        "all association members with status",
        (
            sql =>
"select $mcol,c.address,c.city,c.provstate,c.country,c.pcode,c.phone1,c.phone2,c.fax,c.email contact_email,c.web,c.privacy,(select status from member_status force index (member_id) where member_status.member_id=m.member_id order by member_status_id desc limit 1) status from member m left join account a on a.member_id = m.member_id left join contact c on c.account_id = a.account_id where m.section_id=? and m.type like 'member%' group by member_id order by $sort",
            nparam => 1,
            mode   => "r",
            keys   => ["member", "member_status", "contact"],
        )
    );

    # members to expire
    $db->set_query(
        "members to expire",
        (
            sql =>
"select $mcol,(select status from member_status force index (member_id) where member_status.member_id=m.member_id order by member_status_id desc limit 1) status from member m where m.section_id=? and m.type like 'member%' and expirydate <= curdate() and (select status from member_status force index (member_id) where member_status.member_id=m.member_id order by member_status_id desc limit 1) in ('active','pending')",
            nparam => 1,
            mode   => "r",
            keys   => ["member", "member_status"],
        )
    );

    # members to archive
    $db->set_query(
        "members to archive",
        (
            sql =>
"select $mcol,(select status from member_status force index (member_id) where member_status.member_id=m.member_id order by member_status_id desc limit 1) status from member m where m.section_id=? and m.type like 'member%' and expirydate < ? and (select status from member_status force index (member_id) where member_status.member_id=m.member_id order by member_status_id desc limit 1) != 'archived'",
            nparam => 2,
            mode   => "r",
            keys   => ["member", "member_status"],
        )
    );

    # members to trash - abandoned applications and unapproved members past expiry
    $db->set_query(
        "members to trash",
        (
            sql =>
"select $mcol,(select status from member_status force index (member_id) where member_status.member_id=m.member_id order by member_status_id desc limit 1) status from member m where m.section_id=? and m.type like 'member%' and (expirydate is null or expirydate < curdate()) and TIMESTAMPADD(DAY,60,m.ctime) < curdate() and (select status from member_status force index (member_id) where member_status.member_id=m.member_id order by member_status_id desc limit 1) in ('incomplete') and (select note from member_status where member_status.member_id=m.member_id order by member_status_id desc limit 1) like 'application received%'",
            nparam => 1,
            mode   => "r",
            keys   => ["member", "member_status"],
        )
    );

    $db->set_query(
        "member fulltext search",
        (
            sql =>
"select $mcol,(select status from member_status force index (member_id) where member_status.member_id=m.member_id order by member_status_id desc limit 1) status,match (first_name,last_name,email,organization) against (? in boolean mode) as relevance from member m where m.section_id=? and match (first_name,last_name,email,organization) against (? in boolean mode) or m.member_id = ? order by relevance desc",
            nparam => 4,
            mode   => "r",
            keys   => ["member", "member_status"],
        )
    );

    $db->set_query(
        "member search not fulltext",
        (
            sql =>
"select $mcol,(select status from member_status force index (member_id) where member_status.member_id=m.member_id order by member_status_id desc limit 1) status from member m where m.section_id=? and concat(first_name,last_name,organization) like ? or m.member_id = ?",
            nparam => 3,
            mode   => "r",
            keys   => ["member", "member_status"],
        )
    );

    $db->set_query(
        "members by name with status and wildcard not fulltext",
        (
            sql =>
"select $mcol,(select status from member_status force index (member_id) where member_status.member_id=m.member_id order by member_status_id desc limit 1) status from member m where m.section_id=? and (first_name = ? or last_name = ? or organization = ?)",
            nparam => 4,
            mode   => "r",
            keys   => ["member", "member_status"],
        )
    );

    # all meta data
    $db->set_query(
        "all member metadata",
        (
            sql =>
              "select m.type,a.* from member_attribute a, member m where a.member_id=m.member_id and m.section_id=?",
            nparam => 1,
            mode   => "r",
            keys   => ["member", "member_attribute"],
        )
    );

    # known membership types
    $db->set_query(
        "known membership types",
        (
            sql    => "select distinct type from member where section_id = ?",
            nparam => 1,
            mode   => "r",
            keys   => ["member"],
        )
    );

    # all member contact data
    $db->set_query(
        "all member contacts",
        (
            sql =>
"select m.member_id,m.first_name,m.middle_name,m.last_name,c.* from member m, account a, contact c where a.member_id=m.member_id and c.account_id=a.account_id and m.section_id=?",
            nparam => 1,
            mode   => "r",
            keys   => ["member", "account", "contact"],
        )
    );

    # members by attribute value with set datatype
    $db->set_query(
        "members by attribute value in set",
        (
            sql =>
"select $mcol, member_attribute_id, a.value value from member m,member_attribute a where m.member_id = a.member_id and name = ? and locate(?, replace(a.value,'; ',',')) and visibility = 'visible' and type != 'guest' $section_sql order by $sort",
            nparam => $this->scope() eq "local" ? 3 : 2,
            mode => "r",
            keys => ["member", "member_attribute"],
        )
    );

    # count of visible members by meta value
    my @visible_status = $this->visible_status();
    @visible_status = map { "'$_'" } @visible_status;
    my $status_list = join(",", @visible_status);
    my $type_sql;
    $type_sql = " and m.type in " . $this->fetch_member_type_sql()
      if ($this->fetch_member_type_sql);
    $db->set_query(
        "count of visible members by meta value",
        (
            sql =>
"select m.member_id, m.section_id, member_attribute_id, a.value, count(*) as count,(select status from member_status force index (member_id) where member_status.member_id=m.member_id order by member_status_id desc limit 1) status from member m,member_attribute a where m.member_id = a.member_id and name = ? and visibility = 'visible' and type != 'guest' and ((select status from member_status force index (member_id) where member_status.member_id=m.member_id order by member_status_id desc limit 1) is NULL or (select status from member_status force index (member_id) where m.member_id > 0 and member_status.member_id=m.member_id $section_sql $type_sql order by member_status_id desc limit 1) in ($status_list)) group by value order by member_attribute_id",
            nparam => $this->scope() eq "local" ? 2 : 1,
            mode => "r",
            keys => ["member", "member_attribute", "member_status"],
        )
    );

    # revenues report
    $db->set_query(
        "dues revenues",
        (
            sql =>
"select receivable.*,receivable_item.note,receivable_item.item,receivable_item.fulfilled_on,account.account_id,account.name account_name,member.member_id,member.type,member.first_name,member.last_name,member.member_id,member.email,member.organization,member.expirydate,receivable.date,receivable.receivable_id,receivable_item.cost from member,account,receivable,receivable_item where member.section_id = ? and account.account_id=receivable.account_id and receivable.receivable_id=receivable_item.receivable_id and receivable.status='active' and member.member_id=receivable_item.objid and receivable_item.objtype='member' order by receivable_item_id",
            nparam => 1,
            mode   => "r",
            keys   => ["account", "receivable", "receivable_item", "member"],
        )
    );

    # revenues report by date
    $db->set_query(
        "dues revenues by date",
        (
            sql =>
"select receivable.*,receivable_item.note,receivable_item.item,receivable_item.fulfilled_on,account.account_id,account.name account_name,(select status from member_status force index (member_id) where member_status.member_id=member.member_id order by member_status_id desc limit 1) status,member.member_id,member.type,member.first_name,member.last_name,member.member_id,member.email,member.organization,member.expirydate,receivable.date,receivable.receivable_id,receivable_item.cost from member,account,receivable,receivable_item where member.section_id = ? and receivable.date > ? and receivable.date < ? and account.account_id=receivable.account_id and receivable.receivable_id=receivable_item.receivable_id and receivable.status='active' and member.member_id=receivable_item.objid and receivable_item.objtype='member' order by receivable_item_id",
            nparam => 3,
            mode   => "r",
            keys   => ["account", "receivable", "receivable_item", "member"],
        )
    );

    if ($share{DB}{map}->is_mapped("member_category")) {
        $db->set_query(
            "all member categories",
            sql =>
"select m.member_id,mc.member_category_id,mc.category from member m,member_category mc, member_category_join mcj where mc.member_category_id = mcj.member_category_id and mcj.member_id = m.member_id order by m.member_id;",
            nparam => 0,
            mode   => "r",
            keys   => ["member_category", "member_category_join"]
        );
    }

    # email dupe check
    $db->set_query(
        "email dupe check",
        (
            sql =>
"select * from member where section_id = ? and email = ? and (type like 'member/%' or type like 'secondary') and access >= 1",
            nparam => 2,
            mode   => "r",
            keys   => ["member"],
        )
    );
}

sub contact_type {
    my $this         = shift;
    my $contact_type = $session{Membership_contact_type}
      || &preference("Membership.directory.contacts_type");
    return $contact_type;
}

# contact = contact match hash; automatically sets contact_data to true
# member = member match hash
# sort = field to sort on
# type = single or comma delimited list of member types
sub fetch_match_member {
    my ($this, %opt) = @_;
    if ($this->scope() eq "local") {
        $opt{member}{"section_id"} = $this->get_section_id();
    }
    my (%member_match, %contact_match);
    my $contact_type = $opt{contact}{type} || $this->contact_type();
    delete $opt{contact}{type};
    my $sort = $opt{sort} ? $opt{sort} : $this->sort_field($opt{type});
    %member_match = %{$opt{member}} if (keys %{$opt{member}});

    # default parameters applied to member and member_attribute records outside of control panel
    if (!$this->{admin}) {
        my $default_match = $this->run_handler("Membership_default_match")
          || &preference("Membership.directory.default_member_match");
        if ($default_match && keys %$default_match) {
            %member_match = (%$default_match, %member_match);
        }
    }
    foreach my $key (keys %member_match) {
        my $datatype = $this->get_datatype($key);
        if ($share{DB}->{map}->is_mapped("member", $key)) {
            $member_match{"m." . $key} = $member_match{$key};
            if ($key eq $sort) {
                $sort = "m." . $sort;
            }
            delete $member_match{$key};

            # 'Other' value in olist context represents anything other than the regexp list
        } elsif ($datatype =~ /olist/ && $member_match{$key} eq "Other") {
            my $regexp = $share{DB}->{map}->get_datatype_attr($datatype, "regexp");
            $member_match{$key} = '^' . $regexp . '$';
        }
    }
    %contact_match = %{$opt{contact}} if (keys %{$opt{contact}});
    my %match = (%contact_match, %member_match);

    # compose where regexp statement for matching the sort field on non alphabetical character
    my $other_sql;
    if ($match{$sort} eq "Other") {
        delete $match{$sort};
        $other_sql = " and $sort not regexp '^[a-zA-Z].*'";
    }
    return if (!keys %match);
    my @params;
    my @columns = $this->get_member_columns(1);
    my @mcol    = map { "m." . $_ } @columns;
    my $mcol    = join ",", @mcol;
    my $sql     = "select $mcol";

    # add attribute fields
    my @meta = $this->get_visible_meta();
    my (@extracol, $join);
    my $nattr = 1;
    foreach my $meta (@meta) {
        push @extracol, "a$nattr.value $meta";
        if ($join) {
            $join =
              "($join) left join member_attribute a$nattr on a$nattr.member_id=m.member_id and a$nattr.name='$meta'";
        } else {
            $join =
              "member m left join member_attribute a$nattr on a$nattr.member_id=m.member_id and a$nattr.name='$meta'";
        }
        $nattr++;
    }
    $sql .= "," . join(",", @extracol) if (@extracol);
    foreach my $key (keys %match) {
        my $datatype = $this->get_datatype($key);
        push @params, $match{$key} if ($datatype !~ /^(set:)/);
    }

    my @visible_status = $this->visible_status(%opt);
    @visible_status = map { "'$_'" } @visible_status;
    my $status_list = join(",", @visible_status);

    # append having statement
    my ($having, @params_set) = $this->sql_locate(\%match);
    $having = "having " . $having if ($having);
    my $where = $share{DB}->sql_condition(\%match);
    push @params, @params_set;
    $where  = $this->replace_name_with_alias(string => $where,  meta => \@meta);
    $having = $this->replace_name_with_alias(string => $having, meta => \@meta);
    $where .= ' and m.type in ' . $this->fetch_member_type_sql($opt{type})
      if ($this->fetch_member_type_sql($opt{type}));
    my $id_sql =
      &preference("Membership.directory.show_secondary")
      ? "if(m.type='secondary',m.parent_id,m.member_id)"
      : "m.member_id";

    if ($opt{status}) {
        $where .=
" and (select status from member_status force index (member_id) where member_status.member_id=$id_sql order by member_status_id desc limit 1) = '$opt{status}'";
    } elsif ($status_list) {
        $where .=
" and (select status from member_status force index (member_id) where member_status.member_id=$id_sql order by member_status_id desc limit 1) in ($status_list)";
    }
    $where .= " $other_sql";

    # add status column
    $sql .=
",(select status from member_status force index (member_id) where member_status.member_id=m.member_id order by member_status_id desc limit 1) status ";

    # add contact columns if needed
    if (keys %contact_match || $contact_type) {
        $join .= " member m" if (!scalar @meta);
        $join .= " left join account a on a.member_id = m.member_id left join contact c on c.account_id = a.account_id";
        $sql .=
",c.address contact_address,c.city contact_city,c.provstate contact_provstate,c.country contact_country,c.pcode contact_pcode,c.phone1 contact_phone1,c.phone2 contact_phone2,c.fax contact_fax,c.email contact_email,c.web contact_web,c.privacy contact_privacy ";
    }

    # tables
    $sql .= $join ? "from $join" : "from member m";

    # condition
    # filter contacts that are private by current user
    if (keys %contact_match || $contact_type) {
        my $privacy;
        if (!$share{DB}->is_manager) {
            if ($share{DB}->is_member()) {
                $privacy = "('public','members')";
            } else {
                $privacy = "('public')";
            }
        }
        $where .= " and c.privacy in $privacy"
          if ( $privacy
            && &preference("Membership.directory.use_contact_privacy"));
        $where .= " and (c.type = '$contact_type' or c.type is null)"
          if ($contact_type ne "any" && $share{Page});
    }
    $sql .= " $where group by m.member_id $having order by $sort";
    my $per_page = $opt{limit} || &preference("Membership.records_per_page") || 400;
    my $offset = ($this->page() - 1) * $per_page;
    if (!$opt{nolimit}) {
        $sql .= " limit $offset,$per_page";
    }
    my $records = $share{DB}->custom_query($sql, @params);

    # flag that we have prefetched metadata
    $share{Membership}{prefetch} = 1;
    my $stat = $this->run_handler("Membership_post_fetch_match", $records);
    $records = $stat if ($stat);
    return wantarray ? @$records : $records;
}

sub visible_status {
    my ($this, %opt) = @_;

    # visible status defines list of visible statuses for indexes
    # if preference is any or we are in control panel then status is ignored
    my $visible_status;
    if (defined($opt{visible_status})) {
        $visible_status = $opt{visible_status};
    } else {
        if (   &preference("Membership.visible_status") eq "any"
            || $this->{admin})
        {
            $visible_status = "";
        } else {
            $visible_status = &preference("Membership.visible_status");
        }
    }
    my @visible_status = split(/,/, $visible_status);
    return wantarray ? @visible_status : \@visible_status;
}

# pagination used for reports and indexes

sub paginate {
    my ($this, $count) = @_;
    my $per_page     = &preference("Membership.records_per_page") || 400;
    my $current_page = $this->page();
    my $npages       = int($count / $per_page + 0.99);
    my @indexlink;
    for (my $page = 1 ; $page <= $npages ; $page++) {
        my $url = $this->link(
            pro  => "dir",
            type => $this->{type},
            attr => $this->{attr},
            page => $page,
            _id  => $this->{input}{_id}
        );
        my $class = ($current_page == $page) ? "PagedIndexCurrent" : undef;
        push @indexlink, $ml->a($page, {href => $url, class => $class});
    }
    if ($current_page < $npages) {
        my $next = $current_page + 1;
        my $url  = $this->link(
            pro  => "dir",
            type => $this->{type},
            attr => $this->{attr},
            page => $next,
            _id  => $this->{input}{_id}
        );
        push @indexlink, $ml->a($msg{"Next"} . "&nbsp;&gt;", {href => $url});
    }
    my $out;
    if (@indexlink > 1) {
        $out .=
          $ml->div(join("&nbsp;", @indexlink), {class => "PagedIndex"});
    }
    return $out;
}

sub page {
    my ($this) = @_;
    my $page = $this->{input}{page} || 1;
    return $page;
}

sub login {
    my ($this, $uid, $pre_auth) = @_;
    my $user = $share{DB}->fetch($config{auth}{user_table}, $uid);
    if ($user) {
        my $col = $config{auth}{user_access_column};
        if (   $pre_auth
            || $user->{$col} < $share{DB}->level()
            || ($user->{$col} == 1 && $share{DB}->level() == 1))
        {
            if ($share{DB}->is_admin()) {
                $this->{admin_uid} = $share{DB}->my_uid();
            }
            $share{DB}->clear_login;
            $share{DB}->do_login($user);
            $session{key_contact_uid}                  = $this->{input}{old_uid};
            $session{admin_uid}                        = $this->{admin_uid};
            $session{receivable_uid}                   = $this->{admin_uid};
            $session{Membership}{skip_password_change} = 1;
            &ExSite::Module::read_conf("Login");
            my $url = $this->{section}->get_url();

            if (my $pid = &preference("Login.goto")) {
                my $p = new ExSite::Page(id => $pid);
                if ($p->get && $p->get->{section_id} == $user->{section_id}) {
                    $url = $p->get_url_dynamic_full;
                }
            }
            if ($pre_auth) {
                return &redirect("$config{server}{server}$config{server}{CGIpath}/login.cgi");
            }
            return &redirect($url);
        }
    }
    return $this->error("Sorry, you cannot switch to that user ID.");
}

#
sub delete_cart_membership {
    my ($this, $item) = @_;

    # THIS IS A PAY OBJECT
    my $id       = $item->getdata("objid");
    my $inv      = $this->{invoice};
    my $itemlist = $inv->loaditems();
    my $m        = new Modules::Membership::Member(id => $id);

    # remove related child memberships and cart items
    while (my $item = $itemlist->next()) {
        next if ($item->getdata("objid") == $id);
        my $r = $share{DB}->fetch("member", $item->getdata("objid"));
        if ($r && $r->{parent_id} == $id) {
            $item->delete();
            if ($item->getdata("note") =~ /^New:/) {
                my $child = new Modules::Membership::Member(id => $r->{member_id});
                $child->delete();
            }
        }
        if ($item->getdata("note") =~ /^(Reinstate|Prorated)$/ && $item->getdata("member_id") == $id) {
            $item->delete();
        }
    }

    # remove prelim authorization + member record if new membership dues deleted
    if ($item->getdata("note") =~ /^New/) {
        my $m = new Modules::Membership::Member(id => $id);
        $m->delete();
        if ($id == $share{DB}->my_uid) {
            if (keys %{$session{prelim_identity}}) {
                delete $session{prelim_identity};
            }
        }
    }

    # remove membership dues cart item if reinstatement fee is deleted
    if ($item->getdata("note") =~ /^(Reinstate|Prorated)$/) {
        $itemlist->reset();
        while (my $item = $itemlist->next()) {
            if ($item->getdata("member_id") == $id) {
                $item->delete();
            }
        }
    }

    # remove event registrations for member-only events if renewal is removed
    if (eval "require Modules::EvtReg") {
        if ($item->getdata("note") =~ /^Renew/) {
            my $a = $m->account();
            if ($id == $share{DB}->my_uid) {
                my $out;
                $itemlist->reset();
                while (my $item = $itemlist->next()) {
                    if ($item->getdata("objtype") eq "evt_reg") {
                        &ExSite::Module::get_module("EvtReg");
                        my $reg = new Modules::Registration::Registration(id => $item->getdata("objid"));
                        my $fee = $reg->fee();
                        if ($session{Membership}{evt_renewal_pass} && $fee->getdata("access") eq "members") {
                            $out .= &Modules::EvtReg::validate_delete_item($this, $item,
                                "Membership renewal not found - [[item]] removed from cart.");
                        }
                    }
                }
                return $out if ($out);
            }
        }
    }
    return undef;
}

1;
