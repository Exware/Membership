#!/usr/bin/perl
#-----------------------------------------------------------------------
#
#   Copyright 2001-2009 Exware Solutions, Inc.  http://www.exware.com
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

package Modules::Membership::Member;

# a person is a combination of:
# account record
# member record
# extended member attributes
# contacts

use strict;
use ExSite::Config;
use ExSite::Misc;
use ExSite::Module;
use ExSite::ObjectMeta;
use ExSite::Time;
use ExSite::URI;
use ExSite::Util;
use Modules::ID::Contact;
use Modules::ID::Location;
use Modules::Finance::Account;
use Modules::Membership::Config qw(confrule);
use POSIX qw(ceil);

use vars qw(@ISA $ml);
@ISA = qw(ExSite::ObjectMeta Modules::BaseDCD);

#=== OBJECT CREATION

sub new ($%) {
    my ($this, %opt) = @_;
    my $obj = {};
    my $class = ref($this) || $this;
    bless $obj, $class;
    $obj->initialize_object;
    $obj->{metaclass} = "Modules::Membership::MemberMeta";
    $obj->setup(%opt);
    $share{DB}{map}->load_submaps("Finance");
    $share{DB}{map}->load_submaps("AddrBook");
    $share{DB}{map}->load_submaps("Membership");
    $share{DB}{map}->load_submaps("MemberDirectory");
    $ml = &get_obj("ML");

    if (!$share{Membership}{Config}) {
        $share{Membership}{Config} = new Modules::Membership::Config;
    }
    return $obj;
}

sub meta {
    my $this   = shift;
    my $member = shift;
    return $this->{meta} if ($this->{meta});
    my $class = $this->{metaclass};
    if (eval "require $class") {

        # instantiate the module
        $this->{meta} = $class->new($this->{type}, $this);
    } else {
        $this->fatal("ObjectMeta: failed to load $class: $@");
    }
    return $this->{meta};
}

sub my_type { "member"; }

sub showauth {
    my ($this, $key) = @_;
    my %fields = (
        member_id    => 1,
        honorific    => 1,
        organization => 1,
        first_name   => 1,
        middle_name  => 1,
        last_name    => 1,
        photo        => 1,
        thumbnail    => 1,
        ctime        => 1
    );
    my %private_fields = (expirydate => 1);
    return 1 if ($this->is_me() && $private_fields{$key});
    return ($this->allow() && $fields{$key});
}

# membername method in membership class contains similar logic
# options:
# verbose=>0|1, precedence=last
# precedence option is a preference but can be overridden by caller

sub name {
    my ($this, %opt) = @_;
    my $name = $this->run_handler("member_name", %opt);
    return $name if (defined $name);
    if ($this->defined()) {
        my $org        = $this->showdata("organization");
        my $name1      = $this->showdata("first_name");
        my $name2      = $this->showdata("middle_name");
        my $name3      = $this->showdata("last_name");
        my $precedence = $opt{precedence}
          || $share{Membership}{Config}->{config}{name}{precedence};

        # organizational logic
        # name type is organization and
        # no types can add secondary contacts or all types can add secondary contacts or THIS type can add
        my $pref          = &preference("Membership.secondary.types_allowed");
        my @types_allowed = ();
        if (ref $pref eq "ARRAY") {
            @types_allowed = @$pref;
        } elsif ($pref) {
            @types_allowed = split(/,/, $pref);
        }
        my $membertype = $this->member_type();
        if (
            (!$this->getdata("parent_id") && $org && $share{Membership}{Config}->{config}{name}{type} eq "organization")
            && $this->member_type() ne "secondary"
            && (
                !&preference("Membership.secondary.add")
                || (
                    &preference("Membership.secondary.add")
                    && (  !scalar @types_allowed
                        || grep(/$membertype/, @types_allowed)) > 0
                )
            )
          )
        {
            $name = $org;
            if ($opt{verbose} && $name1 && $name3) {
                $name = "$org ($name1 $name3)";
            }
        } elsif ($opt{verbose}
            && $name1
            && $name3
            && $share{Membership}{Config}->{config}{name}{type} eq "organization")
        {
            $name = "$name1 $name3";
            $name .= " ($org)" if ($org);
        } elsif ($precedence eq "last") {
            $name = $name3;
            if ($name1) {
                $name .= ", " if ($name);
                $name .= $name1;
                if ($name2) {
                    $name .= " $name2";
                }
            }
        } else {
            $name = $share{DB}->user_name($this->get);
            $name = "$name1 $name3" if (!$name && $name1 && $name3);
            $name = $org if (!$name);
        }
    }

    # trim leading and trailing whitespace
    $name =~ s/^\s+//;
    $name =~ s/\s+$//;
    return $name;
}

sub uid {
    my $this = shift;
    return $this->defined
      ? $this->{data}{$config{auth}{user_uid_column}}
      : undef;
}

sub gid {
    my $this = shift;
    return $this->defined
      ? $this->{data}{$config{auth}{user_gid_column}}
      : undef;
}

sub passtype {
    my $this       = shift;
    my $pass_store = $this->getdata("password");
    my $passtype;
    if ($pass_store =~ /^\[(cleartext|des|crypt|md5\+?)\] (.+)$/) {

        # storage method coded into the stored password
        $passtype   = $1;
        $pass_store = $2;
    }
    return $passtype;
}

sub parent {
    my $this = shift;
    if ($this->getdata("parent_id")) {
        return new Modules::Membership::Member(id => $this->getdata("parent_id"));
    }
    return;
}

sub account {
    my $this = shift;
    if ($this->loaded) {
        if (  !$this->{account}
            || $this->{account}->getdata("member_id") != $this->uid)
        {
            if ($this->{children}{account}) {
                $this->{children}{account}->reset();
                my $accounts = $this->{children}{account};
                while (my $a = $accounts->next()) {
                    $this->{account} = $a;
                    last if ($this->{account}->getdata("section_id") == $this->gid());
                }
            }
        }
        if (!$this->{account}) {

            # no account - make one
            if ($this->uid) {
                my $acct = new Modules::Finance::Account();
                $acct->setdata("name", $this->name(verbose => 1));
                $acct->setdata("member_id",  $this->uid());
                $acct->setdata("section_id", $this->gid());
                $share{DB}->info("creating account");
                if ($acct->force_save()) {
                    $this->{account} = $acct;
                }
            }
        }
    }
    return $this->{account};
}

sub contacts {
    my $this = shift;
    my $acct = $this->account();

    # returns an ObjectList object
    return $acct->get_contacts() if $acct;
    return;
}

sub email {
    my $this  = shift;
    my $email = $this->getdata("email");
    if (!$email) {
        my $contacts = $this->contacts();
        if ($contacts) {
            ### FIXME: no precedence to contact types
            while (my $c = $contacts->next()) {
                $email = $c->getdata("email");
                last if $email;
            }
        }
    }
    return $email;
}

sub is_me {
    my $this = shift;
    return 0 if (!$this->uid);
    return ($this->uid == $share{DB}->my_uid);
}

sub is_member {
    my $this = shift;

    # the user is tied to a particular site
    if ($config{auth}{scope} eq "local" && $this->gid) {
        my $site = $share{DB}->this_site;
        my $sid = ref $site eq "HASH" ? $site->{section_id} : $site;
        return 0 if ($sid && $sid != $this->gid);
    }

    return 1 if ($this->getdata("type") =~ /^member/);
    if (my $parent = $this->parent) {
        return ($parent && $parent->is_member());
    }
    return 0;
}

sub member_type {
    my $this = shift;
    my $type = $this->getdata("type");
    $type =~ s/^member\///;
    $type =~ s/^guest\///;
    return $type;
}

sub is_parent {
    my $this = shift;
    return 0 if (!$share{DB}->authorize);
    my $id = $this->uid();
    if ($id && $id == $share{DB}->my_user_record->{parent_id}) {
        return 1;
    }
    return 0;
}

sub is_child {
    my $this = shift;
    my $pid  = $this->getdata("parent_id");
    if ($pid && $pid == $share{DB}->my_uid) {
        return 1;
    }
    return 0;
}

sub is_sibling {
    my $this = shift;
    my $pid  = $this->getdata("parent_id");
    my $user = $share{DB}->my_user_record;
    if ($pid && $pid == $user->{parent_id}) {
        return 1;
    }
    return 0;
}

sub privacy {
    my $this = shift;
    my $privacy =
         $this->getdata("privacy")
      || $share{Membership}{Config}->{config}{privacy}
      || "administrators";
    return $privacy;
}

sub allow {
    my $this = shift;
    my $stat = $this->run_handler("member_allow");
    return $stat if ($stat);
    my $privacy = $this->privacy();
    return 1 if ($this->is_me || $this->is_child);
    my $sid = $this->get_section_id;
    if (   $share{Membership}{Config}->{config}{scope}{$sid} eq "local"
        && $share{Page}
        && $share{Page}->id("section") != $this->gid())
    {
        # user is looking at member from wrong website
        return 0;
    } elsif (!$share{Page} && !$share{DB}->is_manager($this->gid())) {

        # admin is looking at member of another group
        return 0;
    } elsif ($this->is_archived && !$share{DB}->is_manager()) {
        return 0;
    } elsif ($privacy =~ /public/) {
        return 1;
    } elsif ($privacy =~ /administrators/ && $share{DB}->is_manager()) {
        return 1;
    } elsif ($privacy =~ /members/ && $share{DB}->is_member()) {
        return 1;
    }
    return 0;
}

sub is_visible {
    my ($this) = @_;
    return 1 if (!$share{Page} && $share{DB}->is_manager());
    if ($this->getdata("visibility") eq "visible") {
        return 1;
    } elsif ($this->is_me || $this->is_child) {
        return 1;
    }
    return 0;
}

sub is_key_contact {
    my $this = shift;
    my $stat = $this->run_handler("member_is_key_contact");
    return $stat if (defined $stat);
    return 0;
}

# get_profile_data : returns a hash of profile values; values included are:
# - member record
# - all member_meta data
# - photo & thumbnail
# - contact cards

sub get_profile_data {
    my ($this) = @_;
    my %profile;

    # fetch basic profile data
    my $member = $this->{data};
    foreach my $key (keys %$member) {
        $profile{$key} = $this->showdata($key);
    }

    # replace photos with IMG links
    $profile{photo}     = $this->show_photo();
    $profile{thumbnail} = $this->show_photo(1);

    # add extended attributes (only show allowed attributes)

    foreach my $meta ($this->meta_allowed()) {
        $profile{$meta} = $this->meta->showdata($meta);
    }

    # add contact info

    my $contacts = $this->contacts();
    my %contact;
    if ($contacts) {
        $contacts->reset();
        while (my $c = $contacts->next()) {
            if ($c->allow()) {
                my $type = $c->getdata("type");
                my $key  = "contact_$type";
                my $n    = exists $profile{$key} ? $c->id() : "";
                $contact{"contact_$type$n"} = $type;
                my $uri  = new ExSite::URI;
                my $path = $share{Membership}{Config}->path($this);
                $uri->path("Membership", $path);
                $profile{"contact_$type$n"} = $c->show(vcard => $uri);
                $profile{"address_$type$n"} = $c->show_address();
            }
        }
    }

    $this->{contact_data} = \%contact;
    $this->{profile_data} = \%profile;
    return wantarray ? %profile : \%profile;
}

sub show {
    my ($this, %opt) = @_;
    my $out;
    if ($this->ok) {
        if (!$this->{data}) { $this->load(); }
        if (!$this->{data}) {
            my $name = $this->name;
            return $this->error("$name does not exist.");
        }

        # privacy check
        if (!$this->allow()) {
            $out .= $this->permission_denied();
            if (!$share{DB}->authorize) {
                my $uri  = new ExSite::URI;
                my $path = $share{Membership}{Config}->path($this);
                $uri->path("Membership", $path);
                my $login_form = $share{DB}->login_form($uri->write_full, $uri->write_full);
                $out .= $login_form;
            }
            return $out;
        }

        my $ml = new ExSite::ML;
        my $m  = $this->{data};

        # profile
        $out .= $this->show_profile();

        # history
        $out .= $this->show_history();

    }
    return $out;
}

sub permission_denied {
    my ($this) = @_;
    my $stat = $this->run_handler("member_permission_denied");
    return $stat if ($stat);
    if (my $message = $share{Membership}{Config}->{config}{message}{permission_denied}) {
        return $this->info($msg{$message});
    }
    return $this->warn($msg{"Sorry, you do not have permission to view this user's profile."});
    return;    # silent failure for snoopers
}

sub show_profile {
    my ($this, %profile) = @_;
    my $out;
    my $ml = &get_obj("ML");
    if ($this->defined) {
        if (keys %profile == 0) {
            %profile = $this->get_profile_data();
        }
        $profile{profile_name} = $this->name(verbose => 1);
        $profile{is_me} = $this->is_me() ? 1 : 0;

        if ($this->allow_edit()) {
            $profile{update_profile} = $ml->a(
                $ml->span("Update"),
                {
                    href => $this->link(pro => "edit_profile"),
                    id   => "update_profile"
                }
            );
            $profile{update_profile_link} = $this->link(pro => "edit_profile")
              if (!$share{Membership}{admin});
            $profile{update_contact_link} = $this->link(pro => "edit_contacts")
              if (!$share{Membership}{admin});
        }
        if (   $share{DB}{map}->is_mapped("member_category_join")
            && $share{DB}->count("member_category_join", {member_id => $this->uid}))
        {
            $profile{member_directory} = 1;
        }
        my $stat = $this->run_handler("set_profile_data", \%profile);
        my @meta = $this->meta()->get_allowed();

        # put it into a template (if any)

        my $gid              = $this->gid();
        my $profile_template = $this->profile_template();
        my $show_extras      = !$share{Page}
          && ( $share{DB}->is_manager($gid)
            || $share{Membership}{Config}->{config}{show_extras});
        if ($profile_template) {
            if ($profile_template !~ /\[\[profile_name\]\]/) {
                $out .= $ml->h1($this->name(verbose => 1), {class => "membername"});
            }
            my $profile = &substitute($profile_template, \%profile);
            if (&html_to_plaintext($profile)) {
                if ($share{Membership}{admin}) {
                    $out .= &ExSite::HTML::BasicBox(pane => $profile);
                } else {
                    $out .= $profile;
                }
            }
            if ($show_extras) {

                # append extra attributes not included in the template
                foreach my $meta (keys %profile) {
                    if ($profile_template =~ /\[\[$meta\]\]/) {
                        delete $profile{$meta};
                    }
                }
            }
        }

        if (!$profile_template || $show_extras) {
            $out .= $ml->h1($this->name, {class => "membername"})
              if (!$profile_template);
            my $in    = new ExSite::Input;
            my $input = $in->combine();
            $out .= $this->show_generic(profile => \%profile, plaintext => 0);
        }
        my $contacts;
        if (scalar keys %{$this->{contact_data}} > 0
            && $share{Membership}{Config}->{config}{profile}{show_contacts})
        {
            foreach my $ckey (keys %{$this->{contact_data}}) {
                next if ($profile_template =~ /\[\[$ckey\]\]/);
                $contacts .= $ml->div($ml->strong(ucfirst $this->{contact_data}{$ckey}) . $profile{$ckey},
                    {class => "ContactPreview"});
            }
            $out .= $contacts if ($contacts);
        }
    }
    return $out;
}

sub profile_template {
    my ($this) = @_;
    my $stat = $this->run_handler("profile_template");
    return $stat if ($stat);
    my $gid = $this->gid();
    my $loc = $share{Page};
    $loc = $share{Section}
      if (!$loc
        && $share{Membership}{Config}->{config}{profile}{template}{control_panel});
    return if (!$loc);
    my $type = $this->member_type();
    my @name;

    if (!$share{Page} && $share{DB}->is_manager($gid)) {
        push(@name, $share{Membership}{Config}->{config}{profile}{admin_template}{$type} || "admin_profile_template");
    } elsif ($share{DB}->is_member) {
        push(@name, $share{Membership}{Config}->{config}{profile}{member_template}{$type} || "member_profile_template");
    } else {
        push(@name, $share{Membership}{Config}->{config}{profile}{public_template}{$type} || "public_profile_template");
    }
    push(@name, $share{Membership}{Config}->{config}{profile}{template}{$type} || "profile_template");
    foreach my $name (@name) {
        if (my $t = $loc->find($name)->get_html()) {
            return $t;
        }
    }
    return;
}

sub show_generic {
    my ($this, %opt) = @_;
    my %profile;
    %profile = %{$opt{profile}} if ($opt{profile});
    my $plaintext = $opt{plaintext};
    my $out;
    my $access = $share{DB}->level();
    if ($this->defined) {
        if (keys %profile == 0) {
            %profile = $this->get_profile_data();
        }

        # generic template (table)
        my (@head, @data);
        my $ncol = $share{DB}{map}->get_ncol("member");
        for (my $icol = 0 ; $icol < $ncol ; $icol++) {
            my %colmap = $share{DB}{map}->get_column("member", $icol);
            next if ($plaintext && $colmap{datatype} =~ /file/);
            if ($colmap{column} =~ /member_id|first_name|last_name|email|type|section_id|expirydate/) {
                next if (!$plaintext);
                next if ($plaintext && $colmap{column} =~ /member_id|section_id|expirydate/);
            }

            if ($profile{$colmap{column}}) {

                # we have data for this column
                # show readable columns, by default - this varies by user
                if ($access >= $colmap{read}) {
                    push @head, $colmap{label};
                    push @data, $profile{$colmap{column}};
                }
            }
        }
        my @meta = $this->meta->get_allowed_sorted();
        foreach my $meta ($this->meta->get_allowed_sorted()) {
            my $label = $this->meta->get_map_info($meta, "label") || $meta;
            if ($profile{$meta}) {

                my $read = $this->meta->get_map_info($meta, "read") || 0;
                if ($access >= $read) {
                    push @head, $label;
                    push @data, $profile{$meta};
                }
            }
        }

        my $r = new ExSite::ReportBuilder();
        $r->headers(\@head);
        $r->data(@data);
        if ($plaintext) {
            my @row = $r->export_data();
            foreach my $row (@row) {
                $out .= join(": ", @$row) . "\n";
            }
        } else {
            $out .= $r->make();
        }
    }
    return $out;
}

sub show_photo {
    my ($this, $thumb) = @_;
    if ($this->defined) {
        my $member = $this->{data};
        if ($thumb) {
            if ($member->{thumbnail} && $this->picture("thumbnail")) {
                return $this->picture("thumbnail");
            } else {
                if ($share{Page}) {
                    if (   $this->member_type() eq "secondary"
                        || $this->parent())
                    {
                        return &preference("Membership.secondary_default_thumbnail");
                    }
                    if (ref &preference("Membership.default_thumbnail") eq "ARRAY") {
                        my @thumbs = @{&preference("Membership.default_thumbnail")};
                        my $index  = int(rand(scalar @thumbs));
                        return $thumbs[$index];
                    } else {
                        return if (!$share{Page});
                        return &preference("Membership.default_thumbnail");
                    }
                }
                return;
            }
        } else {
            if ($member->{photo}) {
                return $this->picture("photo");
            } else {
                return if (!$share{Page});
                if ($this->member_type() eq "secondary" || $this->parent()) {
                    return &preference("Membership.secondary_default_photo");
                }
                if (ref &preference("Membership.default_photo") eq "ARRAY") {
                    my @photos = @{&preference("Membership.default_photo")};
                    my $index  = int(rand(scalar @photos));
                    return $photos[$index];
                } else {
                    return &preference("Membership.default_photo");
                }
            }
        }
    }
    return;
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

sub picture {
    my ($this, $col) = @_;
    my $out;
    my $ml  = &get_obj("ML");
    my $img = new ExSite::Image($this->getdata($col));
    my ($width, $height) = $img->dim;
    my $filename = &clean_filename($img->{filename});
    if (!-e $this->diskpath . "/" . $this->id() . "/" . $filename) {

        # not published; go to the database
        return $this->showdata($col);
    } else {
        if ($col =~ /thumb/) {
            $filename = $this->htmlpath . "/" . $this->id() . "/th_${filename}";
        } else {
            $filename = $this->htmlpath . "/" . $this->id() . "/" . $filename;
        }
    }
    my $out;
    if (&MimeType($img->{filename}) =~ /^image/) {
        $out = $ml->img(
            undef,
            {
                src    => $filename,
                height => $height,
                width  => $width,
                alt    => $img->{filename}
            }
        );
    } else {

        # invalid content
        $this->error($msg{"Image processing error"});
        return undef;
    }
    return $out;
}

sub show_history {
    my $this = shift;
    #
    # can include things like:
    #
    # - membership history
    # - registrations
    # - comments/postings
    #
    my $stat = $this->run_handler("member_history");
    return $stat if ($stat);
}

sub show_mini_profile {
    my ($this, $profile_url, %profile) = @_;
    my $out;
    if ($this->defined) {
        if (keys %profile == 0) {
            %profile = $this->get_profile_data();
        } else {

            # replace photos with IMG links
            $profile{photo}     = $this->show_photo();
            $profile{thumbnail} = $this->show_photo(1);
        }
        $profile{profile_name} = $this->name();
        $profile{profile_url} = $profile_url || "#";
        my $stat = $this->run_handler("set_profile_data", \%profile);
        my $extras;
        my $extrakey =
          $share{Membership}{Config}->{config}{mini_profile_extras};
        if ($extrakey) {
            if ((ref $extrakey) ne "ARRAY") { $extrakey = [$extrakey]; }
            foreach my $key (@$extrakey) {
                $extras .= $this->showdata($key) . $ml->br;
            }
        }
        $profile{profile_extras} = $extras;
        my $profile_template = $this->mini_profile_template($profile_url);
        $out .= &substitute($profile_template, \%profile);
    }
    return $out;
}

# return mini profile template for member
sub mini_profile_template {
    my ($this, $profile_url) = @_;
    my $stat = $this->run_handler("mini_profile_template", $profile_url);
    return $stat if (defined $stat);
    my $gid = $this->gid();
    my $loc =
      $share{Page} || $share{Section} || new ExSite::Section(id => $gid);
    my $type = $this->member_type();
    my $name = $share{Membership}{Config}->{config}{mini_profile}{template}{$type}
      || "mini_profile_template";
    my $profile_template = $loc->find($name)->get_html();

    if (!$profile_template) {
        my $ml = new ExSite::ML;
        $profile_template = $ml->table(
            $ml->tr(
                    $ml->td("[[thumbnail]]")
                  . $ml->td($ml->a("[[profile_name]]", {href => $profile_url}) . $ml->br . "[[profile_extras]]")
            ),
            {class => "MemberMiniProfile"}
        );
    }
    return $profile_template;
}

# misc DB ops

sub loaded {
    my $this = shift;
    $this->load() if (!$this->{data} || 
        !$this->{children} || 
        !$this->{children}{member_status} ||
        !$this->{children}{account});
    return ($this->{data} && $this->{children});
}

sub load {
    my ($this, $data) = @_;
    $this->SUPER::load($data);
    $this->{children} = $this->get_children("member_status");

    # load accounts local to current section first
    my $list = new ExSite::ObjectList(type => "account");
    my $site = $share{DB}->this_site;
    my $sid  = $site->{section_id};
    $list->load({member_id => $this->{id}, section_id => $sid}, "account_id");
    if (!$list->count) {
        $list->load({member_id => $this->{id}}, "account_id");
    }
    $this->{children}{account} = $list;
    return $this->{data};
}

sub allow_edit {
    my $this = shift;
    if ($this->is_me) {
        return 1;
    } elsif ($share{DB}->is_manager($this->gid())) {
        return 1;
    } elsif ($this->is_child()) {

        # primary members can optionally edit their secondaries
        return &preference("Membership.secondary.edit");
    } elsif ($this->is_parent()) {

        # secondary members can optionally edit their primaries
        return 1 if ($this->is_key_contact());
        return &preference("Membership.parent.edit");
    }
    return 0;
}

#-------------------------------------------------------------------------
# Forms

sub validate_column {
    my ($this, $table, $column, $datum) = @_;
    my @hidden =
      split(/,/, &preference("Membership.typehide." . $share{member_type}));
    my %i = map { $_ => 1 } @hidden;
    my $validate = $this->run_handler("Membership_column_validate", $column, $share{member_type});
    return 0 if ($validate eq "no");
    return 0 if ($i{$column});

    # skip validation for admins when there is no data
    # admins do not need to enter non-standard required fields
    if (   $table eq "member"
        && $column !~ /^(first_name|last_name|organization|email)$/
        && $share{Membership}{admin}
        && !$datum)
    {
        return 0;
    }
    return undef;
}

sub validate {
    my $this = shift;
    $share{member_type} = $this->member_type();
    $share{DB}->handler("validate_column", \&validate_column);
    my @err;
    if ($this->parent) {
        if (&preference("Membership.secondary.use_parent_type")) {
            if ($this->parent->getdata("type") ne $this->getdata("type")) {
                push(@err, $msg{"Membership type is not valid for this secondary member."});
            }
        } else {
            my @types = split(/,/, &preference("Membership.secondary.type"));
            my $membertype = $this->member_type();
            if ($membertype ne "secondary" && scalar grep(/$membertype/, @types) == 0) {
                push(@err, $msg{"Secondary member does not have a valid type."});
            }
        }
    }
    if ($this->action() eq "insert") {
        if ($this->member_type() eq "secondary" && !$this->parent()) {
            push(@err, $msg{"Please set a \"Related to\" association for this secondary member."});
        }
    }
    return @err if (scalar @err);
    return $this->SUPER::validate();
}

# synchronize contact information of secondary members with associated location when new location is set for member

sub sync_location {
    my $this = shift;
    if ($share{DB}{map}->is_mapped("member", "location_id")) {

        # this is a primary member, no need to synchronize
        return undef if (!$this->parent());

        my $l = new Modules::ID::Location(id => $this->getdata("location_id"));
        if ($l->exists()) {
            ExSite::Module::read_conf("Organization");
            my $privacy = ExSite::Config::preference("Membership.contact_privacy") || "public";
            my $a = $this->account;
            return undef if (!$a);
            my $type = ExSite::Config::preference("Organization.contact_type");

            # main_contact = official address of member
            my $main_contact = $a->get_contact($type);
            if (!$main_contact || $main_contact->getdata("type") ne $type) {
                $main_contact =
                  new Modules::ID::Contact(data => {account_id => $a->id(), type => $type, privacy => $privacy});
            }

            # delete extraneous contacts of same type
            my @contacts = $share{DB}->fetch_match("contact", {account_id => $a->id(), type => $type});
            foreach my $data (@contacts) {
                my $o = new Modules::ID::Contact(data => $data);
                $o->delete() if ($data->{contact_id} != $main_contact->id());
            }
            if ((ref $main_contact) =~ /Contact/ && (ref $l) =~ /Location/) {
                foreach my $key (qw(address city provstate pcode country phone1 phone2 fax)) {
                    $main_contact->setdata($key, $l->get_contact_data($key));
                }
                if ($this->getdata("type") eq "guest") {
                    $main_contact->setdata("privacy", "administrators");
                } else {
                    $main_contact->setdata("privacy", $privacy);
                }
                $main_contact->setdata("email", $this->email());
                $main_contact->force_save();
            }
            delete $this->{children};
            delete $this->{account};
        }
    }
}

# save record
# update account name if changed

sub save {
    my ($this, %opt) = @_;
    $this->setdata("mtime", undef);

    # synchronize organization with parent
    if ($this->parent
        && ($this->parent->getdata("organization") ne $this->getdata("organization")))
    {
        $this->setdata("organization", $this->parent->getdata("organization"));
    }

    my $stat = $this->SUPER::save(%opt);
    if ($stat) {
        if (!$this->account) {
            $this->error("Account for this member is missing");
            return 0;
        }
        if ($this->account->allow()) {
            if ($this->account->name() ne $this->name(verbose => 1)) {
                $this->account->setdata("name", $this->name(verbose => 1));
                $this->account->force_save();
            }
        }
    }
    $this->sync_location();
    $this->run_handler("post_save_member");
    return $stat;
}

# edit basic record

sub edit {
    my $this = shift;
    if ($this->ok) {
        if ($this->allow_edit()) {
            return $this->DB()->make(table => $this->{type}, record => $this->{id}, @_);
        } else {
            return $this->error("Permission denied.");
        }
    }
    return $this->show_diagnostics("error", "html");
}

# edit extended attributes

sub editmeta {
    my ($this, %opt) = @_;
    if ($this->ok) {
        if ($this->allow_edit()) {
            my $in   = new ExSite::Input;
            my $post = $in->post;

            # get the formdata, parsed into columns
            my %pdata = $share{DB}->parse_parts(%$post);
            if (keys %pdata) {
                my %metadata;
                foreach my $key (keys %pdata) {
                    if ($key =~ /^meta_(.+)$/) {
                        $metadata{$1} = $pdata{$key};
                        delete $pdata{$key};
                    }
                }

                # update metadata
                my $olddata = $this->meta_get();
                my @meta_err;
                foreach my $key (keys %metadata) {
                    if (!$this->meta_allowed($key)) {
                        push(@meta_err, $this->error("$key: invalid member metadata"));
                    } elsif ($metadata{$key} ne $olddata->{$key}) {
                        if (!$this->meta_set($key, $metadata{$key})) {
                            push(@meta_err, $this->meta()->show_diagnostics("error", "html"));
                        }
                    }
                }
                if (scalar @meta_err) {
                    return join("\n", @meta_err);
                }
                $this->meta()->save();
                &redirect($post->{reply});
            }

            $this->load();
            $this->meta()->load();
            $this->DB()->set_action($this->action());    # insert/update
            my $p = &ExSite::Module::service_page("Membership");
            $this->DB()->form(%opt);

            # meta fields
            my @metafields;
            if ($opt{meta}) {
                @metafields = split(/,/, $opt{meta});
            } else {
                @metafields = $this->meta()->get_allowed();
            }
            if (@metafields == 0) {
                my %meta = $this->meta()->get_all();
                @metafields = keys %meta;
            }
            foreach my $key (sort @metafields) {

                # make record if allowed and it does not yet exist
                if ($this->meta()->is_allowed($key)
                    && !defined $this->meta_get($key))
                {
                    my $data = $this->meta()->make_record($key);
                    $this->meta()->insert($data);
                }
                $this->meta()->input("meta_$key", $key);
            }
            my $uri = new ExSite::URI;
            $uri->service_page("Membership");
            $uri->query(pro => undef);
            $uri->plaintext();
            $this->DB()->form()->input(
                type  => "hidden",
                name  => "reply",
                value => $uri->write_full
            );
            my $loc = $share{Page} || $share{Section};
            if ($loc && $loc->find($opt{template})) {
                my $template = $loc->find($opt{template})->get_html();
                $this->DB()->form()->template($template);
            }
            return $this->DB()->form()->make;
        } else {
            return $this->error("Permission denied.");
        }
    }
    return $this->show_diagnostics("error", "html");
}

# edit images

sub edit_photo {
    my ($this, %opt) = @_;
    my $ml = &get_obj("ML");
    my $out;
    my $jcrop;
    if ($this->ok) {
        if ($this->allow_edit()) {
            my $in   = new ExSite::Input;
            my $data = $in->post();

            if (   $data->{imagef} eq "crop"
                && defined $data->{x}
                && defined $data->{y})
            {
                my $img = new ExSite::Image($this->getdata("photo"));
                my %data;
                $img->convert("-crop $data->{w}x$data->{h}+$data->{x}+$data->{y}");
                $data{photo} = $img->encode;
                $img->thumb;
                $data{thumbnail} = $img->encode;
                $share{DB}->update("member", \%data, {$config{auth}{user_uid_column} => $this->uid});
                $out .= $ml->p($msg{"Your photo has been cropped as seen below."});
                $out .= $ml->p($ml->a($msg{"Go back to your profile"}, {href => $this->link(pro => undef)}));
            }

            my %opt;

            # if the profile image needs to fit within a defined container size
            # use xmin, xmax and aspectratio to allow the user maximum flexibility in cropping
            %opt = (
                xmin => &preference("Membership.photo.xmin") || 200,
                xmax => &preference("Membership.photo.xmax") || 200,
                ymin => &preference("Membership.photo.ymin"),
                ymax => &preference("Membership.photo.ymax"),
                aspectratio => &preference("Membership.photo.ar")
            );

            if (exists $data->{photo}) {
                my $filedata = $in->fetch_file("photo");
                my $img      = new ExSite::Image($filedata);
                my $type     = &MimeType($img->name);
                if ($img->web_ready || $img->jpeg) {
                    if (
                        $img->scale(
                            &preference("Membership.photo.shrinksize") || 400,
                            &preference("Membership.photo.shrinksize") || 400,
                            -1, "-quality 95"
                        )
                      )
                    {
                        my ($w, $h) = $img->dim;
                        my %data;
                        $data{photo} = $img->encode;
                        $this->setdata("photo", $img->encode);
                        $img->thumb($config{thumbnail_size}, "-quality 100");
                        $this->setdata("thumbnail", $img->encode);
                        $this->force_save();
                        $jcrop = 1
                          if ( ($w - 25) > $opt{xmin}
                            && ($h - 25) > $opt{ymin});
                        $out .= $ml->p($msg{"Thank you. Your photo has been updated."});
                        $out .= $ml->p(
                            $msg{
"If you wish to crop your image please make the appropriate selection by resizing the box below and clicking on the \"crop\" button."
                            }
                        ) if ($jcrop);
                        $out .= $ml->p($msg{"Happy with your image?. "}
                              . $ml->a($msg{"Go back to your profile"}, {href => $this->link(pro => undef)}));
                        $out .= $ml->br();
                        $out .=
                          $ml->div($this->show_photo(), {id => "cropbox"});
                        $out .= &jcrop(%opt) if ($jcrop);
                    } else {
                        $out .= $this->error(
                            $msg{
"This photo could not be reduced to a web-friendly size.  Please try uploading a different photo."
                            }
                        );
                    }
                } else {
                    $out .= $this->error(
                        $msg{
"This photo could not be converted to a web-friendly JPEG format.  Please try uploading a different photo."
                        }
                    );
                }
            } else {
                if ($this->getdata("photo")) {
                    my $img = new ExSite::Image($this->getdata("photo"));
                    my ($w, $h) = $img->dim;
                    $jcrop = 1
                      if ( ($w - 25) > $opt{xmin}
                        && ($h - 25) > $opt{ymin});
                    $out .= $ml->h1($msg{"Current photo:"});
                    $out .= $ml->p(
                        $msg{
"If you wish to crop your current photo please make the appropriate selection by resizing the box below and clicking on the \"crop\" button. You may also upload a new photo below."
                        }
                    ) if ($jcrop);
                    $out .=
                      $ml->div($this->show_photo(), {id => "cropbox"});
                    $out .= &jcrop(%opt) if ($jcrop);
                }
                $out .= $ml->h1(&substitute($msg{preference("Membership.heading.upload_photo")}, {name => $this->name}));
                $out .= $ml->p($msg{"Image upload tips:"});
                $out .= $ml->ul(
                    [
                        $msg{"save your image in jpeg, gif or png format"},
                        $msg{
"large files may require a few minutes to upload, please shrink your image if you are on a slow connection"
                        }
                    ]
                );

                my $f = new ExSite::FormBuilder(method => "post");
                $f->multipart;
                $f->input(
                    name     => 'photo',
                    type     => 'file',
                    prompt   => 'Photo',
                    required => 1,
                );
                $out .= $f->make();
            }
            return $ml->div($out, {class => "edit_photo"});
        } else {
            return $this->error($msg{"Permission denied."});
        }
    }
    return $this->show_diagnostics("error", "html");
}

#------------------------------------------------------------------------
# jcrop : form for cropping an image
# pass in container div id of img
# xmin and ymin are minimum dimensions of cropbox (optional)
# ar is aspect ratio of box (optional)
# thumb is thumbnailing or other options to be passed as a hidden field
#------------------------------------------------------------------------

sub jcrop {
    my %opt = @_;
    my ($id, $xmin, $ymin, $ar, $thumb) =
      ($opt{id} || "cropbox", $opt{xmin} || 100, $opt{ymin} || 100, $opt{aspectratio}, $opt{thumb});
    my $out;
    my $ml          = &get_obj("ML");
    my $load_jquery = <<END;
if (typeof jQuery === "undefined") {
    var script_tag = document.createElement('script');
    script_tag.setAttribute("type","text/javascript");
    script_tag.setAttribute("src",\"$config{jquery}\");
    document.getElementsByTagName("head")[0].appendChild(script_tag);
}
END
    $out .= $ml->script($load_jquery, {type => "text/javascript"});
    $out .= $ml->script(
        undef,
        {
            src  => "$config{server}{HTMLpath}/_Modules/Membership/js/jquery.Jcrop.js",
            type => "text/javascript"
        }
    );
    $out .= $ml->link(
        undef,
        {
            rel  => "stylesheet",
            href => "$config{server}{HTMLpath}/_Modules/Membership/jquery.Jcrop.css",
            type => "text/css"
        }
    );
    my ($maxsize, $ar);
    $maxsize = ",maxSize: [$opt{xmax},$opt{ymax}]"
      if ($opt{xmax} && $opt{ymax});
    $ar = ",\naspectRatio: $opt{aspectratio}" if ($opt{aspectratio});
    $out .= <<END;
    <script language="Javascript">
    jQuery(document).ready(function(){
    \$(window).load(function(){
    var api = \$.Jcrop('div#$id > img',{
        onChange: showCoords,
        onSelect: showCoords,
        setSelect: [0,0,$xmin,$ymin],
        minSize: [$xmin,$ymin] $maxsize $ar
    });
    });
    });

    // Our simple event handler, called from onChange and onSelect
    // event handlers, as per the Jcrop invocation above
    function showCoords(c)
    {
    jQuery('#x').val(c.x);
    jQuery('#y').val(c.y);
    jQuery('#x2').val(c.x2);
    jQuery('#y2').val(c.y2);
    jQuery('#w').val(c.w);
    jQuery('#h').val(c.h);
    };

    </script>
END
    require ExSite::FormBuilder;
    my $f = new ExSite::FormBuilder(method => "post");
    $f->template($ml->p("[[status:prompt]] [[status:input]] [[note:prompt]] [[note:input]] [[buttons]]"));
    $f->set("buttons", $ml->input(undef, {type => "submit", value => $msg{"Crop"}}));
    $f->input(type => "hidden", name => "x",      id    => "x");
    $f->input(type => "hidden", name => "y",      id    => "y");
    $f->input(type => "hidden", name => "x2",     id    => "x2");
    $f->input(type => "hidden", name => "y2",     id    => "y2");
    $f->input(type => "hidden", name => "w",      id    => "w");
    $f->input(type => "hidden", name => "h",      id    => "h");
    $f->input(type => "hidden", name => "thumb",  value => $thumb);
    $f->input(type => "hidden", name => "imagef", value => "crop");
    $out .= $f->make();
    return $out;
}

sub change_password {
    my ($this) = @_;
    my $ml = &get_obj("ML");
    my $out;
    if ($this->ok) {
        if ($this->is_me() || $share{Membership}{admin}) {
            my $in   = new ExSite::Input;
            my $data = $in->post();
            if (exists $data->{passwd0} && !$share{change_password_error}) {
                if ($share{Membership}{admin} && !$this->is_me()) {
                    if ($data->{passwd1} ne $data->{passwd2}) {
                        $out .= $ml->p($msg{"New passwords do not match."});
                    } else {
                        my $stat = $share{DB}->set_password($data->{passwd1}, $this->id);
                        if ($stat) {
                            $out .= $this->error($stat);
                            $share{change_password_error} = 1;
                            $out .= $this->change_password();
                        } else {
                            $out .= $ml->p($msg{"The password was changed successfully."});
                        }
                    }
                } else {
                    my $err = $share{DB}->change_password($data->{passwd0}, $data->{passwd1}, $data->{passwd2});
                    if ($err) {
                        $out .= $this->error($err);
                        $share{change_password_error} = 1;
                        $out .= $this->change_password();
                    } else {
                        $out .= $ml->p($msg{"Your password was changed successfully."});
                    }
                }
            } else {
                $out .= $ml->h1(&substitute($msg{preference("Membership.heading.change_password")}, {name => $this->name}));
                my $f = new ExSite::FormBuilder(method => "post");
                if ($share{Membership}{admin} && !$this->is_me()) {
                    $f->input(
                        name  => 'passwd0',
                        type  => 'hidden',
                        value => $this->getdata("password"),
                    );
                } else {
                    $f->input(
                        name     => 'passwd0',
                        type     => 'password',
                        prompt   => $msg{'Old Password'},
                        required => 1,
                    );
                }
                $f->input(
                    name     => 'passwd1',
                    type     => 'password',
                    prompt   => $msg{'New Password'},
                    required => 1,
                );
                $f->input(
                    name     => 'passwd2',
                    type     => 'password',
                    prompt   => $msg{'New Password (confirm)'},
                    required => 1,
                );
                $out .= $f->make();
            }
            return $ml->div($out, {class => "change_password_form"});
        } else {
            return $this->error($msg{"You cannot change this user's password."});
        }
    }
    return $this->show_diagnostics("error", "html");
}

sub do_edit {
    my $this = shift;
    if ($this->ok) {
        if ($this->allow_edit()) {
            $this->DB()->set("action", "update");
            return $this->DB()->do();
        } else {
            return $this->error($msg{"Permission denied."});
        }
    }
    return $this->show_diagnostics("error", "html");
}

sub expirydate {
    my $this = shift;
    my $type = $this->getdata("type");
    if ($session{renewal_type}) {
        $type = $session{renewal_type};
        if (   $type !~ /^(staff|secondary)$/
            && $type !~ /^guest/
            && $type !~ /^member\//)
        {
            $type = "member/$type";
        }
    }
    if ($type =~ /^member/) {
        my $expiry_date = $this->getdata("expirydate");
        if ($expiry_date =~ /^(0+\-0+\-0+)$/) { $expiry_date = undef; }
        return $expiry_date;
    } elsif ($type eq "secondary") {

        # secondaries ride on their parent member's membership
        my $parent = $this->parent();
        if ($parent) {

            # danger of infinite loop if relationships are circular
            return $parent->expirydate;
        } else {
            $this->warn($msg{"Secondary member has no parent membership."});
            return;
        }
    }
    return;
}

sub days_to_expiry {
    my $this = shift;
    my $t    = new ExSite::Time;
    my $diff = $t->diffdays($this->expirydate, "sql_date");
    return $diff;
}

# set_expiry - set the expirydate field to an appropriate value
# rollover_date = MM-DD explicit expire/renewal date for all members
#     -> optional, use when org. has a specific start date for memberships
# renewal_window = number of days before expiry that renewal can occur
#     -> eg. set to 30 to allow renewal 30 days before expiry
# expiry_date = treat as member's current expiry date
#     -> defaults to whatever is in the member record

# nothing occurs if expiry_date is in the future by a number of days
# greater than renewal window

sub set_expiry {
    my ($this, $rollover_date, $renewal_window, $expiry_date) = @_;
    my $length = &preference("mterm", "member", $this->id())
      || &preference("Membership.anniversary.length");
    my $unit = &preference("Membership.anniversary.period") || "year";

    # renewal_type is set when we are in the process of renewing
    my $type = $session{renewal_type} || $this->getdata("type");
    return if ($type =~ /^(guest|secondary)/);

    my $today = new ExSite::Time;
    my $out;
    $expiry_date or $expiry_date = $this->getdata("expirydate");

    # null date workaround
    if ($expiry_date =~ /^(0+\-0+\-0+)$/) { $expiry_date = undef; }
    $renewal_window or $renewal_window = 1;
    my $date = new ExSite::Time();    # today
    if ($expiry_date) {
        my $expiry = new ExSite::Time($expiry_date, "sql_date");
        my $diffdays = $date->diff($expiry) / 86400;

        # grace period in days
        my $grace_window = &preference("Membership.late_grace_period");
        if ($grace_window + $diffdays < 0) {

            # reset expiry date if member has been expired for more days
            # than allowed for by grace period
            $expiry_date = undef;
        }
    }

    # member's expiry date (defaults to today)
    my $anniversary = new ExSite::Time($expiry_date, $expiry_date ? "sql_date" : undef);

    # diffdays is # of days from now til membership expires
    my $diffdays = int($date->diff($anniversary) / 86400);
    if ($diffdays <= $renewal_window) {

        # we are in the early renewal window - roll forward to next year
        if ($rollover_date) {

            # term is fixed
            if ($rollover_date =~ /^[01]?\d\-[0123]?\d$/) {

                # rollover year should be the greater of current expirydate's year or current year
                my $year;
                if ($anniversary->write("year") > $date->write("year")) {
                    $year = $anniversary->write("year");
                } else {
                    $year = $date->write("year");
                }
                my $rolldate = "$year-$rollover_date";
                my $diffdays = int($date->diff($rolldate, "iso_date") / 86400);
                if ($diffdays > $renewal_window) {

                    # rollover date is in the future; it is our new expiry date
                    $expiry_date = $rolldate;
                } else {

                    # rollover date is in the past, or in our renewal window
                    # our expiry date should be 1 year hence
                    $date->set($rolldate, "iso_date");
                    $date->add_approximate($length, $unit);
                    my $diffdays = int($today->diff($date, "iso_date") / 86400);
                    if ($diffdays <= $renewal_window) {
                        $date->add_approximate(1, "year");
                    }
                    $expiry_date = $date->write("sql_date");
                }
            } else {
                $out .= $this->error("Invalid rollover date: $rollover_date");
            }
            $date->set($expiry_date, "sql_date");
        } else {

            # no master rollover date; membership lasts n year(s) from sign-up by default
            if ($anniversary) {
                $date->set($anniversary->write("sql_date"), "sql_date");
                if ($date->validate()) {

                    # invalid date; reset to today
                    $date->set();
                }
            }
            $date->add_approximate($length, $unit);
        }
        if (&preference("Membership.month_end_expirydate")) {

            # force expiry date to be last day of month
            $date->write("sql_date") =~ /(\d{4})-(\d{2})-(\d{2})/;
            $date->set("$1-$2-" . $date->days_in_month, "sql_date");
        }

        # force minimum membership term in years
        my $min_term = &preference("Membership.min_membership_term");
        my $diffyears = $today->diff($date, "sql_date") / 31536000;
        if ($min_term && ($diffyears < $min_term)) {
            $date->add_approximate(ceil($min_term - $diffyears), "year");
        }
        $this->setdata("expirydate", $date->write("sql_date"));
        $this->run_handler("Member_post_set_expiry");
        return $this->expirydate();
        ### WARNING : not saved!
    } else {
        $out .=
          $this->warn("Membership expiry date is already $diffdays days in the future.  Expiry date not changed.");
    }
    return;
}

#-----------------------------------------------------------------------
# promotion - change a member from a secondary (having a parent) to a
# primary (no parent). This will also demote the previous primary to become
# a secondary.

sub promote {
    my $this = shift;
    my $stat = $this->run_handler("promote_member");
    return if defined $stat;
    my $out;
    my $ml     = &get_obj("ML");
    my $name   = $this->name;
    my $uid    = $this->uid;
    my $parent = $this->parent();
    my $type   = $this->member_type();
    if (!$parent) {

        if ($this->getdata("type") eq "secondary") {
            $out .= $this->warn("No existing parent record.");
            $this->setdata("parent_id", 0);
            $this->setdata("type",      "guest");
            if ($this->save()) {
                $out .= $ml->info("$name (UID $uid) was promoted to a primary member/user.");
            }
        } else {
            $out .= $this->warn("This member is already the primary user.");
        }
        return $out;
    }
    $this->setdata("parent_id", 0);
    if ($this->getdata("type") eq "secondary") {
        $this->setdata("type", $parent->getdata("type") || "guest");
        $this->setdata("expirydate", $parent->getdata("expirydate"))
          if ($parent->status);
    }

    my $pname = $parent->name;
    my $puid  = $parent->uid;
    if ($parent->defined) {

        # copy specified parent meta data to promoted member
        my @meta;
        if (ref $config{Membership}{promote}{copy_meta} eq "ARRAY") {
            @meta = @{$config{Membership}{promote}{copy_meta}};
        } else {
            @meta = $this->meta()->get_allowed();
        }
        foreach my $key (@meta) {
            $this->meta_set($key, $parent->meta_get($key));
        }
    }
    if ($this->force_save()) {
        $out .= $ml->info("$name (UID $uid) was promoted to a primary member/user.");
        if ($type eq "secondary" || !$this->status_obj) {
            $share{DB}->update("member_status", {member_id => $this->id()}, {member_id => $parent->id()});
            $this->set_status($parent->status, "member promotion; copied status from UID $puid");
        }

        if ($parent->defined) {
            my $r = $share{DB}->fetch_match("member", {parent_id => $parent->uid});
            $share{DB}->update("member", {parent_id => $uid}, {parent_id => $puid});
            if ($share{DB}{map}->is_mapped("member_category_join")) {
                $share{DB}->update("member_category_join", {member_id => $uid}, {member_id => $puid});
            }
            if ($share{DB}{map}->is_mapped("location")) {

                # remove existing contact record and replace with locations
                my $type = ExSite::Config::preference("Organization.contact_type");
                my $c    = $this->account->get_contact($type);
                $c->delete();
                my @locations =
                  $share{DB}->fetch_match("location", {member_id => $puid});
                foreach my $loc (@locations) {
                    my $l = new Modules::ID::Location(id => $loc->{location_id});
                    $l->setdata("member_id", $uid);
                    $l->force_save();
                    my $contacts = $l->contacts();
                    $contacts->reset();
                    while (my $contact = $contacts->next()) {
                        $contact->setdata("account_id", $this->account->id);
                        $contact->force_save();
                    }
                }
            }

            # re-tie membership receivables - defined as invoices with any membership items
            my @receivable = $share{DB}->fetch_match("receivable", {account_id => $parent->account->id()});
            foreach my $r (@receivable) {
                my $inv = new Modules::Finance::Receivable(id => $r->{receivable_id});
                my $payments = $inv->get_payments();
                if ($payments) {
                    while (my $pay = $payments->next()) {
                        $pay->setdata("account_id", $this->account->id());
                        $pay->force_save();
                    }
                }
                next if (!$inv->is_active());
                $inv->setdata("account_id", $this->account->id());
                $inv->force_save();
                my $itemlist = $inv->loaditems();
                if ($itemlist && $itemlist->count()) {
                    while (my $item = $itemlist->next()) {
                        if (   $item->getdata("objtype") eq "member"
                            && $item->getdata("objid") == $parent->id())
                        {
                            $item->setdata("objid", $this->id());
                            $item->force_save();
                        }
                    }
                }
            }
            $parent->setdata("parent_id", $this->uid());
            $parent->setdata("type", "secondary") if ($type eq "secondary");
            if ($parent->force_save()) {
                $out .= $ml->warn("The former primary, $pname (UID $puid), was demoted to secondary status.");
                $out .= $ml->p($ml->a("Return to promoted profile", {href => $this->link(pro => undef)}));
                #### update parent status???

            } else {
                $out .= $this->error("Failed to demote previous primary $pname (UID $puid).");
                $out .= $this->warn("There are now two primary users in this user group.");
                $out .= $share{DB}->show_diagnostics("html");
            }
        } else {
            $out .= $this->warn("Previous primary not found.");
        }
    } else {
        $out .= $this->error("Failed to promote $name (UID $uid) to primary member/user.");
        $out .= $share{DB}->show_diagnostics("html");
    }
    return $out;
}

#-----------------------------------------------------------------------
# membership status

sub use_status {
    my $this = shift;
    return 0 if ($this->member_type() eq "secondary");
    return ($this->is_member() && $share{DB}{map}->is_mapped("member_status"));
}

sub status {
    my $this = shift;
    my $type = $this->getdata("type");
    if ($type =~ /^member/) {
        if ($this->use_status) {
            return $this->{data}{status} if ($this->{data}{status});
            my $st_obj = $this->status_obj();
            return $st_obj->getdata("status") if ($st_obj);
            return &preference("Membership.default_status");
        }
    } elsif ($type eq "secondary") {

        # secondaries ride on their parent member's membership
        my $parent_id = $this->getdata("parent_id");
        if ($parent_id) {

            # danger of infinite loop if relationships are circular
            my $parent = new Modules::Membership::Member(id => $parent_id);
            return $parent->status();
        } else {
            $this->warn("Secondary member has no parent membership.");
        }
    } elsif ($type eq "staff") {

        # staff are permanently active
        return "active";
    }
    return;
}

sub status_obj {
    my $this = shift;
    if ($this->use_status) {
        $this->load() if (!defined $this->{children}{member_status});
        if ($this->{children}{member_status}) {
            return $this->{children}{member_status}->last();
        }
    }
    return;
}

sub is_active {
    my $this = shift;
    return ($this->status eq "active");
}

sub is_incomplete {
    my $this = shift;
    return ($this->status eq "incomplete");
}

sub is_pending {
    my $this = shift;
    return ($this->status eq "pending");
}

sub is_expired {
    my $this = shift;
    return ($this->status eq "expired");
}

sub is_archived {
    my $this = shift;
    return ($this->status eq "archived");
}

sub is_past_expiry {
    my $this        = shift;
    my $expiry_date = $this->getdata("expirydate");
    if ($expiry_date) {
        my $t = new ExSite::Time();
        if ($t->compare($expiry_date, "sql_date") < 0) {

            # expiry date is in the past
            return 1;
        }
        return 0;
    }
    return;
}

sub is_renewable {
    my ($this, $ignore_permissions, $renewal_window) = @_;
    my $stat = $this->run_handler("Membership_is_renewable", $this);
    return $stat if (defined $stat);
    if ($this->is_member() && $this->member_type() ne "secondary") {
        if (   $ignore_permissions
            || $this->is_me
            || $this->is_child
            || $this->is_parent
            || $this->is_sibling
            || $share{DB}->is_manager($this->gid()))
        {
            my $status = $this->status;
            if ($status eq "active") {
                $renewal_window = $renewal_window
                  || &preference("Membership.early_renewal_period");
                my $t = new ExSite::Time;
                my $diff = int($t->diff($this->expirydate, "sql_date") / 86400);
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

sub is_valid_member {
    my $this = shift;
    my $type = $this->getdata("type");

    # the user is tied to a particular site
    if ($config{auth}{scope} eq "local" && $this->gid) {
        my $site = $share{DB}->this_site;
        my $sid = ref $site eq "HASH" ? $site->{section_id} : $site;
        return 0 if ($sid != $this->gid);
    }

    if ($type =~ /^member/) {

        # members are directly responsible for their own membership status
        if ($this->use_status) {

            # check membership status
            my $status = $this->status;

            if ($status eq "active") {
                return 1;
            } elsif ($status eq "pending") {
                if ($this->status_obj) {
                    my $statuses = $this->{children}{member_status};
                    while (my $prev = $statuses->previous()) {

                        # recently expired or active members retain privileges for grace period
                        if (   $prev
                            && $prev->getdata("status") =~ /^(expired|active)$/)
                        {
                            my $date = new ExSite::Time($this->getdata("expirydate"), "sql_date");
                            $date->add(&preference("Membership.late_grace_period"), 'days');
                            return 1 if ($date > new ExSite::Time);
                        }
                    }
                }
                return &preference("Membership.pending_are_members");
            } elsif ($status eq "expired") {
                if ($share{EvtReg}{members_fee} || $share{members_fee}) {

                    # if member is in process of renewing give them temporary access
                    # Pay_delete handler should be installed to halt checkout if renewal is no longer in cart
                    my $inv = $session{invoice};
                    my @items =
                      $share{DB}->custom_query("select note from receivable_item where receivable_id = ?", $inv);
                    foreach my $item (@items) {
                        if ($item->{note} =~ /^Renew:/) {
                            $session{Membership}{evt_renewal_pass} = 1;
                            return 1;
                        }
                    }
                    my $pref = &preference("Membership.expired_get_member_rate");
                    return $pref if (defined $pref);
                }
                return &preference("Membership.expired_are_members");
            } elsif ($status eq "archived") {
                if ($share{EvtReg}{members_fee} || $share{members_fee}) {

                    # if member is in process of renewing give them temporary access
                    # Pay_delete handler should be installed to halt checkout if renewal is no longer in cart
                    my $inv = $session{invoice};
                    my @items =
                      $share{DB}->custom_query("select note from receivable_item where receivable_id = ?", $inv);
                    foreach my $item (@items) {
                        if ($item->{note} =~ /^Renew:/) {
                            $session{Membership}{evt_renewal_pass} = 1;
                            return 1;
                        }
                    }
                }
                return 0;
            }
            return 0;
        }
        return 1;
    } elsif ($type eq "secondary") {

        # secondaries ride on their parent member's membership
        my $parent_id = $this->getdata("parent_id");
        if ($parent_id && $parent_id != $this->id) {
            my $parent = new Modules::Membership::Member(id => $parent_id);
            return $parent->is_valid_member();
        } else {
            $this->warn("Secondary member has no parent membership.");
            return 0;
        }
    } elsif ($type eq "staff") {

        # staff automatically have membership privileges
        return 1;
    } elsif ($type =~ /^guest/) {

        # guests do not have membership privileges
        return 0;
    }
    return;
}

# has this member been approved (active) at least once

sub is_approved_member {
    my $this = shift;
    if ($this->use_status) {
        my $statuses = $this->{children}{member_status};
        $statuses->reset();
        while (my $prev = $statuses->next()) {
            return 1 if ($prev->getdata("status") eq "active");
        }
    }
    return 0;
}

sub set_status {
    my ($this, $status, $note) = @_;
    if ($this->use_status) {
        my $my_uid = $share{DB}->my_uid() || 0;
        my %status = (
            member_id      => $this->uid,
            transaction_id => $this->gid . "-" . $my_uid . "-" . time,
            change_by      => $my_uid,
            status         => $status,
            note           => $note
        );
        $status{member_type} = $this->member_type()
          if ($share{DB}{map}->is_mapped("member_status", "member_type"));
        $status{expirydate} = $this->expirydate()
          if ($share{DB}{map}->is_mapped("member_status", "expirydate"));
        my $status_id = $share{DB}->insert("member_status", \%status);
        if ($status_id) {

            # reload our status history
            my %children = $this->get_children("member_status");
            $this->{children}{member_status} = $children{member_status};
        }
        return $status_id;
    }
    return;
}

sub set_status_active {
    my ($this, $note) = @_;
    $this->set_status("active", $note);
}

sub set_status_incomplete {
    my ($this, $note) = @_;
    $this->set_status("incomplete", $note);
}

sub set_status_pending {
    my ($this, $note) = @_;
    $this->set_status("pending", $note);
}

sub set_status_expired {
    my ($this, $note) = @_;
    $this->set_status("expired", $note);
}

sub set_status_archived {
    my ($this, $note) = @_;
    $this->set_status("archived", $note);
}

# membership status management methods

# approve: pending -> active

sub status_approve {
    my $this = shift;
    if ($this->use_status && $this->is_pending) {
        my $stat = $this->run_handler("approve_membership");
        return if defined $stat;
        $this->set_status_active("approved by " . $share{DB}->my_name());
        $this->enable();
    }
    return;
}

# reject: restore previous state

sub status_reject {
    my $this = shift;
    if ($this->use_status) {
        $this->load() if (!defined $this->{data});
        if ($this->{children}{member_status}) {
            my $stat = $this->run_handler("reject_membership");
            return if defined $stat;
            my $latest;
            my $previous;
            while (my $stat = $this->{children}{member_status}->pop()) {
                if (!$latest) {
                    $latest = $stat;
                } elsif (!$previous
                    && $stat->getdata("status") ne $latest->getdata("status"))
                {
                    $previous = $stat;
                    last;
                }
            }
            if ($latest && $latest->getdata("status") eq "pending") {
                if ($previous && $previous->getdata("status") ne "pending") {
                    $previous->clone();
                    $previous->setdata("note", "rejected pending application; restored previous status");
                    $previous->save();
                } else {

                    # no previous status - bogus membership application?
                    $this->status_archive("application declined");
                }
            }
        }
    }
    return;
}

# expire: active->expired

sub status_expire {
    my $this = shift;
    if ($this->use_status) {
        if ($this->is_active) {
            my $stat = $this->run_handler("expire_membership");
            return if defined $stat;
            $this->set_status_expired();

            # change expiry date to today
            my $today = new ExSite::Time;
            $this->setdata("expirydate", $today->write("sql_date"));
            $this->save or $this->error("failed to update expiry date");
        }
    }
}

# archive: expired->archived or pending->archived

sub status_archive {
    my $this = shift;
    if ($this->use_status) {
        my $stat = $this->run_handler("archive_membership");
        return if defined $stat;
        return $this->set_status_archived(@_);
    }
    return 0;
}

# dearchive: archived->expired

sub status_dearchive {
    my $this = shift;
    if ($this->use_status) {
        my $stat = $this->run_handler("dearchive_membership");
        return if defined $stat;
        $this->set_status_expired(@_);
        $this->enable;
    }
}

# enable/disable login privileges

sub disable {
    my $this  = shift;
    my $login = $this->getdata("login");
    $this->setdata("login",  "NOLOGIN:$login");
    $this->setdata("access", 0);
    return $this->force_save;
}

sub enable {
    my $this     = shift;
    my $login    = $this->getdata("login");
    my $is_dirty = 0;
    if ($login =~ /^NOLOGIN:/) {
        $login =~ s/^NOLOGIN://;
        $this->setdata("login", $login);
        $is_dirty = 1;

        ### FIXME: validate that this login is still unique
    }
    if ($this->getdata("access") < 1) {
        $this->setdata("access", 1);
        $is_dirty = 1;
    }
    $this->set_password();
    my $stat;
    $stat = $this->force_save if ($is_dirty);
    return $stat;
}

# can this member log in?

sub enabled {
    my $this  = shift;
    my $mdata = $this->{data};
    return ($mdata->{login} && $mdata->{password} && $mdata->{access});
}

# assign randomized password if one does not already exist

sub set_password {
    my ($this, $passwd, $login) = @_;
    if (!$this->getdata("password")) {
        my $ntry = 0;
        do {
            $passwd =
              &randtext(2, "23456789") . &randtext(4, "abcdefghjkmnpqrstuvwxyz") . &randtext(2, "23456789");
            $ntry++;
        } until ($ntry > 10 || !$share{DB}->validate_password($passwd, $login, 1));
        if ($ntry > 10) {
            $this->error("Could not generate password - please contact the administrator.");
        } else {
            my $passtype = $config{auth}{password_storage_method};
            my $pass_store = $share{DB}->encode_password($passwd, $passtype);

            # since we have assigned this password attach the passtype to flag user to set new password
            $pass_store = "[$passtype] $pass_store"
              if ($passtype ne "cleartext");
            $this->setdata("password", $pass_store);
        }
    } else {
        $passwd = $this->getdata("password");
    }
    return $passwd;
}

# convervative non-recursive delete

sub delete {
    my $this = shift;
    if ($this->ok) {
        return $this->DB()->trash_key($this->{type}, $this->{id});
    }
    return;
}

# member is an orphan ( unattached secondary member )

sub is_orphan {
    my $this = shift;
    if ($this->member_type() eq "secondary") {
        my $parent = $this->parent();
        if ($parent && $parent->ok()) {
            return 0;
        }
        return 1;
    }
    return 0;
}

1;
