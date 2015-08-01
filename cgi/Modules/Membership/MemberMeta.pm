package Modules::Membership::MemberMeta;

#-----------------------------------------------------------------------
#
#   Copyright 2001-2008 Exware Solutions, Inc.  http://www.exware.com
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

# Modules::Membership::MemberMeta - Member-specific meta-data
# based on ExSite::Attribute

use strict;
use ExSite::Config;
use ExSite::Attribute;
use ExSite::Misc;
use ExSite::Base;

use vars qw(@ISA);
@ISA = qw(ExSite::Attribute);

# create an attribute object, and tie it to a record

sub new {
    my ($this, $table, $id) = @_;

    # NB: $table is ignored, used only for compatibility with Attribute
    my $obj = {};
    my $class = ref($this) || $this;
    bless $obj, $class;
    $obj->setup($config{auth}{user_table}, $id);
    return $obj;
}

# change record
##
sub setup {
    my ($this, $table, $member) = @_;

    # NB: $table is ignored, used only for compatibility with Attribute

    # metadata for this record
    $this->{loaded} = undef;
    $this->{raw}    = undef;    # copy of metadata records
    $this->{data}   = undef;    # attribute values
    $this->{dirty}  = undef;    # flag changed metadata

    # basic defs
    $this->{attr_table} = "member_attribute";
    $this->{attr_id}    = "member_attribute_id";
    $this->{idcol}      = "member_id";
    $this->{table}      = $config{auth}{user_table};
    if ((ref $member) =~ /Member/) {

        # member is passed as an object
        if ($member->defined) {
            $this->{member}    = $member->get();
            $this->{parent_id} = $member->getdata("parent_id");
        }
        $this->{id} = $member->id;
    } elsif ($member =~ /^\d+$/) {

        # member id
        $this->{member}    = $share{DB}->fetch($this->{table}, $member);
        $this->{parent_id} = $this->{member}{parent_id};
        $this->{id}        = $member;
    } else {

        # undefined
        $this->{id} = undef;
    }

    # allowed metadata for this record
    # (don't reset this unless we are also changing sections)
    if (&preference("Membership.meta.scope") eq "global") {
        if ($this->{member}) {
            $this->{section_id} = 0;
            $this->{allowed}    = undef;
        }
        if (!$this->{section_id}) {
            $this->{section_id} = 0;
        }
    } else {

        if ($this->{member}) {
            if ($this->{member}{section_id} != $this->{section_id}) {
                $this->{section_id} = $this->{member}{section_id};
                $this->{allowed}    = undef;
            }
        }
        if (!$this->{section_id}) {
            my $section = $share{DB}->this_site();
            if ($section) {
                $this->{section_id} = $section->{section_id};
            }
        }
    }
    return;
}

# load up the attributes

sub load_allowed {
    my ($this) = @_;
    if (&preference("Membership.meta.scope") eq "global") {
        my @attr = $this->fetch_match({member_id => 0, section_id => 0});
        $this->{allowed} = &keywise("name", \@attr);
    }

    # allowed attributes are attached to the user's section
    if ($this->{section_id}) {
        my @attr = $this->fetch_match({member_id => 0, section_id => $this->{section_id}});
        $this->{allowed} = &keywise("name", \@attr);
    }
}

# allowed attribute names

sub is_allowed {
    my ($this, $name) = @_;
    my $stat = $share{DB}->run_handler("member_meta_is_allowed", $this, $name);
    return $stat if (defined $stat);
    $this->load_allowed();
    if (scalar keys %{$this->{allowed}} > 0) {

        # we have an explicit allow list for attributes
        return exists $this->{allowed}{$name};
    }

    # there is no allow list; no attributes are permitted
    return 0;
}

sub get_allowed {
    my ($this) = @_;
    my $member = $this->{member};

    # check for configurable meta fields
    my $stat = $share{DB}->run_handler("member_meta_get_allowed", $member);
    return @$stat if (defined $stat);
    my $type;
    $type = $member->{type} if (ref($member) eq "HASH");
    $type =~ s/^member\///;

    # check for configurable meta fields
    my $allowed;
    if ($type) {
        $allowed = &preference("Member.meta_allowed.$type")
          || &preference("Membership.meta_allowed.$type");
    }
    if ($type eq "secondary"
        || ($this->{parent_id} || $member->{parent_id}) && &preference("Membership.meta_allowed._secondary"))
    {
        $allowed = &preference("Member.meta_allowed._secondary")
          || &preference("Membership.meta_allowed._secondary");
    }
    if (defined $allowed) {
        return if (!$allowed);
        return split /,/, $allowed;
    }
    my @allowed    = $this->SUPER::get_allowed;
    my $disallowed = &preference("Member.meta_disallowed.$type")
      || &preference("Membership.meta_disallowed.$type");
    if (($this->{parent_id} || $member->{parent_id})
        && &preference("Membership.meta_disallowed._secondary"))
    {
        $disallowed = &preference("Member.meta_disallowed._secondary")
          || &preference("Membership.meta_disallowed._secondary");
    }
    if ($disallowed) {
        $disallowed =~ s/,/\|/g;
        return grep(!/^($disallowed)$/, @allowed);
    }
    return @allowed;
}

# remove an allowed attribute (admin users only)
# also removes all attributes of that name

sub remove_allowed {
    my ($this, $name) = @_;
    my $db = $share{DB};
    if ($db->is_admin && $this->{section_id}) {
        if ($this->is_allowed($name)) {

            # remove from database

            my @attr = $this->fetch_match({section_id => $this->{section_id}, name => $name});
            my @trash;
            foreach my $attr (@attr) {
                push @trash, $this->{attr_table}, $attr->{$this->{idcol}};
            }
            $this->trash(@trash);

            # remove from current object

            delete $this->{allowed}{$name};
            delete $this->{raw}{$name};
            delete $this->{data}{$name};
            delete $this->{dirty}{$name};

            return scalar @attr;
        } else {
            $this->error("ExSite::Attribute::remove_allowed: invalid attribute: $name");
        }
    } else {
        $this->error("ExSite::Attribute::remove_allowed: permission denied");
    }
    return 0;
}

# copy one or all attributes to another record ID

sub copy {
    my ($this, $id, $name) = @_;
    if ($this->ok) {
        my @keys = $name ? ($name) : keys %{$this->{data}};
        if (@keys > 0 && $id) {
            my $a = new Modules::Membership::MemberMeta($id);
            foreach my $k (@keys) {
                $a->set_nosave($k, $this->get($k));
            }
            $a->save();
        }
    }
}

sub required {
    my ($this, $name) = @_;
    my $stat = $share{DB}->run_handler("member_meta_required", $name);
    return $stat if (defined $stat);
    return ($this->get_map_info($name, "validate") =~ /soft|hard/) || 0;
}

sub clear_all {
    my ($this) = @_;

    if ($this->ok) {
        $this->load;
        my @attr = $this->fetch_match({member_id => $this->{id}});
        if (@attr > 0) {
            $this->trash(@attr);
            $this->{raw}   = undef;
            $this->{data}  = undef;
            $this->{dirty} = undef;
            return scalar @attr;
        }
    }
    return 0;
}

# generate an input tag to accept attribute values
# NOTE: cannot do foreign key refs in attributes

# DB accessors, for easier overloading
# All low-level DB logic is defined here.

# make_record() - create a datahash suitable for inserting into the DB
# use insert() to actually insert it.

sub make_record {
    my ($this, $name, $value, $id) = @_;
    my $data = {
        name      => $name,
        value     => $value,
        member_id => (defined $id ? $id : $this->{id}),
    };
    if (!$data->{id}) {
        $data->{section_id} = $this->{section_id};
    }
    return $data;
}

# input : use $this->SUPER::input();
# except for member type, which has special rules

sub input {
    my ($this, $input_name, $name, $value) = @_;
    if ($this->is_allowed($name) && !$input_name) {
        my $rec      = $this->recid($name);
        my $datatype = $this->get_datatype($name);
        my $out;
        $out .= $share{DB}->input_html(
            name  => join($config{form}{sepchar}, $this->{attr_table}, $rec, "member_id"),
            type  => "hidden",
            value => $this->{id},
        );
        $out .= $share{DB}->input_html(
            name  => join($config{form}{sepchar}, $this->{attr_table}, $rec, "name"),
            type  => "hidden",
            value => $name,
        );
        $out .= $this->input_column(
            prompt   => $this->label($name),
            record   => $rec,
            size     => $this->get_map_info($name, "size"),
            datatype => $datatype,
            value    => ($value || $this->get($name))
        );
    } else {
        return $this->SUPER::input($input_name, $name, $value);
    }
}

sub label {
    my ($this, $name) = @_;
    my $label = $this->get_map_info($name, "label") || ucfirst $name;
    return $msg{$label};
}

sub input_old {
    my ($this, $input_name, $name, $value) = @_;
    if ($name eq "type") {
        $this->load();
        my @mtype;
        my @attr = $share{DB}->fetch_match(
            "attribute",
            {
                tablename => "section",
                id        => $this->{section_id},
                name      => "Profile.membership_fee.%",
            }
        );
        foreach my $attr (@attr) {
            my @attr_field = split /\./, $attr->{name}, 3;
            push @mtype, $attr_field[2];
        }
        if (@mtype > 0) {
            if (!$input_name) {
                my $rec = $this->recid($name);
                my $sc  = $config{form}{sepchar};
                $input_name = "$this->{attr_table}${sc}${rec}${sc}value";
            }
        } else {
            @mtype = ("Regular");
        }
        return $share{DB}->input_html(
            type    => "radio",
            options => \@mtype,
            name    => $input_name,
            prompt  => $this->label($name),
            value   => ($value || $this->get($name)),
            required => ($this->get_map_info($name, "validate") =~ /soft|hard/) || 0,
        );
    }
    return $this->SUPER::input($input_name, $name, $value);
}

1;
