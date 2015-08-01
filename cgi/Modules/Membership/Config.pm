#!/usr/bin/perl

package Modules::Membership::Config;

use strict;
use ExSite::Config qw(%config %share %store &read_exsite_conf &preference);
use Modules::BaseDCD;
use Exporter;

use utf8;
use Unicode::Normalize;

use vars qw(@ISA @EXPORT_OK %conf);
@ISA = qw(Modules::BaseDCD Exporter);
@EXPORT_OK = qw(confrule);

%conf = (

    #================================================================
    # SYSTEM SETTINGS

    # flag to denote that defaults have been loaded
    _default => 1,

    isRestricted => 0,

    #================================================================
    # ASSOCIATION SETTINGS

    membership => {acctcode => 1},

    # default administrator email address / used as from address for notifications or as fallback email address
    owner_email => undef,

    # where administrator notifications are sent
    notification_email => undef,

    # if applicaable, date on which memberships for the association expire
    rollover_date => undef,

    # length of membership if no rollover date
    anniversary => {
        length => 1,
        period => "year"
    },

    # how soon before expiry can you renew?
    early_renewal_period => 40,

    # days after expiry before a member is archived
    late_grace_period => 90,

    # statuses considered valid member
    pending_are_members => 0,
    expired_are_members => 1,

    # member-only rates for expired members
    expired_get_member_rate => 0,

    # statuses for which members are visible in indexes
    visible_status => "active,expired",

    # approval of memberships is manual or auto
    # set manual if admins need to approve memberships paid via e-commerce
    approval => "auto",

    # default privacy level to apply to profile and address cards for new members
    privacy => "members only",

    # default privacy level to apply to address cards, falls back on privacy
    contact_privacy => undef,

    # settings for preventing duplicate members
    dupe_check => {
        email => 1
    },

    # membership renewal reminder emails
    renewal_email => {

        # reminders are sent on x days before and after expiry
        days => "-30,1,30",

        # exclude secondary members from receiving renewal emails
        exclude_secondary => 0
    },

    # application fee (amount)
    application_fee => 0,

    # prorated fees
    prorated_renewal     => 0,
    prorated_application => 0,

    # allow user to choose their membership term
    # comma delimted list of periods (e.g 1,2,3 (years by default))
    selectable_terms => undef,

    # do guests have login privileges
    guest_login => 0,

    # allow guests to upgrade to a membership
    allow_upgrades => 0,

    # allow guests to upgrade to a membership
    allow_length_select => 0,

    # disallow members from changing their membership type upon renewal
    force_renewal_type => 1,

    # membership "homepage"
    home_page_filename => "members_home.html",

    #================================================================
    # SECONDARY MEMBERS

    secondary => {

        # allow secondary members
        allow => 0,

        # maximum number of secondary members allowed
        limit => undef,

        # charge membership dues to secondary members
        fee => 0,

        # what are secondary members called?
        label        => "Contact",
        plural_label => "Other Contacts",

        # how are secondary members shown
        # link_to_profile, mailto, contact_info, mini_profile, linked_mini_profile, name
        format    => "link_to_profile",
        container => "p",

        # add secondary link in user profile
        add => 0,

        # primary users can edit secondaries' data
        edit => 1,

        # member types which allow secondary members
        types_allowed => undef,

        # override secondary member type with that of parent
        use_parent_type => 0,

        # secondary member type
        type => "secondary",

        # require secondary members to checkout even when there are no charges
        must_checkout => 0,

        # require renewal if renewable prior to adding secondary members
        # prevents active secondary members from existing under expired primary memberships
        disallow_if_renewable => 0
    },

    #================================================================
    # PROFILE/FORM SETTINGS

    name => {

        # default or organization
        type => undef,

        # ordering = first, last
        precedence => "first"
    },

    default_photo     => "<img src=[[nophoto.png]]>",
    default_thumbnail => "<img src=[[nophoto_s.png]]>",
    photo             => {
        shrinksize => 250,
    },

    # printable membership card / requires css
    show_membership_card => 0,

    profile => {
        template => {

            # use a template in the control panel
            control_panel => 0
        }
    },

    # embedded contact form
    contact_form => {
        enabled  => 0,
        type     => undef,
        fields   => "address,city,provstate,country,pcode,phone1",
        required => "address,city,pcode,phone1"
    },

    # hide the following member columns from membership forms
    hide => "middle_name",

    meta => {

        # scope is local|global
        # global allows shared meta attributes amongst sections
        scope => "local"
    },

    #================================================================
    # INDEX, DIRECTORY AND SEARCH SETTINGS

    sort_field => "last_name",

    directory => {
        use_contact_privacy => 0,
        contacts_type       => undef,
        new_member_limit    => 5,

        # show counts when browsing by attribute
        show_count => 1,

        # show members with a secondary membership type
        show_secondary => 0,

        # when to use alphalist
        alphalist_min => 300
    },

    # advanced search
    advanced_search => {
        admin_columns => undef,
        columns       => "first_name,last_name"
    },

    #================================================================
    # LABELS, MESSAGES and HEADINGS

    # display the cost in membership type menus?
    show_cost_in_menu => 1,

    # headings
    heading => {
        new_member     => "New [[type]] Sign-up",
        new_guest      => "Registration",
        update_profile => "Update Profile"
    },

    # messages
    message => {
        expired  => "Your membership expired [[ago]] on [[date]].",
        expiring => "Your membership expires in [[ndays]] day(s) on [[date]].",
        renew_group =>
"<h1>Renew Members Of Your [[label]]</h1><p>The following members of your [[label]] are currently up for renewal. Check them off and click 'renew' to add them to your shopping cart. You will have an opportunity to add new members on the next screen.</p>",
        good_status     => "Your membership is in good standing.",
        archived_status => "Your membership has lapsed, and is now archived.",
        pending_payment => "Your membership is pending payment on your outstanding balance.",
        pending_status  => "Your membership is pending approval.",
        incomplete_status =>
"Your membership application is incomplete. If you require assistance, please contact the website administrator.",
        no_membership => "You do not have a membership.",
        email_extended =>
"Thank you for registering as a member at [[sitename]].\nTo complete your application please login using the following credentials:\n\nLogin: [[login]]\nPassword: [[password]]",
        set_password =>
"Our records indicate that you have not set your own password for this website. For security reasons, please setup a new password below.",
        create_password => "Your login is [[login]]. Please create a new password for this website below.",
        key_contact_description =>
"As a designated contact for your organization, you have been given permission to switch to your Primary Member's account.",
        key_contact_login_label => "Login as [[name]]",
        key_contact_message     => "You are temporarily logged in as the primary contact for your organization."
    },

    # labels
    label => {
        view_profile      => "View Profile",
        edit_profile      => "Update Profile",
        edit_contacts     => "Update Addresses",
        edit_photo        => "Update Photo",
        edit_password     => "Change Password",
        back_to_profile   => "Back to Profile",
        add_secondary     => "Add Secondary Contact",
        account_statement => undef
    },

    # billing
    receivable_item_name        => "Membership Dues",
    receivable_item_description => "[[purchase_type]] membership ([[type]]) for [[name]] [[start]] - [[end]]",

);

# confrule: find and return a configuration rule
# context: context within Membership system (e.g type)
# value: value of context
# e.g context: membertype value: Professional

sub confrule {
    my ($preference, $context, $value) = @_;
    my $conf;
    if ($context eq "membertype" && $value) {
        my $conf = &preference("Membership._type.$value.$preference");
        return $conf if ($conf);
    }
    $conf = &preference("Membership.$preference");
    return $conf;
}

sub new {
    my $this = shift;
    my $opt  = shift;

    my $obj = {};
    my $class = ref($this) || $this;
    bless $obj, $class;

    # setup base object
    $obj->initialize_object;
    foreach my $key (keys %$opt) { $obj->{$key} = $opt->{$key}; }

    # default configuration
    my $whatami = "Membership";
    if (!$store{"config:Membership"}{_default}) {
        delete $store{"config:Membership"};
    }
    $config{$whatami} = \%conf;

    # load site-specific configurations
    $obj->read_conf($whatami);

    # include configurations in the object, so we don't need to know our
    # module name
    $obj->{config} = $config{$whatami};

    # load preferences
    my $site = $share{DB}->this_site();
    if ($site) {
        my $section_id = $site->{section_id};
        my @attr       = $share{DB}->fetch_match(
            "attribute",
            {
                tablename => "section",
                id        => $section_id
            }
        );
        foreach my $attr (@attr) {
            my @param = split /\./, $attr->{name};
            next if shift @param ne $whatami;
            my $conf = $obj->{config};
            while (my $p = shift @param) {
                if (@param == 0) {
                    $conf->{$p} = $attr->{value};
                } elsif (!exists $conf->{$p}) {
                    $conf->{$p} = {};
                    $conf = $conf->{$p};
                } else {
                    $conf = $conf->{$p};
                }
            }
        }
    }

    # automatically read the default input source
    $obj->read;

    # misc setup
    $obj->setup($opt);

    return $obj;
}

# dummy setup
sub setup {
    my ($this, $opt) = @_;
    return;
}

# return path info to member when linking
sub path {
    my ($this, $member) = @_;
    my $stat = $this->run_handler("Membership_path", $member);
    if ($stat) {
        return wantarray ? @$stat : join "/", @$stat;
    }
    my @path;
    my $name = $member->name();
    $name = utf8_normalize($name);
    $name =~ s/[^\w]+/-/g;
    $name =~ s/^\-|\-$//g;
    push @path, ($member->uid . '-' . $name || $member->uid);
    unshift @path, undef;
    return wantarray ? @path : join "/", @path;
}

sub utf8_normalize {
    my $text = shift;
    require Unicode::Normalize;
    require Encode;
    $text = Encode::decode("utf8", $text);
    $text = Unicode::Normalize::NFD($text);
    $text =~ s/\pM//g;
    return $text;
}

1;
