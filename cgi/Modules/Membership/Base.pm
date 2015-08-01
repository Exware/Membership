#!/usr/bin/perl

package Modules::Membership::Base;

use strict;

use ExSite::Config;
use ExSite::Mail;
use ExSite::Misc;
use ExSite::Util;
use Modules::Membership::Config qw(confrule);

use vars qw(@ISA $ml);

@ISA = qw(Modules::Membership::Config);

# setup - initialize object

sub setup {
    my ($this, $opt) = @_;
    $ml = &get_obj("ML");
    my $sid = $this->get_section_id;
    if ($sid && !$this->{section}) {
        $this->{section} = new ExSite::Section(id => $sid);
    }
    return;
}

sub DB {
    my ($this) = @_;
    return $share{DB};
}

# member() - return a Member object for the current profile operation

sub member {
    my $this = shift;
    if (!exists $this->{Member}) {
        $this->{Member} = new Modules::Membership::Member();
    }
    return $this->{Member};
}

# self() - return a Member object for current user

sub self {
    my $this = shift;
    if (!exists $this->{Self}) {
        $this->{Self} = new Modules::Membership::Member(id => $share{DB}->my_uid());
    }
    return $this->{Self};
}

# find - retrieve a member_id by path

sub find {
    my ($this) = @_;
    my $input  = new ExSite::Input;
    my @mpath  = @{$this->{input}{path}};
    if (@mpath > 0) {
        my $name = shift @mpath;
        $name =~ /^(\d+)-/;
        my $uid = $1;
        return $uid if ($uid =~ /\d+/);
    }
    return;
}

sub membername {
    my ($this, $member) = @_;

    # use object for data sanitization
    # but don't use Member object, which is slow
    my $name;
    my $org   = $member->{organization};
    my $name1 = $member->{first_name};
    my $name2 = $member->{middle_name};
    my $name3 = $member->{last_name};
    my $type  = $member->{type};
    $type =~ s/^member\///;

    if (
          !$member->{parent_id}
        && $org
        && $this->allow_add_secondary($type)
        && (   &preference("Member.name.type") eq "organization"
            || &preference("Membership.name.type") eq "organization")
      )
    {
        $name = $org;
    } elsif (!$name1 && !$name2 && !$name3) {
        $name = $org || "NO-NAME";
    } elsif (&preference("Membership.name.precedence") eq "last") {
        $name = $name3 || "NO-LAST-NAME";
        if ($name1) {
            $name .= ", " . $name1;
            if ($name2) {
                $name .= " $name2";
            }
        }
    } else {

        # first name precedence
        my @name;
        push @name, $name1 if $name1;
        push @name, $name2 if $name2;
        push @name, ($name3 || "NO-LAST-NAME");
        $name = join " ", @name;
    }

    # trim leading and trailing whitespace
    $name =~ s/^\s+//;
    $name =~ s/\s+$//;
    return $name;
}

sub load_membership {
    my ($this) = @_;
    my $sid = $this->{section}->id();

    # load members
    $this->{members} = $share{DB}->get_query("all members with status", $sid);

    # load metadata
    my @allmeta = $share{DB}->get_query("all member metadata", $sid);
    $this->{membermeta} = {};
    foreach my $meta (@allmeta) {
        $this->{membermeta}{$meta->{member_id}}{$meta->{name}} =
          $meta->{value};
    }

    # load contact info
    my @alladdr = $share{DB}->get_query("all member contacts", $sid);
    $this->{memberaddr} = {};
    foreach my $add (@alladdr) {
        if (!exists $this->{memberaddr}{$add->{member_id}}) {
            $this->{memberaddr}{$add->{member_id}} = [];
        }
        push @{$this->{memberaddr}{$add->{member_id}}}, $add;
    }
}

sub fetch_metadata {
    my ($this, $type) = @_;
    my $sid = $this->{section}->id();
    $type or $type = "member";
    if (ref $type eq "ARRAY") {
        $type = join "|", @$type;
    }
    $share{DB}->set("max_select", 1000);
    my $meta = $share{DB}->get_query("all member metadata", $sid);
    my %meta;
    while (scalar @$meta > 0) {
        foreach my $m (@$meta) {
            next if ($m->{type} !~ /$type/);
            if (!exists $meta{$m->{member_id}}) {
                $meta{$m->{member_id}} = {};
            }
            if ($m->{type} =~ /$type/) {
                $meta{$m->{member_id}}{$m->{name}} = $m->{value};
            }
        }
        $meta = $share{DB}->fetchrows();
    }
    $share{DB}->set("max_select", 0);
    return wantarray ? %meta : \%meta;
}

# set login

sub set_login {
    my ($this, $login) = @_;
    my $m = $this->member;
    if (!$m->getdata("login")) {
        my $email = $m->getdata("email");
        if ($login) {
            if ($share{DB}->count("member", {login => $login}) > 0) {
                my $ntry = 2;
                while ($share{DB}->count("member", {login => "$login.$ntry"}) > 0) {
                    $ntry++;
                }
                $login = "$login.$ntry";
            }
            $m->setdata("login", $login);
        } elsif ($email
            && $share{DB}->count("member", {login => $email}) == 0)
        {
            $m->setdata("login", $email);
            $login = $email;
        } else {
            my $first  = $m->getdata("first_name");
            my $middle = $m->getdata("middle_name");
            my $last   = $m->getdata("last_name");
            my $login  = $middle ? "$first.$middle.$last" : "$first.$last";
            if ($share{DB}->count("member", {login => $login}) > 0) {
                my $ntry = 2;
                while ($share{DB}->count("member", {login => "$login.$ntry"}) > 0) {
                    $ntry++;
                }
                $login = "$login.$ntry";
            }
            $m->setdata("login", $login);
        }
    } else {
        $login = $m->getdata("login");
    }
    return $login;
}

# assign randomized password if one does not already exist

sub set_password {
    my ($this, $passwd, $login) = @_;
    my $m = $this->member;
    return $m->set_password($passwd, $login);
}

# manual approval - boolean check for manual approval of member applications
# array of manual_approval types can only be set in config at the moment
# if single approval type then preference is a scalar
# ignore_admin - ignore the fact that we are in control panel
sub manual_approval {
    my ($this, $type, $ignore_admin) = @_;
    return 0 if ($type eq "secondary");
    return 0 if ($type eq "guest");
    if (ref &preference("Membership.manual_approval") eq "ARRAY") {
        my $pref = &preference("Membership.manual_approval");
        if (scalar grep(/^$type$/, @$pref) > 0) {
            return 1;
        }
    } elsif (&preference("Membership.manual_approval") eq $type) {
        return 1;
    }
    if (&preference("Membership.approval") eq "auto") {
        return 0;
    } elsif (!$ignore_admin && $this->{admin}) {
        return 0;
    } elsif (&preference("Membership.approval") eq "manual") {
        return 1;
    }
    return 0;
}

# complete the membership renewal process

sub renew_member {
    my ($this, $receivable_id) = @_;
    my $out;
    my $db = $share{DB};
    if ($this->member->status() =~ /expired|archived/ && $receivable_id) {
        $this->member->set_status_pending("intent to renew; invoice $receivable_id");
    }
    $ml = &get_obj("ML");

    my $errcnt = &AtError();

    # set type according to what was purchased
    my $renewal_type = $this->member->member_type();
    my $inv          = new Modules::Finance::Receivable(id => $receivable_id);
    my $itemlist     = $inv->loaditems();
    if ($itemlist) {
        while (my $item = $itemlist->next()) {
            if (   $item->getdata("objtype") eq "member"
                && $item->getdata("objid") == $this->member->id()
                && $item->getdata("note") =~ /Renew:/)
            {
                $renewal_type = $item->getdata("note");
                $renewal_type =~ s/^Renew://;
                $this->member->setdata("type", $this->type_value($renewal_type));
            }
        }
    }
    my $is_comp;
    if (
        !$receivable_id
        && $this->membership_fee(
            member        => $this->member,
            membertype    => $renewal_type,
            purchase_type => "Renew"
        ) < 0.01
      )
    {
        $is_comp = 1;
    }
    if ($this->{admin}) {

        # set expiry date
        $this->member->set_expiry(&preference("Membership.rollover_date"),
            &preference("Membership.early_renewal_period") || 0);
        $this->member->set_status_active("renewed by " . $share{DB}->my_name);
    } elsif (($inv && $inv->has_payments())
        || ($inv && $inv->is_paid)
        || $is_comp)
    {
        # set expiry date
        $this->member->set_expiry(&preference("Membership.rollover_date"),
            &preference("Membership.early_renewal_period") || 0);
        if ($is_comp) {
            $this->member->set_status_active("automatic renewal for comp membership");
        } else {
            $this->member->set_status_active("automatic renewal on receipt of payment");
        }
        $this->member->enable();
        if (!$this->member->getdata("login")) {
            $this->set_login();
        }
        $this->set_password();
    }

    # save changes
    if (defined $this->member->force_save() && $share{Page}) {

        # notifications
        $this->renewal_email($receivable_id);
    }

    if (&AtError($errcnt)) {
        $out .= $ml->p($msg{"Problem:"});
        $out .= &ExSite::Config::show_diagnostics();
        $out .=
          $ml->p(
            $msg{"Please contact the website administrator for assistance in sorting out any problems noted above."});
    }
    return $out;
}

# complete membership upgrade

sub upgrade_member {
    my ($this, $receivable_id) = @_;
    my $out;
    my $db = $share{DB};
    $ml = &get_obj("ML");
    my $errcnt = &AtError();

    # set type according to what was purchased
    my $renewal_type = $this->member->member_type();
    my $inv          = new Modules::Finance::Receivable(id => $receivable_id);
    my $itemlist     = $inv->loaditems();
    if ($itemlist) {
        while (my $item = $itemlist->next()) {
            if (   $item->getdata("objtype") eq "member"
                && $item->getdata("objid") == $this->member->id()
                && $item->getdata("note") =~ /^Upgrade:/)
            {
                $renewal_type = $item->getdata("note");
                $renewal_type =~ s/^Upgrade://;
                $this->member->setdata("type", $this->type_value($renewal_type));
                $this->member->force_save();
                $this->member->set_status_active("membership upgrade; invoice $receivable_id");
            }
        }
    }
    if (&AtError($errcnt)) {
        $out .= $ml->p($msg{"Problem:"});
        $out .= &ExSite::Config::show_diagnostics();
        $out .=
          $ml->p(
            $msg{"Please contact the website administrator for assistance in sorting out any problems noted above."});
    }
    return $out;
}

# finalize_application - complete the membership sign-up process (part 1)
# - setup login, access
# - attach account
# - set expiry date

sub finalize_application {
    my ($this, $receivable_id) = @_;
    my $out;
    my $db = $share{DB};
    $ml = &get_obj("ML");
    my $errcnt = &AtError();

    # attach account
    if ($receivable_id) {
        my $inv = new Modules::Finance::Receivable(id => $receivable_id);
        if ($inv->defined) {
            my $acct = $inv->account();
            if ($acct->defined) {

                # must use low-level update to skirt security rules, because
                # user is not yet fully authorized
                my $acctdata = $acct->get();
                $acctdata->{member_id} = $this->member->id
                  if (!$acctdata->{member_id});
                $share{DB}->update("account", $acctdata);
            }

            # set type according to what was purchased
            my $new_type = $this->member->member_type();
            my $itemlist = $inv->loaditems();
            if ($itemlist) {
                while (my $item = $itemlist->next()) {
                    if (   $item->getdata("objtype") eq "member"
                        && $item->getdata("objid") == $this->member->id)
                    {
                        $new_type = $item->getdata("note");
                        $new_type =~ s/^New://;
                        $this->member->setdata("type", $this->type_value($new_type));
                    }
                }
            }
        }
    }

    # setup login
    $this->set_login();
    $this->member->setdata("ctime", new ExSite::Time->write("sql_datetime"));
    if (!$this->member->getdata("access")) {
        if ($this->member->is_member || &preference("Membership.guest_login")) {
            $this->member->setdata("access", 1);
        }
    }

    # we must be the user - need to authenticate to get necessary
    # privs to complete the setup
    if (!$db->my_user_record() && !$share{auto_login}) {
        $db->do_login($this->member->get());
        $share{auto_login} = 1;
    }
    if (!$this->member->getdata("visibility")) {
        my $visibility = &preference("Membership.visibility") || "visible";
        $this->member->setdata("visibility", $visibility);
    }
    if (!$this->member->getdata("privacy")) {
        my $privacy = &preference("Membership.privacy") || "public";
        $this->member->setdata("privacy", $privacy);
    }

    # set expiry date and save
    my $e = $this->member->set_expiry(&preference("Membership.rollover_date"),
        &preference("Membership.early_renewal_period") || 0);
    $this->member->force_save();

    my $type = $this->member->getdata("type");
    if ($this->manual_approval($type)) {

        # approval comes later
        if (&AtError($errcnt)) {
            $out .= $ml->p($msg{"Problem:"});
            $out .= &ExSite::Config::show_diagnostics();
            $out .=
              $ml->p(
                $msg{"Please contact the website administrator for assistance in sorting out any problems noted above."}
              );
        }

        # incorporate an extended application process whereby users are logged in before next steps are taken
        if ($this->is_extended_application && $this->member->is_incomplete) {
            $this->member->enable();
            $this->set_password();
            $this->member->force_save();
            if ($receivable_id) {
                $this->notify_application(manual_approval => 1);
            }
        } else {
            $this->notify_application(manual_approval => 1);
        }
        $out .= $ml->p($msg{&preference("Membership.review_message")});
    } else {
        $out .= $this->approve_application($receivable_id);
        $this->notify_application();
    }
    if (   $this->member->is_incomplete()
        && $receivable_id
        && !&preference("Membership.manual_pending"))
    {
        $this->member->set_status_pending("application awaiting approval");
    }

    # clear temporary identity data from session
    if ($this->member->is_me()) {
        delete $session{prelim_identity};
        delete $session{use_prelim_identity};
        delete $db->{authenticated};
    }

    # login new user automatically if possible
    if (!$db->authenticate()) {
        $session{invoice} = 0;
        $db->do_login($this->member->get());
        $share{auto_login} = 1;
    }
    $out .= $this->run_handler("post_finalize_application", $receivable_id);
    return $out;
}

# extended_application - check if we need to go through extended application process
# - extended applicants are not fully finalized (statuses and emails)
# - can be for all member types or specific member types comma delimited
sub is_extended_application {
    my $this = shift;
    my $pref = &preference("Membership.extended_application");
    if (&preference("Membership.extended_application") == 1) {
        return 1;
    }
    my @types = split(/,/, $pref);
    my $type = $this->member->member_type();
    if (scalar grep(/^$type$/, @types)) {
        return 1;
    }
    return 0;
}

# approve_application - complete the membership sign-up process (part 2)
# - activate membership
# - set password
# - set expiry date
# - notify user

sub approve_application {
    my ($this, $receivable_id) = @_;

    # try to guess receivable if none passed
    my $account = $this->member->account();
    if (!$receivable_id) {
        if ($account) {
            my %children = $account->get_children();
            my $recv     = $children{receivable};
            if ($recv) {
                $recv->reset();
                while (my $r = $recv->next()) {
                    if ($r->is_active()) {
                        $receivable_id = $r->id;
                        last;
                    }
                }
            }
        }
    }
    my $inv = new Modules::Finance::Receivable(id => $receivable_id);

    # switch to receivable's account in case it is different
    $account = $inv->account();
    $share{DB}->info("approving receivable $receivable_id");
    my $out;
    my $db = $share{DB};
    $ml = &get_obj("ML");

    my $errcnt = &AtError();

    my $type = $this->member->member_type();

    # set expiry date
    if ($this->member->days_to_expiry() <= 0) {

        # we should only set_expiry again when it is in past because time may have past since invoice was created
        # expiry date should be based on invoice creation date in this case
        $this->member->set_expiry(&preference("Membership.rollover_date"),
            &preference("Membership.early_renewal_period") || 0);
    }

    if ($this->{admin}) {
        $this->member->set_status_active("approved by " . $share{DB}->my_name);
    } elsif (
        !$inv
        && int(
            $this->membership_fee(
                member     => $this->member,
                membertype => $type
            )
        ) == 0
      )
    {
        $this->member->set_status_active("automatic approval of comp membership");
    } elsif ($inv && $inv->get_payments() && $inv->get_payments()->count
        || ($account && $account->balance() == 0))
    {
        $this->member->set_status_active("automatic approval on receipt of payment; invoice $receivable_id");
    } elsif (
        !$inv
        && int(
            $this->membership_fee(
                member     => $this->member,
                membertype => $type
            )
        ) == 0
      )
    {
        $this->member->set_status_active("automatic approval of comp membership");
    } elsif (!$this->member->is_pending) {
        $this->member->set_status_pending("application pending receipt of offline payment");
    }

    if ($this->member->account) {
        $this->member->account->setdata("member_id", $this->member->id);
        $this->member->account->force_save();
    }
    $this->member->enable();
    $this->set_login();
    $this->set_password();

    # save changes
    my $email_status = "not sent";
    if (defined $this->member->force_save()) {
        my $mtype = $this->member->member_type();

        # notifications
        if (
            (
                   ($receivable_id && $inv->is_paid())
                || ($account && $account->balance() == 0)
                || (
                    !$inv
                    && int(
                        $this->membership_fee(
                            member     => $this->member,
                            membertype => $mtype
                        )
                    ) == 0
                )
            )
          )
        {
            if (!$this->{admin}) {
                my $stat = $this->welcome_email();
                $email_status = "sent" if ($stat);
            }
        }
    }

    if (&AtError($errcnt)) {
        $out .= $ml->p($msg{"Problem:"});
        $out .= &ExSite::Config::show_diagnostics();
        $out .=
          $ml->p(
            $msg{"Please contact the website administrator for assistance in sorting out any problems noted above."});
    }
    if ($share{Page}) {
        $out .= $ml->p($msg{&preference("Membership.final_message")});
    } else {
        my $msg .=
          $ml->p(&substitute("A welcome email was $email_status to [[name]].", {name => $this->member->name}));
        if ($email_status eq "not sent") {
            $msg .= &ExSite::HTML::Button(
                label => "Send Welcome Email",
                url   => $this->link(
                    pro   => "welcome_email",
                    uid   => $this->member->id,
                    small => 1
                )
            );
        }
        $out .= &ExSite::HTML::ErrorBox(
            title => "Notification",
            pane  => $msg
        );
    }
    $out .= $this->run_handler("post_approve_application", $receivable_id);
    return $out;
}

sub get_fees {
    my ($this, $sid) = @_;
    my @fees = $share{DB}->fetch_match("member_fee", {section_id => $sid});
    my $fees = new ExSite::ObjectList(type => "member_fee", list => \@fees);
    $fees->loadmore({section_id => 0});
    $this->{fees} = $fees;
}

# get_allowed_fees - get list of fees which are open on date (default now)
# sid = section_id
# date = optional date on which fees are open
sub get_allowed_fees {
    my ($this, $sid, $date) = @_;
    my $stat = $this->run_handler("Membership_get_allowed_fees", $sid, $date);
    return $stat if ($stat);
    my $fees = $this->get_fees($sid);
    my $allowed_fees = new ExSite::ObjectList(type => "member_fee");
    if ($fees) {
        $fees->reset;
        while (my $fee = $fees->next) {
            $fee->update_recurring_dates($date);
            if ($fee->allow(date => $date)) {
                $allowed_fees->push($fee->get());
            }
        }
    }
    if ($allowed_fees->count > 1) {
        $allowed_fees->sort("sortkey", "member_fee_id");
    }
    return $allowed_fees;
}

# member = member object
# type = member type
# purchase_type = New|Renew
sub membership_fees {
    my ($this, %opt) = @_;
    $opt{member} = $opt{member} || $this->member();
    my %fee;
    my $sid  = $this->section_id;
    my $fees = $this->get_allowed_fees($sid);
    while (my $fee = $fees->next()) {
        $opt{membertype} = $fee->getdata("type");
        $fee{$fee->getdata("type")} = $fee->cost(%opt);
    }
    if (&preference("Membership.secondary.fee") && !&preference("Membership.secondary.use_parent_type")) {
        $opt{type} = "secondary";
        $fee{secondary} = $this->run_handler("Membership_renewal_fee", %opt);
    }
    return wantarray ? %fee : \%fee;
}

# member = member object
# type = member type
sub membership_fee {
    my ($this, %opt) = @_;
    my $membertype = $opt{type} || $opt{membertype};
    my %fee        = $this->membership_fees(%opt);
    my $fee        = $fee{$membertype};
    return $fee;
}

sub notify_application {
    my ($this, $manual_approval) = @_;
    my $stat = $this->run_handler("Membership_notify_application");
    return if ($stat || $this->{admin} || $share{DB}->is_manager);
    if (!$manual_approval) {
        return undef if (!&preference("Membership.notify_application"));
    }
    my $member     = $this->member();
    my $member_id  = $this->member()->id();
    my $section    = $share{DB}->this_site();
    my $section_id = $section->{section_id};
    my $subject    = ucfirst $this->member->member_type . " application received from " . $this->member->name;
    my $message    = $subject . "\n\n";
    $message .= "User information:\n\n";
    $message .= $member->show_generic(plaintext => 1);
    $message .=
"\nAdministrator link to application:\n$config{server}{server}$config{server}{CGIpath}/$config{prog}{ctrlpanel}/Membership?section_id=$section_id&pro=member&uid=$member_id\n";
    my $owner =
         &preference("Membership.notification_email")
      || &preference("Membership.owner_email")
      || $share{DB}->owner_email($section);
    &ExSite::Mail::send(
        to      => $owner,
        from    => $owner,
        subject => $subject,
        body    => $message
    );
}

sub get_merge {
    my ($member, $receivable_id) = @_;
    my $stat = $share{DB}->run_handler("Membership_get_merge", $member, $receivable_id);
    return $stat if ($stat);
    my $data     = $member->get();
    my $sid      = $data->{section_id};
    my $section  = new ExSite::Section(id => $sid);
    my $title    = $section->get_my("title");
    my $passtype = $config{auth}{password_storage_method};
    my $password = ($passtype eq "cleartext") ? $data->{password} : "HIDDEN";
    my ($url, $uri);

    if (!$config{Login}) {
        &ExSite::Module::read_conf("Login");
    }
    if (my $pid = &preference("Login.goto")) {
        my $p = new ExSite::Page(id => $pid);
        if ($p->get && $p->get->{section_id} == $data->{section_id}) {
            $url = $p->get_url_dynamic_full;
        }
    }

    # expires 14 days in the future, must be set for persistent login
    my $authtoken = $share{DB}->make_authtoken($data, 14, undef);
    if ($url) {
        $uri = new ExSite::URI(uri => $url);
    } else {
        $uri = new ExSite::URI;
        my $page = &ExSite::Module::service_page("Membership", $sid);
        if ($page) {
            my $version =
              $share{Page} ? $share{Page}->get_my("version") : "";
            $page->set_version($version);
            $uri = $page->get_uri_dynamic;
            $uri->clear_query;
        }
    }
    $uri->query(_auth => $authtoken);
    my $login_url = $uri->write_full();
    my ($account_email, $account_name);

    # account may be tied to parent
    if ($receivable_id) {
        my $inv = new Modules::Finance::Receivable(id => $receivable_id);
        if (my $account = $inv->account) {
            $account_name  = $account->name;
            $account_email = $account->email;
        }
    }
    my $merge = {
        account_email => $account_email,
        account_name  => $account_name,
        expirydate    => $data->{expirydate},
        first_name    => $data->{first_name},
        last_name     => $data->{last_name},
        login         => $data->{login},
        login_url     => $login_url,
        member_id     => $data->{member_id},
        name          => $data->{first_name} || $data->{organization},
        organization  => $data->{organization},
        password      => $password,
        sitename      => $section->get_my("title"),
        sitetitle     => $title,
        siteurl       => $section->get_url(),
    };
    return $merge;
}

sub welcome_email {
    my ($this, $member) = @_;
    $member = $member || $this->member;
    my $type = $member->getdata("type");
    if ($type =~ /^guest/ && !&preference("Membership.guest_login")) {
        $this->error("Guest login is currently disabled on this website.");
        return;
    }

    # when type is secondary we must wait for parent to be approved
    if ($type eq "secondary") {
        if (!$member->parent()) {
            $this->error($msg{"This secondary member has not been attached to a primary member."});
            return undef;
        } elsif (!$member->parent->is_approved_member() && !$share{DB}->is_manager()) {
            $this->warn($msg{"Welcome email will not be sent until this membership has been approved."});
            return undef;
        }
    }
    $type =~ s/^member\///;
    $type =~ s/^guest\///;
    my $sid     = $this->section_id();
    my $section = new ExSite::Section(id => $sid);
    my $title   = $section->get_my("title");
    my $loc     = $share{Page} || $section;
    my $email   = $member->email();
    return if (!$loc || !$email);
    my $owner =
         &preference("Membership.welcome_email.from")
      || &preference("Membership.owner_email")
      || $share{DB}->owner_email($section->get());
    my $template;

    if (   $member->parent()
        && $this->get_template("secondary_welcome_email", $sid))
    {
        $template = $this->get_template("secondary_welcome_email", $sid);
        $this->notify_primary_new_secondary($member->parent);
    } else {
        if ($this->get_template("${type}_welcome_email_template", $sid)) {
            $template = $this->get_template("${type}_welcome_email_template", $sid);
        } elsif ($this->get_template("welcome_email_template", $sid)) {
            $template = $this->get_template("welcome_email_template", $sid);
        }
    }
    if (!$template) {
        $template =
          $msg{&get_file("$config{server}{HTMLroot}$config{server}{HTMLpath}/_Modules/Membership/welcome-email.txt")};
    }
    my $message = &substitute($template, &get_merge($member));
    my $subject = &preference("Membership.label.welcome_email")
      || "Welcome to $title!";
    &ExSite::Mail::send(
        to      => $email,
        from    => $owner,
        subject => $msg{$subject},
        body    => $message,
    );

    # send to members with type = secondary when approval required
    if ($this->manual_approval($member->getdata("type"), 1)) {
        my $secondary = $member->get_child("member");
        my $tmp       = $this->{Member};
        if ($secondary && $secondary->count > 0) {
            while (my $sec = $secondary->next()) {
                if ($sec->getdata("type") eq "secondary") {
                    $this->welcome_email($sec);
                }
            }
        }
        $this->{Member} = $tmp;
    }
    return $message;
}

sub notify_primary_new_secondary {
    my ($this, $parent) = @_;
    my $sid      = $this->section_id();
    my $section  = new ExSite::Section(id => $sid);
    my $template = $this->get_template("notify_primary_new_secondary", $sid);
    if ($template) {
        my $email = $parent->email();
        my $owner = &preference("Membership.owner_email")
          || $share{DB}->owner_email($section->get());
        my $subject = "A secondary member has been added to your profile";
        my $message = &substitute($template, &get_merge($this->member));
        if ($email) {
            &ExSite::Mail::send(
                to      => $email,
                from    => $owner,
                subject => $msg{$subject},
                body    => $message,
            );
        }
    }
}

sub renewal_email {
    my ($this, $receivable_id) = @_;
    my $sid     = $this->section_id();
    my $section = new ExSite::Section(id => $sid);
    my $email   = $this->member->email();
    return if (!$email);
    my $owner = &preference("Membership.owner_email")
      || $share{DB}->owner_email($section->get());
    my ($template, $ctemplate);
    my $loc = $share{Page} || $section;
    my $ctemplate = $loc->find("renewal_email_template");

    if ($ctemplate && $ctemplate->get_html()) {
        $template = $ctemplate->get_html();
    }
    return if (!$template);
    my $message = &substitute($template, &get_merge($this->member, $receivable_id));

    # reinstantiate section to expand again
    my $section = new ExSite::Section(id => $sid);

    # expand cms references
    # exsite must be set to not use relative urls
    $message = $section->expand(html => $message);
    my $title = $section->get_my("title");
    &ExSite::Mail::send(
        to      => $email,
        from    => $owner,
        subject => $msg{"Thank you for renewing your membership with $title"},
        body    => $message,
    );
}

sub member_column_label {
    my ($this, $column) = @_;
    if ($column eq "name") {
        return "Name";
    }
    return $this->DB->{map}->get_column_attr("member", $column, "label");
}

# dropdown selector for roster reports
sub member_quicklink {
    my ($this, $uid) = @_;
    my $out;
    my @options;
    my $options;
    my @links = (
        {text => "== select ==", value => ""},
        {
            text  => &preference("Membership.label.view_profile"),
            value => "member"
        },
        {
            text  => &preference("Membership.label.edit_profile"),
            value => "edit_profile"
        },
        {
            text  => &preference("Membership.label.edit_contacts"),
            value => "edit_contacts"
        },
        {
            text  => &preference("Membership.label.edit_photo"),
            value => "edit_photo"
        },
        {text => "Status history",    value => "member_status"},
        {text => "Account Statement", value => "acct"},
        {text => "DELETE",            value => "delete"},
    );
    foreach my $item (@links) {
        next if (!$item->{text});
        my %attr = (value => $item->{value});
        $options .= $ml->option($item->{text}, \%attr);
    }
    $out .= $ml->form(
        $ml->p(
            $ml->select(
                $options,
                {
                    class    => "quicklink",
                    name     => "pro",
                    onchange => "document.quicklinks$uid.submit()"
                }
            )
          )
          . $ml->input(undef, {type => "hidden", name => "uid", value => $uid})
          . $ml->input(
            undef,
            {
                type  => "hidden",
                name  => "section_id",
                value => $this->get_section_id
            }
          ),
        {name => "quicklinks$uid", method => "get"}
    );
    return $out;
}

sub select_options {
    my $this = shift;
    my $list = shift;
    my @options;
    foreach my $item (@$list) {
        push @options, {value => $item, text => $item};
    }
    return \@options;
}

# use_column_names = use column names instead of labels for headings
sub roster {
    my ($this, %opt) = @_;
    my $use_column_names =
      $opt{use_column_names} || $this->{input}{use_column_names};
    my $stat = $this->run_handler("membership_roster", %opt);
    my $format = $opt{rpt_format} || $this->{input}{rpt_format};
    if ($format eq "json") {
        $opt{contact} = 1;
    }
    return $stat if $stat;

    my $type   = $opt{type}   || $this->{input}{type};
    my $status = $opt{status} || $this->{input}{status};
    my $members        = $opt{data};
    my $contact        = &preference("Membership.roster.contact") || $opt{contact} || $this->{input}{contact};
    my $excl_secondary = $opt{excl_secondary} || $this->{input}{excl_secondary};
    my $limit          = &preference("Membership.full_roster_max") || 1000;
    my $out;
    if (   !$members
        && (!$type   || scalar @$type == 0)
        && (!$status || scalar @$status == 0))
    {
        return $this->roster_filter_form(%opt);
    } elsif ($this->{input}{pro} eq "Roster") {
        $out .= $this->roster_filter_form(
            type           => $type,
            status         => $status,
            contact        => $contact,
            excl_secondary => $excl_secondary
        );
    }

    my $typere = (ref $type eq "ARRAY")   ? join("|", @$type)   : "";
    my $statre = (ref $status eq "ARRAY") ? join("|", @$status) : "";
    my $knownstatre = "active|pending|expired|archived";

    my @options = (
        {text => "More actions", value => undef},
        {
            text  => "Membership Overview",
            value => $this->link(_bare => 2, __plaintext => 1, pro => "summary")
        },
        {
            text  => "Search",
            value => $this->link(_bare => 2, __plaintext => 1, pro => "search")
        },
    );
    my $options;
    foreach my $item (@options) {
        my %attr = (
            class => "toplink",
            title => $item->{text},
            value => $item->{value}
        );
        $options .= $ml->option($item->{text}, \%attr);
    }
    $out .= $ml->h1("Roster");
    if ($this->{admin}) {

        my $uri = new ExSite::URI;
        $uri->setup("$config{server}{CGIpath}/ctrl-panel.cgi/Email");
        my $clist_data;
        $uri->query(
            emcmd      => "new",
            section_id => $this->get_section_id(),
            list_id    => $clist_data
        );

        my @options = (
            $ml->a(
                "Excel",
                {href => $this->link(rpt_format => "excel", use_column_names => $this->{input}{use_column_names})}
            ),
            $ml->a(
                "CSV", {href => $this->link(rpt_format => "csv", use_column_names => $this->{input}{use_column_names})}
            )
        );
        if ($this->{input}{pro} !~ /search/) {
            push(@options, $ml->a("Email", {href => $this->link(rpt_format => "json", use_column_names => 1)}));
        }
        $out .= &ExSite::HTML::ToolBar(tools => [join(" ", @options)]);
    }

    # setup report
    my $r     = new ExSite::ReportBuilder();
    my $title = $statre . " " . $typere . " Roster";
    $title =~ s/\|/ /g;
    $r->title($title);

    my $columns;
    my @head;
    if (ref $columns ne "ARRAY" || $format) {
        $columns = [];
        my $cols = [
            "member_id",    "title", "honorific", "name", "first_name", "last_name",
            "organization", "type",  "email",     "expirydate"
        ];
        foreach my $col (@$cols) {
            my $display = $share{DB}{map}->get_column_attr("member", $col, "display");

            # show expirydate regardless of dbmap settings
            $display = "brief" if ($col eq "expirydate");

            # name is not an actual database column
            $display = "brief" if ($col eq "name" && (!$format || $format eq "json"));
            if ($display =~ /brief|key/ && !$format || $display =~ /key|brief|full/ && $format) {
                push @$columns, $col;
            }
        }
    }
    if (!$use_column_names) {
        @head = map { $this->member_column_label($_) } @$columns;
    } else {
        @head = @$columns;
    }
    if (&preference("Membership.roster.include_login")) {
        unless (defined $opt{include_login} && $opt{include_login} == 0) {
            push(@head, ("login", "password"));
        }
    }

    my @meta;
    if (ref &preference("Membership.roster.report_meta") eq "ARRAY") {
        @meta = @{&preference("Membership.roster.report_meta")};
    } elsif (&preference("Membership.roster.report_meta") =~ /\w/) {
        @meta = (&preference("Membership.roster.report_meta"));
    } elsif ($format) {
        @meta = $this->get_all_meta();
    }
    foreach my $meta (@meta) {
        next
          if (!$format
            && $this->member->meta->get_map_info($meta, "display") ne "brief");
        next
          if ($this->member->meta->get_map_info($meta, "read") > $share{DB}->authorize);
        if ($use_column_names) {
            push @head, $meta;
        } else {
            push @head, $this->member->meta->get_map_info($meta, "label") || $meta;
        }
    }
    push @head, "status";
    if ($contact) {
        if ($use_column_names) {
            if ($format eq "json") {
                push @head, qw(account_id contact_id type address city provstate country pcode phone1);
            } else {
                push @head,
                  qw(contact_account_id contact_contact_id contact_type contact_address contact_city contact_provstate contact_country contact_pcode contact_phone1);
            }
        } else {
            push @head, qw(Account ContactID Contact Address City Prov/State Country PostalCode Phone1);
        }
    }
    $r->headers(\@head);

    # fetch data
    my $section_id = $this->{section}->id;
    my $metadata;
    $metadata = $this->fetch_metadata($type) if (!$opt{prefetch});
    if (!$members) {
        if (ref $type ne "ARRAY" || scalar @$type > 1) {
            $members = $share{DB}->get_query("all members with status", $section_id);
        } elsif (scalar @$type == 1) {
            $members = $share{DB}->get_query("all members by type with status", $section_id, $type->[0]);
        }
    }

    # pagination parameters
    my $page    = $this->{input}{page} || 1;
    my $perpage = 500;
    my $end     = $page * $perpage;
    my $begin   = $end - ($perpage - 1);

    # populate the report
    my $i;
    if ($members) {
        while (scalar @$members > 0) {
            foreach my $m (@$members) {
                my @mdata;
                next if ($m->{parent_id} && $excl_secondary);
                next
                  if ($typere && $m->{type} !~ /^($typere)$/);    # skip other types
                next
                  if ($type && scalar @$type > 0 && !$m->{type});    # undefined types
                if ($statre =~ /other/) {
                    next if ($m->{status} =~ /$knownstatre/);
                } else {
                    next if ($statre && $m->{status} !~ /$statre/);
                }

                # cursor to mark the row number in full report
                $i++;

                # push empty rows into report if record is outside of current page
                if ($format !~ /csv|excel/ && ($i < $begin || $i > $end)) {
                    $r->push(@mdata);
                    next;
                }

                # setup member as a generic object, not a member object - this
                # is to avoid the extra queries that a member object might
                # try to execute while we are still in the middle of our roster
                # query
                push(@mdata, $this->roster_row_data($columns, $m, $format));
                if (&preference("Membership.roster.include_login")) {
                    unless (defined $opt{include_login}
                        && $opt{include_login} == 0)
                    {
                        push(@mdata, $m->{login});
                        push(@mdata, $m->{password});
                    }
                }

                my $nmeta;
                foreach my $meta (@meta) {
                    next
                      if (!$format
                        && $this->member->meta->get_map_info($meta, "display") ne "brief");
                    next
                      if ($this->member->meta->get_map_info($meta, "read") > $share{DB}->authorize);
                    $nmeta++;

                    # use object if data was preloaded
                    if ($opt{prefetch}) {
                        my $mem = new ExSite::Object(type => "member", data => $m);
                        push @mdata, $mem->showdata($meta);
                    } else {
                        push @mdata, $metadata->{$m->{member_id}}{$meta};
                    }
                }

                push @mdata, $m->{status};

                if ($contact) {
                    my @acct = $share{DB}->fetch_child("account", "member", $m->{member_id});
                    if (@acct == 0) {
                        push @mdata, $m->{member_id}, '', '', '', '', '', '', '', '', '';
                        $r->push(@mdata);
                        next;
                    }
                    my @contact = $share{DB}->fetch_child("contact", "account", $acct[0]{account_id});
                    my %order;
                    &ExSite::Module::read_conf("MembershipReports");
                    if (my $p = &preference("MembershipReports.pref_contact")) {
                        my @pref = split(/,/, $p);
                        for (my $i = 0 ; $i < scalar @pref ; $i++) {
                            $order{$pref[$i]} = $i;
                        }
                        @contact = sort { $order{$a->{type}} <=> $order{$b->{type}} } @contact;
                    }
                    my $c = $contact[0];
                    push @mdata, $c->{account_id}, $c->{contact_id}, $c->{type}, $c->{address},
                      $c->{city}, $c->{provstate}, $c->{country}, $c->{pcode},
                      $c->{phone1};
                    $r->push(@mdata);
                    my $ncol = scalar @$columns + $nmeta;
                    if (&preference("Membership.roster.include_login")) {
                        unless (defined $opt{include_login}
                            && $opt{include_login} == 0)
                        {
                            $ncol = $ncol + 2;
                        }
                    }
                } else {
                    $r->push(@mdata);
                }
            }
            $members = $share{DB}->fetchrows();
        }
    }
    $share{DB}->set("max_select", 0);
    if ($format eq "excel") {
        $ml->cancel;
        print "Content-type: application/vnd.ms-excel\ncontent-disposition: attachment; filename=Roster.xls\n\n";
        $r->export("excel");
    } elsif ($format eq "csv") {
        $ml->cancel;
        print "Content-type: text/comma-separated-values\nContent-disposition: attachment; filename=Roster.csv\n\n";
        print $r->export("csv");
    } elsif ($format eq "json") {
        my $headers = $r->{headers};
        my $data    = $r->{data};
        my $total   = (ref $data eq "ARRAY") ? scalar @$data : 0;
        my $date    = new ExSite::Time->write("date");
        my $title   = $r->{title} . " (as of $date)";
        my %history = (
            member_report_id => 0,
            title            => $title,
            headers          => JSON::to_json($headers),
            data             => JSON::to_json($data),
            total            => $total
        );
        my $h           = new ExSite::Object(type => "member_report_history", data => \%history);
        my $hid         = $h->force_save();
        my @saved_lists = $share{DB}->fetch_match("contact_list", {name => $r->{title} . '%', type => "custom"});
        my $data        = {
            section_id  => $this->get_section_id,
            name        => $title,
            type        => "custom",
            dupe        => "all",
            unsubscribe => "exclude",
            parameters  => "member_report_history_id=" . $hid,
            include     => 0
        };
        my $cid;

        if (scalar @saved_lists) {
            my $s     = $saved_lists[-1];
            my $clist = new ExSite::Object(
                type => "contact_list",
                id   => $s->{contact_list_id},
            );
            $clist->setdata("parameters", $data->{parameters});
            $clist->setdata("name",       $title);
            $clist->force_save();
            $cid = $s->{contact_list_id};
        } else {
            my $clist = new ExSite::Object(
                type => "contact_list",
                data => $data
            );
            $cid = $clist->force_save();
        }
        my $uri = new ExSite::URI(plaintext => 1);
        $uri->setup("$config{server}{CGIpath}/ctrl-panel.cgi/Email");
        $uri->query(
            emcmd      => "new",
            section_id => $this->get_section_id(),
            list_id    => $cid
        );
        &redirect($uri->write_full());
    } else {

        # HTML
        $r->foot($this->report_footer());
        $r->paginate(500);
        $r->{class} = "Report Roster";
        $config{report}{dynamic_paginate} = 0;
        $r->set("dynamic", 1);
        $out .= $r->make();
    }
    return $out;
}

sub roster_row_data {
    my ($this, $columns, $data, $rpt_format) = @_;
    my @mdata;
    foreach my $col (@$columns) {
        if ($col eq "name") {
            my $class         = $data->{parent_id} ? "secondary" : "primary";
            my @types_allowed = $this->types_allow_secondary();
            my $type          = $data->{type};
            $type =~ s/^member\///;

            # only highlight secondary members if secondary members have same type
            # and secondary members are allowed for the current type
            $class = undef
              if ( !&preference("Membership.secondary.use_parent_type")
                || !scalar grep(/$type/, @types_allowed));
            my $title = $this->membername($data);
            $title .= " ($class member)" if ($class);
            my $name = $ml->a(
                $this->membername($data),
                {
                    class => $class ? "member $class" : "member",
                    title => $title,
                    href  => $this->link(
                        pro => "member",
                        uid => $data->{"member_id"}
                    )
                }
            );
            $name = $ml->div(
                $ml->span($name) . "&nbsp"
                  . $ml->span(
                    $ml->a(
                        "update",
                        {
                            href  => $this->link(uid => $data->{member_id}, pro => "edit_profile"),
                            class => "button_context button_sm"
                        }
                    )
                  ),
                {class => "rostername"}
            );
            $name = $this->membername($data) if ($rpt_format eq "json");
            push(@mdata, $name);
        } else {

            # keep data unformatted if exporting to file
            if ($rpt_format =~ /excel|csv/i) {
                push(@mdata, $data->{$col});
            } else {
                push(@mdata, $data->{$col});
            }
        }
    }
    return @mdata;
}

sub roster_filter_form {
    my ($this, %opt) = @_;
    my $type             = $opt{type};
    my $status           = $opt{status};
    my $contact          = $opt{contact};
    my $excl_secondary   = $opt{excl_secondary};
    my $use_column_names = $opt{use_column_names};
    my $state            = (!$type || @$type == 0) && (!$status || @$status == 0);
    my $f                = new ExSite::FormBuilder(
        method => "get",
        action => $this->link(type => undef, status => undef)
    );
    my $some_checked;
    my @type = sort $this->known_membership_types();

    $f->input(
        name     => "type",
        id       => "types",
        prompt   => "Membership Type",
        type     => "select",
        multiple => 1,
        options  => \@type,
        value    => ref($type) eq "ARRAY" ? join("; ", @$type) : $type
    );

    my @status = qw(active pending expired archived);
    $f->input(
        name     => "status",
        id       => "status",
        prompt   => "Membership&nbsp;status",
        type     => "select",
        multiple => 1,
        options  => \@status,
        value    => ref($status) eq "ARRAY" ? join("; ", @$status) : $status
    );
    $f->input(
        name    => "contact",
        prompt  => "include contact information",
        type    => "checkbox",
        checked => $contact ? 1 : 0
    );
    $f->input(
        name    => "excl_secondary",
        prompt  => "exclude secondary members",
        type    => "checkbox",
        checked => $excl_secondary ? 1 : 0
    );
    my $br = $ml->br;

    $f->input(
        type  => "hidden",
        name  => "section_id",
        value => $this->{input}{section_id}
    );
    $f->input(type => "hidden", name => "pro", value => "roster");
    if ($use_column_names) {
        $f->input(type => "hidden", name => "use_column_names", value => 1);
    }

    $f->template(
        $ml->table(
            $ml->tr(
                    $ml->td("[[type:prompt]]" . $ml->br . "[[type:input]]",     {valign => "top"})
                  . $ml->td("[[status:prompt]]" . $ml->br . "[[status:input]]", {valign => "top"})
            ),
            {class => 'RosterFilters'}
          )
          . $ml->p("[[contact:input]] [[contact:prompt]]$br" . "[[excl_secondary:input]] [[excl_secondary:prompt]]")
    );
    $f->set("buttons", $ml->input(undef, {type => "submit", value => "Generate Roster"}));
    my $pane;
    $pane .= $f->make();
    $pane .= &ExSite::HTML::MultiSelect(id => "status");
    $pane .= &ExSite::HTML::MultiSelect(id => "types");
    return &ExSite::HTML::AccordionBox(
        titles => ["Filters"],
        panes  => [$pane],
        states => [$state],
    );
}

sub report_footer {
    my ($this) = @_;
    my $foot = "Report generated " . localtime;
    $foot .= " &bull; ";
    $foot .=
      $ml->a("Excel", {href => $this->link(rpt_format => "excel")}, {class => "button_sm"});
    $foot .= " &bull; ";
    $foot .= $ml->a("CSV", {href => $this->link(rpt_format => "csv")}, {class => "button_sm"});
    $foot .= " &bull; ";
    $foot .= $ml->a("Print", {href => "javascript:print()"});
    return $foot;
}

sub get_member_columns {
    my ($this, $thumb) = @_;
    my @allcol = $share{DB}->get_columns("member");
    my @mcol;
    foreach my $col (@allcol) {
        next if ($col =~ /photo/);
        next if ($col =~ /thumbnail/ && !$thumb);
        push @mcol, $col;
    }
    return @mcol;
}

sub fetch_member_type_sql {
    my ($this, $types) = @_;
    my $stat = $this->run_handler("Membership_fetch_member_type_sql");
    return $stat if ($stat);
    my $types = $types || &preference("Membership.directory.visible_types");

    # return if visible types not defined or we are in the control panel
    return if (!$types || !$share{Page});
    if (ref $types ne "ARRAY") {
        $types = [split(/,/, $types)];
    }
    my $sql = '(' . join(",", map { "'$_'" } @$types) . ')';
    return $sql;
}

sub get_all_meta {
    my ($this) = @_;
    my $meta = new Modules::Membership::MemberMeta;
    return $meta->get_allowed();
}

sub get_visible_meta {
    my ($this) = @_;
    if (my $pref = &preference("Membership.visible_meta")) {
        return split(/,/, $pref);
    }
    my @meta = $this->get_all_meta;
    my @visible_meta;
    foreach my $meta (@meta) {
        next
          if ($this->member->meta->get_map_info($meta, "read") > $share{DB}->authorize);
        next
          if ($this->member->meta->get_map_info($meta, "display") ne "brief");
        push(@visible_meta, $meta);
    }
    return wantarray ? @visible_meta : \@visible_meta;
}

# string = where statement part of sql statement
# meta = list of attributes
sub replace_name_with_alias {
    my ($this, %opt) = @_;
    my $string = $opt{string};
    my @meta   = @{$opt{meta}};
    for (my $i = 0 ; $i < scalar @meta ; $i++) {
        my $datatype = $this->get_datatype($meta[$i]);
        my $name     = $meta[$i];
        my $n        = $i + 1;
        if ($datatype =~ /olist/ && $this->{input}{value} eq "Other") {
            if ($opt{string} =~ /$name = \?/) {
                $opt{string} =~ s/$name = \?/$name not regexp \?/;
            }
        }
        $opt{string} =~ s/$name/a$n\.value/g;
    }
    return $opt{string};
}

sub sql_locate {
    my ($this, $match) = @_;
    my @where;
    my @params;
    foreach my $key (keys %$match) {
        my $datatype = $this->get_datatype($key);
        if ($datatype =~ /^(set:)/) {
            push @where,  "locate(?, replace($key,'; ',',')) ";
            push @params, $match->{$key};
            delete $match->{$key};
        }
    }
    return (join(' && ', @where), @params);
}

# add sql wildcard characters to unfixed datatypes
sub wildcards_to_match {
    my ($this, $match) = @_;
    foreach my $key (keys %$match) {
        my $datatype = $this->get_datatype($key);
        if ($datatype !~ /^(int|key(:|=)|list:|olist:|set:)/) {
            $match->{$key} = '%' . $match->{$key} . '%';
        }
    }
    return %$match;
}

# get the datatype for a attribute, member or contact table field
sub get_datatype {
    my ($this, $key) = @_;
    my $datatype;
    if ($this->member->meta->is_allowed($key)) {
        $datatype = $this->member->meta->get_datatype($key);
    } elsif ($share{DB}->{map}->get_column_attr("member", $key, "datatype")) {
        $datatype =
          $share{DB}->{map}->get_column_attr("member", $key, "datatype");
    } elsif ($share{DB}->{map}->get_column_attr("contact", $key, "datatype")) {
        $datatype =
          $share{DB}->{map}->get_column_attr("contact", $key, "datatype");
    }
    return $datatype;
}

# return the database value for membership type
sub type_value {
    my ($this, $type) = @_;
    if (   $type !~ /^(staff|secondary)$/
        && $type !~ /^guest/
        && $type !~ /^member\//
        && $type)
    {
        return "member/$type";
    }
    return $type;
}

# get a content template by content name and section
sub get_template {
    my ($this, $name, $section_id, $revision, $version) = @_;
    $revision = $revision || "newest";
    my $loc = $share{Page} || $share{Section};
    if (!$loc && $section_id) {
        $loc = new ExSite::Section(id => $section_id);
    }
    if ($loc) {
        my $ctemplate = $loc->find($name);

        # always get the newest revision of templates
        $loc->set_revision($revision);
        my $html = $ctemplate->get_html();
        return $html;
    }
    return;
}

sub scope {
    my ($this) = @_;
    my $sid = $this->get_section_id;
    return "local" if ($this->{admin});
    return &preference("Membership.scope.$sid") || "local";
}

sub allowed_membership_types {
    my ($this, $section_id) = @_;
    $section_id or $section_id = $this->gid;
    my $fees = $this->get_allowed_fees($section_id);
    my %type;
    while (my $fee = $fees->next()) {
        $type{$fee->getdata("type")}++;
    }
    my @type = keys %type;
    return wantarray ? @type : \@type;
}

sub known_membership_types {
    my ($this, $section_id) = @_;
    $section_id or $section_id = $this->gid;
    my @mtype = $share{DB}->get_query("known membership types", $section_id);
    my @type = map { $_->{type} } @mtype;
    return wantarray ? @type : \@type;
}

sub gid {
    my $this = shift;
    if ($this->member && $this->member->defined && $this->member->gid()) {
        return $this->member->gid();
    } elsif ($share{Page}) {
        return $share{Page}->id("section");
    } else {
        if (!$this->{section}) {
            $this->{section} = $share{Section};
        }
        return $this->{section} ? $this->{section}->id() : 0;
    }
}

sub whatami {
    my ($this, $plural) = @_;
    my $whatami = &preference("Membership.whatami") || "member";
    if ($plural) {
        return $whatami . "s";
    }
    return $whatami;
}

sub section_id {
    my $this = shift;
    if ($share{Page}) {
        return $share{Page}->get_my("section_id");
    } elsif ($this->get_section_id) {
        return $this->get_section_id;
    } elsif ($share{Section}) {
        return $share{Section}->get_my("section_id");
    } elsif ($this->member && $this->member->getdata("section_id")) {
        return $this->member->getdata("section_id");
    } else {
        my %u = $share{DB}->my_user_record();
        return $u{section_id} if ($u{section_id});
    }
    return 0;
}

# member types which allow secondary members
sub types_allow_secondary {
    my $this          = shift;
    my $pref          = &preference("Membership.secondary.types_allowed");
    my @types_allowed = ();
    if (ref $pref eq "ARRAY") {
        @types_allowed = @$pref;
    } elsif ($pref) {
        @types_allowed = split(/,/, $pref);
    }
    return wantarray ? @types_allowed : \@types_allowed;
}

# is the member an organization
# context optionally used for context sensitive queries
sub is_organization {
    my ($this, $member, $context) = @_;
    $member = $member->get() if (ref $member =~ /Member/);
    my $stat = $this->run_handler("Membership_is_organization", $member, $context);
    return $stat if ($stat);
    my @types_allow_secondary = $this->types_allow_secondary();
    if (scalar @types_allow_secondary) {
        my $type = $member->{type};
        if (grep(/$type/, @types_allow_secondary)) {
            return 1;
        }
    }
    return 1;
}

sub sort_field {
    my ($this,$type) = @_;
    my $stat = $this->run_handler("Membership_sort_field");
    return $stat if ($stat);
    my @types = split(/,/, $type);
    $type = $types[0];
    $type =~ s/^member\///;
    my $sort = confrule("sort_field", "membertype", $type);
    if (!$sort) {
        if (&preference("Membership.name.type") eq "organization") {
            $sort ="organization";
        } else {
            $sort = "last_name";
        }
    }
    return $sort;
}

# are secondary members allowed to be added by current member
sub allow_add_secondary {
    my ($this, $type, $member, $allow_public) = @_;
    my $member = $member || $this->member();
    $type = $type || $member->member_type();
    if (my $pid = $this->{input}{pid}) {
        if (   !$allow_public
            && !$share{DB}->is_manager()
            && $pid != $share{DB}->my_uid)
        {
            return 0;
        }
    }
    return 0 if ($member->member_type eq "staff");
    return 0
      if ( $member->is_renewable(1)
        && !$this->is_renewing()
        && &preference("Membership.secondary.disallow_if_renewable"));
    my $types_allowed = $this->types_allow_secondary();
    my $secondary     = $member->get_child("member");
    if ($secondary) {
        return 0 if ($this->secondary_limit($type) && $secondary->count() >= $this->secondary_limit($type));
    }
    return 1
      if (!scalar @$types_allowed
        && &preference("Membership.secondary.allow"));
    if (scalar grep(/$type/, @$types_allowed) > 0) {
        return 1;
    }
    return 0;
}

# returns the maximum number of secondary members allowed for membership type
sub secondary_limit {
    my ($this, $type) = @_;
    return confrule("secondary.limit", "membertype", $type);
}

# is current member renewing their membership
sub is_renewing {
    my ($this) = @_;
    my $inv = new Modules::Finance::Receivable(id => $session{invoice});
    my $itemlist = $inv->loaditems();
    my $is_renewing;
    if ($itemlist) {
        while (my $item = $itemlist->next()) {
            if (   $item->getdata("objtype") eq "member"
                && $item->getdata("objid") == $share{DB}->my_uid)
            {
                $is_renewing = 1;
            }
        }
    }
    return $is_renewing;
}

1;
