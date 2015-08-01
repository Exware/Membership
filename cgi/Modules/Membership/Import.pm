package Modules::Membership::Import;

use strict;
use ExSite::Config qw(&get_obj %config %share %session &read_exsite_conf);
use Modules::BaseDCD;
use Modules::Membership::Base;
use ExSite::Misc;
use ExSite::Time;
use ExSite::HTML;
use Text::Iconv;

use vars qw(@ISA $ml);
@ISA = qw(Modules::Membership::Base);

$ml = &get_obj("ML");

sub topview {
    my ($this) = @_;
    my $action = $this->{input}{pro};
    if ($action eq "form") {
        return $this->import_form();
    } elsif ($action eq "do_import1") {
        return $this->do_import1();
    } elsif ($action eq "export_i") {
        return $this->export();
    }
    my $out;
    $out .= $ml->h1("Membership Import Tool");
    $out .= $ml->p("This feature allows you to add or update multiple members in your membership database.");
    $out .= $ml->p("It is an advanced tool that should be used cautiously.");
    $out .= $ml->h3("How do I use this?");
    $out .= $ml->p(
"Step 1. Export your members by clicking the export button below, choosing the type of members to export and choosing the CSV export option from the top."
    );
    $out .= $ml->p("Step 2. Open the CSV file using any spreadsheet program.");
    $out .= $ml->p(
"Step 3. The system will create new members based on any new rows that are added.  IMPORTANT: do not enter anything into the first column (member_id) otherwise nothing will be imported."
    );
    $out .= $ml->p("Step 4. Under import membership data, attach your modified spreadsheet and submit.");
    $out .= $ml->p(
"Step 5. The system will show a list of all new members who were imported. Any issues will be reported in red text."
    );
    $out .= $ml->h3("Export current membership database");
    $out .= &ExSite::HTML::Button(
        label => "Export",
        url   => $this->link(pro => "export_i")
    );
    $out .= $this->import_form;
    return $out;

}

sub import_form {
    my ($this) = @_;
    my $fb = new ExSite::FormBuilder;
    $fb->action($this->link(pro => "do_import1"));
    $fb->input(
        type     => "file",
        name     => "spreadsheet",
        prompt   => "Select your CSV spreadsheet",
        required => 1
    );
    $fb->input(name => "vcard", type => "hidden", value => 99999);
    $fb->buttons(submit => 1);
    my $out = $ml->h3("Import membership data");
    $out .= $fb->make;
    return $out;
}

sub export {
    my ($this) = @_;
    return $this->roster(
        include_login    => 0,
        rpt_format       => "csv",
        contact          => 1,
        use_column_names => 1
    );
}

sub do_import1 {
    my ($this)   = @_;
    my $input    = new ExSite::Input;
    my $encoding = $config{server}{db}{charset};
    my $t        = new ExSite::Time;
    my $stamp    = $t->write("unix_timestamp");
    my $trial;
    my $filename;
    if ($this->{input}{confirm}) {
        $trial    = 0;
        $filename = $this->{input}{filename};
    } else {
        $trial = 1;
        $filename = $input->fetch_file("spreadsheet", "name");
        return $this->error("File was not found.") if (!$filename);
        my $raw = $input->fetch_file("spreadsheet", "raw");
        open FILE, ">/tmp/$filename" or die $!;
        print FILE $raw;
        close FILE;
    }

    my $book;
    my $out;
    my @rows;
    if ($filename =~ /\.csv$/) {
        require Text::CSV_XS;
        my $csv = Text::CSV_XS->new(
            {
                binary             => 1,
                sep_char           => ",",
                escape_char        => undef,
                allow_loose_quotes => 1
            }
        ) or die "Cannot use CSV: " . Text::CSV_XS->error_diag();
        open my $fh, "<:encoding($encoding)", "/tmp/$filename" or die "$!";
        while (my $row = $csv->getline($fh)) {
            push @rows, $row;
        }
        $csv->eof or $csv->error_diag();
        close $fh;
    } else {
        $out .= $ml->error("
	Please try again. Your file does not seem to be a comma seperated file ( does not have a csv extension )."
        );
        $out .= $this->import_form;
        return $out;
    }
    my $header         = shift @rows;
    my @member_columns = $this->get_member_columns;
    my ($out, $num_errors, $num_inserts, $num_updates);
    my $position = $this->{input}{position} || 2;
    my $n = $position;
    foreach my $row (@rows) {
        my @err;
        my $is_skip;
        my $is_exist;
        my $is_contact_row = 1;
        my $contact;
        my $c;
        my $member = $this->{Member} = new Modules::Membership::Member();

        my $i = 0;
        my $status;
        foreach my $col (@$header) {
            my $value = $row->[$i];
            my $converter = Text::Iconv->new($encoding, $encoding);
            $value = $converter->convert($value);

            # are we updating an existing member
            if ($col eq "member_id" && $value) {
                $is_exist = 1;
                $member = $this->{Member} = new Modules::Membership::Member(id => $value);
            }

            my $datatype = $this->get_datatype($col);
            if ($col eq "email") {

                # sync email and login if necessary
                if ($member->getdata("email") ne $value) {
                    my @exist = $share{DB}->fetch_match("member", {login => $value});
                    if (!scalar @exist && $member->getdata("email") eq $member->getdata("login")) {
                        $member->setdata("login", $value);
                    }
                }
            } elsif ($datatype eq "money") {
                $value =~ s/^\$//;
            }

            # check for contact data
            if ($col =~ /contact_(\w+)/) {
                $contact->{$+} = $value;

                # WARNING: column names can collide with attribute names
                # check for member data
            } elsif (scalar grep(/$col/, @member_columns) > 0) {
                $member->setdata($col, $value);

                # check for member meta data
            } elsif ($member->meta()->is_allowed($col)) {
                if ($member->meta_get($col) ne $value) {
                    if (!$member->meta_set($col, $value)) {
                        push(@err, $member->meta()->show_diagnostics("error"));
                    }
                }
            } elsif ($col eq "status"
                && (!$is_exist || $member->status() ne $value))
            {
                $status = $value || "active";
            }
            $i++;
        }
        $out .= $ml->p("Analyzing row " . $n) if ($trial);
        if (my $type = $member->getdata("type")) {
            $is_contact_row = 0;
        }

        push(@err, $member->validate());
        if (scalar @err) {
            $out .= join("\n", map { $ml->error($_) } @err);
            $num_errors += scalar @err;
            next;
        }

        # inserting a new member
        if (!$is_exist && !$is_contact_row) {
            $member->setdata("ctime",      undef);
            $member->setdata("access",     0);
            $member->setdata("section_id", $this->get_section_id);
            $c = new Modules::ID::Contact(data => $contact);
            if (!$trial) {
                $this->set_password();
                $this->finalize_application();
                $member->save();
                $session{last_member_id} = $member->id;
                $num_inserts++;
                if (grep(/\w+/, values %$contact)) {
                    $c->setdata("account_id", $member->account->id());
                    $c->setdata("privacy",    "public")
                      if (!$contact->{privacy});
                    $c->save();
                }
                my $name = $ml->a(
                    $this->membername($member->get()),
                    {
                        class => "membername",
                        title => $this->membername($member->get()),
                        href  => $this->link(
                            pro   => "member",
                            uid   => $member->id,
                            _bare => 2
                        ),
                        target => "_blank"
                    }
                );
            }

            # member exists - do an update - could be a contact row or member row
        } else {
            $c = new Modules::ID::Contact(id => $contact->{contact_id});
            if (!$trial) {
                if ($is_contact_row && !$member->getdata("member_id")) {

                    # last_member_id keeps track of the last member operated on
                    # we attach the contact to this member in cases where no member id has been specified
                    $member = new Modules::Membership::Member(id => $session{last_member_id});
                }
                my $null = "NULL";
                $member->setdata("mtime", \$null);
                $member->save();
                $session{last_member_id} = $member->id;
                $num_updates++;
                my $id = $member->id;
                if (grep(/\w+/, values %$contact)) {
                    map { $c->setdata($_, $contact->{$_}) } keys %$contact;
                    $c->setdata("account_id", $member->account->id());
                    my $ok = $c->save();
                }
                my $name = $ml->a(
                    $this->membername($member->get()),
                    {
                        class  => "membername",
                        title  => $this->membername($member->get()),
                        href   => $this->link(pro => "member", uid => $member->id),
                        target => "_blank"
                    }
                );
            }
        }
        if ($status) {
            $member->set_status($status, "member import by admin");
        }

        # output for review
        if ($trial) {
            if (!$is_contact_row) {
                $out .= $member->show_generic();
                $out .= $c->show() if (grep(/\w+/, values %$contact));
            } elsif (grep(/\w+/, values %$contact)) {
                $out .= $c->show();
            }
        }
        $n++;
        $position = $n;
        last if (($num_inserts + $num_updates) == 5);
    }
    my $summary;
    if ($num_errors) {
        $summary =
          $ml->info(
            "You have $num_errors errors to resolve before you can import this spreadsheet. See below for details.");
    } elsif ($trial) {
        $summary .= $ml->info(
"Your spreadsheet has been validated and an overview of the members to be imported are shown below. If the information below is correct please click the button below to have the system begin the import."
        );

        my $f = new ExSite::FormBuilder();
        $f->input(name => "filename", type => "hidden", value => $filename);
        $f->input(name => "confirm",  type => "hidden", value => 1);
        $f->buttons(submit => "Begin Import");
        $summary .= $f->make;
    } elsif ($position < scalar @rows) {
        $summary .= $ml->p("Please wait while your members are imported.");
        $summary .= $ml->p("Progress: working on row $position / " . scalar @rows);
        my $f = new ExSite::FormBuilder();
        $f->name("background_import");
        $f->input(name => "filename", type => "hidden", value => $filename);
        $f->input(name => "confirm",  type => "hidden", value => 1);
        $f->input(name => "position", type => "hidden", value => $position);
        $f->buttons(submit => 0, cancel => 0, reset => 0);
        my $autosubmit =
          $ml->script("setTimeout(document.background_import.submit(),10000)", {type => "text/javascript"});
        $summary .= $f->make;
        $summary .= $autosubmit;
    } else {
        $summary .= $ml->h1("Import Complete");
        $summary .= $ml->info(
"The system has finished importing your updates to the membership database. Please carefully review your data to ensure that all expected changes have been made."
        );
    }
    return $summary . $out;
}
