# VM NEWS, releases 8.3.0 onwards

If you are upgrading from a previous version of VM, look through the entries
since that version to see how you might be affected.

Earlier releases are in NEWS-2.md, 8.0.0 through 8.2.0b1, and NEWS-1.md, 4.10
through 7.19.  This is the newest file, so new entries go at the front of it.

## VM 9.0.0 released

  * **A field width and a maximum work as `printf` does**
    (emacs-vm/vm#848), in `vm-summary-format` and in
    `vm-mime-button-format-alist`.  The maximum cuts the substitution and the
    width pads what is left, where before the maximum was applied to the
    padded text: `%20.4s` answered four spaces and is now four columns of the
    subject in a column twenty wide.  A width beginning with `0` fills with
    zeros only where the substitution is a number, so `%010w` is `    Monday`
    rather than `0000Monday`, and a `-` beats a `0` as it does in `printf`,
    so `%-05l` is `2    ` rather than `20000`.  A format that writes the two
    with the same number, as the default `%-17.17F` does, is unaffected, and
    so is every button format VM ships.

  * **`%%` is a single `%` in a summary or MIME button format**
    (emacs-vm/vm#847).  It was in a format that had something else to
    substitute, and was not in one that had nothing: a `vm-summary-format` of
    `100%% done` summarised as `100%% done`.  A specifier VM does not know is
    left as it stands now as well, where a `%q` beside a live specifier used
    to break every line with "Not enough arguments for format string".

  * **`%r` and `%R` work in `vm-summary-format`** (emacs-vm/vm#846).  Both
    have been documented, in the docstring and in the manual, as the
    recipients of the message: the `To` and `Cc` headers together, as
    addresses and as full names.  Both were left in the summary as the
    literal text `%r`, the compiler's regexp having no `r` or `R` in it
    while the branches it feeds have called `vm-su-to-cc` and
    `vm-su-to-cc-names` all along.

  * **A message saved into an IMAP mailbox keeps its labels**
    (emacs-vm/vm#828).  VM uploads the copy with `APPEND` and sent only
    `\Answered` and `\Seen` with it, so a labelled message arrived in the
    destination mailbox with nothing on it.  The copy now carries the system
    flags, the labels, and `filed`, `written`, `forwarded` and
    `redistributed`, which are the keywords the synchronising path sends.
    Never `\Deleted`: a message is not saved into a mailbox in order to be
    deleted from it.

    What the destination will keep is asked before anything is sent, in its
    PERMANENTFLAGS.  A server that does not take a keyword may refuse the
    whole `APPEND`, which would lose the copy rather than the label, so a
    keyword the mailbox does not name is left out and VM says which.

  * **The line suggesting `vm-check-configuration` waits to be read**
    (emacs-vm/vm#844).  It was said in the middle of the startup, where the
    folder totals and the messages a fetch prints came after it and
    overwrote it: a reader saw something go past and had to dig it out of
    the log buffer to find out what it said.  It is said once Emacs is idle
    now, and stays there until you do something.  It also says what it
    means: "VM has 2 settings missing.  M-x vm-check-configuration says
    which, and what to set".

  * **`vm-check-configuration` checks the `From` header VM writes**
    (emacs-vm/vm#832).  VM puts the value of `vm-mail-header-from` into a
    composition verbatim, after `From: `, so a value that is not an address
    is a header nobody can reply to and nothing said so.  A reader who
    wanted a header on every composition and reached for this variable
    rather than `mail-default-headers` sent every message as
    `From: IMAP-FCC: Sent`, and the copies filed on the server carried it
    too.

  * **`M-x vm-backup-folder` keeps a copy of a folder as it is on disk**
    (emacs-vm/vm#843).  Emacs backs a file up on the first save of its
    buffer and not again, so a folder saved earlier in the session has no
    copy of what is on disk now.  This makes one, named as Emacs would name
    a backup, so it lands wherever `backup-directory-alist` and the
    numbered-backup settings put your others.

    It runs from anywhere in a folder, the summary included.  `backup-buffer`
    does nothing there: a summary or presentation buffer visits no file, so a
    command that clears `buffer-backed-up` and calls it appears to do nothing
    at all.

  * **A send that has stopped can be interrupted with `C-g`**
    (emacs-vm/vm#842).  A sender that runs a program, which is what
    `sendmail-send-it` does with `msmtp` or `sendmail` behind it, waited in
    `call-process-region`.  That stops the whole of Emacs: no timer runs and
    nothing reads the keyboard, so the `C-g` was not ignored but never seen,
    and a mailer waiting on a server that had stopped answering could only be
    got out of with a signal sent from outside Emacs.  VM now runs the
    program so that Emacs can see the keystroke.

    Interrupting kills the program, which unsends nothing: the message may
    have reached the server already, so look at what arrived before sending
    it again.

  * **`vm-pgg` is removed, and `vm-epg` replaces it** (emacs-vm/vm#568).
    Both halves of that happen in this release: 8.3.2 had vm-pgg and no
    vm-epg, so if you are upgrading from it, the package you were using is
    gone and its replacement is new to you.

    **A configuration with `(require 'vm-pgg)` in it will not start.**  The
    file is gone, so the `require` signals "Cannot open load file".  Change
    it to `(require 'vm-epg)`.

    vm-epg does what vm-pgg did: it reads and writes PGP/MIME and inline
    armour, signs, encrypts, and attaches a public key.  It uses the `epg`
    (EasyPG) interface to gpg that Emacs comes with, where vm-pgg used
    `pgg`, which Emacs has had in `lisp/obsolete` since 24.1.

    Your settings do not carry over, the two having different option names,
    so `M-x customize-group RET vm-epg RET` is worth a visit.  Each command
    keeps its name bar the prefix: `vm-epg-sign`, `vm-epg-encrypt`,
    `vm-epg-sign-and-encrypt`, `vm-epg-attach-public-key`.

    If you keep your own copy of `vm-pgg.el` on `load-path`, loading both
    still conflicts and vm-epg still says so: the two define the same MIME
    display handlers, and whichever loads last wins.

  * **A composition carries a `From` header** (emacs-vm/vm#832).  Emacs's
    `mail-setup-with-from` asks for one and defaults to `t`, and VM read the
    variable nowhere, so a composition had no `From` header unless
    `vm-mail-header-from` was set.  The message that went out still got one,
    added by the sender, but the copies an `Fcc` or `IMAP-FCC` header files
    are written before the send: a copy in a Sent mailbox on an IMAP server
    named no sender at all.  The header is built the way Emacs builds it, so
    what reaches the recipient is unchanged.  Set `mail-setup-with-from` to
    `nil` for the old behaviour.

  * **`vm-mime-7bit-composition-charset` is removed** (emacs-vm/vm#697).  It
    was consulted nowhere, so setting it did nothing, and the manual told you
    to set it to declare a composition's character set.  Emacs knows which
    characters are in the buffer and VM asks it; set
    `vm-coding-system-priorities` to put your own order on the answer.  Its
    sibling `vm-mime-8bit-composition-charset` went earlier for the same
    reason.

  * **Killing a composition you have written in keeps it as a draft**
    (emacs-vm/vm#824), in `vm-save-killed-messages-folder`, with VM saying
    where it went and which key takes it up again.  A composition buffer
    belongs to no file, so Emacs did not put the question it puts about an
    unsaved file, and `kill-buffer` and everything bound to it,
    `kill-this-buffer` included, took an unsent message with no warning.  One
    user gave up on VM over it, having lost countless drafts.

    Nothing is kept for a composition you have written nothing in, and
    nothing is said about it either.

    `vm-save-killed-message` had this in it already, but only for those who
    had switched `vm-postpone-mode` on, which is not the default and which
    also binds four keys nobody asked for.  Every composition arranges it
    now, and the option's default changes from `ask` to `always`: set it back
    to `ask` to be asked each time, or to nil not to keep drafts at all.  Nil
    leaves the new option `vm-confirm-killing-a-composition`, on by default,
    to ask before the writing is lost.

  * **Visiting a folder says that it is getting new mail** (emacs-vm/vm#825),
    where it used to say the totals of the folder as it stood.  The fetch is
    asynchronous, so at that moment it has not brought anything in yet, and
    what a reader was told last was the cached count: on a folder with a large
    cache, where reading the mailbox takes a while before the first message
    arrives, that reads as nothing having happened.  `vm-get-new-mail` says
    the same.  The mail arrives as it always did, and the arrival still
    reports what came.

  * **The blocking IMAP and POP implementation is gone** (emacs-vm/vm#822),
    137 functions and about 4000 lines of `vm-imap.el` and `vm-pop.el`.  Every
    command a folder can run reaches its server through the asynchronous
    driver, and nothing waits except where waiting is the point: a message
    body a save or a copy must have in hand, and folder-name completion.  Both
    of those wait on the folder's own session, so `C-g` works.

    Nine functions that carried autoload cookies went with it, so an init file
    that calls one by name will now get a void-function error.  They were
    internal, documented nowhere, and named no command: `vm-imap-make-session`,
    `vm-imap-end-session`, `vm-imap-move-mail`, `vm-imap-save-message`,
    `vm-imap-synchronize-folder`, `vm-imap-folder-check-mail`,
    `vm-pop-move-mail`, `vm-pop-synchronize-folder` and
    `vm-pop-folder-check-mail`.  Use the commands instead:
    `vm-get-new-mail`, `vm-imap-synchronize`, `vm-save-folder`.

  * **A POP server with no UIDL no longer works** (emacs-vm/vm#822).  UIDL is
    what tells one message from another between sessions, so without it VM
    cannot say which messages it has already fetched.  The old implementation
    kept count by deleting each message as it took it, which is not something
    it can do from a process filter and not what a reader who leaves mail on
    the server asked for.  VM now says so and fetches nothing, rather than
    fetching the same mail twice or deleting what you meant to keep.

  * **A POP maildrop message over `vm-pop-max-message-size` is left on the
    server, and VM now says so** (emacs-vm/vm#822), naming its size and the
    limit, as the IMAP side does.  It used to be passed over in silence.  The
    manual said VM asked, message by message, whether to fetch it; it has not
    asked since the fetch became asynchronous, and the manual says what
    happens now.

  * **A POP body line beginning with a dot arrived without it**
    (emacs-vm/vm#822): `.hidden` came out as `hidden`.  The asynchronous
    reader undoubled the dot RFC 1939 asks a server to stuff, and then the
    older cleaning-up pass took a second one off.  Fixed.

  * **Mail fetched from an IMAP maildrop into a babyl folder** was written
    without the babyl folder header (emacs-vm/vm#822), so the crash box read
    back as no folder type at all.  Fixed.

  * **A bug report now carries the trace of the session still running**
    (emacs-vm/vm#822).  `vm-imap-submit-bug-report` and
    `vm-pop-submit-bug-report` used to end the folder's session so that its
    trace reached the ring they read.  The sessions are asynchronous now, and
    ending one would abort a fetch in flight, so the running session's trace
    goes into the report where it is: a report can be made about a fetch while
    it is happening, and nothing is closed to collect it.

  * **New option `vm-session-trace-max-size`** (emacs-vm/vm#822), the most of
    one session trace a bug report carries, 100000 characters by default.  A
    synchronisation of a large mailbox leaves most of a megabyte of near
    identical `FETCH` lines behind it, and a report that size cannot be sent.
    Beyond the limit the middle of the trace is left out and the report says
    how much; nil sends the whole of every trace.

  * **An IMAP maildrop message over `vm-imap-max-message-size` is left on the
    server, and VM says so** (emacs-vm/vm#822).  It used to ask, message by
    message, whether to fetch it, delete it or skip it, showing the headers
    to decide by.  Nothing can be asked from inside a process filter, and the
    fetch is asynchronous now, so the question has gone: the message stays
    where it is and a warning names its size and the limit.  Raise the limit
    and the next `vm-get-new-mail` brings it in.

    This is what the option always said happened in a local folder.  An IMAP
    *folder* is unchanged: an oversize message there is fetched as its headers
    where `vm-enable-external-messages` includes `imap`, and the body comes
    from the server when it is read.

  * **`rpop` is no longer a POP authentication method** (emacs-vm/vm#822).
    It was RFC 1081's trusted-host scheme: a privileged source port stood
    for the authentication and the password went to the server under another
    verb.  Essentially no server offers it, VM's implementation only sent the
    password differently, and it was the one method the asynchronous driver
    would never have served.

    A maildrop that asks for it now says so and names what to write instead,
    rather than failing as an authentication VM does not recognise.  Use
    `pass`, or `apop` where the server offers a timestamp.

  * **CRAM-MD5 no longer costs you the asynchronous IMAP driver**
    (emacs-vm/vm#822).  A maildrop asking for it was refused by the driver
    and served by the blocking implementation, so every fetch from such an
    account held Emacs still.  The driver speaks CRAM-MD5 now, and those
    maildrops are as asynchronous as the rest.

  * **A POP maildrop asking for `apop` no longer sends its password in
    clear** (emacs-vm/vm#823).  The asynchronous POP driver read every field
    of the maildrop except the authentication method, and always
    authenticated with `USER` and `PASS`.  So `pop:host:110:apop:you:*` was
    served by the method `apop` exists to avoid, and nothing said so.

    APOP is done by the driver now.  An `rpop` maildrop is refused by it and
    served by the blocking implementation, as before.  A server offering no
    APOP timestamp is an error rather than a fall back to `PASS`: falling
    back would send the password in clear, which is what the maildrop asked
    not to happen.

    This affects POP maildrops in `vm-spool-files` and the POP mail check.  A
    POP folder was never affected: it goes through the blocking
    implementation, which has always read the field.

  * `vm-word-wrap-paragraphs` and `vm-word-wrap-paragraphs-in-reply` no
    longer need the `longlines` library, and no longer load it
    (emacs-vm/vm#817).  It has been obsolete since Emacs 24.4 and says so as
    it loads, which is what was reported.  VM wraps the lines itself, in
    fourteen lines of its own, and the result is the same except that
    `longlines` left a space at the end of every line it wrapped.

    Two things change for anyone who had this on:

      * The column wrapped to is `vm-paragraph-fill-column`, or
        `vm-fill-long-lines-in-reply-column` in a reply, as both have always
        documented themselves to be.  The old code used
        `vm-fill-paragraphs-containing-long-lines`, which is the threshold
        for what counts as a long line and not the column, so a column of 30
        produced lines of 59.
      * A wrapped line no longer ends in a space.  Under RFC 3676 a trailing
        space is a soft line break, so the old output meant something to a
        recipient reading `format=flowed` that was never intended.

  * `M-x vm-check-configuration` says what is missing from a VM setup and
    what to set for each (emacs-vm/vm#816).  It checks the settings that have
    to be right before anything works: which mail agent Emacs uses, the
    address mail goes out from, how it is sent, where folders are kept, and
    where new mail comes from.

    It never runs by itself.  What VM does unasked is say once per session,
    in one line, that the command would have something to report, and only
    where it would; `vm-suggest-checking-configuration` set to nil stops
    even that.

    It reports only settings whose default either does nothing or does
    something the reader did not choose, so silence is meaningful.  Two are
    worth naming: `user-mail-address` when Emacs has invented it from the
    machine's name, which sends mail from an address nobody can reply to; and
    a maildrop with a type VM does not know or the wrong number of fields,
    which the parsers accept where it is written and which then fails from
    inside a session, saying something about the server.

  * VM no longer supports XEmacs, which has had no release since 2013 and
    which VM has not been tested against for as long (emacs-vm/vm#708).  The
    alternative implementations of menus, toolbars, extents and mouse
    handling that stood beside the Emacs ones are gone, along with
    `--with-emacs=xemacs` and the `xemacs-package` make target.

    Removed options and functions, each of which meant nothing in Emacs:

      * `vm-toolbar-orientation`.  The toolbar goes where the frame parameter
        `tool-bar-position` says.
      * `vm-toolbar`, which held a toolbar instantiator in XEmacs's own
        format and was read nowhere else.
      * `vm-use-lucid-highlighting`, which chose XEmacs's `highlight-headers`
        package over VM's own header highlighting.  There is no such package
        here, so VM's own always did the work.
      * `user-home-directory`, an XEmacs function VM defined for Emacs and
        never called.  `(expand-file-name "~")` is the Emacs way to it.

    A nil or an integer in `vm-use-toolbar` is now ignored rather than
    meaning flushright or a run of blank pixels.  The Emacs toolbar has
    neither.

    Two things that never worked here now do.  `vm-serial-set-token` called
    XEmacs's `read-expression` and so raised `void-function` every time it
    was used interactively; it reads with `read-minibuffer` now.  An
    `audio/basic` part is no longer offered for internal display, having been
    played by XEmacs's sound support and by nothing on this side.

  * VM asks before sending a message with a `Bcc` header, unless
    `send-mail-function` removes that header itself (emacs-vm/vm#815).
    `smtpmail-send-it` does: it works out the recipients first and then
    deletes it.  `sendmail-send-it` does not and cannot, because it passes
    `-t` and those addresses are how the transport learns whom to deliver to,
    so it hands the header over and trusts the program behind
    `sendmail-program` to remove it.

    Where that program does not, everyone on the message reads who was blind
    copied and nothing anywhere reports a failure.  That is what was
    reported.  It asks rather than refusing because a working sendmail,
    postfix or exim does remove the header and Emacs cannot tell one of those
    from a transport that does not; declining says what to change and where
    the manual explains it.  Set `vm-check-bcc-removal` to nil to stop it
    asking, if you know your transport removes it.

  * The manual has a Setting Up chapter (emacs-vm/vm#790): a configuration
    built one piece at a time, from making VM the mail agent Emacs uses
    through local and server folders, the summary, viewing, composing,
    sending, the address book and keys of your own.  It sits after Starting
    Up, which is where a new reader is.

    `example.vm` is the same settings in one file, and it now loads.  It did
    not: `(setq vm-primary-inbox POP IMAP)` offered two alternative values as
    if you could give both, so every setting after it was silently never
    made.  Five bare `require` calls stopped the whole configuration where
    they stood when a package was missing, five keys were bound to commands
    VM does not have, and `W` was bound twice in consecutive lines.  The
    original author's own name and address are out of it.

  * The keys VM 8 gives its own commands are bound (emacs-vm/vm#632).  `!`
    flags a message, `<` and `>` promote and demote a subthread, and `V O`,
    `V U`, `V D` and `V ?` do their virtual folder commands.

    They were left unbound in 8.2.0 because they had meant different things
    in different versions, and each was bound instead to a stub that reported
    the key as having an optional binding.  So the manual gave `!` for
    flagging a message and typing `!` answered an error.

    `vm-v7-key-bindings`, its alias `vm-legacy-key-bindings` and the stub are
    removed.  A preferences file calling either gets a void-function error
    rather than a set of keys the manual no longer describes.  The VM 7 set,
    to paste into your init file if you want it back:

    ```elisp
    (define-key vm-mode-map "<" 'vm-beginning-of-message)
    (define-key vm-mode-map ">" 'vm-end-of-message)
    (define-key vm-mode-map "b" 'vm-scroll-backward)
    (define-key vm-mode-map "e" 'vm-edit-message)
    (define-key vm-mode-map "w" 'vm-save-message-sans-headers)
    (define-key vm-mode-map "a" 'vm-set-message-attributes)
    (define-key vm-mode-map "i" 'vm-iconify-frame)
    (define-key vm-mode-map "*" 'vm-burst-digest)
    (define-key vm-mode-map "!" 'shell-command)
    (define-key vm-mode-map "=" 'vm-summarize)
    (define-key vm-mode-map "L" 'vm-load-init-file)
    (define-key vm-mode-map "\M-l" 'vm-edit-init-file)
    (define-key vm-mode-map "%" 'vm-change-folder-type)
    (define-key vm-mode-map "\M-g" 'vm-goto-message)
    ```

    `vm-v8-key-bindings` and `vm-current-key-bindings` stay, since the manual
    told readers to call them, and now bind what is bound already.

  * The contrib directory is gone, and one of its files is now part of VM as
    `vm-org.el` (emacs-vm/vm#812).  Nothing in contrib was installed, built,
    documented or tested, so nothing there was linted and what rotted in it
    rotted quietly.

    The two patch files it also held are dropped.  `vm-mime.el-w3m.patch`
    offered a choice between Emacs/W3 and emacs-w3m for HTML, which
    `vm-mime-text/html-handler` has done for years over four choices rather
    than two.  `attempted-locking.diff` added `lock-buffer` calls so that two
    Emacsen on one folder would notice each other, and Emacs already does
    that itself: a modified folder buffer has a `.#` lock file beside it.
    Neither had applied to this tree since the sources moved into `lisp/`.

    `require` it and a `vm:` link in an Org file names a folder and a message
    in it, so `C-c C-o` opens the folder and shows that message, and `C-c l`
    in a VM folder makes such a link.  Org carries no VM support of its own,
    so this is it.  The link storing half had not worked since Org 9.3
    removed the variable it hung on, and said nothing about it.

    Dropped: `vm-sumurg.el`, which called an XEmacs-only function at top
    level and so had never loaded on GNU Emacs; `org-html-mail.el`, which
    needs `orgstruct-mode` and `org-export-as-html`, both gone from Org;
    `vm-blueman.el`, a 2006 Usenet posting with no copyright statement and an
    anonymous author; `vm-bogofilter.el`; and
    `vm-mime-display-internal-application.el`.  They are in the history.

  * VM colours quoted text and the signature in a message body
    (emacs-vm/vm#811).  Set `vm-enable-body-faces`: quoted text then wears a
    face per level of quoting, from `vm-citation-faces`, and the signature
    wears `vm-signature-face`.  Off by default, since it changes how every
    message looks.

    This is what was worth keeping of the `u-vm-color.el` add-on removed in
    the same release.  That had to be wired up by hand and was never
    documented; this is in the manual, under previewing, and has faces named
    the way the rest of VM's are.

  * `u-vm-color.el` is gone (emacs-vm/vm#811).  It was bundled in 8.1.x and
    half of it was superseded in 8.1.93a, 2010-08-28, when
    `vm-summary-enable-faces` replaced `u-vm-color-summary-mode`; the NEWS
    entry then told readers to delete the hook because Emacs could hang with
    it kept.  It was never documented in the manual.

    The rest of it still worked: `u-vm-color-fontify-buffer` coloured
    headers, two levels of citation and the signature in the message body,
    and VM has nothing of its own for citations or signatures.  Anyone who
    had wired that up by hand loses it.  The file is in the history if it is
    wanted back.

    Removing it uncovered a fault in the build: `lisp/Makefile.in` wrote the
    four `vm-configure-` assignments into `vm-autoloads.el` before requiring
    the file that defines them.  `u-vm-color.el` was the only source sorting
    before the `vm-` ones, so compiling it pulled `vm-vars` in first and the
    whole-directory lint never saw the free variables.  The two lines are in
    the other order now, which is the order the XEmacs rule beside it has
    always had.

  * VM says when an IMAP server will not keep a label (emacs-vm/vm#601).  A
    server whose PERMANENTFLAGS does not offer `\*` keeps no keywords of its
    own, and Gmail is one.  It takes the STORE and answers OK all the same,
    so nothing failed and nothing was refused: the label was simply gone the
    next time the mailbox was read.

    Said once per folder, and only where a label is actually being sent, so a
    reader who sets none is never told about a limit that does not touch
    them.  The label is still lost; what changes is that the loss is no
    longer silent.

  * `M-x vm-pcrisis-check-configuration` says what is wrong with your
    Personality Crisis rules and what to do about each (emacs-vm/vm#806).
    Rules are data, so a mistake in them is not a Lisp error and nothing
    stops: a rule keyed on a condition that is not defined never runs, one
    naming an action that is not defined does nothing, and a name defined
    twice loses its second definition.

    The first of those is why the command exists.  A fallback written as
    `("default" "from-home")` with no condition called "default" contributes
    nothing, so the composition keeps `user-mail-address`, which is the
    address most people would have got anyway.  Such a rule can sit in an
    init file for years.

    The same check runs as each composition begins and warns about the first
    thing it finds.

  * A long header is folded, and a long encoded word is split
    (emacs-vm/vm#794).  VM wrote every header on one line however long it
    grew, so a subject of any length went out past what RFC 5322 allows: 78
    characters a line SHOULD NOT exceed and 998 it MUST NOT.  A run of 8-bit
    text became a single RFC 2047 encoded word whatever its length, where
    that standard sets 75.

    Thirty accented words made a line of 325 with an encoded word of 316 in
    it; four hundred plain words made a line of 2008.  Both are now lines of
    75 or less.

    What VM chooses is unchanged: the same charset, the same quoted-printable
    or base64, the same run of adjacent words encoded together.  Only the
    line breaks are new, and a reader joins them back up.  Splitting a run
    into several encoded words in a row is lossless for the same reason: RFC
    2047 section 6.2 has a decoder drop the whitespace between two adjacent
    encoded words.

    A run with no whitespace in it still goes out long, there being nowhere
    to break it.

  * Loading `vm-message-history.el`, `vm-postpone.el` or `vm-serial.el` no
    longer switches it on (emacs-vm/vm#788).  Each has a mode instead, all
    three autoloaded, so no `require` is needed:

    ```elisp
    (vm-message-history-mode 1)
    (vm-postpone-mode 1)
    (vm-serial-mode 1)
    ```

    An init file that says only `(require 'vm-message-history)` and expects
    the history keys, or `(require 'vm-postpone)` and expects a killed
    composition to be offered as a draft, or `(require 'vm-serial)` and
    expects a composition sent from a source buffer to be killed with it,
    needs the matching line above.  Turning a mode off undoes what it did,
    which was not possible before.

    The reason is that loading was never something a reader asked for:
    Customize loads any file that declares a group under `vm-ext` in order to
    answer a question about a VM option, so `C-h v` on any VM variable
    installed the hooks, keys and advice of all three.  `vm-biff` and
    Personality Crisis went the same way earlier in this cycle, and #785 was
    the case where it broke something.

    `C-c C-d` (`vm-postpone-message`) and `vm-continue-postponed-message` are
    unaffected: VM binds the first itself and both are autoloaded, so
    postponing and continuing work with the mode off.

  * `vm-default-folder-type` no longer offers `BellFrom_`, and neither does
    `vm-change-folder-type` (emacs-vm/vm#787).  VM reads a BellFrom_ folder it
    is handed, and creates none.  That format is From_ without the blank line
    between messages, so it has no signature of its own: `vm-get-folder-type`
    answers `From_` for it, and a folder VM wrote as one was read back as
    From_ with each message swallowing the headers of the next.  The manual
    has recommended converting old BellFrom_ folders to From_ since 2000.

    Reading is unchanged.  A folder whose name or `vm-folder-type-by-extension-alist`
    entry says `BellFrom_` is still read as one, and
    `vm-default-From_-folder-type` still takes either value, that option being
    how you say which of the two From-style formats your delivery agent
    writes.

    A configuration that still sets `vm-default-folder-type` to `BellFrom_`
    gets what it asks for, and VM now says once at startup what that means.

  * `vm-pgg` no longer takes PGP away from `vm-epg` (emacs-vm/vm#785).  Both
    answer for the same three MIME types, and whichever was loaded last used
    to hold them.  Loading `vm-pgg` was not always deliberate: it declared
    its customization group as a child of `vm-ext`, so anything that asked
    Customize about that group loaded it, `C-h v` on a VM option among them,
    and PGP then stopped working for a reader who had never asked for
    `vm-pgg`.  That group is declared in `vm-vars.el` now, so only opening
    the group itself loads the file, and loading it with `vm-epg` present
    installs nothing at all: no handlers, no advice, no compose hook.

    Nothing changes for anyone using `vm-pgg` alone, which still works and
    is still deprecated.

  * `vm-epg` encrypts a message to its recipients and to nobody else
    (emacs-vm/vm#782).  `vm-pgg`, through the obsolete `pgg` package, always
    added your own key, so a copy filed with `FCC:` could be read back.
    Under `vm-epg` it cannot be, and neither can anything else you keep of
    what you sent.  To encrypt to yourself as well, say so to GnuPG rather
    than to VM, with a line in `~/.gnupg/gpg.conf`:

        encrypt-to YOUR-KEY-ID

    VM passes `gpg` no `--no-encrypt-to`, so that setting is honoured.  It
    is `gpg`'s setting, so it applies to everything that encrypts on your
    behalf, not to VM alone.

  * IMAP CRAM-MD5 authentication now works with a password that is not
    plain ASCII, and with one longer than 64 characters (emacs-vm/vm#772).
    HMAC is defined over octets; VM XORed character codes and padded to 64
    characters, so an accented password produced a digest the server
    rejected and VM reported the correct password as incorrect.  A password
    over 64 characters raised "strings not of equal length" instead of
    logging in, RFC 2104's rule that an over-long key is hashed first not
    having been implemented.  What goes on the wire changes for those
    passwords, and is now what every other client sends.  APOP was never
    affected.

  * New command `vm-convert-caches-to-mboxcl2` converts the POP and IMAP
    caches VM wrote before it named its caches for their type
    (emacs-vm/vm#768).  Those are read as From_, where a message whose body
    holds a line beginning `From ` can split the cache in two; mboxcl2 ends a
    message by a byte count instead.  It finds them, asks once, and converts
    and renames each, keeping the previous contents in a backup file.  A
    prefix argument asks about each cache.  Nothing is refetched.

    A folder that is already sound is now renamed rather than left under a
    name that does not say its type, which `vm-change-folder-type` and
    `C-u M-x vm-change-folder-type` both do too.

    `vm-check-folder` reports a cache whose name states no type and names the
    command, so such a cache now gets a report buffer where it used to be one
    line in the echo area.  Nothing in it is wrong, and the report says so.

    Converting a folder on disk now refuses a folder you are reading rather
    than killing its buffer, which had left its summary and presentation
    answering "Folder buffer has been killed" (emacs-vm/vm#770).  Quit the
    folder and convert it again.  The buffer a failed visit leaves behind is
    still killed, and now takes its summary and presentation with it.

  * `vm-default-folder-type` set to mboxcl2 now names what it creates:
    saving to a folder that does not exist, an `FCC:` to one, and a postponed
    draft all create `NAME.mboxcl2` (emacs-vm/vm#767, emacs-vm/vm#766).  A
    folder's type is read back from its name, so mboxcl2 under a name that
    says nothing would be read as From_ next time and split wherever a body
    line begins `From `.  The option still decides only the folders VM
    creates: your existing folders are read as they always were.  The default
    is `From_`, so nothing changes unless you asked for mboxcl2.

  * Personality Crisis acts on the signature Emacs inserted, without being
    told to expect one.  `vm-pcrisis-expect-default-signature` now defaults to
    on, so a rule saying `(vm-pcrisis-signature "")` deletes the signature VM
    put in the composition from `mail-signature`, and one naming a file
    replaces it rather than adding a second (emacs-vm/vm#540).  It found only
    a signature it had inserted itself before, and did nothing, quietly, to
    any other.  What it looks for is a line of exactly `-- `, so a signature
    in quoted text is not it; set the variable to nil to keep a signature
    action away from a signature Personality Crisis did not insert.

  * HTML quoted in a reply is broken at 80 columns, whatever window the
    message was read in.  An HTML part has no line breaks of its own, so
    whatever converts it to text decides where its lines end: emacs-w3m used
    the width of the window, lynx 72 columns, so the same message quoted
    differently from one day to the next (emacs-vm/vm#369).  Set the new
    `vm-html-in-reply-column` to another number for lines of that width, or to
    `window-width` for what VM did before.  Displaying a message is unaffected
    and still fills to the window.

  * `vm-change-folder-type` with two prefix arguments converts a folder on
    disk into a file you name, leaving the folder you converted exactly as it
    was: no backup is made, since nothing is overwritten, and nothing is
    offered for deletion.  That is how to look at a conversion before trusting
    it, or to repair a copy of a folder rather than the folder
    (emacs-vm/vm#763).  One prefix argument still converts a folder on disk in
    place, and no prefix argument the folder you are in.

    A name that cannot hold the type is now refused rather than written, and
    the command says what to call it instead: an mboxcl2 folder called
    `out.mbox` would be read back as From_ and split wherever a body line
    begins `From `, and one called `out` with no extension the same.  Nothing
    is asked of a name that could not state the type anyway, `From_` being the
    type a folder has when its name says nothing.

  * `C-u M-x vm-imap-synchronize` no longer deletes mail on the server.  It
    used to delete every message the mailbox had and the cache folder did not,
    with no confirmation and no report of how many (emacs-vm/vm#752).  That is
    what a reader who expunged messages offline looks like, but it is also what
    a cache looks like after being truncated, restored from a partial backup or
    read under the wrong folder type, and in those cases the whole mailbox went.
    The expunges you make are sent either way: VM records each one as you make
    it and keeps the record in the folder, so it survives a session that could
    not reach the server.  The prefix argument keeps its other meaning, which is
    to send every message's flags rather than only those that changed.

  * Declining `vm-continue-what-message`'s offer of the drafts folder starts
    a new message, where it used to do nothing at all (emacs-vm/vm#755).  The
    command is the key you press to write mail -- the manual binds
    `vm-continue-what-message-other-window` to `C-x m` in place of
    `compose-mail` -- and answering no to the drafts it found left you with
    no composition and no drafts folder either.  The drafts are untouched and
    still there to continue.  `vm-continue-what-message` nil, never continue,
    composes for the same reason.  `vm-zero-drafts-start-compose` still
    decides what happens when there are no drafts anywhere.

  * IMAP and POP are asynchronous and do not block Emacs.  Fetching mail,
    loading a body, sending flag changes, expunging, saving, quitting,
    synchronising, filing an `FCC:` copy and listing mailboxes all happen
    while you carry on reading (emacs-vm/vm#473).  The folder is not locked
    while they do: read it, move about it, delete, mark, label and expunge as
    usual, and work that needs the server runs when the session now running
    has finished, one session to a folder.  New mail arrives a bunch at a
    time, so a large mailbox fills in while you read it.  The mode line says
    what the folder is doing and how far it has got, in the face
    `vm-net-session-face`.  Two things still wait and say so: saving or
    copying a message whose body is still on the server, and completing a
    folder name.  `C-g` works in both.

  * `C-u C-u g` (`vm-get-new-mail` with two prefix arguments) fetches every
    message an IMAP mailbox has and the folder has not, including those VM
    has recorded as retrieved once already.  The record is what stops a
    message deleted here from coming back; this is how to refill a cache
    folder that lost messages some other way, which nothing could ask for
    before (emacs-vm/vm#751).  One prefix argument still gathers from a
    folder the reader names.

  * VM reads and writes mboxcl2, the mbox variant that keeps a
    `Content-Length` header and stores a message exactly as it arrived
    (emacs-vm/vm#466).  A fallout: the folder type is now called `mboxcl2`
    rather than `From_-with-Content-Length`, and `vm-trust-content-length`
    rather than `vm-trust-From_-with-Content-Length`.  The old names still
    work.

  * A folder's name can say what format it is.  A folder whose name ends in
    `.mboxcl2` is created as one, rather than as whatever
    `vm-default-folder-type` says, and is read as one -- so a folder named
    that way which has no `Content-Length` headers is refused, with how to
    convert it, instead of being read as a From_ folder in silence
    (emacs-vm/vm#610, emacs-vm/vm#620).  The option is
    `vm-folder-type-by-extension-alist`, which matches the extension
    literally and replaces `vm-folder-type-by-name-alist`
    (emacs-vm/vm#741).

    `.mbox` is From_, which is what everything outside VM means by an mbox
    file (emacs-vm/vm#750).  From_ is never put into a name that has not got
    it, since it is the type a folder has when its name says nothing: `INBOX`
    converted to From_ is still `INBOX`, and a folder you called `sent.mbox`
    keeps that name.  Not `.mboxcl`: VM has no mboxcl type, and mboxcl quotes
    `From ` lines in bodies where mboxcl2 does not.

  * An mboxcl2 folder is kept sound.  Every message VM writes into one carries
    a `Content-Length`, which is how the end of a message is found there, and
    a message that has none is refused rather than guessed at;
    `vm-mboxcl2-strict` nil opens such a folder so that
    `vm-change-folder-type` can repair it (emacs-vm/vm#612).

  * `vm-change-folder-type` with a prefix argument converts a folder on disk,
    without visiting it, and keeps the folder as it was in a backup file --
    as does changing a visited folder's type.  That is how to repair a folder
    VM will not read, since such a folder cannot be visited to convert it
    (emacs-vm/vm#613).

  * `vm-check-folder` says what the folder you are in is and whether it is
    sound, and writes nothing: the type VM reads it as, what its name says,
    what its contents say, how many messages it holds against how many
    walking the separators finds, and for an mboxcl2 folder whether every
    `Content-Length` matches its body.  A sound folder is one line in the echo
    area.

    What the contents say is counted over every message and the name is not
    consulted for it, which is the case a name cannot answer: VM takes a
    folder's type from its name, so a folder carrying a length that fits on
    every message under a name that does not say mboxcl2 is read as From_ and
    split wherever a body line begins `From `.  The check reports that, and
    names the rename and the conversion.

    With a prefix argument it asks for a folder file and checks that, without
    visiting it, which is the only way to check a folder VM will not read.

  * An IMAP or POP cache VM creates is named `imap-cache-<md5>.mboxcl2` and
    written in that type.  A cache that already exists keeps its name and is
    read as whatever it is: nothing is converted and nothing is refetched.

  * `vm-default-folder-type` is `From_` on every platform.  It was mboxcl2 on
    Solaris, AIX and System V, and it decides only folders that do not exist
    yet.

  * VM no longer decides between From_ and mboxcl2 by sniffing the first
    message of a folder, so `vm-trust-content-length` is deprecated
    (emacs-vm/vm#736).  Name the type instead, by the folder's extension or
    with `vm-default-folder-type`, which is the answer for a folder that
    cannot be renamed such as `INBOX`.  The sniffing still happens where the
    option is switched on, and warns once per folder.

  * Personality Crisis and vm-biff must now be switched on, since loading a
    file no longer does it and neither says anything when it is off.  Add
    `(vm-pcrisis-mode 1)` (emacs-vm/vm#561) or `(vm-biff-mode 1)`
    (emacs-vm/vm#512) to your init file; without the first, compositions are
    set up with none of your rules, and without the second there is no
    new-mail notification.  `(require 'vm-pcrisis)` now only loads the file,
    and is not needed at all since the mode is autoloaded.  An argument of -1
    switches either off, which a feature installed by loading a file could
    never be.

  * Thirty-four compatibility aliases and twelve further obsolete names are
    gone, each marked obsolete for at least four release cycles
    (emacs-vm/vm#594).  An init file that names one now calls a function that
    does not exist, or sets a variable nothing reads; the ticket lists every
    name and its replacement.  Two to watch: the sixteen `vm-summary-*-face`
    names lose the `-face` suffix, and
    `vm-mime-forward-local-external-bodies` set to t becomes
    `vm-mime-forward-saved-attachments` set to nil, the same thing the other
    way up.

  * vm-rfaddons.el is gone, and with it `vm-enable-addons` (emacs-vm/vm#606).
    What was worth keeping is part of VM, each with an ordinary option:

    * shrunken headers, `vm-enable-shrunken-headers`
    * the composition checks, `vm-check-recipients`,
      `vm-check-for-empty-subject`, `vm-clean-subject-prefixes` and
      `vm-open-line-in-quoted-text`
    * saving attachments as mail arrives, `vm-auto-save-all-attachments`
    * answering a return receipt, `vm-handle-return-receipts`
    * a Date header of your own, `vm-mail-mode-fake-date-p`

    Half of the file went as superseded or little used, and with it the `.`,
    `T` and `C-c C-a` rebindings, so those keys are VM's own again.

  * Emacs/W3 is no longer one of the HTML viewers, the browser having been
    dropped from Emacs and being in no package archive (emacs-vm/vm#707).  If
    your init file sets `vm-mime-text/html-handler` to `emacs-w3`, set it to
    `emacs-w3m`, `w3m`, `lynx` or nil; the default `auto-select` no longer
    considers it.  The `vm-url-browser` values `w3-fetch` and
    `w3-fetch-other-frame` and the `url-w3` entry in
    `vm-url-retrieval-methods` are gone the same way.  emacs-w3m is a
    different package and is unaffected.

  * Three misspelled option names were corrected, and the misspellings are
    gone (emacs-vm/vm#589).

  * New PGP/MIME support, vm-epg, built on the epg interface that comes with
    Emacs.  vm-pgg still works but is deprecated (emacs-vm/vm#581,
    emacs-vm/vm#568).

  * Sent copies work with IMAP.  An `FCC:` header naming an IMAP maildrop
    files the copy on the server instead of writing a file named after the
    maildrop (emacs-vm/vm#605), and VM files every copy itself, in the format
    of the folder it goes to (emacs-vm/vm#597).  The `FCC:` header stays in
    the composition, so correcting a message and sending it again files it
    again.

    An `IMAP-FCC:` header is filed the same way, with nothing to add to
    `mail-send-hook` (emacs-vm/vm#68).  It was wired up by hand until now,
    so a reader who did not notice the instruction got no copy at all; a
    configuration that still adds `vm-imap-save-composition` to the hook is
    harmless, since by then the copy is filed and the header gone.

  * VM works with BBDB again; its integration had been written for BBDB 2.x
    (emacs-vm/vm#567).

  * vm-pcrisis and vm-pine are part of VM rather than add-ons to install.
    vm-pine is now called vm-postpone.

  * Personality Crisis is named after VM: every `vmpc-` symbol is now
    `vm-pcrisis-`, so the feature turns up when you complete `M-x vm-`, run
    `C-h a vm-`, or look through the `vm` customize tree
    (emacs-vm/vm#657).  Everything an init file can name keeps working under
    its old name: the options carry a value saved by customize across, and
    the conditions and actions are aliased too, since rules name them as
    data.  `~/.vmpc-auto-profiles` and the `vmpc-profile` field written into
    BBDB records keep their names, being a file and a field rather than
    symbols.

  * Incoming mail can be filed and labelled by a table of virtual folder
    selectors, `vm-virtual-filter-alist` (emacs-vm/vm#542).

  * Everything VM says is kept, timed, in the buffer `*VM Log*`, each line
    giving the real and CPU time since the line before it.  `M-x vm-show-log`
    shows it and nothing has to be turned on.  `vm-log-level` takes a level
    on `vm-verbosity`'s scale and records up to it without changing what
    reaches the echo area; `vm-log-max-lines` bounds the buffer.

  * Long headers can be folded to one line, with a widget to unfold them:
    `vm-enable-shrunken-headers`.

  * A whole directory can be attached at once, with
    `vm-attach-files-in-directory`.

  * An attachment can be sent under a name other than the file it came from,
    with `vm-mime-rename-attachment` (emacs-vm/vm#392).

  * An unfinished composition is offered as a draft when you leave Emacs,
    rather than left as a file to find later (emacs-vm/vm#160).

  * A meeting invitation is shown as what it says, rather than as a
    `text/calendar` attachment (emacs-vm/vm#85).

  * Mail from a sender who wrapped their own text is re-wrapped to your
    window, and VM can mark its own wrapping the same way, per RFC 3676
    format=flowed (emacs-vm/vm#78).

  * MIME parameters with international characters are understood and generated
    per RFC 2231 (emacs-vm/vm#367).

  * IMAP folders are quicker to read: several message bodies are fetched in
    one command (emacs-vm/vm#185).

  * Labels reach the server more reliably.  A flag the server refuses no
    longer stops the others being stored (emacs-vm/vm#391), a refused change
    is no longer overwritten by the server's stale copy (emacs-vm/vm#270), and
    a copy saved to another folder says so when it carries the server's flags
    rather than yours (emacs-vm/vm#38).

  * Labels on an arriving message are added to the folder's list, so they turn
    up in completion; and `vm-expunge-label`, `vm-list-unused-labels`,
    `vm-expunge-unused-labels` and `vm-sync-labels` sort out a list that has
    gone astray (emacs-vm/vm#269).

  * VM no longer asks a server to clear `\Recent`, which RFC 3501 forbids and
    some servers answered with an error (emacs-vm/vm#389).

  * A line too long to send as it stands goes out quoted-printable, so it
    arrives as the one line you wrote instead of the whole message being
    BASE64 (emacs-vm/vm#593).

  * A Subject with an accent in it is encoded rather than sent raw when
    `vm-send-using-mime` is off (emacs-vm/vm#606).

  * A folder visited through a symbolic link is saved through the link, rather
    than replacing it with a file (emacs-vm/vm#532).

  * `t` keeps your place in a long message instead of jumping to the top
    (emacs-vm/vm#513).

  * A count larger than the messages left acts on the ones that are there,
    instead of refusing (emacs-vm/vm#550).

  * Killing a folder buffer takes its virtual folders with it, and asks first
    if any have unsaved changes (emacs-vm/vm#573).

  * Opening a folder that has another name says so, since saving writes only
    one of them (emacs-vm/vm#185).

  * `vm-sync-thunderbird-status` is now an ordinary option
    (emacs-vm/vm#589).

  * VM says less as it works: `vm-verbosity` defaults to 5, the level its own
    documentation calls normal.

  * `vm-movemail-program` defaults to the movemail Emacs came with
    (emacs-vm/vm#566).

  * `vm-drop-buffer-name-chars` defaults to dropping control characters and
    `/` rather than everything outside US-ASCII, so a folder name in your own
    language keeps its letters.

  * New `vm-startup-hook`, run once when VM starts (emacs-vm/vm#565).

  * New `vm-folder-cache-file`, the cache file of the folder you are in.

  * `vm-save-message-hook` is a variable at last.  `vm-save-message` has run
    it for years, but it was declared nowhere, so it could not be customized.

  * File name prompts offer the history VM keeps for them (emacs-vm/vm#587).

  * The manual has an appendix listing every command and user option, taken
    from the code, so it cannot fall behind (emacs-vm/vm#586).

  * ImageMagick 7 is supported, which renamed `convert` to a subcommand of
    `magick`.

  * The drag-and-drop attach command on macOS, `vm-ns-attach-file`, is gone;
    dropping a file into a composition was never bound to it in the way its
    own comment described (emacs-vm/vm#531).

  * For anyone writing against VM: message reverse links moved out of the
    message structure into `vm-reverse-link-table` (emacs-vm/vm#453), and VM
    has a test suite which a contributor is expected to add to.

## VM 8.3.2 released 2025-12-29

  * Address problem with 8.3.1 change where some user configurations for emacs
    30 need vm to evaluate autoloads.el.

## VM 8.3.1 released 2025-12-27

  * Work around ELPA install problem caused by emacs 28 not adding a provide
    call when generating autoloads.el.

## VM 8.3.0 released 2025-12-22

  * VM development converted from bzr to git and moved to GitLab.

  * Merged multiple pending changes.

  * Update to modern e-lisp.

  * Added `vm-version-commit` variable and function for the git commit id.

  * Removed the outdated copy of vcard support in favour of the standard
    package, which must be installed for vcard functionality to work.

  * Enable `vm-mail-check-recipient-format` by default.

  * Multiple bug fixes.
