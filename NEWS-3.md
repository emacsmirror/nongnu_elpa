# VM NEWS, releases 8.3.0 onwards

If you are upgrading from a previous version of VM, look through the entries
since that version to see how you might be affected.

Earlier releases are in NEWS-2.md, 8.0.0 through 8.2.0b1, and NEWS-1.md, 4.10
through 7.19.  This is the newest file, so new entries go at the front of it.

## VM 8.x.x released

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
