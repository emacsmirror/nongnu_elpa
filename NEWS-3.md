# VM NEWS, releases 8.3.0 onwards

If you are upgrading from a previous version of VM, look through the entries
since that version to see how you might be affected.

Earlier releases are in NEWS-2.md, 8.0.0 through 8.2.0b1, and NEWS-1.md, 4.10
through 7.19.  This is the newest file, so new entries go at the front of it.

## VM 9.0.0 released 2026-10-18

Three changes a reader meets whatever they do: IMAP and POP no longer block
Emacs, XEmacs support is gone, and `vm-pgg` is replaced by `vm-epg`.

### Upgrading from 8.3.2

What stops working.  Each is covered below.

  * `(require 'vm-pgg)` fails.  Write `(require 'vm-epg)`; the settings do
    not carry over.
  * An init file that reads a removed option as it sets it, `(setq X (nconc
    X ...))` or `(add-to-list 'X ...)`, stops with "Symbol's value as
    variable is void".  Twelve options are gone, and
    `vm-mime-default-face-charsets` with them.
  * Thirty-four compatibility aliases and twelve further obsolete names are
    gone.
  * A renamed name still works and now reports the name to use.  Every
    `vmpc-` name is `vm-pcrisis-`.
  * Personality Crisis, vm-biff, message history, postpone and serial must be
    switched on: `(vm-pcrisis-mode 1)` and so on.  Loading the file no longer
    does it.
  * `vm-rfaddons.el` and `vm-enable-addons` are gone, so `.`, `T` and
    `C-c C-a` are VM's own keys again.
  * `vm-v7-key-bindings` and `vm-legacy-key-bindings` are gone.
  * `vm-mime-text/html-handler` set to `emacs-w3` has to change.
  * `%n` in a `vm-summary-format` of your own should become `%3n` to keep the
    column it had.
  * A POP server that does not offer UIDL no longer works.
  * Nine internal IMAP and POP functions that carried autoload cookies are
    gone.
  * XEmacs is not supported.

### IMAP and POP

  * **IMAP and POP are asynchronous and do not block Emacs**
    (emacs-vm/vm#473).  Fetching mail, loading a body, sending flag changes,
    expunging, saving, quitting, synchronising, filing an `FCC:` copy and
    listing mailboxes all happen while you read, one session to a folder, and
    the mode line says what the folder is doing in `vm-net-session-face`.  Two
    things still wait and say so: a message body a save or a copy must have in
    hand, and folder-name completion.  `C-g` works in both.

  * **The blocking IMAP and POP implementation is gone** (emacs-vm/vm#822),
    137 functions and about 4000 lines.  Nine internal functions that carried
    autoload cookies went with it, `vm-imap-make-session` and
    `vm-pop-move-mail` among them; use the commands `vm-get-new-mail`,
    `vm-imap-synchronize` and `vm-save-folder` instead.

  * **CRAM-MD5 no longer costs you the asynchronous driver**
    (emacs-vm/vm#822).  Such a maildrop was refused by the driver and served
    by the blocking implementation, so every fetch from it held Emacs still.

  * **IMAP CRAM-MD5 works with a password that is not plain ASCII, and with
    one over 64 characters** (emacs-vm/vm#772).  HMAC is defined over octets
    and VM XORed character codes, so an accented password was reported as
    incorrect; an over-long key is hashed first now, as RFC 2104 says.

  * **A POP maildrop asking for `apop` no longer sends its password in clear**
    (emacs-vm/vm#823).  The driver read every field of the maildrop except the
    authentication method.  A server offering no APOP timestamp is an error
    rather than a fall back to `PASS`.

  * **`rpop` is no longer a POP authentication method** (emacs-vm/vm#822).
    Essentially no server offers RFC 1081's trusted-host scheme.  A maildrop
    asking for it now says what to write instead: `pass`, or `apop` where the
    server offers a timestamp.

  * **A POP server with no UIDL no longer works** (emacs-vm/vm#822).  UIDL is
    what tells one message from another between sessions.  VM says so and
    fetches nothing, rather than fetching the same mail twice or deleting what
    you meant to keep.

  * **A POP message over `vm-pop-max-message-size` is left on the server, and
    VM says so** (emacs-vm/vm#822), naming its size and the limit.  It used to
    be passed over in silence.

  * **An IMAP message over `vm-imap-max-message-size` is left on the server,
    and VM says so** (emacs-vm/vm#822).  It used to ask message by message,
    which nothing can do from inside a process filter.  Raise the limit and
    the next `vm-get-new-mail` brings it in.  An IMAP folder is unchanged.

  * **A POP body line beginning with a dot arrived without it**
    (emacs-vm/vm#822): `.hidden` came out as `hidden`, the stuffed dot having
    been taken off twice.

  * **Mail fetched from an IMAP maildrop into a babyl folder** lost the babyl
    header, so the crash box read back as no folder type at all
    (emacs-vm/vm#822).

  * **Visiting a folder says that it is getting new mail** (emacs-vm/vm#825),
    where it used to report the cached totals.  The fetch is asynchronous, so
    at that moment nothing has arrived yet.

  * **A bug report carries the trace of the session still running**
    (emacs-vm/vm#822).  It used to end the folder's session to collect the
    trace, which would now abort a fetch in flight.

  * **New option `vm-session-trace-max-size`** (emacs-vm/vm#822), the most of
    one session trace a bug report carries, 100000 characters by default.  A
    large synchronisation leaves most of a megabyte behind it and a report
    that size cannot be sent.

  * **`C-u C-u g` fetches every message an IMAP mailbox has and the folder has
    not** (emacs-vm/vm#751), including those recorded as retrieved already.
    That is how to refill a cache folder that lost messages some other way.

  * **`C-u M-x vm-imap-synchronize` no longer deletes mail on the server**
    (emacs-vm/vm#752).  It used to delete every message the mailbox had and
    the cache did not, with no confirmation: a cache truncated or restored
    from a partial backup took the whole mailbox with it.  The expunges you
    make are sent either way.

  * **IMAP folders are quicker to read**: several message bodies are fetched
    in one command (emacs-vm/vm#185).

  * **An IMAP or POP cache VM creates is named `imap-cache-<md5>.mboxcl2`**
    and written in that type.  A cache that already exists keeps its name and
    is read as whatever it is.

  * **VM no longer asks a server to clear `\Recent`** (emacs-vm/vm#389), which
    RFC 3501 forbids and some servers answered with an error.

### Labels

  * **A message saved into an IMAP mailbox keeps its labels**
    (emacs-vm/vm#828).  The `APPEND` used to carry `\Answered` and `\Seen` and
    nothing else.  What the destination will keep is asked in its
    PERMANENTFLAGS first, and a keyword it does not name is left out rather
    than risking a refusal of the whole `APPEND`.

  * **VM says when an IMAP server will not keep a label** (emacs-vm/vm#601).
    A server whose PERMANENTFLAGS lacks `\*` keeps no keywords, Gmail among
    them, and answers OK all the same: the label was simply gone the next time
    the mailbox was read.  Said once per folder, and only where a label is
    being sent.

  * **Labels reach the server more reliably.**  A flag the server refuses no
    longer stops the others being stored (emacs-vm/vm#391), a refused change
    is no longer overwritten by the server's stale copy (emacs-vm/vm#270), and
    a copy saved to another folder says when it carries the server's flags
    rather than yours (emacs-vm/vm#38).

  * **Labels on an arriving message join the folder's list**, so they turn up
    in completion; `vm-expunge-label`, `vm-list-unused-labels`,
    `vm-expunge-unused-labels` and `vm-sync-labels` sort out a list that has
    gone astray (emacs-vm/vm#269).

### Sending mail

  * **An address header is encoded only where RFC 2047 allows it**
    (emacs-vm/vm#909).  `To: "Göran Uddeborg" <goeran@uddeborg.se>` could not
    be sent at all.  Section 5 names the only three places an encoded word may
    stand and VM keeps to them: a quoted display name is encoded whole, a
    comment keeps its parentheses, and an address is not touched, one outside
    US-ASCII being refused rather than encoded.  Reserved characters go out as
    `=XX`, a word covers a whole run between whitespace, and a header carrying
    one folds at 76 characters.

  * **A long header is folded, and a long encoded word is split**
    (emacs-vm/vm#794).  VM wrote every header on one line however long: thirty
    accented words made a line of 325, four hundred plain words one of 2008.
    Both are now 75 or less.  The charset and the encoding VM chooses are
    unchanged.

  * **A line too long to send as it stands goes out quoted-printable**
    (emacs-vm/vm#593), so it arrives as the one line you wrote instead of the
    whole message being base64.

  * **A Subject with an accent in it is encoded rather than sent raw** when
    `vm-send-using-mime` is off (emacs-vm/vm#606).

  * **VM asks before sending a message with a `Bcc` header**, unless
    `send-mail-function` removes that header itself (emacs-vm/vm#815).
    `smtpmail-send-it` does; `sendmail-send-it` cannot, since it passes `-t`
    and those addresses are how the transport learns whom to deliver to, so it
    trusts the program behind `sendmail-program` to remove it.  Set
    `vm-check-bcc-removal` to nil to stop it asking.

  * **A send that has stopped can be interrupted with `C-g`**
    (emacs-vm/vm#842).  A sender running a program waited in
    `call-process-region`, which stops the whole of Emacs, so the `C-g` was
    never seen.  Interrupting unsends nothing: look at what arrived before
    sending again.

  * **A composition carries a `From` header** (emacs-vm/vm#832).  VM read
    `mail-setup-with-from` nowhere, so the copy an `Fcc` or `IMAP-FCC` files,
    which is written before the send, named no sender at all.  Set
    `mail-setup-with-from` to nil for the old behaviour.

  * **Sent copies work with IMAP.**  An `FCC:` naming an IMAP maildrop files
    the copy on the server rather than writing a file named after the maildrop
    (emacs-vm/vm#605), and VM files every copy itself in the format of the
    folder it goes to (emacs-vm/vm#597).  An `IMAP-FCC:` header is filed the
    same way, with nothing to add to `mail-send-hook` (emacs-vm/vm#68).

  * **`vm-mime-7bit-composition-charset` is removed** (emacs-vm/vm#697).  It
    was consulted nowhere.  Emacs knows which characters are in the buffer;
    `vm-coding-system-priorities` puts your own order on the answer.

  * **Mail from a sender who wrapped their own text is re-wrapped to your
    window**, and VM can mark its own wrapping the same way, per RFC 3676
    format=flowed (emacs-vm/vm#78).

  * **An attachment can be sent under a name other than the file it came
    from**, with `vm-mime-rename-attachment` (emacs-vm/vm#392).

  * **A whole directory can be attached at once**, with
    `vm-attach-files-in-directory`.

  * **MIME parameters with international characters** are understood and
    generated per RFC 2231 (emacs-vm/vm#367).

  * **Word wrapping no longer needs the obsolete `longlines` library**
    (emacs-vm/vm#817).  VM wraps in fourteen lines of its own.  Two things
    change for anyone who had it on: the column is `vm-paragraph-fill-column`,
    or `vm-fill-long-lines-in-reply-column` in a reply, as both have always
    documented; and a wrapped line no longer ends in a space, which under RFC
    3676 was a soft line break nobody intended.

### Composing and drafts

  * **Killing a composition you have written in keeps it as a draft**
    (emacs-vm/vm#824), in `vm-save-killed-messages-folder`, with VM saying
    where it went and which key takes it up again.  `kill-buffer` used to take
    an unsent message with no warning.  `vm-save-killed-message` now defaults
    to `always`: set it to `ask` to be asked each time, or to nil to keep no
    drafts, which leaves `vm-confirm-killing-a-composition` to ask before the
    writing is lost.

  * **Postponing no longer asks whether to kill the composition.**  The guard
    added for emacs-vm/vm#824 could not tell that postponing had just filed
    the draft.

  * **New `vm-after-postpone-message-hook`**, run in the composition buffer
    once the draft is filed and the source message dealt with, where
    `vm-postpone-message-hook` runs before the composition is written.

  * **Declining the offer of the drafts folder starts a new message**
    (emacs-vm/vm#755), where `vm-continue-what-message` used to do nothing at
    all.  The drafts are untouched and still there to continue.

  * **An unfinished composition is offered as a draft when you leave Emacs**,
    rather than left as a file to find later (emacs-vm/vm#160).

### Folders and folder types

  * **VM reads and writes mboxcl2** (emacs-vm/vm#466), the mbox variant that
    keeps a `Content-Length` header and stores a message exactly as it
    arrived.  The type is now called `mboxcl2` rather than
    `From_-with-Content-Length`, and the option `vm-trust-content-length`; the
    old names still work.

  * **A folder's name can say what format it is.**  A name ending `.mboxcl2`
    is created and read as one, so such a folder with no `Content-Length`
    headers is refused, with how to convert it, instead of being read as From_
    in silence (emacs-vm/vm#610, emacs-vm/vm#620).  The option is
    `vm-folder-type-by-extension-alist`, which matches the extension literally
    and replaces `vm-folder-type-by-name-alist` (emacs-vm/vm#741).  `.mbox` is
    From_ (emacs-vm/vm#750), and From_ is never put into a name that has not
    got it.

  * **An mboxcl2 folder is kept sound** (emacs-vm/vm#612).  Every message VM
    writes into one carries a `Content-Length`, and one that has none is
    refused rather than guessed at; `vm-mboxcl2-strict` nil opens such a
    folder so that `vm-change-folder-type` can repair it.

  * **`vm-change-folder-type` with a prefix argument converts a folder on
    disk**, without visiting it, and keeps the old one in a backup
    (emacs-vm/vm#613).  That is how to repair a folder VM will not read.

  * **Two prefix arguments convert into a file you name**, leaving the folder
    you converted exactly as it was (emacs-vm/vm#763).  A name that cannot
    hold the type is refused, with what to call it instead.

  * **`vm-check-folder` says what the folder is and whether it is sound**, and
    writes nothing: the type VM reads it as, what its name and its contents
    say, the message count against what walking the separators finds, and for
    an mboxcl2 folder whether every `Content-Length` matches its body.  A
    prefix argument checks a folder file without visiting it.

  * **`vm-default-folder-type` is `From_` on every platform.**  It was mboxcl2
    on Solaris, AIX and System V, and it decides only folders that do not
    exist yet.

  * **VM no longer sniffs the first message to choose between From_ and
    mboxcl2**, so `vm-trust-content-length` is deprecated (emacs-vm/vm#736).
    Name the type instead, by the folder's extension or with
    `vm-default-folder-type`.

  * **`vm-default-folder-type` no longer offers `BellFrom_`**
    (emacs-vm/vm#787), and neither does `vm-change-folder-type`.  That format
    has no signature of its own, so a folder VM wrote as one was read back as
    From_ with each message swallowing the headers of the next.  Reading one
    is unchanged.

  * **mboxcl2 names what it creates** (emacs-vm/vm#766, emacs-vm/vm#767):
    saving to a folder that does not exist, an `FCC:` to one and a postponed
    draft all create `NAME.mboxcl2`, since a folder's type is read back from
    its name.

  * **New command `vm-convert-caches-to-mboxcl2`** (emacs-vm/vm#768) converts
    the POP and IMAP caches VM wrote before it named caches for their type.
    Those are read as From_, where a body line beginning `From ` can split the
    cache in two.  It asks once, keeps a backup and refetches nothing.
    Converting a folder on disk now refuses one you are reading rather than
    killing its buffer (emacs-vm/vm#770).

  * **`M-x vm-backup-folder` keeps a copy of a folder as it is on disk**
    (emacs-vm/vm#843).  Emacs backs a file up on the first save of its buffer
    and not again, so a folder saved earlier in the session has no copy of
    what is on disk now.

  * **A folder visited through a symbolic link is saved through the link**
    (emacs-vm/vm#532), rather than replacing it with a file.

  * **Killing a folder buffer takes its virtual folders with it**, and asks
    first if any have unsaved changes (emacs-vm/vm#573).

  * **Opening a folder that has another name says so** (emacs-vm/vm#185),
    since saving writes only one of them.

  * **Gathering mail takes a file of another type, and a single message**
    (emacs-vm/vm#908).  `C-u M-x vm-get-new-mail` used to refuse a file whose
    folder type differed from the folder's own, whatever
    `vm-convert-folder-types` said; it converts now.  A `.eml` file is wrapped
    in the folder's separators and gathered as one message.

  * **A count larger than the messages left acts on the ones that are there**
    (emacs-vm/vm#550), instead of refusing.

### Reading and display

  * **HTML messages display on a stock Emacs: `shr` is a handler now.**
    `vm-mime-text/html-handler` offered only `emacs-w3m`, `w3m` and `lynx`,
    each of which needs something installed.  `shr` is what `auto-select`
    falls back to, and it fetches no image while rendering: the new
    `vm-mime-shr-inhibit-images` is on by default, a remote image being how a
    sender learns that the message was opened.

  * **Emacs/W3 is no longer one of the HTML viewers** (emacs-vm/vm#707), the
    browser having been dropped from Emacs.  If your init file sets
    `vm-mime-text/html-handler` to `emacs-w3`, set it to `emacs-w3m`, `w3m`,
    `lynx` or nil.  emacs-w3m is a different package and is unaffected.

  * **`vm-mime-text/html-blocker` and `vm-mime-text/html-blocker-exceptions`
    are obsolete** (emacs-vm/vm#845).  Both were documented as stopping an
    HTML part loading a sender's remote image and neither ever did: the loop
    tested `(or t ...)`.  What does block one is `vm-w3m-safe-url-regexp`,
    which by default matches `cid:` and nothing else.

  * **HTML quoted in a reply is broken at 80 columns**, whatever window the
    message was read in (emacs-vm/vm#369).  emacs-w3m used the window width
    and lynx 72, so the same message quoted differently from one day to the
    next.  Set `vm-html-in-reply-column` to `window-width` for what VM did
    before.

  * **Emacs retrieves `message/external-body` URLs; wget, curl and fetch are
    gone.**  `url-retrieve-synchronously` speaks http, https, ftp and file, so
    nothing need be installed.  `vm-url-retrieval-methods` remains as the
    permission switch, nil still meaning never retrieve, and the new
    `vm-url-retrieval-timeout` bounds the wait.

  * **VM's own browser handlers are gone; `browse-url` chooses the browser.**
    The Netscape, Mosaic, Opera, Mozilla, Konqueror and Firefox senders, their
    `-new-window` variants and eighteen `vm-*-program` options are removed,
    none of them having worked for years.  `vm-url-browser` still defaults to
    `browse-url`, so the browser is `browse-url-browser-function`, the setting
    every other Emacs package follows.

  * **VM colours quoted text and the signature in a message body**
    (emacs-vm/vm#811).  Set `vm-enable-body-faces`: quoted text wears a face
    per level of quoting from `vm-citation-faces`, and the signature wears
    `vm-signature-face`.  Off by default.

  * **Long headers can be folded to one line**, with a widget to unfold them:
    `vm-enable-shrunken-headers`.

  * **A meeting invitation is shown as what it says**, rather than as a
    `text/calendar` attachment (emacs-vm/vm#85).

  * **`vm-mime-charset-converter-alist` works, seventeen years after it
    stopped** (emacs-vm/vm#879).  Both places that would run a converter asked
    first whether VM could display the charset, which has answered yes for
    everything since 2009.  A converter now runs where Emacs has no coding
    system for the charset, `windows-874`, `x-mac-roman` and `unknown-8bit`
    being three that arrive in mail.  `vm-mime-default-face-charsets` and
    `vm-mime-default-face-charset-exceptions` are removed with it, so an init
    file that extends either must drop those lines (emacs-vm/vm#883).

  * **`t` keeps your place in a long message** instead of jumping to the top
    (emacs-vm/vm#513).

  * **ImageMagick 7 is supported**, which renamed `convert` to a subcommand of
    `magick`.

### Summary, sorting and keys

  * **`%n` is the message number and the width does the padding**
    (emacs-vm/vm#861).  VM padded to three columns itself, so `%-3n` did
    nothing and `%05n` answered `00  1`.  The default is `%3n` now; write that
    in a format of your own to keep the column you had.

  * **A field width and a maximum work as `printf` does** (emacs-vm/vm#848),
    in `vm-summary-format` and in `vm-mime-button-format-alist`.  `%20.4s`
    answered four spaces and is now four columns of the subject in a column
    twenty wide.  Every format VM ships is unaffected.

  * **`%%` is a single `%`** (emacs-vm/vm#847) in a summary or MIME button
    format, and a specifier VM does not know is left as it stands rather than
    breaking every line.

  * **`%r` and `%R` work in `vm-summary-format`** (emacs-vm/vm#846).  Both
    were documented as the recipients of the message and both came out as
    literal text.

  * **`G` sorts by `reversed-addressees`** (emacs-vm/vm#850), which it offered
    in completion and then refused.  The manual's table of sort keys now says
    what each key sorts by.

  * **The keys VM 8 gives its own commands are bound** (emacs-vm/vm#632): `!`
    flags a message, `<` and `>` promote and demote a subthread, and `V O`,
    `V U`, `V D` and `V ?` run their virtual folder commands.  Each answered
    an error before.  `vm-v7-key-bindings`, its alias `vm-legacy-key-bindings`
    and the stub are removed; the VM 7 bindings they installed are in this
    file's history, to paste into an init file.

### Personality Crisis

  * **Personality Crisis is named after VM** (emacs-vm/vm#657): every `vmpc-`
    symbol is now `vm-pcrisis-`, so the feature turns up when you complete
    `M-x vm-`, run `C-h a vm-` or look through the `vm` customize tree.  An
    old name is not an alias; it reports the name to use.
    `~/.vmpc-auto-profiles` and the `vmpc-profile` field written into BBDB
    records keep their names, being a file and a field rather than symbols.

  * **`M-x vm-pcrisis-check-configuration` says what is wrong with your rules
    and what to do about each** (emacs-vm/vm#806).  Rules are data, so a
    mistake in them is not a Lisp error and nothing stops: a rule keyed on a
    condition that is not defined never runs and can sit in an init file for
    years.  The same check runs as each composition begins.

  * **Personality Crisis acts on the signature Emacs inserted**
    (emacs-vm/vm#540).  `vm-pcrisis-expect-default-signature` defaults to on,
    so `(vm-pcrisis-signature "")` deletes the signature VM put in from
    `mail-signature` and one naming a file replaces it rather than adding a
    second.  Set it to nil to keep a signature action away from a signature
    Personality Crisis did not insert.

  * **vm-pcrisis and vm-pine are part of VM** rather than add-ons to install.
    vm-pine is now called vm-postpone.

### PGP

  * **`vm-pgg` is removed and `vm-epg` replaces it** (emacs-vm/vm#568,
    emacs-vm/vm#581).  Both halves happen in this release, so an 8.3.2 reader
    loses the package they were using and gains one that is new to them.  A
    configuration with `(require 'vm-pgg)` will not start: change it to
    `(require 'vm-epg)`.  vm-epg reads and writes PGP/MIME and inline armour,
    signs, encrypts and attaches a public key, over the `epg` interface Emacs
    ships rather than the obsolete `pgg`.  The settings do not carry over, so
    `M-x customize-group RET vm-epg RET` is worth a visit; each command keeps
    its name bar the prefix.

  * **`vm-epg` encrypts a message to its recipients and to nobody else**
    (emacs-vm/vm#782).  `vm-pgg` always added your own key, so an `FCC:` copy
    could be read back; under vm-epg it cannot.  To encrypt to yourself as
    well, say so to GnuPG with `encrypt-to YOUR-KEY-ID` in
    `~/.gnupg/gpg.conf`, which VM honours.

  * **A copy of `vm-pgg.el` of your own no longer takes PGP away from
    `vm-epg`** (emacs-vm/vm#785).  Both answer for the same three MIME types
    and whichever loaded last used to hold them; loading vm-pgg with vm-epg
    present now installs nothing at all.

### Options, diagnostics and hooks

  * **`M-x vm-check-configuration` says what is missing from a VM setup and
    what to set for each** (emacs-vm/vm#816): which mail agent Emacs uses, the
    address mail goes out from, how it is sent, where folders are kept and
    where new mail comes from.  It never runs by itself; VM says once per
    session that the command would have something to report, and
    `vm-suggest-checking-configuration` nil stops even that.

  * **That line waits to be read** (emacs-vm/vm#844).  It was said in the
    middle of the startup, where the folder totals overwrote it.  It is said
    once Emacs is idle now, stays until you do something, and says what it
    means: "VM has 2 settings missing.  M-x vm-check-configuration says which,
    and what to set".

  * **`vm-check-configuration` checks the `From` header VM writes**
    (emacs-vm/vm#832).  The value of `vm-mail-header-from` goes into a
    composition verbatim, so a value that is not an address is a header nobody
    can reply to: a reader who reached for it rather than
    `mail-default-headers` sent every message as `From: IMAP-FCC: Sent`.

  * **A name VM has renamed reports the name to use** (emacs-vm/vm#901).
    Seventy old names were aliases, and an alias said nothing:
    `define-obsolete-variable-alias` warns only where the file naming the old
    name is byte-compiled, and nobody byte-compiles `~/.vm`.  Setting one now
    reports what to write instead, and so does naming one as a Personality
    Crisis condition or action.  The lists are `vm-renamed-variables` and
    `vm-renamed-functions` in `vm-renamed.el`.

  * **Twelve user options are gone, eleven of which no code read**
    (emacs-vm/vm#880).  Four bounded what one fetch brings in,
    `vm-imap-messages-per-session` and its three siblings, which a fetch that
    does not block Emacs has no reason to stop part way.  Gone with them:
    `vm-imap-connection-mode`, `vm-imap-sync-on-get`,
    `vm-imap-ensure-active-sessions`, `vm-imap-expunge-retries`,
    `vm-pop-read-quit-response`, `vm-mime-encode-words-regexp`,
    `vm-forwarded-message-preamble-format` and
    `vm-remember-passwords-insecurely`.  An init file that reads one as it
    sets it stops with "Symbol's value as variable is void"
    (emacs-vm/vm#883): delete those lines.  `vm-pop-max-message-size` and
    `vm-imap-max-message-size` are unaffected.

  * **Thirty-four compatibility aliases and twelve further obsolete names are
    gone** (emacs-vm/vm#594), each marked obsolete for at least four release
    cycles; the ticket lists every name and its replacement.  Two to watch:
    the sixteen `vm-summary-*-face` names lose the `-face` suffix, and
    `vm-mime-forward-local-external-bodies` set to t becomes
    `vm-mime-forward-saved-attachments` set to nil.

  * **Three misspelled option names were corrected**, and the misspellings are
    gone (emacs-vm/vm#589).

  * **`vm-sync-thunderbird-status` is an ordinary option**
    (emacs-vm/vm#589).

  * **VM says less as it works**: `vm-verbosity` defaults to 5, the level its
    own documentation calls normal.

  * **`vm-movemail-program` defaults to the movemail Emacs came with**
    (emacs-vm/vm#566).

  * **`vm-drop-buffer-name-chars` drops control characters and `/`** rather
    than everything outside US-ASCII, so a folder name in your own language
    keeps its letters.

  * **New `vm-startup-hook`**, run once when VM starts (emacs-vm/vm#565).

  * **New `vm-folder-cache-file`**, the cache file of the folder you are in.

  * **`vm-save-message-hook` is a variable at last.**  `vm-save-message` has
    run it for years, but it was declared nowhere, so it could not be
    customized.

  * **File name prompts offer the history VM keeps for them**
    (emacs-vm/vm#587).

  * **Everything VM says is kept, timed, in the buffer `*VM Log*`**, each line
    giving the real and CPU time since the line before it.  `M-x vm-show-log`
    shows it, `vm-log-level` records up to a level on `vm-verbosity`'s scale
    without changing what reaches the echo area, and `vm-log-max-lines` bounds
    the buffer.

  * **Incoming mail can be filed and labelled by a table of virtual folder
    selectors**, `vm-virtual-filter-alist` (emacs-vm/vm#542).

### Loading and modes

  * **Loading `vm-message-history.el`, `vm-postpone.el` or `vm-serial.el` no
    longer switches it on** (emacs-vm/vm#788).  Each has an autoloaded mode
    instead, so no `require` is needed: `(vm-message-history-mode 1)`,
    `(vm-postpone-mode 1)`, `(vm-serial-mode 1)`.  Customize loads any file
    declaring a group under `vm-ext`, so `C-h v` on a VM variable used to
    install the hooks, keys and advice of all three.  `C-c C-d` and
    `vm-continue-postponed-message` are unaffected.

  * **Personality Crisis and vm-biff must be switched on**:
    `(vm-pcrisis-mode 1)` (emacs-vm/vm#561) or `(vm-biff-mode 1)`
    (emacs-vm/vm#512).  Without the first, compositions are set up with none
    of your rules; without the second there is no new-mail notification.  An
    argument of -1 switches either off, which a feature installed by loading a
    file could never be.

### Removed

  * **XEmacs is no longer supported** (emacs-vm/vm#708), it having had no
    release since 2013.  The alternative implementations of menus, toolbars,
    extents and mouse handling are gone, with `--with-emacs=xemacs` and the
    `xemacs-package` make target.  `vm-toolbar-orientation`, `vm-toolbar`,
    `vm-use-lucid-highlighting` and `user-home-directory` are removed, each
    having meant nothing in Emacs.  Two things that never worked here now do:
    `vm-serial-set-token` reads with `read-minibuffer`, and an `audio/basic`
    part is no longer offered for internal display.

  * **vm-rfaddons.el is gone, and with it `vm-enable-addons`**
    (emacs-vm/vm#606).  What was worth keeping is part of VM with an ordinary
    option each: shrunken headers, the composition checks, saving attachments
    as mail arrives, answering a return receipt and a Date header of your own.
    The `.`, `T` and `C-c C-a` rebindings went with the rest, so those keys
    are VM's own again.

  * **The contrib directory is gone** (emacs-vm/vm#812), and one of its files
    is part of VM as `vm-org.el`: `require` it and a `vm:` link in an Org file
    names a folder and a message, so `C-c C-o` opens it and `C-c l` makes such
    a link.  Nothing in contrib was installed, built, documented or tested.
    `vm-sumurg.el`, `org-html-mail.el`, `vm-blueman.el`, `vm-bogofilter.el`
    and `vm-mime-display-internal-application.el` are dropped and in the
    history.

  * **`u-vm-color.el` is gone** (emacs-vm/vm#811).  Half of it was superseded
    in 2010 and it was never documented in the manual; what it did to a
    message body is what `vm-enable-body-faces` now does.  The file is in the
    history if it is wanted back.

  * **The macOS drag-and-drop attach command `vm-ns-attach-file` is gone**
    (emacs-vm/vm#531); dropping a file into a composition was never bound to
    it in the way its own comment described.

### The manual, other packages and developers

  * **The manual has a Setting Up chapter** (emacs-vm/vm#790): a configuration
    built one piece at a time, from making VM the mail agent Emacs uses
    through local and server folders, the summary, viewing, composing,
    sending, the address book and keys of your own.  `example.vm` is the same
    settings in one file, and it now loads: it did not, so every setting after
    the second was silently never made.

  * **The manual has an appendix listing every command and user option**,
    taken from the code so it cannot fall behind (emacs-vm/vm#586).

  * **VM works with BBDB again** (emacs-vm/vm#567); its integration had been
    written for BBDB 2.x.

  * **For anyone writing against VM**: message reverse links moved out of the
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
