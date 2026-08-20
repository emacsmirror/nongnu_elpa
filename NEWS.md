# VM NEWS

If you are upgrading from a previous version of VM, look through the entries
since that version to see how you might be affected.

## IMAP and POP no longer stop Emacs

Fetching mail, loading a message body, sending flag changes, expunging,
saving, quitting, synchronising, filing a copy of what you send, making and
listing mailboxes: all of it happens while you carry on reading.  Emacs is not
held while VM talks to a server.

The mode line of the folder, its summary and its presentation says what the
folder is doing, how far it has got and how much is waiting for it:

    VM: inbox   3 (of 412)   fetching 24/340 +1 

It says what is happening rather than which protocol is doing it, in the face
`vm-net-session-face` -- give that face a background if you want it louder.
Quitting a folder stops what it was doing rather than leaving a session writing
into a buffer that is going away.

The folder is not locked while this goes on: read it, move about it, delete,
mark, label and expunge as usual.  Work that needs the server is done when the
session running now has finished -- a folder runs one session at a time, since
two writing into it would interleave two sets of messages, flags and expunges
in one buffer and one cache file.

Two things still wait, and say so: saving or copying a message whose body is
still on the server, which cannot be written without it, and completing a
folder name, which has to answer with the names it has.  `C-g` works in both.

New mail arrives a bunch at a time rather than all at the end, so a large
mailbox fills in while you read it.

`vm-imap-synchronize` is the exception worth knowing: it is the command whose
job is the expensive half, and it too now runs in the background.

## Two features must now be switched on

Loading a file no longer switches it on.  If your init file loads either of
these and you have not added the line below, the feature is doing nothing --
silently, since neither has anything to say when it is off.

  * **Personality Crisis**: `(vm-pcrisis-mode 1)`

    Without it, compositions are set up with none of your rules: no From
    address chosen for you, no signature, no headers.  `(require 'vm-pcrisis)`
    was how one switched it on in earlier releases, and now only loads the
    file; the mode is autoloaded, so the `require` is not needed at all
    (emacs-vm/vm#561).

    While you are there: every name now begins with `vm-pcrisis-` where it
    began with `vmpc-` (emacs-vm/vm#657).  Replacing that prefix is the whole
    change -- `vm-pcrisis-conditions`, `vm-pcrisis-actions`,
    `vm-pcrisis-reply-rules`, and the functions a rule names, such as
    `vm-pcrisis-signature`.  The old names still work, so this can wait.

    Two keep the old spelling, being files rather than symbols:
    `~/.vmpc-auto-profiles`, and the `vmpc-profile` field written into BBDB
    records.

  * **vm-biff**: `(vm-biff-mode 1)`

    Without it, no new-mail notification (emacs-vm/vm#512).

Both can be switched off again with an argument of -1, which is the reason
for the change: a feature installed by loading a file could never be
switched off.

## Names that were obsolete are gone

Thirty-four compatibility aliases have been removed (emacs-vm/vm#594).  Each
had been marked obsolete for at least four release cycles, and each printed a
warning when a byte-compiled configuration used it.  If your init file sets one
of these, it now sets a variable nothing reads, or calls a function that does
not exist:

  * the sixteen `vm-summary-*-face` names, plus `vm-summary-filed` and
    `vm-summary-written` -- use the face names without the suffix,
    `vm-summary-deleted` and so on
  * `vm-mime-save-all-attachments-types`, `vm-mime-delete-all-attachments-types`
    and their `-exceptions` -- use `vm-mime-saveable-types`,
    `vm-mime-deletable-types` and theirs
  * `vm-flag-message-read`, `vm-flag-message-unread`,
    `vm-imap-folder-check-for-mail`, `vm-pop-folder-check-for-mail`,
    `vm-decode-postponed-mime-message`, `vm-decode-postponed-mime-button`,
    `vm-mime-attach-object-from-message`, `vm-mime-nuke-alternative-text/html`,
    `vm-sort-threads-by-youngest-date`
  * `vm-mime-alternative-select-method`,
    `vm-mime-attachment-infer-type-for-text-attachments`,
    `vm-w3m-use-w3m-minor-mode-map`, `vm-mime-uuencode-decoder-program` and
    `-switches`

`C-h v` or `C-h f` on the old name will say it is void; the replacement for
each is in the list above.

Twelve more went with them, having been marked obsolete just as long but
needing a little more than a deleted line:

  * `vm-imap-server-list`, `vm-load-headers-only`, `vm-mime-show-alternatives`
    and `vm-summary-faces-mode` (the variable; the command of that name stays)
    were variables nothing read -- setting one had done nothing for years
  * `vm-mime-yank-attachments` was an alias of `vm-include-mime-attachments`
  * `vm-mime-save-all-attachments` and `vm-mime-delete-all-attachments` were
    aliases of `vm-save-all-attachments` and `vm-delete-all-attachments`
  * `vm-run-message-hook` and `vm-run-message-hook-with-args` took their
    arguments the other way round from `vm-run-hook-on-message` and its
    `-with-args`
  * `vm-mime-fsfemacs-encode-composition` and
    `vm-mime-fsfemacs-encode-text-part` were older copies of
    `vm-mime-encode-composition-internal` and `vm-mime-encode-text-part`,
    called from nowhere
  * `vm-mime-forward-local-external-bodies` decided the default of
    `vm-mime-forward-saved-attachments`, which is now simply `t`.  If you set
    the old one to t, set `vm-mime-forward-saved-attachments` to nil instead:
    it says the same thing the other way up.

## Emacs/W3 is no longer one of the HTML viewers

The W3 browser was dropped from Emacs and is in no package archive, so nothing
could supply it (emacs-vm/vm#707).  If your init file sets
`vm-mime-text/html-handler` to `emacs-w3`, set it to `emacs-w3m`, `w3m`, `lynx`
or nil; the default `auto-select` no longer considers it.  `vm-url-browser`
values `w3-fetch` and `w3-fetch-other-frame` are gone the same way, as is the
`url-w3` entry in `vm-url-retrieval-methods`, which no code ever implemented.

emacs-w3m is a different package and is unaffected.

## A log of what VM did, and how long it took

Everything VM says is kept in the buffer `*VM Log*`, timed.  `M-x vm-show-log`
shows it; nothing has to be turned on.  Each line says when, and how long
since the line before it:

```
14:03:12.481 +2.140s +0.310cpu [6] INBOX: Retrieving message 400 (of 100000)...
```

An interval that is all real time and no CPU went on the network.

`vm-log-level` is for the messages that are *not* shown: it takes a level on
`vm-verbosity`'s scale and records everything up to it, so `(setq
vm-log-level 10)` keeps the detail of a slow operation without changing what
appears in the echo area.  `vm-log-max-lines` bounds the buffer, oldest lines
first.

## A folder says what type it is, and VM no longer guesses

From_ and mboxcl2 are the same folder but for a `Content-Length` header on each
message, so a folder cannot say which it is by looking like one.  VM used to
decide by sniffing the first message, which read a maintainer's 1.1 GB IMAP
cache as mboxcl2 while 6394 of its 6459 messages had no length.  The type is
now something a folder is told, in one of two places.

  * `vm-folder-type-by-name-alist` matches the **whole** file name, where it
    matched only the last part of it.  One rule can then answer for a
    directory, which is what a primary inbox called INBOX needs, having no
    suffix to match and no way to be renamed:

    ```elisp
    (setq vm-folder-type-by-name-alist
          '(("\\.mboxcl2\\'"  . mboxcl2)
            ("/mail/current/" . mboxcl2)))
    ```

    A suffix rule keeps working unchanged.  A rule anchored at the front with
    `` \` `` has to allow for the directories now, or it matches nothing.

  * **An IMAP or POP cache VM creates is named `imap-cache-<md5>.mboxcl2`**
    and written in that type.  A cache is VM's own file and VM writes every
    message in it, so its type is known rather than guessed at, and the
    lengths make the boundaries exact.  A cache that already exists keeps its
    name and is read as whatever it is; nothing is converted and nothing is
    refetched.

`vm-default-folder-type` is `From_` on every platform now.  It was mboxcl2 on
Solaris, AIX and System V, a guess about the local delivery agent, and it
decides only folders that do not exist yet.

`vm-trust-content-length` is **deprecated**.  It is what turned the sniffing
on, and setting `vm-default-folder-type` to mboxcl2 used to require it: a
statement about new folders was also a statement about every folder read.  Say
it in the name instead.  For this release the sniffing still happens where it
is switched on, and warns once per folder, naming the rule that would settle
the question.

## VM 8.x.x released

  * VM reads and writes mboxcl2, the mbox variant that keeps a
    `Content-Length` header and stores a message exactly as it arrived
    (emacs-vm/vm#466).  A fallout: the folder type is now called `mboxcl2`
    rather than `From_-with-Content-Length`, and `vm-trust-content-length`
    rather than `vm-trust-From_-with-Content-Length`.  The old names still
    work.

  * `vm-change-folder-type` with a prefix argument converts a folder on disk,
    without visiting it, and keeps the folder as it was in a backup file --
    as does changing a visited folder's type.  That is how to repair a folder
    VM will not read, since such a folder cannot be visited to convert it
    (emacs-vm/vm#613).

  * An mboxcl2 folder is kept sound.  Every message VM writes into one carries
    a `Content-Length`, which is how the end of a message is found there, and
    a message that has none is refused rather than guessed at;
    `vm-mboxcl2-strict` nil opens such a folder so that
    `vm-change-folder-type` can repair it (emacs-vm/vm#612).

  * A folder's name can say what format it is.  A folder whose name ends in
    `.mboxcl2` is created as one, rather than as whatever
    `vm-default-folder-type` says, and is read as one -- so a folder named
    that way which has no `Content-Length` headers is refused, with how to
    convert it, instead of being read as a From_ folder in silence
    (emacs-vm/vm#610, emacs-vm/vm#620).  `vm-folder-type-by-name-alist` is the
    option.

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

  * A long line survives the trip.  A line too long to send as it stands goes
    out quoted-printable, so it arrives as the one line you wrote instead of
    the whole message being BASE64 (emacs-vm/vm#593).

  * A Subject with an accent in it is no longer sent raw when
    `vm-send-using-mime` is off (emacs-vm/vm#606).

  * Mail from a sender who wrapped their own text is re-wrapped to your
    window, and VM can mark its own wrapping the same way, per RFC 3676
    format=flowed (emacs-vm/vm#78).

  * An attachment can be sent under a name other than the file it came from,
    with `vm-mime-rename-attachment` (emacs-vm/vm#392).

  * A whole directory can be attached at once, with
    `vm-attach-files-in-directory`.

  * An unfinished composition is offered as a draft when you leave Emacs,
    rather than left as a file to find later (emacs-vm/vm#160).

  * A meeting invitation is shown as what it says, rather than as a
    `text/calendar` attachment (emacs-vm/vm#85).

  * Headers that arrive as raw 8-bit bytes are readable, not octets
    (emacs-vm/vm#368, emacs-vm/vm#11), and MIME parameters with international
    characters are understood and generated per RFC 2231 (emacs-vm/vm#367).

  * An HTML part sent to an external browser takes its pictures and its
    character set with it, so it looks like the message rather than a page of
    broken images (emacs-vm/vm#506, emacs-vm/vm#387).

  * HTML quoted in a reply keeps its own line breaks (emacs-vm/vm#369).

  * `t` keeps your place in a long message instead of jumping to the top
    (emacs-vm/vm#513).

  * A count larger than the messages left acts on the ones that are there,
    instead of refusing (emacs-vm/vm#550).

  * Long headers can be folded to one line, with a widget to unfold them:
    `vm-enable-shrunken-headers`.

  * Acting on an attachment whose body is still on the server says so instead
    of doing nothing (emacs-vm/vm#386).

  * IMAP folders are quicker to read: several message bodies are fetched in
    one command (emacs-vm/vm#185).

  * Labels reach the server more reliably.  A flag the server refuses no
    longer stops the others being stored (emacs-vm/vm#391), a refused change
    is no longer overwritten by the server's stale copy (emacs-vm/vm#270), and
    a copy saved to another folder says so when it carries the server's flags
    rather than yours (emacs-vm/vm#38).

  * VM no longer asks a server to clear `\Recent`, which RFC 3501 forbids and
    some servers answered with an error (emacs-vm/vm#389).

  * Labels on an arriving message are added to the folder's list, so they turn
    up in completion; and `vm-expunge-label`, `vm-list-unused-labels`,
    `vm-expunge-unused-labels` and `vm-sync-labels` sort out a list that has
    gone astray (emacs-vm/vm#269).

  * Incoming mail can be filed and labelled by a table of virtual folder
    selectors, `vm-virtual-filter-alist` (emacs-vm/vm#542).

  * A folder visited through a symbolic link is saved through the link, rather
    than replacing it with a file (emacs-vm/vm#532).

  * Opening a folder that has another name says so, since saving writes only
    one of them (emacs-vm/vm#185).

  * Killing a folder buffer takes its virtual folders with it, and asks first
    if any have unsaved changes (emacs-vm/vm#573).

  * Thunderbird folders keep in step: a flag you turn off in VM is turned off
    in the file (emacs-vm/vm#602), and `vm-sync-thunderbird-status` is now an
    ordinary option (emacs-vm/vm#589).

  * The manual has an appendix listing every command and user option, taken
    from the code, so it cannot fall behind (emacs-vm/vm#586).

  * File name prompts offer the history VM keeps for them (emacs-vm/vm#587).

  * New PGP/MIME support, vm-epg, built on the epg interface that comes with
    Emacs.  vm-pgg still works but is deprecated (emacs-vm/vm#581,
    emacs-vm/vm#568).

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
    data.  `~/.vmpc-auto-profiles` keeps its name, being a file rather than
    a symbol.

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

  * ImageMagick 7 is supported, which renamed `convert` to a subcommand of
    `magick`.

  * The drag-and-drop attach command on macOS, `vm-ns-attach-file`, is gone;
    dropping a file into a composition was never bound to it in the way its
    own comment described (emacs-vm/vm#531).

  * Three misspelled option names were corrected, and the misspellings are
    gone (emacs-vm/vm#589).

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

## VM 8.2.0b1 

### CHANGES

* `C-x C-s' is now bound to `vm-save-folder', which is the right thing to
 do in any case.

* `vm-mail-check-recipients' removed from default `vm-enable-addons'
 because it does not handle MIME headers.  If you would like to use it,
 please add it to the variable in the vm-preferences-file.

* New variable `vm-virtual-default-directory' allows you to set the
 `default-directory' for virtual folder buffers.

* The default value of `vm-auto-folder-case-fold-search' is now `t',
 which is in line with the manual.

* VMPC (formerly called "VM-Pcrisis") is being integrated into VM.  See
 the section on "Add-ons" in the VM manual.
 Variables such as `vmpc-reply-alist' have been renamed to
 `vmpc-reply-rules'.  `vmpc-action-alist' has been renamed to
 `vmpc-default-rules'.  (The old names continue to work.)

* vm-avirtual.el add-on package has a new variable
 `vm-virtual-make-up-auto-folder-names' which controls whether virtual
 folder names are used as auto-folders.  (Default: t)

* Folders visited read-only will not be saved to disk even when VM makes
 internal changes to the folder buffers.  New variable
 `vm-preserve-read-only-folders-disk' can be customized to override this
 behavior.

* The %t and %T format specifiers in vm-summary-format listed only the
 "addressees" (listed in the "To" headers) instead of all recipients.
 New specifiers %r and %R can be used to list all recipients.

* New variable `vm-mime-forward-saved-attachments' controls whether saved
 attachments are included in forwarded messages.  (This replaces the
 former undocumented variable `vm-mime-forward-local-external-bodies'.
 That variable is now obsolete.)

* The default value of `vm-stunnel-wants-configuration-file' changed to
 `t' because stunnel version 4 is now the default, and it needs a
 configuration file.

* New command `vm-mail-from-folder' (bound to `m' in VM).  It is like
 `vm-mail', but depends on the current folder as its parent folder and
 the current message as its parent message.  Setting the variable
 `vm-mail-use-sender-address' to `t' causes the command to pick up the
 sender of the current message as the recipient of the new message.

* New variables `vm-use-presentation-minor-modes' and
 `vm-presentation-minor-modes' control the minor modes to be used with
 presentation text prepared by other tools such as emacs-w3m.  The
 old variable `vm-w3m-use-w3m-minor-mode-map' is obsolete.

* The variable `vm-mutable-frames' renamed to
 `vm-mutable-frame-configuration' to convey its sense better.  The old
 name still works.

* Invoking `vm-auto-archive-messages' requires an additional confirmation
 to guard against accidental invocation.  Set the variable
 `vm-confirm-for-auto-archive' to nil to turn this off.

* `vm-mark-matching-messages' renamed to `vm-mark-messages-by-selector'
 and all related commands renamed similarly.  The old names will
 continue to work.

IMPROVEMENTS

* New command: `vm-create-virtual-folder-same-recipient' (bound to V R)
 selects the messages with the same recipient as the current message.

* New variable: `vm-auto-expunge-postponed-folder' (default nil) expunges
 the postponed-folder whenever a postponed message is continued and sent.

* Prefix argument 0 applies the operation to all messages in a folder. 

* New commands `vm-create-search-folder-other-window' and
 `vm-create-search-folder-other-frame'.  Search folders can now have
 their own summary-formats that differ from those of the base folders.

* Added support for Secure MIME (S/MIME) signatures and encryption (still
 experimental at this stage).  See the info section on Secure MIME.  To
 enable the verification of S/MIME signatures, set the variable
 `vm-mime-verify-signatures' to t.  Thanks to Arik Mitschang for this
 contribution.

* Quitting a search folder using `vm-quit-no-change'
 prevents VM from changing the message pointer in the underlying folder.

* Enhanced the Summary display to show the user mentioned in the
 "Reply-To" header (called the "principal"), when both the sender and
 the recipient match `vm-summary-uninteresting-senders'.  The variable
 `vm-summary-principal-marker' controls the tag used for showing the
 principal (default "For: ").

* New variable `vm-mime-multipart/related-show-method' to force the
 display of MIME multipart/related attachments.

* VM-Pcrisis extended to handle `vm-mail-from-folder'.

* New variable `vm-subject-tag-prefix' allows the subject tags added by
 mailing lists to be ignored during sorting etc. (similar to
 `vm-subject-ignored-prefix').  Another variable
 `vm-subject-tag-prefix-exceptions' can specify exceptions to this.
 `vm-summary-strip-subject-tags' removes subject tags from summary lines
 as well.

* New variable `vm-save-using-auto-folders' (default t), can be used to
 turn off auto-folder suggestions during message save.

* New variable (actually added in 8.2.0a)
 `vm-fill-paragraphs-containing-long-lines-in-reply' which specifies the
 maximum line length beyond which filling should be done in composed
 messages (just before they are sent).

* Newly documented command `vm-fill-long-lines-in-reply' fills a message
 being composed, along with its included text.

* International character sets supported in IMAP mailbox names.

## VM 8.2.0b (2011-12-28)

### CHANGES

* New customization variable `vm-spam-score-headers' allows the
 extraction of spam scores.  (Replaces the former variable
 `vm-vs-spam-score-headers' used by vm-avirtual.el.) 

* The variable `vm-mime-alternative-select-method' renamed to
 `vm-mime-alternative-show-method' to make it clear that it only applies
 to the viewing of messages.  The new variable
 `vm-mime-alternative-yank-method' controls the selection of
 alternatives for citation in replies.

* `vm-submit-bug-report' now uses Emacs message-mode for composing the
 bug report (whereas it previously used mail-mode with VM-specific
 tweaks).  Please do C-h m to find the functions you might need.

* Terminology: Interactively created virtual folders are now called
 "Search Folders".  They have a stronger connection to their parent
 folders and inherit some attributes, e.g., the read-only property.

IMPROVEMENTS

* See the new info manual section on "IMAP folders" for newly documented
 functions.  In particular, `vm-list-imap-folders' now lists the message
 counts in the IMAP folders.  

* New variable: `vm-sort-messages-by-delivery-date' allows messages to be
 sorted by the date of their delivery instead of the date sent.

* New virtual folder selectors added: `message-id', `uid' (for IMAP) and
 `uidl' (for POP).

* New command `vm-create-virtual-folder-of-threads' (bound to `V T')
 allows you to select entire threads into a virtual folder instead of
 individual messages.  There are also new virtual folder selectors
 `thread' and `thread-all'. 

* The trace of POP/IMAP sessions are retained in buffers named "trace of
 POP session..." or "trace of IMAP session...".  They are useful for
 troubleshooting any problems with mail server connections.

* Setting `vm-stunnel-program' to nil asks VM to use the built-in SSL
 functionality of Emacs, available in Gnu Emacs 24.

* New functions `vmpc-folder-match' and `vmpc-folder-account-match' in
 the vm-pcrisis package.

* New variable `vm-mail-auto-save-directory' where message composition
 buffers are auto-saved.

## VM 8.2.0a (2011-02-28)

### CHANGES

* The configuration of headers-only messages, introduced in 8.1.90a, has
 changed.  The variable `vm-load-headers-only' has been replaced by a
 new variable `vm-enable-external-messages'.  It should be set to 'imap
 to allow external messages in IMAP folders and
 `vm-imap-max-message-size' be customized to control the size of
 messages that will be external.

* If you download mail from IMAP spool files, the 8.1.x versions of
 VM had a bug which allowed the the `X-VM-IMAP-Retrieved' headers
 to grow unnecessarily.  This can slow down the saving of folders
 into which you downloaded IMAP mail.  To solve the problem, run
 the command `vm-prune-imap-retrieved-list' after installing
 version 8.2.0.  (See the info manual under "IMAP Spool Files".)

* A set of inessential key bindings (a, b, e, i, w, L, M-l, !, <, >, *,
 %) have been removed from the standard VM key bindings.  If you would
 like to use them, add the line:
          (vm-legacy-key-bindings)
 to your vm-preferences-file (~/.vm.preferences).  To use the current
 key bindings instead, use the line
          (vm-current-key-bindings)
 Or, you might bind these keys to some other operations of your choice.

 However, `vm-edit-message' is available via a new key binding `C-c
 C-e'. 

* The default value of `vm-url-browser-function' (invoked by mouse-2) is
 changed to 'browse-url, which is an Emacs standard web-browsing
 function.  To invoke your favourite browser, customize
 `browse-url-browser-function'.  Cf. Emacs manual.

* The mouse-3 context menu for URL's in messages updated to eliminate
 obsolete web browsers.  Entries added for Firefox, Mozilla and Opera.

* The function `vm-mouse-send-url-to-konqueror-new-browser' renamed
 to `vm-mouse-send-url-to-konqueror-new-window', to be consistent
 with other similar functions.

* The default settings of `vm-mime-deleteable-types' and
 `vm-mime-saveable-types' do not include the types listed in
 `vm-mime-external-content-types-alist'.  You might need to add them
 explicitly in your vm-preferences-file.

* The variable name `vm-auto-displayed-mime-content-types' changed
 to `vm-mime-auto-displayed-content-types' for consistency with
 other variable names.  (The corresponding `-exceptions' variable
 changed as well.)

* The variable name `vm-mime-attachment-infer-type-for-text-attachments'
 changed to `vm-infer-mime-types-for-text'.

* Plain text forwarding has been extended to deal with MIME attachments.
 The command `vm-forward-message-plain' (bound to `Z') uses this method.
 (The normal `z' key forwards messages encapsulated using
 `vm-forwarding-digest-type'.)  There are also associated variables
 `vm-forwarded-headers-plain' and `vm-unforwarded-header-regexp-plain',
 which determine the headers included in the forwards.

* The meaning of the variable `vm-included-mime-types-list' is
 changed.  It need only mention MIME type/subtype pairs that are
 not handled by default.  The types "text/plain", "text/enriched"
 and "message/rfc822" are now handled by default.

* The attributes vector has been expanded to 16 elements for
 compatibility with Mozilla Thunderbird.  The first time a folder
 is written, this will cause extra time to be taken for "stuffing"
 the attributes.  But this is only a one-time cost.

* New variable `vm-include-mime-attachments' allows the inclusion of
 MIME attachments in replies.  This functionality was originally
 part of vm-pine.el under the name `vm-mime-yank-attachments'.
 That functionality is now obsolete.  Replace references to
 `vm-mime-yank-attachments' in your customization by the new variable.
 
* The command `vm-flag-message-read' (.) introduced in 8.1.93a is
 renamed to `vm-mark-message-read' for consistency of terminology.

IMPROVEMENTS

* New option 'internal-only for `vm-mime-honor-content-disposition',
 which means the content-disposition will be honored for only internally
 displayable types.

* New variable `vm-mime-alternative-yank-method' controls the selection
 of MIME alternatives during yanking of messages (as well as including and
 forwarding).

* Added a variable `vm-verbosity' to control the granularity of
 informative messages displayed by VM.  Levels 5-8 are recommended, with
 8 corresponding to the current level.

* New operations for manual control of thread indentation for
 dealing with long (and deep) message threads.  See info under
 "Threaded Summaries".

* A number of "point-to-point" attachment operations have been
 added: 
 - `vm-dired-attach-file' and `vm-dired-do-attach-files' from
 dired buffers.
 - `vm-attach-message-to-composition' and
 `vm-reader-map-attach-to-composition' from VM folders.
 - a drag-and-drop feature that can be used in the window system.

* New command `vm-switch-to-folder' defined to quickly return to a
 previously buried folder.  (Originally in vm-rfaddons.)

* New custom command `vm-toggle-best-mime' in vm-rfaddons to toggle
 between 'best and 'best-internal' MIME altrenatives.  (Thanks to
 Alley Stoughton for this addition.)

* New variable `vm-include-text-basic' can be used to enable the
 fallback method of quoting message text in replies.  It should
 be normally left with the default value of nil.

* VM refrains from repeatedly checking for new mail once it has
 found some new mail on the spool. Set `vm-mail-check-always' to
 override this behavior.  

* When a predefined virtual folder is quit, all the component
 folders that it depends on will also be quit automatically.

* When an interactive virtual folder is quit, the message pointer
 in the virtual folder is transferred to the original folder.
 This facility can be used to search for particular messages by
 using virtual folders.

* Restored the [Emacs] and [Undo] menu buttons that were removed in
 version 8.0.8.  For environments that do not support such buttons,
 drop-down menus will be used instead.  The variable
 `vm-use-menubar-buttons' can be used to use drop-down menus
 always.  (Thanks to Tim Cross for the fixes.)

* Hooks `vm-arrived-message-hook' and `vm-arrived-messages-hook'
 made to work correctly for IMAP folders.

* New variable `vm-thunderbird-folder-directory' and command
 `vm-visit-thunderbird-folder' allow the handling of Thunderbird
 folders without interference with VM's own folders.

* New variable `vm-sort-subthreads' allows the internal messages of
 threads to be sorted into subthreads (the default) or via the
 normal sorting criteria.

* Better support for message/external-body MIME type, with
 external-bodies loaded on demand.  If you have
 message/external-body as an element in
 `vm-mime-auto-displayed-content-types', you should remove it to
 access the new functionality.

* Newly documented commands: `[' (`vm-previous-button') and `]'
 (`vm-next-button') allow navigation inside message
 presentation buffers.

* New command: `!' (`vm-toggle-flag-message') allows you to flag a
 message as being important.  This adds a "!" mark in the Summary
 line for the message and highlights it with the high-priority face.

* New variable: `vm-summary-visible' specifies which messages
 should remain visible in folded thread summaries.

* New feature: vm-mime-external-content-types-alist allows
 emacs-lisp functions to be used for external viewing, e.g.,
 you can use `browse-url-of-file' to view html . 

## VM 8.1.93a (2010-08-28)

### CHANGES

** New feature: Invoking vm-load-init-file with a prefix argument
 loads the init-file (~/.vm) without loading the preferences-file
 (~/.vm.preferences).  This is a good way to run VM with the
 default settings, much like `emacs -Q'.  We are advising all
 users to split their init-files to make use of this feature.  See
 info section on "Starting Up".

** New feature: vm-summary-enable-faces allows summary lists with
 faces turned on.  (This was formerly an add-on contributed by
 Robert Fenk under the name vm-summary-faces-mode. But there are
 several changes.  In particular, the face names do not end in
 "-face" following the Emacs naming conventions.)  See the info
 section on "Summaries" for more information.  If you currently
 use the u-vm-color package for colorizing the Summary buffers,
 please remove the feature, i.e., delete a line like 
  (add-hook 'vm-summary-mode-hook 'u-vm-color-summary-mode)
 from your VM initialization file.

 Some users have reported that Emacs hangs if this line is retained.

** Commands renamed:
 `vm-mime-save-all-attachments' => `vm-save-all-attachments'(C-c C-s)
 `vm-mime-delete-all-attachments' => `vm-delete-all-attachments'(C-c C-d)

IMPROVEMENTS

* Sorting of messages extended to work with threads.  By default,
 threads are sorted by "activity", i.e., the date of their most
 recent activity.  But they can also be sorted by other sort keys.
 (The variable `vm-sort-threads-by-youngest-date' is now defunct.)

* New feature: thread-folding in the Summary window allows message
 threads to be collapsed into single line summaries.  The
 following new variables control the behavior of thread folding.
   `vm-summary-enable-thread-folding',
   `vm-summary-show-thread-count' and
   `vm-summary-thread-folding-on-motion' 
 New commands:
   `vm-toggle-thread' (T), `vm-expand-all-threads' (E) and 
   `vm-collapse-all-threads' (C).
 See the info file for details.  Thanks to Arik Mitschang for this
 contribution. 

* New experimental feature: `vm-enable-thread-opeartions' enables
 "thread operations", a method of invoking operations (such as
 deleting or saving) on message threads.  See the info file for
 details.  Thanks to Arik Mitschang for this contribution.

* New variables: `vm-summary-thread-indentation-by-references'
 controls whether threads are indented by their original nesting
 level or according to the nesting level within the folder.
 `vm-summary-maximum-thread-indentation' specifies the maximum
 depth of indentation to be displayed.

* New command: `vm-kill-thread-subtree' (K) allows a thread subtree
 to be deleted.  This amounts to the same thing as
 `vm-delete-message' invoked as a thread operation.

* The calculation of threads improved using Jamie Zawinski's
 ideas.  Threads are correctly identified even if some of the
 messages are missing.

* Added EasyPG storage of passwords for mail server accounts.  See
 info index under "passwords".

* Virtual folder facility extended to work with POP and IMAP
 folders.  But, there are still some outstanding problems with it.

* Resolved performance problems in summary generation.  It works
 quite fast now.

* New variable: `vm-mime-parts-display-separator' allows you to
 insert a string as a separator between MIME parts.

* New command: `vm-save-attachments' allows you to save all the
 attachments of a message under your own file names instead of the
 original file names given in the message.

* New command: `vm-flag-message-read' (.) allows you to mark an
 unread or new message as read.

### BUG FIXES

* Fixed various issues flagged by the Emacs 23 compiler warnings.

## VM 8.1.925a (2010-07-17)
## VM 8.1.92a (2010-07-10)

IMPROVEMENTS

* Headers-only mode (external messages) for IMAP folders is now
 completed.  It operates by fetching messages into the Folder buffers,
 leading to a more reliable operation.

* New command `vm-list-imap-folders' can be used to list the
 folders on an IMAP server.

* In headers-only mode for external messages, a limited number of
 messages can be fetched on demand for message preview.  New variable
 `vm-fetched-message-max' specifies this number.  (Default is 10.)

* New variable `vm-imap-default-account' allows IMAP-FCC copies to
 be routed there.

* New variable `vm-imap-server-timeout' allows timeout during a wait
 for output from an IMAP server.

* New variable `vm-imap-ensure-active-sessions' asks VM to ensure
 that an IMAP session is active before issuing commands.

## VM 8.1.90a (2010-05-11)

IMPROVEMENTS:

** This version contains an experimental feature of using IMAP folders in
 "headers-only" mode for external server messages, with body loaded only
 on demand.  This helps to keep the folder sizes small and VM to run
 faster.  However, this code is in a preliminary stage.  Please use it
 with CAUTION.

 variable: vm-load-headers-only (or vm-enable-external-messages)

 If set to t, all new messages will be loaded to the cache-folder
 in headers-only mode.  The body is loaded on demand when a
 message is displayed in the Presentation Buffer.  This is a
 temporary load and is lost as soon as you move to another
 message.  

 To permanently load a message body into the Folder Buffer, use:

 command: vm-load-message (bound to 'o')

 This command discards the current body of the message, if any,
 and refreshes it from the server copy.

 command : vm-unload-message (bound to 'O')

 This command discards the current body of the message from the
 Folder Buffer and leaves it empty.

 FAILURE RECOVERY: If the cache folder gets corrupted for any
 reason, just delete it from the file system.  A new cache folder
 will be generated upon the next visit.

* New variable `vm-imap-refer-to-inbox-by-account-name' allows IMAP
 folders named "INBOX" to be referred to by their account names
 inside VM.

* The command `vm-fix-my-summary!!!' renamed to `vm-fix-my-summary'
 to make it easier to type.

* The chatter of minibuffer messages during paging of mail is
 reduced: messages about MIME decoding are emitted only if the new
 variable `vm-emit-messages-for-mime-decoding' is non-nil, and
 messages about end of messages are emitted only of
 `vm-auto-next-message' is non-nil.

* IMAP session dialogue restructured using UID queries, which makes
 VM more reliable in handling real-time changes on the server side.

* New variable `vm-imap-connection-mode' can be set to 'offline to
 allow IMAP cache folders to be used offline.  After connecting to
 the network, do `C-u M-x vm-imap-synchronize' to force full
 synchronization. 

* Improved error messages arising in IMAP sessions with the server.

* New variable `vm-do-fcc-before-mime-encode' (formerly in
 vm-rfaddons) allows you to save fcc copies of messages before
 mime-encoding them.

** New variables `vm-expunge-before-quit' and `vm-expunge-before-save'
 introduced to allow automatic expunge.  They are nil by default.  

## VM 8.1.2 

* VM made safe for use with Gnu Emacs 23, by removing a few calls
  to the `next-line' function (which was redefined in this Emacs).

* Several critical problems with Thunderbird inter-operability
  were corrected.  Manual section on Thunderbird folders added.

* Extended Org mode email links to work for virtual folders.

### CHANGES

** The default values of `vm-pop-expunge-after-retrieving' and
  `vm-imap-expunge-after-retrieving' changed to nil to help new
  users.

* `vm-fill-long-lines-in-reply-column' initialized to the default value
  of `fill-column'.

* All MIME messages are now decoded in the Presentation buffer,
  unless they have US-ASCII as their charset.  In particular,
  messages with 8bit charsets are treated this way.  Such messages
  are not regarded "plain messages" any more.

## VM 8.1.1 (2010-04-26)

** The variable vm-always-use-presentation-buffer is deprecated.
  Please remove all settings for this variable in your init file.
  The default behaviour will be to always use the presentation
  buffer.  Report any problems that might arise as a result.

* Extended Org mode email links to handle POP and IMAP folders.
  (Use org-vm.el in the VM contrib directory until the Org mode
  distribution gets updated.)

* Added autoloads for easy inter-operation with the Org mode.

* Added a section on History and Administration in the info manual.

* Made the autoloads compatible with VM 7.19 instructions.

* Fixed the build process to treat version info better.

* Removed a few incompatibilities with XEmacs.

* Mode line format reverted to the original one in 7.19.  The new
  mode line format is available in the variable
  `vm-mode-line-format-robf'.  It can be installed by adding a
  vm-mode-hook. 

## VM 8.1.0 (2010-03-21)

KNOWN PROBLEMS:

* Automatic filling is turned off for some plain text messages for
  safety reasons.  Please help us by sending us sample messages
  for which filling fails.

* IMAP folders occasionally give spurious connection errors.
  Doing vm-get-new-mail ('g') resumes the connection.

MAJOR NEW FEATURES:

* Support for reading and replying to messages in HTML.

* Full support for IMAP servers.  (See "IMPROVEMENTS for
  imap-folders" below.) 

CHANGES:

** New boolean variable `vm-word-wrap-paragraphs' controls the word
  wrapping of paragraphs in messages using the longlines library.
  The variable is set to nil by default. When it is set to t,
  paragraphs are word wrapped and the value of the variable
  `vm-fill-paragraphs-containing-long-lines' is immaterial (as
  long it is non-nil).  Set vm-word-wrap-paragraphs to nil to
  enable the usual filling functionality.

** vm-pgg is not loaded by default because it is a set up as an
  add-on.  Users should load it from their .emacs file by using
  the sequence
       (require 'vm-autoloads)
       (require 'vm-pgg)

** The variable `vm-mime-show-alternatives' is deprecated.  Set
  the variable `vm-mime-alternative-show-method' to 'all to
  get the same effect.

* Moved Robert's user-defined summary functions to the core:
 - S for human readable size
 - P for indication of attachments
 - p for indication of a postponed message

IMPROVEMENTS:

* Display number of drafts and postponed messages in the modeline
  and use a more compact modeline.  To use this feature, include
  this line in your .vm file:

    (setq vm-mode-line-format vm-mode-line-format-robf)

* The variable `vm-paragraph-fill-column', previously removed in
  earlier versions of this release, is brought back.

** The commands `vm-mime-save-all-attachments' and
  `vm-mime-delete-all-attachments' have been moved to the VM core
  (from vm-rfaddons).  New variables:
	 vm-mime-deletable-types 
	    (formerly `vm-mime-delete-all-attachments-types')
	 vm-mime-deletable-type-exceptions
	    (formerly `vm-mime-delete-all-attachments-types-exceptions')
	 vm-mime-savable-types
	    (formerly `vm-mime-save-all-attachments-types')
	 vm-mime-savable-type-exceptions
	    (formerly `vm-mime-save-all-attachments-types-exceptions')         
	 vm-mime-attachment-save-directory
	 vm-mime-attachment-source-directory
	 vm-mime-all-attachments-directory
  See the info file section on MIME attachments for details.

  The options for vm-rfaddons.el should not include
  `save-all-attachments' and should be removed if it is currently
  being used.  The option `take-action-on-attachments' is not
  included by default.

* `vm-quit-no-change' offers to delete the auto-save file if there is
  one.  (This wasn't getting done due to a bug in FSF Emacs.)

* `vm-delete-duplicate-messages' now works by comparing message ID's.
  (from Noah Friedman's vm-addons).

* New boolean variable `vm-sort-threads-by-youngest-date' allows
  threads to be sorted by their youngest date or oldest date.

* `vm-yank-message' function streamlined a bit.  New variable
  `vm-include-text-from-presentation' can be used to extract the
  included message text from the presentation buffer.

** text/html handling controlled by a new variable
  `vm-mime-text/html-handler' which is set to 'auto-select by
  default.  It causes VM to locate the best library among
  emacs-w3m, external w3m, w3 and lynx to display html
  internally.  (This replaces the earlier variable
  `vm-mime-use-w3-for-text/html'.)

** vm-delete-duplicate-messages now works by comparing message ID's.
  (from Noah Friedman's vm-addons).

* vm-yank-message function streamlined somewhat.  New variable
  `vm-include-text-from-presentation' used to extract message text
  from presentation buffer.  (This replaces the variable
  `vm-reply-include-presentation' used in vm-rfaddons.)

* The variable `vm-mime-yank-attachments' is set to nil by default,
  so that we are not surprised by unexpectedly large mail messages.

* The variable `vm-mime-require-mime-version-header' is set to nil
  by default, so that we will be tolerant of bad MIME senders.

* Allow for sorting the headers of composition buffers by calling the
  function `vm-reorder-message-headers' interactively.  You may configure
  the order by the new variable `vm-mail-header-order'.  This can be
  useful if some broken MUAs (e.g. Tobit) mess up the messages due to the
  header order.

* Added hiding and protection of headers in composition buffers.  See the
  new variable `vm-mail-mode-hidden-headers' for customization. (Thanks to
  Eric Schulte for the initial code posted in gnu.emacs.vm.info)

* Added the function `vm-mime-list-part-structure' to list the mime part
  structure of a message.

* Added function `vm-mime-nuke-alternative-text/html' which can be used to
  get rid of alternative text/html parts.

* VMPC: Better action reader and a default profile which is used if no
  email addresses could be found.  The meaning of the arguments for
  `vmpc-prompt-for-profile' has been slightly simplified, see the doc
  string for details.  

* Removed `vm-paragraph-fill-column', the value is now taken from
  `vm-fill-paragraphs-containing-long-lines' thus allowing to fill to the
  available window with.

* Replaced `vm-fill-paragraphs-containing-long-lines' by the faster and
  more flexible version from vm-rfaddons.el.  Also cleaned up calls to the
  fill function and removed code duplication.  The code using longline.el
  remains in vm-rfaddons.el, but it must be used explicitly now in an
  advice.

* Moved the variable `vm-fill-long-lines-in-reply-column' from
  vm-rfaddons.el to VM core.  It is not necessary to hook the fill
  function, just set the variable.

* Errors caused by `vm-retrieved-spooled-mail-hook' are reported and
  assimilation of messages continues instead of aborting.

* Handle filenames also from the disposition fields "name", "filename*"
  and "name*", where the latter two get decoded as they might contain 8bit
  chars.

* Uncoupled searching of MIME images from source location.  The search
  should be a bit smarter now allowing to place the images outside of the
  source tree now.

* Added syncing of message status when visiting a mbox of Thunderbird.
  Not all message flags are interchangeable and the message summary
  file (.msf) of Thunderbird will get removed by VM in order to force
  Thunderbird to rebuild it.  Also VMs folder index will be skipped if
  it is older than the folder in order to update VMs message status flags.

* Improved text/html displaying by w3m.  Inline images are now extracted
  correctly and they also display now.  Added a generic handler code to
  support also other HTML handlers.

* Added variable `vm-restore-saved-summary-formats' to restore
  each folder's summary format to what was saved previously.
  (Uday S. Reddy)

* A prefix argument to `vm-fix-my-summary!!!' will kill a folders local
  summary format which was restored by `vm-restore-saved-summary-formats'.

* The button for an image or PDF shows a thumbnail now when possible.
  This requires ImageMagick.  (Thanks to Eric Schulte for the idea and
  initial code.)

* Allow to reorder messages headers before sending by setting the new
  variable `vm-mail-reorder-message-headers'.

* Allow UTF-8 encoded messages to be displayed on tty.  (Ulrich Müller)

### BUG FIXES

* `vm-quit-no-change' made to honour the setting of the variable
  `delete-auto-save-files'. (Uday S. Reddy)

* Allow the use of iso-8859-1 for outgoing mail under Emacs 23
  (instead of spurious iso-2022-jp).  (Ulrich Müller)

* Coding system set to binary when reading and writing to allow
  for 8-bit content.  (Julian Bradfield)

IMPROVEMENTS for pop-folders (Uday S. Reddy)

* Added the variable `vm-pop-debug' to keep trace buffers.

* New commands `vm-pop-start-bug-report' and `vm-pop-submit-bug-report'
  which track POP session details.


IMPROVEMENTS for imap-folders (Uday S. Reddy)

** New variable `vm-imap-account-alist' allows multiple IMAP
  accounts to be handled uniformly.  The variable
  `vm-imap-server-list' is now obsolete.  IMAP folders should be
  specified in the minibuffer using the account:mailbox format.
  See the info node on IMAP folders.

* New variable `vm-load-headers-only' to enable headers-only
  downloading of IMAP folders.    (This is still experimental.)

* IMAP-FCC is extended to work for virtual folders, but only if
  the real parent message is an IMAP message.

* Made server expunge more robust.  Added new variable
  `vm-imap-expunge-retries' to force retries for sluggish servers.

* Allow message attributes as well as labels to be saved on server.

* Changed vm-imap-get-new-mail to do synchronization: reading and writing
  message attributes & labels, expunge messages in the cache.  Added
  variable `vm-imap-sync-on-get' to control this behavior.

* Added command `vm-imap-synchronize' to do full synchronization. 

* Trapping IMAP server errors uniformly.

* Added variable `vm-imap-tolerant-of-bad-imap' to allow minor
  violations of the IMAP spec by IMAP servers.

* New commands `vm-imap-start-bug-report' and `vm-imap-submit-bug-report'
  which track IMAP session details.

## VM 8.0.14 2009-12-16

BUGFIXES

* Removed an incompatibility of the mapvector procedure with XEmacs.

## VM 8.0.13 2009-11-29

MANAGEMENT CHANGES:

* VM being maintained by "VM development team", vm@lists.launchpad.net,
  consisting of Robert Fenk, Uday Reddy and Ulrich Müller.

BUGFIXES:

* VM-Cache entries were broken by encoding the pretty printed cache string
  instead of the individual strings.  This bug was introduced in 8.0.10 by
  the bug fix for correctly storing the cached multibyte summary entries.
  It causes building of the summary to fail.  Broken cache entries are now
  detected and removed while loading a folder.

## VM 8.0.12 2008-11-05

IMPROVEMENTS:

* Display version info when calling `vm-version' interactively.  (Thanks
  to Ulrich Müller)

* Yanking of messages uses the same MIME decoding as the presentation
  now.  See the new variable `vm-mime-yank-attachments' to configure if
  attachments are also yanked.

* `u-vm-color.el' is bundled and maintained with VM now.  Ulf Jasper handed
  it over to me as he switched to Gnus.

BUGFIXES:

* Detect w3 by using `locate-library' instead of checking for a bound
  `w3-about'. (Thanks to Klaus Straubinger)

* vm.revno.el was not installed anymore b "make install".  (Thanks to
  Ulrich Müller for reporting)

* Insert `emacs-version' instead of creating wrong version string for
  XEmacs, i.e. the patch level was the major version. (Thanks to Stephen
  Turnbull)

* Correctly locate the data directory for the pixmaps when running as a
  XEmacs package.

* Check for some MIME character sets that may be available in recent
  XEmacs.  (Thanks to Aidan Kehoe for the patch)

* Some documentation fixes. (Thanks to Michael Ernst for the patches)

* Fixed infinite loop in vm-mime-encode-words on XEmacs  21.5-b28.
  (Thanks to Aidan Kehoe for the patch)

* Detect "score" (additionally to "hits") in "X-Spam-Status:" headers in
  `vm-su-spam-score-aux'. (Patch from Michael Ernst)

* Typo fix in vm-pcrisis.texinfo. (Patch from Michael Ernst)

* Header encoding was BASE64 instead of QP by default and it was not
  encoding whole words, but only the 8bit chars instead. (Thanks to Ulrich
  Müller for reporting)

* MIME text parts interleaved by attachments are now correctly yanked,
  e.g. when replying to a message.

* Limit the buffer-name length and sanitize the used characters. (Thanks
  to Mark Diekhans for reporting)

* Do not fail on corrupted address headers.  (Reported by John Covici)

* Fixed GTK detection and toolbar handling for newer Emacs 22 versions.

Public bug reported:

## VM 8.0.11 2008-08-11

BUGFIXES:

* Removed dependency of vm-revno.el to other lisp sources to avoid
  building it in a release bundle.  (Thanks to Ralf Fassel)

## VM 8.0.10 2008-07-22

NOTES:

* This is the first version of VM 8.* to be also released as a XEmacs
  package.

IMPROVEMENTS:

* Added missing documentation for `vm-user-agent', "?" binding and
  'vm-delete-duplicate-messages'.  (Thanks to Alan Wehmann)

* `vm-message-history.el' now uses a buffer similar to the summary for
  browsing the history.  The buffer replaces the summary buffer when
  present.  Duplicate history entries will be removed.

* Define and use `vm-replace-in-string' which is `replace-in-string'
  from XEmacs to avoid clashes with other GNU Emacs packages defining
  it differently. Unfortunately, GNU Emacs still does not provide this
  handy function. (Thanks to José Miguel Figueroa)

* MIME encoding of header will automatically happen now and has been moved
  from `vm-rfaddons.el' to `vm-mime.el' and `vm-vars.el'.

BUGFIXES:

* Rewrote `vm-message-history.el' to also work for XEmacs.

* Leading lines of a yanked message were accidently taken as headers and
  got removed if `vm-reply-include-presentation' was t.

* Fixed encoding of headers for trailing 8 bit characters.  (Thanks to
  Lutz Euler for the patch)

* Decode (QP-)encoded clear text before decrypting it.

* Use nil as default for `vm-mime-8bit-composition-charset' and thus
  enable proper detection of right charset.  (Thanks to Naoki Saito for
  reporting and debugging)

* Fixed bug in `vm-mime-display-external-generic' for GNU Emacs 23 causing
  corrupted content in the output file.  The old code has been replaced by
  a call to `vm-mime-send-body-to-file' which avoids duplication and works.
  There has been some special handling for `vm-fsfemacs-mule-p', but the
  actual reason for this was unclear so it has been removed.

* Correctly handle `vm-enable-addons' being t.

* Correctly store UTF-8 strings in the X-VM-v5-Data header to avoid
  corruption of summary lines. (Thanks to Yuning Feng for reporting)

* Correctly encode multibyte subjects. (Thanks to Yuning Feng for the
  patch) 

* Use BASE64 for header encoding when there are special chars not quoted
  by QP normally.  You may configure this by `vm-mime-encode-headers-type'.

* qp-decode program handles premature end of QP-encoded stream now
  gracefully. (Thanks to Ralf Fassel for the bug report, fix and testing)

* Added missing newline after "Content-Type" when using the command
  `vm-mime-attach-object-from-message'.  (Thanks to Dan Freed)

## VM 8.0.9 2008-02-20

BUGFIXES:

* Added documentation to `vm-mime-external-content-types-alist' that no
  extra single quotes should be used around %f as the file name is already
  quoted for the shell. (Thanks to Martin Schwenke)

* Fixed version number generation in release script.  It was broken for
  8.0.8, i.e. it was showing 8.0.x-xemacs-542 instead.  Now also other
  branch related information is stored in the file vm-revno.el.

## VM 8.0.8 2008-02-11

IMPROVEMENTS:

* Reactivated "Allow defadvice on function `vm' by recursing on session
  start".  It should work correctly now.

* Added interactive `vm-pipe-message-to-command-discard-output' and
  the non-interactive `vm-pipe-message-to-command-to-string' for using
  it in own functions.

* Added `vm-pipe-messages-to-command*' for bulk piping messages to a
  single command, i.e. like saving to a pipe.  This is substantially
  faster than `vm-pipe-message-to-command*' which call the command on 
  each message separately.  You may want to use it to feed spamassasin.

* Modified key bindings for piping messages, i.e. "|" is a prefix key
  now. Type it twice to get the old pipe command, "|d" will call the 
  discard the output, just display some infos in the mode line. "|s" 
  will call `vm-pipe-messages-to-command' and "|n" will also call it 
  but discard the output.

* Removed vm-easymenu.el and use easymenu.el instead.

* In `vm-save-message-preview', ask the user if the output file already
  exists instead of silently overwriting it.

BUG FIXES:

* Moved [Undo] to Dispose menu and [Emacs] to Help menu as these do not
  work in Emacs 22 anymore when on the menu bar.

* Fixed intermixing of signature and quoted text in reply if
  `vm-reply-include-presentation' is t. (Thanks to Roland Winkler for
  debugging and reporting)

* Fixed yanking of presentation from wrong folder when folder is virtual.
  (Thanks to Roland Winkler for reporting)

* Redistributed flag not displayed in presentation buffer mode line. 
  https://bugzilla.redhat.com/show_bug.cgi?id=428248 (Thanks to Jonathan
  Underwood for the fix)

* `vm-submit-bug-report' gets the variables dynamically now and thus does
  not miss new ones or references old ones anymore. 

* Correctly determine the real folder when postponing compositions started
  from a virtual folder. (Thanks to Uday S. Reddy for reporting and 
  debugging)

* Avoid crash when `vm-mouse-set-mouse-track-highlight' is not called
  within a summary buffer or without a valid message pointer.

* Do not disable modes which do not exist. (Thanks to Uday S. Reddy for
  reporting) 

* Set correct coding-system-for-read for the real messages of
  virtual folders.  (Thanks to Julian Bradfield)

## VM 8.0.7 2008-01-05

BUG FIXES:

* Disable only those minor modes listed in the variable
  `vm-disable-modes-before-encoding' before encoding a
  composition. (Thanks to Alley for reporting and debugging)

* Removed recursion from function `vm' added by 8.0.6, as it 
  causes startup troubles.

* Removed extra newline before attachment buttons. (Thanks to Alley for
  reporting)

* Removed wrongly used calls to `interactive-p'. (Thanks to Alley for
  reporting and debugging)

## VM 8.0.6 2008-01-02

IMPROVEMENTS:

* Rewrote INSTALL to be more consistent and more understandable.

* Allow defadvice on function `vm' by recursing on session start. (Thanks
  to Blueman for the code)

BUG FIXES:

* Ignore empty reply-to in `vm-ignored-reply-to'.

* Quoted the variable `vm-summary-format' in a doc string.

* Fixed typos in the docstring of `vm-mail-send-and-exit'.

* Disable all minor modes before encoding a composition.  This results in
  faster encoding when font-lock was enabled and avoids problems when
  parts of a MIME object button get expanded due to an abbrev and thus the
  extent/overlay gets split into two separate parts causing an encoding
  error.

* Avoid duplicate mime buttons during decoding. (Thanks to Alley for
  reporting)

* Mask 8 bit chars by 0xff in `vm-mime-qp-encode-region' to avoid crash
  for those with all higher order bits set (negative ones?) (Thanks to
  Blueman for the fix.)

## VM 8.0.5 2007-11-03

BUG FIXES:

* Fixed bug caused by fixing `vm-drop-buffer-name-chars' in 8.0.4.  There
  is a 20-40% chance to create a new bug when fixing one.  Regression
  tests would be nice, but we do not have any for VM ;-/

## VM 8.0.4 2007-11-02

IMPROVEMENTS:

* Require cl.el at compile-time only. (Thanks to John J. Foerch)

* Quiet compiler warning about old style backquotes. (Thanks to John
  J. Foerch)

BUG FIXES:

* Correctly call custom-add-load. (Thanks to John J. Foerch and
  Jonathan.underwood) 

* Fixed building of vm-cus-load.el for Emacs 21.

* Use the old default for `vm-primary-inbox', i.e. "~/INBOX".

* Honor a t in `vm-drop-buffer-name-chars' as documented.

## VM 8.0.3 2007-08-15

IMPROVEMENTS:

* Unified `vm-continue-what-message', i.e. first check for composition
  buffers, if none exist then for saved drafts.  Also added new variable
  `vm-zero-drafts-start-compose'.

BUG FIXES:

* Fixed building of autoloads for GNU Emacs.

* Docfixes for vm-pine.el (Thanks to Stephen Eglen).

* Resurrected `vm-add-reply-subject-prefix' which was lost by the commit
  of revno 91.

* Search for BZR only if bzrdir exists and use locate-file only when
  defined.

* Use  vm-mime-8bit-composition-charset as a fallback also for MULE Emacs.

* Fixed defcustom of vm-keep-crash-boxes and vm-spool-files.

* Fixed the section headers of the NEWS file.

## VM 8.0.2 2007-07-25

IMPROVEMENTS:

* Added --with-pixmapdir to configure the location of the pixmaps.

* DESTDIR-Patch (Ulrich Müller).

BUG FIXES:

* Avoid overflow of `buffer-undo-list' when inserting or encoding
  big attachments.

* defcustom of `vm-mime-all-attachments-directory' should list nil.

* Honor pre VM 8.0.0 values of `vm-folder-directory' and
  `vm-primary-inbox'. This should eliminate problems with users which
  never changed the defaults. 

* Use "cygwin-mount" to fix paths when available.

* Activate summary faces only when requested by vm-enable-addons.

* Fixed defcustom of `vm-enable-addons' and added documentation.

* "make install" creates $(bindir) now.

* Separate paths (e.g. otherdirs) only by semicolons to avoid problems on
  Win32.

* Handle paths with spaces correctly.

* Install also pixmaps for GTK enabled Emacs.

* Just use the first subject when replying/forwarding to a set of
  messages.  This avoids long filenames for saved composition buffers.

* Ensure we are compiling with an emacs version >= 21.

* Encode headers regexp and case-fold-search corrected. (Ulrich Müller)

* vm-summary-faces-mode does not leak extents anymore.

## VM 8.0.1 2007-06-29

NOTES:

In order to get more features from vm-rfaddons set the variable
`vm-enable-addons' in your ~/.vm.

BUG FIXES:

* A saner default for vm-shrunken-header-face.

* Added documentation on vm-shrunken-headers-face and
  vm-shrunken-headers-keymap.

* Added a new custom group `vm-faces' for faces.

* Added autoload token for vm-user-agent.

* Use INSTALL_PROGRAM instead of INSTALL_DATA for programs.

* Do not set vm-folder-directory if there is ~/INBOX.  If VM does not get
  mail after upgrading from 7.19 it is probably due to the new default for
  vm-folder-directory, which was nil before.

* Revised the bindings and enabled features to a hopefully less
  controversial setting. 

## VM 8.0.0 2007-05-31

NOTES:

VM is now in my hands and I will do my best to keep it alive! -- Robert

,--------------------------------------------------------------------------
| From: Kyle Jones <kyle_jones@wonderworks.com>
| To: Robert Widhopf-Fenk <hack@robf.de>
| Date: Wed, 21 Feb 2007 13:11:32 -0800
| Subject: Handing over VM?
| 
| Robert Widhopf-Fenk writes:
|  > Hi Kyle,
|  > 
|  > I have been maintaining VM "unofficially" for the last few
|  > years and now I want to become the official maintainer of
|  > VM.
|  > 
|  > Do I get your OK?
| 
| Yes.  Obviously I've moved on, though I've been slow to admit it
| to myself.  Good luck.
`--------------------------------------------------------------------------
	   
* My (robf) VM extensions are now activated by default, where it makes
  sense to me.

* Releases are numbered now MAJOR.MINOR.PATCHLEVEL, where MAJOR is
  increased when fundamental changes occur, MINOR for new features and
  PATCHLEVEL for bugfix releases.

* New cleaner source tree layout.

* Better built system based on configure.  Autoloads are generated only
  for those functions marked with the autoload token now, which are mainly
  interactive function. Thus, loading occurs only on demand and startup
  should be faster.
  
BUG FIXES:

* All bugs reported to gnu.emacs.vm.bugs, gnu.emacs.vm.info and directly
  to me are fixed either by the patches posted by others or me.

* If there are any missing autoloads, please report them and add a
  (require 'vm-SOURCE) to your ~/.vm!

* Probably added numerous new bugs.


IMPROVEMENTS: compared to 7.19 (not vmrf)

* A new icon set based on vm-small-pixmaps.tgz which was floating around.
  This one should fit by height to the one used in XEmacs and Emacs 22,
  but it is slightly larger than those used in Emacs 21.  If you see the
  old icons, the please set the variables `vm-image-directory' and
  `vm-toolbar-pixmap-directory' to nil in your ~/.vm!

* vm-mime-type-converter-alist now also works when replying to messages,
  i.e. for text/html one can use lynx or w3m for the conversion.
  (setq vm-mime-type-converter-alist
	'(("text/html" "text/plain" "lynx -force_html -dump /dev/stdin")))

* Postponing (draft handling) of compositions and continuing of drafts, in
  fact any messages also those from other people. (Info node: Sending
  Messages) 

* New mail header insertion functions for return-receipts, mail-priority
  and FCC.

* More virtual folder selectors and replacements of other functions based
  on selectors. (Info node: Virtual Folders)

* vm-serial.el provides message templates for composition and
  personalizes mass emails. (Info node: TODO)

* vm-biff.el for popups with a list of new messages.

* vm-rfaddons.el has various stuff, look at the source if you are curious
  or miss some VM feature, as it might already be there!


VMRF 7.19.187   2006-10-12

VMRF  2006-09

Mentioned on gnu.emacs.vm.info as a fork.


Local Variables:
mode: text
coding: utf-8
End:
