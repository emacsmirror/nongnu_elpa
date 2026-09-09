# Working on VM with Claude Code

Guidance that came out of working sessions, kept here so it survives them.
`CLAUDE.md` at the top of the tree is the main file: build, test, lint,
contributing workflow, writing rules, architecture. This one adds what that
file does not cover, and does not repeat it. Where the two touch, the
top-level file wins.

## Naming an issue or merge request in a reply

Every issue or merge request mentioned in a reply gets its full plain-text
URL and its title. A bare `#743` is not enough, and neither is a Markdown
link, whose target a terminal does not show.

```
https://gitlab.com/emacs-vm/vm/-/work_items/822 — Drop the blocking IMAP and POP driver
https://gitlab.com/emacs-vm/vm/-/merge_requests/657 — Delete the blocking IMAP and POP implementation
```

Issues take the `work_items` path, merge requests `merge_requests`. Several
at once become a list of URL and title, one per line. Fetch the titles from
the API rather than recalling them: a wrong title is worse than a bare
number.

This slips most often in places where a number arrives already bare, and
each of these counts as getting it wrong:

- a table or list whose first column is the number
- a second or third mention of the same ticket in one reply
- a ticket named as the reason for something rather than as the subject
- numbers copied out of a commit trailer, a label sweep or a derivation
- follow-up replies late in a session, after the first reply did it right

The rule does not expire and brevity does not excuse it. A reply naming ten
tickets carries ten URLs, or it names fewer tickets.

## What goes in a ticket, and how much

Short. The reporter and the dev team read these, not only the maintainer, and
length buries the finding.

- Lead with the conclusion in a sentence.
- Keep what someone would otherwise have to reproduce: a measurement, a diff,
  a table of results.
- Cut the narrative of how it was found, context the reader already has, and
  options nobody asked for.
- A retraction is a line, not a section.
- Reasoning at length belongs in the commit message.

A comment written by Claude says so on its first line, as the top-level
`CLAUDE.md` requires.

## Live work tracking

The ticket cleanup is tracked in a single comment on
https://gitlab.com/emacs-vm/vm/-/work_items/551 — AI ticket mass cleanup,
edited in place and never re-posted. It is a checkbox list by work state.
The per-issue labels say the same thing: `Pending`, `Analyzed`,
`irreproducible`, `Close by hand`.

Do not rewrite the description of that issue. That text is the maintainer's.

## Which issues to pick up

`Robot` marks an issue as one for Claude to take. It says nothing about
priority or about how the issue should be resolved. Leave the label alone
when closing, unless told otherwise.

## Before saying a change works

Run the test runner's own default, with no skip flags:

```sh
cd test && ./test-runner
```

On a machine with `test/vm-live-config.el` filled in, that is the whole suite
against the live IMAP and POP servers, plus the mock pass, the no-optional
pass and the send pass. About four minutes. `--skip-live` means the live
paths of a change are never exercised on the one machine that can exercise
them, which is how four branches once went in with three tested against the
mocks alone.

Report what the summary said, pass by pass.

## Design documents

`dev/docs/design/*.org` describe **what was implemented**, plus a brief note
of what was rejected and why. Not the plan, not an options survey, not a risk
assessment, not migration steps.

- Discussion belongs on the ticket. Put a pointer to the document on the
  ticket, since the document lives on `develop` and the ticket is visible
  from anywhere.
- These files outlive the work by years, so nothing in them may rot: no line
  numbers, no branch names, no merge request numbers. Name functions and
  variables, which can be found by name.
- Verify every symbol named still exists. A rewrite once carried over two
  function names from a rejected option's sketch; neither had ever existed.

When work lands, rewrite its design document from plan to description.

## The manual

Do not explain `vm-epg` by contrast with `vm-pgg`. vm-pgg is deprecated and
slated for removal, so the comparison will read as nonsense once it is gone
and sends the reader after a package that is not there. State what vm-epg
does on its own terms, and put the upgrade contrast in `NEWS-3.md`, which is
dated rather than evergreen and is where someone coming from the older
package looks.

The one "do not load both" warning already in the manual is a live conflict
rather than a comparison, and stays.

## Reporting

Say what the task changed. The working tree holds the maintainer's own
scratch files; leave them out of status summaries entirely, while still
keeping them out of commits.
