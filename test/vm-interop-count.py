#!/usr/bin/env python3
"""How many messages an independent reader finds in a VM folder.

Called by test/vm-interop-test.el with a folder type and a path; prints the
count, or "ERROR <name>" if the reader refuses the file.  Python's mailbox
module is used because it is an implementation of these formats that owes
nothing to VM: a round trip through VM's own reader cannot show that VM's
writer produced something another program can read.

mboxcl2 is read with the plain mbox reader on purpose.  That reader ignores
Content-Length, which is the case the manual's mbox section describes.
"""
import sys
import mailbox

READERS = {
    "From_": mailbox.mbox,
    "BellFrom_": mailbox.mbox,
    "mboxcl2": mailbox.mbox,
    "mmdf": mailbox.MMDF,
    "babyl": mailbox.Babyl,
}


def count(folder_type, path):
    reader = READERS.get(folder_type)
    if reader is None:
        raise SystemExit("unknown folder type: %s" % folder_type)
    box = reader(path, create=False)
    try:
        return len(box)
    finally:
        box.close()


def main(argv):
    if len(argv) != 3:
        raise SystemExit("usage: vm-interop-count.py TYPE FOLDER")
    try:
        print(count(argv[1], argv[2]))
    except Exception as error:               # the reader refusing is a result
        print("ERROR %s" % type(error).__name__)


if __name__ == "__main__":
    main(sys.argv)
