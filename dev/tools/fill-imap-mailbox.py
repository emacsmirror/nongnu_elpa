#!/usr/bin/env python3
"""Fill an IMAP mailbox with generated messages.

What dev/tools/vm-fetch-latency.el wants to measure against: the pauses in a
fetch go with the size of the heap, so a mailbox of thousands is the only one
that shows them.  Point it at a test account, never at real mail.
"""
import argparse
import imaplib
import sys
import time


def parse_args():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("mailbox", help="mailbox to create and fill")
    parser.add_argument("count", type=int, help="how many messages to append")
    parser.add_argument("--host", default="127.0.0.1")
    parser.add_argument("--port", type=int, default=143)
    parser.add_argument("--user", default="vmtest1")
    parser.add_argument("--password", default="vm-lives")
    parser.add_argument("--lines", type=int, default=30,
                        help="body lines per message, which sets its size")
    parser.add_argument("--delete", action="store_true",
                        help="delete the mailbox instead of filling it")
    return parser.parse_args()


def message(number, lines):
    "One whole RFC 5322 message, of a size real mail reaches."
    body = ("Line %d of a message the size of real mail.\r\n" % number) * lines
    return (
        "From: sender%d@example.com\r\n"
        "To: reader@example.com\r\n"
        "Subject: message %d\r\n"
        "Date: Mon, 24 Aug 2026 10:%02d:%02d -0700\r\n"
        "Message-ID: <fill-%d@example.com>\r\n"
        "\r\n%s" % (number, number, number % 60, number % 60, number, body)
    ).encode()


def append(server, mailbox, number, lines):
    "Append one message, raising what the server said if it refused."
    status, answer = server.append(mailbox, None, None, message(number, lines))
    if status != "OK":
        raise RuntimeError("APPEND of message %d failed: %s" % (number, answer))


def fill(server, args):
    "Create the mailbox and append to it, saying how far it has got."
    server.create(args.mailbox)
    start = time.time()
    for number in range(args.count):
        append(server, args.mailbox, number, args.lines)
        if number % 500 == 0:
            print("%d appended, %.1fs" % (number, time.time() - start), flush=True)
    print("%s holds %s" % (args.mailbox, server.select(args.mailbox)[1]))


def remove(server, mailbox):
    "Delete the mailbox, raising what the server said if it refused."
    status, answer = server.delete(mailbox)
    if status != "OK":
        raise RuntimeError("DELETE of %s failed: %s" % (mailbox, answer))
    print("%s deleted" % mailbox)


def main():
    args = parse_args()
    server = imaplib.IMAP4(args.host, args.port)
    try:
        server.login(args.user, args.password)
        if args.delete:
            remove(server, args.mailbox)
        else:
            fill(server, args)
    finally:
        server.logout()


main()
