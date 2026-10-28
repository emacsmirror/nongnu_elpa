"""Check Emacs's real TTY cursor without third-party terminal dependencies."""

import codecs
import errno
import fcntl
import os
from pathlib import Path
import pty
import re
import select
import struct
import subprocess
import termios
import time
import unicodedata

CSI = re.compile(r"\x1b\[([0-9;? >]*)([@-~])")


class Cursor:
    """Track VT cursor movement and validate private OSC checkpoints."""

    def __init__(self):
        self.row = self.col = 0
        self.pending = ""
        self.checks = 0
        self.expected_checks = None
        self.errors = []
        self.references = {}
        self.watch = None
        self.paints = 0
        self.forbidden_sgr = None
        self.background = None
        self.backgrounds = {}

    def sgr(self, params):
        """Track background colors, consuming foreground color arguments too."""
        position = 0
        while position < len(params):
            code = params[position]
            if code in (0, 49):
                self.background = None
            elif 40 <= code <= 47 or 100 <= code <= 107:
                self.background = (5, code - (40 if code < 100 else 92))
            elif code in (38, 48) and position + 1 < len(params):
                mode = params[position + 1]
                count = {2: 3, 5: 1}.get(mode, 0)
                color = tuple(params[position + 1:position + 2 + count])
                if code == 48:
                    self.background = color
                position += 1 + count
            position += 1

    def erase(self, command, mode):
        """Apply erase-line/display to the driver's 40 by 120 terminal."""
        start, end = (self.row * 120, (self.row + 1) * 120) if command == "K" else (0, 40 * 120)
        cursor = self.row * 120 + self.col
        if mode == 0:
            start = cursor
        elif mode == 1:
            end = cursor + 1
        for cell in range(start, end):
            self.backgrounds[divmod(cell, 120)] = self.background

    def feed(self, text):
        data = self.pending + text
        position = 0
        while position < len(data):
            char = data[position]
            if char == "\x1b":
                match = CSI.match(data, position)
                if match:
                    raw, command = match.groups()
                    params = [int(p or 0) for p in raw.split(";")] if not any(
                        c in raw for c in "? >"
                    ) else []
                    n = (params[0] if params else 0) or 1
                    if command in "Hf":
                        self.row = n - 1
                        self.col = ((params[1] if len(params) > 1 else 0) or 1) - 1
                    elif command == "A":
                        self.row = max(0, self.row - n)
                    elif command in "Be":
                        self.row += n
                    elif command in "Ca":
                        self.col += n
                    elif command == "D":
                        self.col = max(0, self.col - n)
                    elif command == "d":
                        self.row = n - 1
                    elif command in "G`":
                        self.col = n - 1
                    elif command == "r":
                        self.row = self.col = 0
                    elif command in "JK":
                        if self.watch and (command == "J" or self.watch[0] <= self.row < self.watch[1]):
                            self.paints += 1
                        self.erase(command, params[0] if params else 0)
                    elif command == "m":
                        if raw == self.forbidden_sgr:
                            self.errors.append((self.checks, "unexpected background", raw))
                        self.sgr(params or [0])
                    position = match.end()
                    continue
                if data.startswith("\x1b]", position):
                    end = data.find("\a", position + 2)
                    if end < 0:
                        break
                    payload = data[position + 2:end]
                    if payload.startswith("777;reference;"):
                        self.references[int(payload.split(";")[2])] = (self.row, self.col)
                    elif payload.startswith(("777;cursor;", "777;compare;")):
                        expected = (
                            self.references.pop(int(payload.split(";")[2]))
                            if payload.startswith("777;compare;")
                            else tuple(map(int, payload.split(";")[2:]))
                        )
                        self.checks += 1
                        if expected != (self.row, self.col):
                            self.errors.append((self.checks, expected, (self.row, self.col)))
                    elif payload.startswith("777;watch;"):
                        self.watch = tuple(map(int, payload.split(";")[2:]))
                        self.paints = 0
                    elif payload == "777;unwatch":
                        self.checks += 1
                        if self.paints:
                            self.errors.append((self.checks, "no table repaint", self.paints))
                        self.watch = None
                    elif payload.startswith("777;forbid-sgr;"):
                        self.forbidden_sgr = payload.removeprefix("777;forbid-sgr;")
                    elif payload == "777;end-forbid-sgr":
                        self.checks += 1
                        self.forbidden_sgr = None
                    elif payload.startswith("777;background;"):
                        row, col, *expected = map(int, payload.split(";")[2:])
                        self.checks += 1
                        actual = self.backgrounds.get((row, col))
                        if actual != tuple(expected):
                            self.errors.append((self.checks, "background", (row, col),
                                                tuple(expected), actual))
                    elif payload.startswith("777;done;"):
                        self.expected_checks = int(payload.split(";")[2])
                    position = end + 1
                    continue
                if len(data) - position < 2 or data.startswith("\x1b[", position):
                    break
                if data[position + 1] in "()":
                    if len(data) - position < 3:
                        break
                    position += 3
                else:
                    position += 2
                continue
            position += 1
            if char == "\r":
                self.col = 0
            elif char == "\n":
                self.row += 1
            elif char == "\b":
                self.col = max(0, self.col - 1)
            elif char == "\t":
                self.col = (self.col // 8 + 1) * 8
            elif char >= " " and not unicodedata.combining(char):
                if self.watch and self.watch[0] <= self.row < self.watch[1]:
                    self.paints += 1
                width = 2 if unicodedata.east_asian_width(char) in "WF" else 1
                for col in range(self.col, self.col + width):
                    self.backgrounds[self.row, col] = self.background
                self.col += width
        # Only an incomplete escape sequence survives to the next input batch.
        self.pending = data[position:]


def main():
    root = Path(__file__).resolve().parent.parent
    master, slave = pty.openpty()
    fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", 40, 120, 0, 0))
    command = [
        os.environ.get("EMACS", "emacs"), "-Q", "-nw",
        "--eval", "(progn (require 'package) "
        "(when (getenv \"EMACS_TEST_PACKAGE_DIR\") "
        "(setq package-user-dir (getenv \"EMACS_TEST_PACKAGE_DIR\"))) "
        "(package-initialize) (setq load-prefer-newer t))",
        "-L", str(root), "-l", str(root / "test/markdown-ts-appear-table-redisplay-test.el"),
        "-f", "markdown-ts-appear-table-redisplay-run",
    ]
    def controlling_terminal():
        os.setsid()
        fcntl.ioctl(0, termios.TIOCSCTTY, 0)

    process = subprocess.Popen(command, cwd=root, stdin=slave, stdout=slave, stderr=slave,
                               preexec_fn=controlling_terminal,
                               env={**os.environ, "TERM": "xterm-256color", "COLORTERM": "truecolor"})
    os.close(slave)
    cursor = Cursor()
    decoder = codecs.getincrementaldecoder("utf-8")("replace")
    transcript = []
    deadline = time.monotonic() + 120
    try:
        while time.monotonic() < deadline:
            ready, _, _ = select.select([master], [], [], 0.2)
            if not ready:
                if process.poll() is not None:
                    break
                continue
            try:
                data = os.read(master, 65536)
            except OSError as error:
                if error.errno == errno.EIO:
                    break
                raise
            if not data:
                break
            text = decoder.decode(data)
            transcript.append(text)
            cursor.feed(text)
        else:
            raise TimeoutError("Emacs redisplay regression timed out")
        code = process.wait(timeout=5)
        if (code or cursor.errors or cursor.references or cursor.watch or cursor.checks == 0
                or cursor.checks != cursor.expected_checks):
            print("".join(transcript)[-8000:])
            raise AssertionError(
                f"exit={code}, checkpoints={cursor.checks}, mismatches={cursor.errors[:20]}"
            )
        print(f"PASS: {cursor.checks} actual terminal cursor checkpoints")
    finally:
        if process.poll() is None:
            process.terminate()
            process.wait(timeout=5)
        os.close(master)


if __name__ == "__main__":
    main()
