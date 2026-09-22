#!/usr/bin/env python3
"""Exercise make load from a real Git baseline in a disposable Emacs daemon.

Run inside the project's development shell, for example:
  python3 tests/test-load-upgrade.py --base <pre-upgrade-commit>
No user Emacs or backend is contacted.  The baseline must predate the Kanban
parse-failures slot and diagnostics-local board fields.
"""

import argparse
import io
import json
import os
from pathlib import Path
import shlex
import subprocess
import tarfile
import tempfile


SETUP = r'''
(require 'ert)
(require 'websocket)
(require 'hermes-kanban)
(should (file-in-directory-p (symbol-file 'hermes-kanban-show) upgrade-base))
(should-not (memq 'parse-failures
                 (mapcar #'car (cl-struct-slot-info 'hermes-kanban--events-tail))))
(setq hermes-instances '(("fixture" . "http://fixture.invalid")))
(defvar upgrade-peer nil)
(defvar upgrade-listener
  (websocket-server 0 :host 'local
                    :on-open (lambda (ws) (setq upgrade-peer ws))))
(defvar upgrade-socket
  (websocket-open (format "ws://127.0.0.1:%s"
                          (process-contact upgrade-listener :service))))
(defun upgrade-wait (predicate)
  (let ((deadline (+ (float-time) 3)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (should (funcall predicate))))
(upgrade-wait (lambda () (and upgrade-peer (websocket-openp upgrade-socket))))
(defvar upgrade-refresh (run-at-time 3600 nil #'ignore))
(defvar upgrade-reconnect (run-at-time 3600 nil #'ignore))
(defvar upgrade-tail
  (hermes-kanban--events-tail-create
   :socket upgrade-socket :buffer (current-buffer) :slug "retained-board"
   :refresh-timer upgrade-refresh :reconnect-timer upgrade-reconnect))
(defvar upgrade-diagnostics nil)
(cl-letf (((symbol-function 'hermes-kanban--api)
           (lambda (&rest _)
             (hermes--promise-resolved
              '((diagnostics . (((task_id . "task") (task_title . "Task")))))))))
  (hermes-kanban--render-diagnostics "retained-board" "Retained")
  (setq upgrade-diagnostics (get-buffer "*Hermes Kanban Diagnostics*")))
(with-current-buffer upgrade-diagnostics
  (should (equal hermes-kanban--slug "retained-board"))
  (should-not (local-variable-p 'hermes-kanban-diagnostics--slug)))
;; Journal the actual function cells, map identities/contents and load metadata.
(defun upgrade-definitions ()
  (let (values)
    (mapatoms (lambda (symbol)
                (when (string-prefix-p "hermes-" (symbol-name symbol))
                  (push (list symbol (and (fboundp symbol) (symbol-function symbol))
                              (and (boundp symbol) (keymapp (symbol-value symbol))
                                   (symbol-value symbol))) values))))
    (sort values (lambda (a b) (string< (symbol-name (car a))
                                       (symbol-name (car b)))))))
(defvar upgrade-defs (upgrade-definitions))
(defvar upgrade-maps
  (mapcar (lambda (entry) (cons (nth 2 entry) (copy-tree (nth 2 entry))))
          (seq-filter (lambda (entry) (nth 2 entry)) upgrade-defs)))
(defvar upgrade-history (copy-tree load-history))
(defvar upgrade-load-path (copy-sequence load-path))
(defvar upgrade-features (copy-sequence features))
'''

VERIFY_REFUSAL = r'''
(should (equal upgrade-defs (upgrade-definitions)))
(dolist (entry upgrade-defs)
  (when (nth 1 entry) (should (eq (nth 1 entry) (symbol-function (car entry)))))
  (when (nth 2 entry) (should (eq (nth 2 entry) (symbol-value (car entry))))))
(dolist (pair upgrade-maps) (should (equal (car pair) (cdr pair))))
(should (equal upgrade-history load-history))
(should (equal upgrade-load-path load-path))
(should (equal upgrade-features features))
(should (hermes-kanban--events-tail-active upgrade-tail))
(should (websocket-openp upgrade-socket))
(should (websocket-openp upgrade-peer))
(should (memq upgrade-refresh timer-list))
(should (memq upgrade-reconnect timer-list))
;; Unchanged old accessors must still retire their own real resources.
(hermes-kanban--events-disconnect upgrade-tail)
(upgrade-wait (lambda () (not (websocket-openp upgrade-peer))))
(should-not (websocket-openp upgrade-socket))
(should-not (memq upgrade-refresh timer-list))
(should-not (memq upgrade-reconnect timer-list))
(should-not (hermes-kanban--events-tail-active upgrade-tail))
(websocket-server-close upgrade-listener)
'''

DIAGNOSTICS = r'''
;; Also exercise a direct library reload, which can retain legacy buffers even
;; when no struct replacement is attempted.  Never infer its board from nil.
(load-file (expand-file-name "lisp/hermes-kanban.el" upgrade-candidate))
(let (requests prompts)
  (cl-letf (((symbol-function 'hermes-kanban--api)
             (lambda (&rest args)
               (push args requests)
               (hermes--promise-make)))
            ((symbol-function 'completing-read)
             (lambda (&rest _) (push t prompts) "blocked")))
    (with-current-buffer upgrade-diagnostics
      (goto-char (point-min))
      (dolist (command (list (key-binding (kbd "g"))
                            (key-binding (kbd "RET"))
                            #'hermes-kanban-set-status))
        (should-error (call-interactively command) :type 'user-error))
      (should-not requests)
      (should-not prompts))
    ;; Reopen through the public board command, then use public view actions.
    (with-temp-buffer
      (hermes-kanban-mode)
      (setq hermes-instance (car hermes-instances)
            hermes-kanban--slug "retained-board" hermes-kanban--name "Retained")
      (call-interactively #'hermes-kanban-diagnostics))
    (with-current-buffer upgrade-diagnostics
      (should (equal hermes-kanban-diagnostics--slug "retained-board"))
      (setq tabulated-list-entries
            (hermes-kanban--diagnostic-rows '(((task_id . "task") (task_title . "Task")))))
      (tabulated-list-print)
      (goto-char (point-min))
      (call-interactively (key-binding (kbd "g")))
      (call-interactively (key-binding (kbd "RET")))
      (call-interactively #'hermes-kanban-set-status))
    (should (equal (mapcar #'car (reverse requests)) '("GET" "GET" "GET" "PATCH")))
    (dolist (request requests)
      (should (equal (nth 3 request) '((board . "retained-board")))))))
'''


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--base", required=True, help="Git ref of the old library")
    parser.add_argument("--candidate", type=Path, help="Candidate source tree (default: checkout)")
    args = parser.parse_args()
    repository = Path(__file__).resolve().parent.parent
    root = (args.candidate or repository).resolve()
    # Preserve the dev-shell executables, not credentials or user configuration.
    with tempfile.TemporaryDirectory(prefix="hermes-load-upgrade-") as temporary:
        work = Path(temporary)
        base = work / "base"
        base.mkdir()
        archive = subprocess.check_output(["git", "archive", args.base], cwd=repository)
        with tarfile.open(fileobj=io.BytesIO(archive)) as source:
            source.extractall(base, filter="data")
        env = {"PATH": os.environ["PATH"], "LANG": "C.UTF-8", "TERM": "dumb"}
        for variable, directory in (("HOME", "home"), ("XDG_CACHE_HOME", "cache"),
                                    ("XDG_CONFIG_HOME", "config"), ("XDG_DATA_HOME", "data"),
                                    ("XDG_STATE_HOME", "state"), ("TMPDIR", "tmp")):
            target = work / directory
            target.mkdir(mode=0o700)
            env[variable] = str(target)
        server = "upgrade"
        emacs = shlex.split(os.environ.get("EMACS_CMD", "emacs"))
        client = ["emacsclient", "-s", str(work / "tmp" / server)]

        def run(command, **kwargs):
            return subprocess.run(command, cwd=root, env=env, text=True,
                                  capture_output=True, timeout=90, **kwargs)

        def evaluate(expression):
            result = run([*client, "--eval", expression])
            if result.returncode:
                raise RuntimeError(result.stdout + result.stderr)
            return result.stdout

        def fixture(name, content):
            path = work / name
            path.write_text(";;; -*- lexical-binding: t; -*-\n" + content)
            evaluate(f"(load-file {json.dumps(str(path))})")

        started = False
        try:
            result = run([*emacs, "-Q", "--eval",
                          f"(setq server-socket-dir {json.dumps(str(work / 'tmp'))})",
                          f"--daemon={server}", "-L", str(base / "lisp")])
            if result.returncode:
                raise RuntimeError(result.stdout + result.stderr)
            started = True
            evaluate(f"(setq upgrade-base {json.dumps(str(base))} "
                     f"upgrade-candidate {json.dumps(str(root))})")
            fixture("setup.el", SETUP)
            evaluate("(setq upgrade-history (copy-tree load-history))")
            result = run(["make", "--no-print-directory", "load", "HERMES_ENV_WRAPPED=1",
                          "EMACSCLIENT=" + shlex.join(client),
                          "EMACS_CMD=" + shlex.join(emacs)])
            if result.returncode == 0 or "Hermes event-tail layout changed" not in result.stderr:
                raise RuntimeError("Expected event-tail preflight refusal:\n" +
                                   result.stdout + result.stderr)
            # Run inline: loading another fixture would itself change load-history.
            evaluate("(progn " + VERIFY_REFUSAL + " t)")
            print("PASS: supported make load refused before definitions/maps changed; "
                  "old socket, peer and timers survived and retired cleanly")
            fixture("diagnostics.el", DIAGNOSTICS)
            print("PASS: legacy diagnostics g/RET/status refused without reads, prompts "
                  "or mutations; public reopen restored exact-board dispatch")
        finally:
            if started:
                result = run([*client, "--eval", "(kill-emacs)"])
                if result.returncode or (work / "tmp" / server).exists():
                    raise RuntimeError("Disposable Emacs did not stop cleanly: " +
                                       result.stdout + result.stderr)


if __name__ == "__main__":
    main()
