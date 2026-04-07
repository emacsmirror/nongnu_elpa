;;; guix.scm --- Build emacs-jabber from the current working tree.
;;
;; Usage:
;;
;;   One-shot install into the user profile:
;;       guix package -f guix.scm
;;
;;   Development shell with all dependencies:
;;       guix shell -D -f guix.scm
;;
;;   Use from Guix Home (see README.org for the exact snippet).
;;
;; This file mirrors the upstream Guix recipe for `emacs-jabber' as
;; closely as possible so it doubles as a local test harness for the
;; recipe before it is merged.  The only intentional differences are:
;;
;;   - `source' is a `local-file' of the current checkout rather than
;;     a `git-fetch' of a pinned commit, so the build always reflects
;;     whatever is on disk.
;;   - `version' is derived from `git describe' at evaluation time.
;;
;; Run `git submodule update --init --recursive' before using this
;; file so that `src/picomemo' is populated.

(use-modules (gnu packages)
             (gnu packages emacs)
             (gnu packages emacs-xyz)
             (gnu packages pkg-config)
             (gnu packages tls)
             (guix build-system emacs)
             (guix gexp)
             ((guix licenses) #:prefix license:)
             (guix packages)
             (guix utils)
             (ice-9 popen)
             (ice-9 rdelim))

(define %source-dir (dirname (current-filename)))

(define (git-output . args)
  "Run `git -C %source-dir ARGS...' and return its trimmed stdout, or
#f if the command fails or produces no output."
  (let* ((port (apply open-pipe* OPEN_READ "git" "-C" %source-dir args))
         (line (read-line port)))
    (close-pipe port)
    (if (eof-object? line) #f line)))

(define %version
  ;; Prefer `git describe' (tag + commit count + short hash) when
  ;; available, otherwise fall back to the short hash, then to the
  ;; hard-coded release version if we are not inside a git checkout.
  (or (git-output "describe" "--tags" "--always" "--dirty")
      (and=> (git-output "rev-parse" "--short" "HEAD")
             (lambda (hash) (string-append "0.10.3-" hash)))
      "0.10.3-git"))

(define (emacs-jabber-file? file stat)
  "Include every file in the checkout except VCS metadata and build
artifacts.  Submodule contents under `src/picomemo' are picked up as
long as the submodule has been initialised."
  (let ((name (basename file)))
    (not (or (string-contains file "/.git/")
             (string=? name ".git")
             (string-suffix? ".elc" file)
             (string-suffix? ".o" file)
             (string-suffix? ".so" file)
             (string-suffix? "~" file)
             (string-suffix? ".tar" file)
             (string-suffix? ".tar.gz" file)))))

(define-public emacs-jabber-git
  (package
    (name "emacs-jabber-git")
    (version %version)
    (source (local-file %source-dir
                        "emacs-jabber-checkout"
                        #:recursive? #t
                        #:select? emacs-jabber-file?))
    (build-system emacs-build-system)
    (arguments
     (list
      #:lisp-directory "lisp"
      #:include #~(cons "^[^/]*\\.so$"
                        %default-include)
      #:emacs emacs                     ;requires gnutls
      #:test-command #~(list "make" "-C" ".." "test")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'build-native-module
            (lambda _
              (invoke "make" "-C" "../src")))
          (add-after 'unpack 'fix-test-runner
            (lambda _
              ;; Replace grep -oP (Perl regex) with a
              ;; POSIX-compatible alternative so the test
              ;; runner counts results correctly.
              (substitute* "../Makefile"
                (("grep -oP '\\^Ran \\\\K\\[0-9\\]\\+'")
                 (string-append
                  "grep -o 'Ran [0-9]*'"
                  " | grep -o '[0-9]*'")))))
          (add-after 'unpack 'disable-failing-tests
            (lambda _
              ;; These 4 tests pass outside the build environment
              ;; but fail inside it.
              (define skip "\n  (skip-unless nil)")
              (substitute*
                  "../tests/jabber-disco-tests.el"
                ((".*query-if-needed-cache-miss \\(\\)"
                  all)
                 (string-append all skip))
                ((".*process-caps-modern.*queries \\(\\)"
                  all)
                 (string-append all skip)))
              (substitute*
                  (string-append
                   "../tests/"
                   "jabber-message-correct-tests.el")
                ((".*correct-last-uses-original-id \\(\\)"
                  all)
                 (string-append all skip))
                ((".*mam-syncing-skipped.*dispatch \\(\\)"
                  all)
                 (string-append all skip))))))))
    (native-inputs (list pkg-config))
    (inputs (list mbedtls))
    (propagated-inputs (list emacs-fsm))
    (home-page "https://thanosapollo.org/projects/jabber/")
    (synopsis "XMPP (Jabber) client for Emacs")
    (description
     "@code{jabber.el} is an XMPP client for Emacs.  XMPP (also known as
\"Jabber\") is an instant messaging system; see @url{https://xmpp.org} for
more information.  It supports OMEMO end-to-end encryption via picomemo.
This package definition builds straight from the current git checkout,
so the installed version always matches the working tree.  Run
@code{git submodule update --init --recursive} before using this file so
that the native OMEMO module can be built.")
    (license (list license:gpl3+       ;gpl2+ elisp, gpl3+ C
                   license:isc))))     ;picomemo

emacs-jabber-git
