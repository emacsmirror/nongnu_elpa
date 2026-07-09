;;; hermes-promise-tests.el --- Tests for hermes-promise  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(let ((root (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))))
  (add-to-list 'load-path (expand-file-name "lisp" root)))

(require 'hermes-promise)

;;; Group 1: settlement

(ert-deftest hermes-promise-test-resolve-fans-out-in-order ()
  "Resolution runs every registered handler once, in registration order."
  (let ((p (hermes--promise-make)) seen)
    (hermes--promise-then p (lambda (v) (push (list 'a v) seen)))
    (hermes--promise-then p (lambda (v) (push (list 'b v) seen)))
    (hermes--promise-resolve p 42)
    (should (equal (reverse seen) '((a 42) (b 42))))))

(ert-deftest hermes-promise-test-settles-once ()
  "Once settled, later resolve/reject calls are ignored."
  (let ((p (hermes--promise-make)) (count 0) value)
    (hermes--promise-then p (lambda (v) (cl-incf count) (setq value v)))
    (hermes--promise-resolve p 1)
    (hermes--promise-resolve p 2)
    (hermes--promise-reject p "no")
    (should (= count 1))
    (should (= value 1))
    (should (eq (hermes--promise-state p) 'resolved))))

(ert-deftest hermes-promise-test-subscribe-after-settle-fires-immediately ()
  "A handler added after resolution fires immediately with the value."
  (let ((p (hermes--promise-make)) value)
    (hermes--promise-resolve p 7)
    (hermes--promise-then p (lambda (v) (setq value v)))
    (should (= value 7))))

;;; Group 2: then / map / catch

(ert-deftest hermes-promise-test-map-transforms-value ()
  "`hermes--promise-map' threads the resolved value through a function."
  (let ((p (hermes--promise-make)) result)
    (hermes--promise-then (hermes--promise-map p #'1+)
                          (lambda (v) (setq result v)))
    (hermes--promise-resolve p 10)
    (should (equal result 11))))

(ert-deftest hermes-promise-test-reject-skips-resolve-hits-catch ()
  "Rejection bypasses resolve handlers and reaches the catch handler."
  (let ((p (hermes--promise-make)) resolved caught)
    (hermes--promise-catch
     (hermes--promise-then p (lambda (v) (setq resolved v)))
     (lambda (r) (setq caught r)))
    (hermes--promise-reject p "boom")
    (should-not resolved)
    (should (equal caught "boom"))))

(ert-deftest hermes-promise-test-catch-recovers ()
  "A catch handler returning a value resolves the next promise."
  (let ((p (hermes--promise-make)) result)
    (hermes--promise-then
     (hermes--promise-catch p (lambda (_r) 'recovered))
     (lambda (v) (setq result v)))
    (hermes--promise-reject p "boom")
    (should (eq result 'recovered))))

(ert-deftest hermes-promise-test-handler-error-rejects-next ()
  "A signalling handler rejects the chained promise with the error message."
  (let ((p (hermes--promise-make)) caught)
    (hermes--promise-catch
     (hermes--promise-then p (lambda (_v) (error "kaboom")))
     (lambda (r) (setq caught r)))
    (hermes--promise-resolve p 1)
    (should (string-match-p "kaboom" caught))))

(ert-deftest hermes-promise-test-handler-quit-rejects-next ()
  "A keyboard quit inside a handler rejects instead of stranding the chain."
  (let ((p (hermes--promise-make)) caught finalized)
    (hermes--promise-finally
     (hermes--promise-catch
      (hermes--promise-then p (lambda (_v) (signal 'quit nil)))
      (lambda (r) (setq caught r)))
     (lambda () (setq finalized t)))
    (hermes--promise-resolve p 1)
    (should (stringp caught))
    (should finalized)))

(ert-deftest hermes-promise-test-finally-quit-rejects-next ()
  "A quit inside a finally thunk rejects the mirrored promise."
  (let ((p (hermes--promise-make)) caught)
    (hermes--promise-catch
     (hermes--promise-finally p (lambda () (signal 'quit nil)))
     (lambda (r) (setq caught r)))
    (hermes--promise-resolve p 1)
    (should (stringp caught))))

(ert-deftest hermes-promise-test-then-chains-returned-promise ()
  "A handler returning a promise defers settlement until that promise settles."
  (let ((p (hermes--promise-make))
        (inner (hermes--promise-make))
        result)
    (hermes--promise-then
     (hermes--promise-then p (lambda (_v) inner))
     (lambda (v) (setq result v)))
    (hermes--promise-resolve p 1)
    (should-not result)
    (hermes--promise-resolve inner 'done)
    (should (eq result 'done))))

;;; Group 3: all

(ert-deftest hermes-promise-test-all-resolves-in-order ()
  "`hermes--promise-all' resolves to values by position, regardless of timing."
  (let ((p1 (hermes--promise-make)) (p2 (hermes--promise-make)) result)
    (hermes--promise-then (hermes--promise-all (list p1 p2))
                          (lambda (v) (setq result v)))
    (hermes--promise-resolve p2 'b)
    (should-not result)
    (hermes--promise-resolve p1 'a)
    (should (equal result '(a b)))))

(ert-deftest hermes-promise-test-all-rejects-on-first-failure ()
  "`hermes--promise-all' rejects with the first rejection reason."
  (let ((p1 (hermes--promise-make)) (p2 (hermes--promise-make)) caught)
    (hermes--promise-catch (hermes--promise-all (list p1 p2))
                           (lambda (r) (setq caught r)))
    (hermes--promise-reject p1 "x")
    (hermes--promise-resolve p2 'b)
    (should (equal caught "x"))))

(ert-deftest hermes-promise-test-all-empty-resolves-to-nil ()
  "`hermes--promise-all' of no promises resolves immediately to nil."
  (let (result settled)
    (hermes--promise-then (hermes--promise-all nil)
                          (lambda (v) (setq settled t result v)))
    (should settled)
    (should-not result)))

;;; Group 4: settled constructors and finally

(ert-deftest hermes-promise-test-resolved-and-rejected-constructors ()
  "`hermes--promise-resolved'/`-rejected' build pre-settled promises."
  (let (resolved rejected)
    (hermes--promise-then (hermes--promise-resolved 5)
                          (lambda (v) (setq resolved v)))
    (hermes--promise-catch (hermes--promise-rejected "no")
                           (lambda (m) (setq rejected m)))
    (should (= resolved 5))
    (should (equal rejected "no"))))

(ert-deftest hermes-promise-test-finally-runs-on-resolve-and-passes-through ()
  "`hermes--promise-finally' runs its thunk on resolve and forwards the value."
  (let ((ran 0) value)
    (hermes--promise-then
     (hermes--promise-finally (hermes--promise-resolved 9)
                              (lambda () (cl-incf ran)))
     (lambda (v) (setq value v)))
    (should (= ran 1))
    (should (= value 9))))

(ert-deftest hermes-promise-test-finally-runs-on-reject-and-passes-through ()
  "`hermes--promise-finally' runs its thunk on reject and forwards the reason."
  (let ((ran 0) reason)
    (hermes--promise-catch
     (hermes--promise-finally (hermes--promise-rejected "boom")
                              (lambda () (cl-incf ran)))
     (lambda (m) (setq reason m)))
    (should (= ran 1))
    (should (equal reason "boom"))))

(ert-deftest hermes-promise-test-finally-throwing-fn-rejects-next ()
  "A signalling finally thunk settles the chain as a rejection, never strands it."
  (let (settled reason)
    (hermes--promise-catch
     (hermes--promise-finally (hermes--promise-resolved 1)
                              (lambda () (error "cleanup failed")))
     (lambda (m) (setq settled t reason m)))
    (should settled)
    (should (string-match-p "cleanup failed" reason))))

(provide 'hermes-promise-tests)
;;; hermes-promise-tests.el ends here
