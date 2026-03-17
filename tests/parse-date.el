;;; parse-date.el --- Tests for time parsing  -*- lexical-binding: t; -*-

(require 'jabber-util)
(require 'ert)

(ert-deftest jabber-parse-time ()
  "Test parsing date string to list value."
  (let ((expected '(26075 52760)))
    (should (equal expected (jabber-parse-time "2024-02-25T23:32:40")))
    (should (equal expected (jabber-parse-time "2024-02-25T23:32:40Z")))
    (should (equal expected (jabber-parse-time "2024-02-25T23:32:40+00:00")))
    (should (equal expected (jabber-parse-time "2024-02-25T23:32:40-00:00")))))

(ert-deftest jabber-parse-fractional-seconds ()
  "Test parsing date string with fractional seconds."
  (let ((expected '(240506851241242650476544 . 140737488355328)))
    (should (equal expected (jabber-parse-time "2024-02-25T23:32:40.500")))
    (should (equal expected (jabber-parse-time "2024-02-25T23:32:40.500Z")))
    (should (equal expected (jabber-parse-time "2024-02-25T23:32:40.500+00:00")))
    (should (equal expected (jabber-parse-time "2024-02-25T23:32:40.500-00:00")))))

(ert-deftest jabber-parse-time-float ()
  "Test parsing date string to float."
  (let ((min-expected 1708903960.549999)
        (max-expected 1708903960.550001))
    (should (< min-expected (float-time (jabber-parse-time "2024-02-25T23:32:40.550")) max-expected))
    (should (< min-expected (float-time (jabber-parse-time "2024-02-25T23:32:40.550Z")) max-expected))
    (should (< min-expected (float-time (jabber-parse-time "2024-02-25T23:32:40.550+00:00")) max-expected))
    (should (< min-expected (float-time (jabber-parse-time "2024-02-25T23:32:40.550-00:00")) max-expected))))

(ert-deftest jabber-parse-time-bignum ()
  "Test parsing date string bignum format."
  ;; going forward timestamps will use bignum rather than lists
  (skip-unless (not (version< emacs-version "29.1")))
  (let (current-time-list (expected 1708903960)) 
    (should (= expected (jabber-parse-time "2024-02-25T23:32:40")))
    (should (= expected (jabber-parse-time "2024-02-25T23:32:40Z")))
    (should (= expected (jabber-parse-time "2024-02-25T23:32:40+00:00")))
    (should (= expected (jabber-parse-time "2024-02-25T23:32:40-00:00")))
    (should (= (- expected 3600) (jabber-parse-time "2024-02-25T23:32:40+01:00")))
    (should (= (+ expected 3600) (jabber-parse-time "2024-02-25T23:32:40-01:00")))))

(ert-deftest jabber-parse-time-cons ()
  "Test parsing date string with fractional seconds to bignum format."
  ;; going forward timestamps will use bignum rather than lists
  (skip-unless (not (version< emacs-version "29.1")))
  (let (current-time-list (expected '(240506851241242650476544 . 140737488355328)))
    (should (equal expected (jabber-parse-time "2024-02-25T23:32:40.500")))
    (should (equal expected (jabber-parse-time "2024-02-25T23:32:40.500Z")))
    (should (equal expected (jabber-parse-time "2024-02-25T23:32:40.500+00:00")))
    (should (equal expected (jabber-parse-time "2024-02-25T23:32:40.500-00:00")))))

(ert-run-tests-batch "^jabber-")
