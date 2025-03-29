;;; relysium-diff-tests.el --- Tests for relysium-diff-string -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Daniel Nguyen

;;; Commentary:

;; This file contains unit tests for the relysium-diff-string function.
;; Run tests with: M-x ert-run-tests-interactively RET t RET

;;; Code:

(require 'ert)
(require 'relysium-diff)

(ert-deftest relysium-diff-string-test-identical-strings ()
  "Test that identical strings produce no conflict markers."
  (let ((original "Line 1\nLine 2\nLine 3")
        (new "Line 1\nLine 2\nLine 3"))
    (should (string= (relysium-diff-string original new) original))))

(ert-deftest relysium-diff-string-test-completely-different-strings ()
  "Test that completely different strings produce appropriate conflict markers."
  (let ((original "Line A\nLine B")
        (new "Line X\nLine Y")
        (expected "<<<<<<< HEAD\nLine A\nLine B\n=======\nLine X\nLine Y\n>>>>>>> Relysium"))
    (should (string= (relysium-diff-string original new) expected))))

(ert-deftest relysium-diff-string-test-partial-changes ()
  "Test that partial changes produce appropriate conflict markers."
  (let ((original "Line 1\nLine 2\nLine 3\nLine 4")
        (new "Line 1\nModified Line 2\nLine 3\nLine 4")
        (expected "Line 1\n<<<<<<< HEAD\nLine 2\n=======\nModified Line 2\n>>>>>>> Relysium\nLine 3\nLine 4"))
    (should (string= (relysium-diff-string original new) expected))))

(ert-deftest relysium-diff-string-test-additions ()
  "Test adding lines."
  (let ((original "Line 1\nLine 3")
        (new "Line 1\nLine 2\nLine 3")
        (expected "Line 1\n<<<<<<< HEAD\n=======\nLine 2\n>>>>>>> Relysium\nLine 3"))
    (message (relysium-diff-string original new))
    (should (string= (relysium-diff-string original new) expected))))

(ert-deftest relysium-diff-string-test-deletions ()
  "Test removing lines."
  (let ((original "Line 1\nLine 2\nLine 3")
        (new "Line 1\nLine 3")
        (expected "Line 1\n<<<<<<< HEAD\nLine 2\n=======\n>>>>>>> Relysium\nLine 3"))
    (should (string= (relysium-diff-string original new) expected))))

(ert-deftest relysium-diff-string-test-multiple-conflicts ()
  "Test multiple conflict regions."
  (let ((original "Line 1\nLine 2\nLine 3\nLine 4\nLine 5")
        (new "Line 1\nModified Line 2\nLine 3\nModified Line 4\nLine 5")
        (expected "Line 1\n<<<<<<< HEAD\nLine 2\n=======\nModified Line 2\n>>>>>>> Relysium\nLine 3\n<<<<<<< HEAD\nLine 4\n=======\nModified Line 4\n>>>>>>> Relysium\nLine 5"))
    (should (string= (relysium-diff-string original new) expected))))

(ert-deftest relysium-diff-string-test-empty-strings ()
  "Test empty strings."
  (let ((original "")
        (new "Line 1\nLine 2")
        (expected "<<<<<<< HEAD\n=======\nLine 1\nLine 2\n>>>>>>> Relysium"))
    (should (string= (relysium-diff-string original new) expected))))

(ert-deftest relysium-diff-string-test-multiline-blocks ()
  "Test handling of multiline blocks of changes."
  (let ((original "Start\nLine 1\nLine 2\nLine 3\nLine 4\nEnd")
        (new "Start\nModified 1\nModified 2\nModified 3\nModified 4\nEnd")
        (expected "Start\n<<<<<<< HEAD\nLine 1\nLine 2\nLine 3\nLine 4\n=======\nModified 1\nModified 2\nModified 3\nModified 4\n>>>>>>> Relysium\nEnd"))
    (should (string= (relysium-diff-string original new) expected))))

(ert-deftest relysium-diff-string-test-whitespace-handling ()
  "Test handling of whitespace differences."
  (let ((original "Line 1\n  Line 2\nLine 3")
        (new "Line 1\nLine 2\nLine 3"))
    (should (string-match-p "<<<<<<< HEAD" (relysium-diff-string original new)))))

(ert-deftest relysium-diff-string-test-trailing-newlines ()
  "Test handling of trailing newlines."
  (let ((original "Line 1\nLine 2\n")
        (new "Line 1\nLine 2"))
    (should (string-match-p "<<<<<<< HEAD" (relysium-diff-string original new)))))

;; Run all tests
(defun relysium-diff-string-run-tests ()
  "Run all relysium-diff-string tests."
  (interactive)
  (ert-run-tests-interactively "^relysium-diff-string-test-"))

(ert-run-tests-interactively "^relysium-diff-string-test-")

(provide 'relysium-diff-tests)
;;; relysium-diff-tests.el ends here
