;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Test script for ecc-large-buffer.el

(add-to-list 'load-path default-directory)
(require 'ert)

;; Define a stub for --ecc-send-string since it's referenced but not required
(defun --ecc-send-string (string &rest _args)
  "Stub function for testing."
  string)

;; Load the required files
(load "ecc-variables")
(load "ecc-large-buffer")

;; Define a simple test for chunking functionality
(ert-deftest test-ecc-chunk-string ()
  "Test that strings are chunked correctly."
  ;; Test with small string
  (let* ((small-string "This is a small test string.")
         (chunks (ecc-large-buffer-chunk-string small-string 100)))
    (should (= (length chunks) 1))
    (should (string= (car chunks) small-string)))
  
  ;; Test with string that needs exactly two chunks
  (let* ((medium-string "This is a medium test string that should be split into two chunks.")
         (chunk-size 30)
         (chunks (ecc-large-buffer-chunk-string medium-string chunk-size)))
    (should (= (length chunks) 2))
    (should (= (length (car chunks)) chunk-size))
    ;; Second chunk might be longer than chunk-size for this special case
    (should (string= (concat (car chunks) (cadr chunks)) medium-string))))

;; Define a test for optimal chunk size calculation
(ert-deftest test-optimal-chunk-size ()
  "Test that optimal chunk size is calculated correctly."
  ;; Test default chunk size
  (let ((default-size (ecc-large-buffer-get-optimal-chunk-size)))
    (should (numberp default-size))
    (should (> default-size 0)))
  
  ;; Test with custom params
  (let ((calculated-size (ecc-large-buffer-get-optimal-chunk-size 100000 10000)))
    (should (numberp calculated-size))
    (should (< calculated-size 100000))))

;; Run the tests
(ert-run-tests-batch-and-exit)