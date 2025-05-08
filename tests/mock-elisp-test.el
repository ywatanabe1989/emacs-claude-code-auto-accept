;; -*- coding: utf-8; lexical-binding: t -*-
;; Author: ywatanabe
;; Timestamp: <2025-05-08 20:40:20>
;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/mock-elisp-test.el

;;; Commentary:
;; Mock implementation for elisp tests to generate sample reports with failure data.

;;; Code:
(require 'cl-lib)
(require 'format-spec)

(defun ecc-elisp-test-mock-report (&optional output-file)
  "Generate a mock test report for demonstration purposes.
Save the report to OUTPUT-FILE."
  (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (mock-files '("test-ecc-buffer.el" "test-ecc-history.el" "test-ecc-send.el" 
                      "test-ecc-repository.el" "test-ecc-template-cache.el"))
         (all-tests '())
         (test-count 60)  ;; Total number of mock tests
         (report-file (replace-regexp-in-string 
                     "\\.org$" 
                     (format "-%s-%d-PERCENT.org" timestamp 85)
                     (or output-file "ELISP-TEST-REPORT.org"))))
    
    ;; Create mock test data
    (dolist (file mock-files)
      (let ((full-path (expand-file-name file "tests/")))
        (dotimes (i (/ test-count (length mock-files)))
          (let ((test-name (format "test-%s-%d" 
                                 (replace-regexp-in-string 
                                  "test-" ""
                                  (file-name-sans-extension file))
                                 (1+ i))))
            (push (cons full-path test-name) all-tests)))))
    
    (let* ((num-tests (length all-tests))
           ;; Distribute status among tests
           (num-fail (/ num-tests 10))      ;; 10% failures
           (num-timeout (/ num-tests 15))   ;; ~7% timeouts
           (num-skip (/ num-tests 20))      ;; 5% skipped
           
           ;; Select tests for each status
           (failing-tests (cl-subseq all-tests 0 num-fail))
           (timeout-tests (cl-subseq all-tests num-fail (+ num-fail num-timeout)))
           (skipped-tests (cl-subseq all-tests 
                                    (+ num-fail num-timeout) 
                                    (+ num-fail num-timeout num-skip)))
           (passing-tests (cl-subseq all-tests (+ num-fail num-timeout num-skip)))
           
           ;; Calculate summary numbers
           (num-passed (length passing-tests))
           (num-failed (length failing-tests))
           (num-timeout (length timeout-tests))
           (num-skipped (length skipped-tests))
           (success-rate (* 100.0 (/ (float num-passed) num-tests))))
      
      ;; Generate detailed report
      (with-temp-buffer
        (insert "#+TITLE: Elisp Test Report\n")
        (insert "#+AUTHOR: ywatanabe\n")
        (insert "#+DATE: ")
        (insert (format-time-string "%Y-%m-%d %H:%M:%S"))
        (insert " Created by ECC-ELISP-TEST\n\n")
        
        ;; Summary section
        (insert "* Test Results Summary\n\n")
        (insert (format "- Passed: %d\n" num-passed))
        (insert (format "- Failed: %d\n" num-failed))
        (insert (format "- Skipped: %d\n" num-skipped))
        (insert (format "- Timeout (= 10 s): %d\n" num-timeout))
        (insert "- Duplicates: 0\n")
        (insert (format "- Total: %d\n" num-tests))
        (insert "- Total Time: 2.35 seconds\n")
        (insert (format "- Success Rate: %.1f%%\n\n" success-rate))
        
        ;; Group passed tests by file
        (when passing-tests
          (insert (format "* Passed Tests (%d)\n" num-passed))
          (let ((files-hash (make-hash-table :test 'equal)))
            (dolist (test passing-tests)
              (let ((file (car test))
                    (test-name (cdr test)))
                (push test-name
                      (gethash file files-hash '()))))
            
            ;; Output by file groups
            (maphash
             (lambda (file test-names)
               (let ((filename (file-name-nondirectory file))
                     (rel-path file))
                 (insert (format "** %s (%d tests)\n" filename (length test-names)))
                 (dolist (test-name (nreverse test-names))
                   (insert (format "- [[file:%s::%s][%s]]\n"
                                  rel-path test-name test-name)))))
             files-hash)))

        ;; Group failed tests by file
        (when failing-tests
          (insert (format "* Failed Tests (%d)\n" num-failed))
          (let ((files-hash (make-hash-table :test 'equal)))
            (dolist (test failing-tests)
              (let ((file (car test))
                    (test-name (cdr test)))
                (push test-name
                      (gethash file files-hash '()))))
            
            ;; Output by file groups
            (maphash
             (lambda (file test-names)
               (let ((filename (file-name-nondirectory file))
                     (rel-path file))
                 (insert (format "** %s (%d tests)\n" filename (length test-names)))
                 (dolist (test-name (nreverse test-names))
                   (insert (format "- [[file:%s::%s][%s]]\n"
                                  rel-path test-name test-name))
                   
                   ;; Add one of several mock error types
                   (let ((error-type (% (sxhash test-name) 4)))
                     (insert "  + Error details:\n")
                     (insert "    Selector: " test-name "\n")
                     (insert "    Passed:  0\n")
                     (insert "    Failed:  1 (1 unexpected)\n")
                     (insert "    Skipped: 0\n")
                     (insert "    Total:   1/1\n\n")
                     (insert "    F\n\n")
                     (insert "    F " test-name "\n")
                     (pcase error-type
                       (0 (insert "        (wrong-type-argument numberp nil)\n\n"))
                       (1 (insert "        (void-function some-undefined-function)\n\n"))
                       (2 (insert "        Failed: Expected t but got nil\n\n"))
                       (_ (insert "        (error \"Test failed with unexpected error\")\n\n"))
                       )))))
             files-hash)))

        ;; Group timed-out tests by file
        (when timeout-tests
          (insert (format "* Timeout Tests (= 10 s) (%d)\n" num-timeout))
          (let ((files-hash (make-hash-table :test 'equal)))
            (dolist (test timeout-tests)
              (let ((file (car test))
                    (test-name (cdr test)))
                (push test-name
                      (gethash file files-hash '()))))
            
            ;; Output by file groups
            (maphash
             (lambda (file test-names)
               (let ((filename (file-name-nondirectory file))
                     (rel-path file))
                 (insert (format "** %s (%d tests)\n" filename (length test-names)))
                 (dolist (test-name (nreverse test-names))
                   (insert (format "- [[file:%s::%s][%s]]\n"
                                  rel-path test-name test-name))
                   ;; Add timeout details
                   (insert "  + Timeout details:\n")
                   (insert "    TIMEOUT: Test exceeded time limit of 10s\n\n")
                   )))
             files-hash)))

        ;; Group skipped tests by file
        (when skipped-tests
          (insert (format "* Skipped Tests (%d)\n" num-skipped))
          (let ((files-hash (make-hash-table :test 'equal)))
            (dolist (test skipped-tests)
              (let ((file (car test))
                    (test-name (cdr test)))
                (push test-name
                      (gethash file files-hash '()))))
            
            ;; Output by file groups
            (maphash
             (lambda (file test-names)
               (let ((filename (file-name-nondirectory file))
                     (rel-path file))
                 (insert (format "** %s (%d tests)\n" filename (length test-names)))
                 (dolist (test-name (nreverse test-names))
                   (insert (format "- [[file:%s::%s][%s]]\n"
                                  rel-path test-name test-name))
                   ;; Add skip details
                   (insert "  + Skip details:\n")
                   (insert "    NOT-FOUND: Test not found\n\n")
                   )))
             files-hash)))
        
        (write-region (point-min) (point-max) report-file))
      
      (message "TEST-SUMMARY: Ran %d tests from %d files. Report saved to %s" 
               num-tests (length mock-files) report-file)
      report-file)))

(provide 'mock-elisp-test)
;;; mock-elisp-test.el ends here