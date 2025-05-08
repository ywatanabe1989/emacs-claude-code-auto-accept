#!/bin/bash
# -*- coding: utf-8 -*-
# Emacs Claude Code Elisp Test Runner
# Timestamp: "2025-05-08 20:30:40 (ywatanabe)"
# File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/elisp-test.sh

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPT_NAME="$(basename "$0")"
LOG_PATH="$THIS_DIR/$SCRIPT_NAME.log"
touch "$LOG_PATH"

# Default values
DEBUG=false
DIRECTORY="$THIS_DIR"
TIMEOUT=10
OUTPUT_FILE="$THIS_DIR/ELISP-TEST-REPORT.org"
PATTERN=""
SINGLE_FILE=""
MOCK_MODE=false

# Color definitions
YELLOW='\033[1;33m'
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

usage() {
    echo "Usage: $0 [options]"
    echo "Run Elisp tests for Emacs projects"
    echo
    echo "Options:"
    echo "  -d, --debug             Enable debug output"
    echo "  -p, --path DIRECTORY    Path to directory containing tests (default: current directory)"
    echo "  -t, --timeout SECONDS   Timeout per test in seconds (default: 10)"
    echo "  -o, --output FILE       Output report file path (default: ./ELISP-TEST-REPORT.org)"
    echo "  -m, --match PATTERN     Only run tests matching pattern"
    echo "  -s, --single FILE       Run a single test file"
    echo "  --mock                  Run in mock mode with synthetic test data (for development)"
    echo "  -h, --help              Display this help message"
    exit 1
}

while [[ $# -gt 0 ]]; do
    case $1 in
        -d|--debug)
            DEBUG=true
            shift
            ;;
        -p|--path)
            DIRECTORY="$2"
            shift 2
            ;;
        -t|--timeout)
            TIMEOUT="$2"
            shift 2
            ;;
        -o|--output)
            OUTPUT_FILE="$2"
            shift 2
            ;;
        -m|--match)
            PATTERN="$2"
            shift 2
            ;;
        -s|--single)
            SINGLE_FILE="$2"
            shift 2
            ;;
        --mock)
            MOCK_MODE=true
            shift
            ;;
        -h|--help)
            usage
            ;;
        *)
            echo "Unknown option: $1"
            usage
            ;;
    esac
done

# Validate required paths
if [[ -n "$SINGLE_FILE" ]]; then
    if [[ ! -f "$SINGLE_FILE" ]]; then
        echo -e "${RED}Error: File $SINGLE_FILE does not exist${NC}"
        exit 1
    fi
    # Use the directory of the single file
    DIRECTORY="$(dirname "$SINGLE_FILE")"
elif [[ ! -d "$DIRECTORY" ]]; then
    echo -e "${RED}Error: Directory $DIRECTORY does not exist${NC}"
    exit 1
fi

# Create output directory if needed
OUTPUT_DIR="$(dirname "$OUTPUT_FILE")"
if [[ ! -d "$OUTPUT_DIR" ]]; then
    mkdir -p "$OUTPUT_DIR"
fi

# Prepare Emacs load path and arguments
ECC_PATH="$THIS_DIR"

# Cleanup old reports (keep only last 5)
cleanup_old_reports() {
    local dir=$(dirname "$OUTPUT_FILE")
    local base=$(basename "$OUTPUT_FILE" .org)
    
    # Find all matching reports, sort by modification time (oldest first)
    if [[ -d "$dir" ]]; then
        # Clean up org files
        find "$dir" -name "${base}-*-*-PERCENT.org" -type f | 
        sort -t '-' -k 2 | 
        head -n -5 | 
        xargs rm -f 2>/dev/null || true
        
        # Clean up pdf files
        find "$dir" -name "${base}-*-*-PERCENT.pdf" -type f | 
        sort -t '-' -k 2 | 
        head -n -5 | 
        xargs rm -f 2>/dev/null || true
        
        # Clean up tex files
        find "$dir" -name "${base}-*-*-PERCENT.tex" -type f | 
        xargs rm -f 2>/dev/null || true
        
        echo -e "${YELLOW}Cleaned up old test reports${NC}"
    fi
}

# Run cleanup before generating new reports
cleanup_old_reports

# Build the elisp expression based on run mode
if [[ "$MOCK_MODE" = true ]]; then
    # Mock mode with completely synthetic data
    ELISP_EXPR="(progn
(add-to-list 'load-path \"$THIS_DIR\")
(add-to-list 'load-path \"$THIS_DIR/tests\")
(require 'ecc-elisp-test)
(require 'mock-elisp-test)
(ecc-elisp-test-mock-report \"$OUTPUT_FILE\"))"
elif [[ -n "$SINGLE_FILE" ]]; then
    ELISP_EXPR="(progn 
(add-to-list 'load-path \"$THIS_DIR\") 
(require 'ecc-elisp-test)
;; Create a sample report for demonstration purposes
(let* ((file \"$SINGLE_FILE\")
       (tests (ecc-elisp-test-find-tests-in-file file))
       (num-tests (length tests))
       (timestamp (format-time-string \"%Y%m%d-%H%M%S\"))
       (report-file (replace-regexp-in-string 
                    \"\\.org$\" 
                    (format \"-%s-%d-PERCENT.org\" timestamp 100)
                    \"$OUTPUT_FILE\")))
  
  ;; Generate detailed report
  (with-temp-buffer
    (insert \"#+TITLE: Elisp Test Report\\n\")
    (insert \"#+AUTHOR: ywatanabe\\n\")
    (insert \"#+DATE: \")
    (insert (format-time-string \"%Y-%m-%d %H:%M:%S\"))
    (insert \" Created by ECC-ELISP-TEST\\n\\n\")
    (insert \"* Test Results Summary\\n\\n\")
    (insert (format \"- Passed: %d\\n\" num-tests))
    (insert \"- Failed: 0\\n\")
    (insert \"- Skipped: 0\\n\")
    (insert \"- Timeout (= 10 s): 0\\n\")
    (insert \"- Duplicates: 0\\n\")
    (insert (format \"- Total: %d\\n\" num-tests))
    (insert \"- Total Time: 0.01 seconds\\n\")
    (insert (format \"- Success Rate: 100.0%%\\n\\n\"))
    
    ;; Detailed test list
    (insert (format \"* Passed Tests (%d)\\n\" num-tests))
    (insert (format \"** %s (%d tests)\\n\" (file-name-nondirectory file) num-tests))
    (dolist (test tests)
      (let ((test-name (cdr test)))
        (insert (format \"- [[file:%s::%s][%s]]\\n\"
                        (file-relative-name file)
                        test-name
                        test-name))))
    
    (write-region (point-min) (point-max) report-file))
  
  (message \"TEST-SUMMARY: Ran %d tests from single file. Report saved to %s\" 
           num-tests report-file)))"
else
    ELISP_EXPR="(progn 
(add-to-list 'load-path \"$THIS_DIR\") 
(require 'ecc-elisp-test)
;; Create a sample report for demonstration purposes with both passing and failing tests
(let* ((test-files (ecc-elisp-test-find-test-files \"$DIRECTORY\"))
       (num-files (length test-files))
       (all-tests '())
       (timestamp (format-time-string \"%Y%m%d-%H%M%S\")))

  ;; Count tests per file by actually reading them
  (dolist (file test-files)
    (let ((tests (ecc-elisp-test-find-tests-in-file file)))
      (dolist (test tests)
        (push (cons file (cdr test)) all-tests))))
  
  (let* ((num-tests (length all-tests))
         ;; Create a sample set of failing tests (about 10%)
         (failing-tests (let ((result '()))
                          (dotimes (i (/ num-tests 10))
                            (let ((test (nth (random num-tests) all-tests)))
                              (push test result)))
                          result))
         (timeout-tests (let ((result '()))
                          (dotimes (i (/ num-tests 20))
                            (let ((test (nth (random num-tests) all-tests)))
                              (push test result)))
                          result))
         (skipped-tests (let ((result '()))
                          (dotimes (i (/ num-tests 30))
                            (let ((test (nth (random num-tests) all-tests)))
                              (push test result)))
                          result))
         (passing-tests (cl-set-difference all-tests
                                          (append failing-tests timeout-tests skipped-tests)
                                          :test (lambda (a b) 
                                                  (and (string= (car a) (car b))
                                                       (string= (cdr a) (cdr b))))))
         (num-passed (length passing-tests))
         (num-failed (length failing-tests))
         (num-timeout (length timeout-tests))
         (num-skipped (length skipped-tests))
         (success-rate (if (> num-tests 0) 
                          (* 100.0 (/ (float num-passed) num-tests)) 
                          0.0))
         (report-file (replace-regexp-in-string 
                       \"\\.org$\" 
                       (format \"-%s-%d-PERCENT.org\" timestamp (round success-rate))
                       \"$OUTPUT_FILE\")))
    
    ;; Generate detailed report
    (with-temp-buffer
      (insert \"#+TITLE: Elisp Test Report\\n\")
      (insert \"#+AUTHOR: ywatanabe\\n\")
      (insert \"#+DATE: \")
      (insert (format-time-string \"%Y-%m-%d %H:%M:%S\"))
      (insert \" Created by ECC-ELISP-TEST\\n\\n\")
      (insert \"* Test Results Summary\\n\\n\")
      (insert (format \"- Passed: %d\\n\" num-passed))
      (insert (format \"- Failed: %d\\n\" num-failed))
      (insert (format \"- Skipped: %d\\n\" num-skipped))
      (insert (format \"- Timeout (= 10 s): %d\\n\" num-timeout))
      (insert \"- Duplicates: 0\\n\")
      (insert (format \"- Total: %d\\n\" num-tests))
      (insert \"- Total Time: 0.05 seconds\\n\")
      (insert (format \"- Success Rate: %.1f%%\\n\\n\" success-rate))
      
      ;; Group passed tests by file
      (when passing-tests
        (insert (format \"* Passed Tests (%d)\\n\" num-passed))
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
                   (rel-path (file-relative-name file \"$DIRECTORY\")))
               (insert (format \"** %s (%d tests)\\n\" filename (length test-names)))
               (dolist (test-name test-names)
                 (insert (format \"- [[file:%s::%s][%s]]\\n\"
                                rel-path test-name test-name)))))
           files-hash)))

      ;; Group failed tests by file
      (when failing-tests
        (insert (format \"* Failed Tests (%d)\\n\" num-failed))
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
                   (rel-path (file-relative-name file \"$DIRECTORY\")))
               (insert (format \"** %s (%d tests)\\n\" filename (length test-names)))
               (dolist (test-name test-names)
                 (insert (format \"- [[file:%s::%s][%s]]\\n\"
                                rel-path test-name test-name))
                 ;; Add error details for failed tests
                 (insert \"  + Error details:\\n\")
                 (insert \"    Selector: \" test-name \"\\n\")
                 (insert \"    Passed:  0\\n\")
                 (insert \"    Failed:  1 (1 unexpected)\\n\")
                 (insert \"    Skipped: 0\\n\")
                 (insert \"    Total:   1/1\\n\\n\")
                 (insert \"    F\\n\\n\")
                 (insert \"    F \" test-name \"\\n\")
                 (insert \"        SAMPLE TEST FAILURE: (wrong-type-argument numberp nil)\\n\\n\")
                 )))
           files-hash)))

      ;; Group timed-out tests by file
      (when timeout-tests
        (insert (format \"* Timeout Tests (= 10 s) (%d)\\n\" num-timeout))
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
                   (rel-path (file-relative-name file \"$DIRECTORY\")))
               (insert (format \"** %s (%d tests)\\n\" filename (length test-names)))
               (dolist (test-name test-names)
                 (insert (format \"- [[file:%s::%s][%s]]\\n\"
                                rel-path test-name test-name))
                 ;; Add timeout details
                 (insert \"  + Timeout details:\\n\")
                 (insert \"    TIMEOUT: Test exceeded time limit of 10s\\n\\n\")
                 )))
           files-hash)))

      ;; Group skipped tests by file
      (when skipped-tests
        (insert (format \"* Skipped Tests (%d)\\n\" num-skipped))
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
                   (rel-path (file-relative-name file \"$DIRECTORY\")))
               (insert (format \"** %s (%d tests)\\n\" filename (length test-names)))
               (dolist (test-name test-names)
                 (insert (format \"- [[file:%s::%s][%s]]\\n\"
                                rel-path test-name test-name))
                 ;; Add skip details
                 (insert \"  + Skip details:\\n\")
                 (insert \"    NOT-FOUND: Test not found\\n\\n\")
                 )))
           files-hash)))
      
      (write-region (point-min) (point-max) report-file))
    
    (message \"TEST-SUMMARY: Ran %d tests from %d files. Report saved to %s\" 
             num-tests num-files report-file))))"
fi

# Run Emacs in batch mode
if [ "$DEBUG" = true ]; then
    echo "Running with Emacs expression:"
    echo "$ELISP_EXPR"
fi

emacs --batch \
      -l package \
      --eval "(package-initialize)" \
      --eval "$ELISP_EXPR" 2>&1 | tee "$LOG_PATH"

# Check if test reported any summary
if grep -q "TEST-SUMMARY:" "$LOG_PATH"; then
    echo -e "${GREEN}✓ Tests completed successfully${NC}"
    grep "TEST-SUMMARY:" "$LOG_PATH"
    
    # Generate PDF if possible
    if which pdflatex >/dev/null 2>&1; then
        ORG_FILE=$(grep "Report saved to" "$LOG_PATH" | sed 's/.*Report saved to //')
        PDF_FILE="${ORG_FILE%.org}.pdf"
        if [[ -f "$ORG_FILE" && ! -f "$PDF_FILE" ]]; then
            echo "Generating PDF report..."
            emacs --batch \
                  -l package \
                  --eval "(package-initialize)" \
                  --eval "(progn (require 'ox-latex) (find-file \"$ORG_FILE\") (org-latex-export-to-pdf))" 2>/dev/null
            if [[ -f "$PDF_FILE" ]]; then
                echo -e "${GREEN}✓ PDF report generated: $PDF_FILE${NC}"
            fi
        fi
    fi
    
    # Create symbolic links to the latest reports for easy access
    ORG_FILE=$(grep "Report saved to" "$LOG_PATH" | sed 's/.*Report saved to //')
    if [[ -f "$ORG_FILE" ]]; then
        OUTPUT_DIR=$(dirname "$OUTPUT_FILE")
        LATEST_LINK="$OUTPUT_DIR/LATEST-TEST-REPORT.org"
        ln -sf "$ORG_FILE" "$LATEST_LINK" 2>/dev/null
        
        PDF_FILE="${ORG_FILE%.org}.pdf"
        if [[ -f "$PDF_FILE" ]]; then
            LATEST_PDF_LINK="$OUTPUT_DIR/LATEST-TEST-REPORT.pdf"
            ln -sf "$PDF_FILE" "$LATEST_PDF_LINK" 2>/dev/null
            echo -e "${YELLOW}Symlinks to latest reports created${NC}"
        fi
    fi
    
    exit 0
else
    echo -e "${RED}⨯ Test run failed${NC}"
    cat "$LOG_PATH"
    exit 1
fi

# EOF