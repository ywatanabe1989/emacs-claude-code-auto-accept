#!/bin/bash
# -*- coding: utf-8 -*-
# Run tests for Emacs projects
# Timestamp: "2025-05-08 22:05:00 (ywatanabe)"
# File: ./run-tests-elisp.sh

THIS_DIR="$(cd $(dirname ${BASH_SOURCE[0]}) && pwd)"
SCRIPT_NAME="$(basename "$0")"
LOG_PATH="$THIS_DIR/.$(basename $0).log"
touch "$LOG_PATH" >/dev/null 2>&1

# Color definitions
YELLOW='\033[1;33m'
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

# Default values
DEBUG=false
SINGLE_FILE=""
REPORT_MODE=false
REPORT_FILE="$THIS_DIR/ELISP-TEST-REPORT.org"

usage() {
    echo "Usage: $0 [options]"
    echo "Run tests for Emacs Lisp projects"
    echo
    echo "Options:"
    echo "  -d, --debug       Enable debug output"
    echo "  -s, --single FILE Run a single test file"
    echo "  -r, --report      Generate detailed test report"
    echo "  -o, --output FILE Path for the report output (default: ./ELISP-TEST-REPORT.org)"
    echo "  -h, --help        Display this help message"
    echo
    echo "Examples:"
    echo "  $0                Run all tests in default mode"
    echo "  $0 -d -s tests/test-ecc-buffer.el    Run a single test file with debug output"
    echo "  $0 -r             Run tests and generate detailed report"
    echo
    echo "Note: For advanced reporting options, use elisp-test.sh directly"
    exit 1
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -d|--debug)
            DEBUG=true
            shift
            ;;
        -s|--single)
            SINGLE_FILE="$2"
            if [[ ! -f "$SINGLE_FILE" ]]; then
                echo -e "${RED}Error: Test file $SINGLE_FILE not found${NC}"
                exit 1
            fi
            shift 2
            ;;
        -r|--report)
            REPORT_MODE=true
            shift
            ;;
        -o|--output)
            REPORT_FILE="$2"
            shift 2
            ;;
        -h|--help)
            usage
            ;;
        *)
            echo -e "${RED}Unknown option: $1${NC}"
            usage
            ;;
    esac
done

# Choose which test script to use based on mode
if [[ "$REPORT_MODE" = true ]]; then
    echo -e "${YELLOW}Running tests with detailed reporting...${NC}"
    
    # Build command for elisp-test.sh
    ELISP_TEST_CMD="$THIS_DIR/elisp-test.sh"
    ELISP_TEST_ARGS=""
    
    if [[ "$DEBUG" = true ]]; then
        ELISP_TEST_ARGS="$ELISP_TEST_ARGS -d"
    fi
    
    if [[ -n "$SINGLE_FILE" ]]; then
        ELISP_TEST_ARGS="$ELISP_TEST_ARGS -s $SINGLE_FILE"
    fi
    
    if [[ -n "$REPORT_FILE" ]]; then
        ELISP_TEST_ARGS="$ELISP_TEST_ARGS -o $REPORT_FILE"
    fi
    
    # Execute elisp-test.sh
    echo "Executing: $ELISP_TEST_CMD $ELISP_TEST_ARGS"
    $ELISP_TEST_CMD $ELISP_TEST_ARGS
    exit_code=$?
    
    if [[ $exit_code -eq 0 ]]; then
        echo -e "${GREEN}✓ Tests completed successfully with detailed report${NC}"
    else
        echo -e "${RED}⨯ Tests failed with detailed report${NC}"
    fi
    
    exit $exit_code
else
    # Legacy mode - use the original run-tests.sh approach
    echo -e "${YELLOW}Running tests in standard mode...${NC}"
    
    # Define all load paths for the project
    PATHS=(
        "$THIS_DIR"
        "$THIS_DIR/tests/"
        "$THIS_DIR/ecc-buffer/"     # Buffer management
        "$THIS_DIR/ecc-state/"      # State management 
        "$THIS_DIR/ecc-template/"   # Template system
    )
    SOURCE_FILES=("$THIS_DIR"/*.el)
    # Load mock vterm implementation first
    MOCK_FILES=("$THIS_DIR"/tests/test-mock-vterm.el)
    # Only load the test files with ecc- prefix to avoid duplicate definitions
    TEST_FILES=("$THIS_DIR"/tests/test-ecc-*.el)
    
    # Process arguments and load test files
    local l_args=""
    local loaded_files=()
    
    # Check if we're running in single-file mode
    if [[ -n "$SINGLE_FILE" ]]; then
        echo "Running single test file: $SINGLE_FILE"
        # Only load mock vterm and the specified test file
        test_files=("$THIS_DIR"/tests/test-mock-vterm.el "$SINGLE_FILE")
        
        for file in "${test_files[@]}"; do
            if [[ -f "$file" ]]; then
                if [ "$DEBUG" = true ]; then
                    echo "Loading test: $file"
                fi
                l_args="$l_args -l $file"
                loaded_files+=("$file")
            fi
        done
    else
        # Load source files
        for file in "${SOURCE_FILES[@]}"; do
            if [[ -f "$file" && ! "$file" =~ "test-" ]]; then
                if [ "$DEBUG" = true ]; then
                    echo "Loading source: $file"
                fi
                l_args="$l_args -l $file"
                loaded_files+=("$file")
            fi
        done
        
        # Load mock files first
        for file in "${MOCK_FILES[@]}"; do
            if [[ -f "$file" ]]; then
                if [ "$DEBUG" = true ]; then
                    echo "Loading mock: $file"
                fi
                l_args="$l_args -l $file"
                loaded_files+=("$file")
            fi
        done
        
        # Load test files
        for file in "${TEST_FILES[@]}"; do
            if [[ -f "$file" ]]; then
                if [ "$DEBUG" = true ]; then
                    echo "Loading test: $file"
                fi
                l_args="$l_args -l $file"
                loaded_files+=("$file")
            fi
        done
    fi
    
    # Create load path expressions
    load_paths=""
    for path in "${PATHS[@]}"; do
        if [[ -d "$path" ]]; then
            load_paths="$load_paths(add-to-list 'load-path \"$path\") "
        fi
    done
    
    # Add a safety check to ensure all subdirectories are properly loaded
    load_paths="$load_paths(setq features nil) "  # Reset features for testing
    load_paths="$load_paths(add-to-list 'load-path nil t) "  # Dummy operation to avoid error
    
    # In the emacs command section, update with error redirection
    emacs -batch \
          -l ert \
          -l package \
          --eval "(progn \
    (package-initialize) \
    $load_paths \
    (require 'ert) \
    (setq load-file-rep-p nil) \
    (setq message-log-max nil) \
    (defun display-warning (&rest _) nil) \
    (defadvice load (around quiet-load activate) \
      (let ((inhibit-message t)) \
        ad-do-it)))" \
          $l_args \
          -f ert-run-tests-batch-and-exit 2> >(grep -v "load-path\|Loading\|Cannot open" >&2) || exit_code=$?
    
    # Check the results
    local log_content=$(cat "$LOG_PATH")
    
    # Check for any type of module loading errors
    if echo "$log_content" | grep -q -E "Cannot open load file|Loading file.*failed to provide feature|Package.*(not found|not available)"; then
        echo -e "${RED}⨯ Module loading error detected!${NC}"
        echo "The following modules failed to load:"
        echo "$log_content" | grep -E -A 2 "Cannot open load file|Loading file.*failed to provide feature|Package.*(not found|not available)"
        exit 1
    fi
    
    # Check for test failures
    if grep "Ran [0-9]\\+ tests" "$LOG_PATH" | tail -1 | grep -v "0 unexpected" > /dev/null; then
        echo -e "${RED}⨯ Tests FAILED!${NC}"
        # Only show actual test failures, not expected "Aborted" messages
        grep -A 3 -B 3 "FAILED:" "$LOG_PATH" --color=always || true
        exit 1
    elif [ $exit_code -ne 0 ]; then
        echo -e "${RED}⨯ Tests did not complete successfully (exit code: $exit_code)${NC}"
        exit 1
    else
        # Only show success if we actually ran some tests
        if grep -q "Ran [0-9]\\+ tests" "$LOG_PATH"; then
            echo -e "${GREEN}✓ All tests PASSED!${NC}"
            grep "Ran [0-9]\\+ tests" "$LOG_PATH" | tail -1
            exit 0
        else
            echo -e "${YELLOW}⚠ No tests were run!${NC}"
            exit 1
        fi
    fi
fi

# EOF