#!/bin/bash
# -*- coding: utf-8 -*-
# Timestamp: "2025-05-08 15:29:17 (ywatanabe)"
# File: ./run-tests.sh

THIS_DIR="$(cd $(dirname ${BASH_SOURCE[0]}) && pwd)"
LOG_PATH="$THIS_DIR/.$(basename $0).log"
touch "$LOG_PATH" >/dev/null 2>&1


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
REQUIRE_PACKAGES=(org cl-lib)

# Color definitions
YELLOW='\033[1;33m'
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

usage() {
    echo "Usage: $0 [-d|--debug] [-h|--help] [-s|--single FILE]"
    echo "Runs tests for this project"
    echo
    echo "Options:"
    echo "  -d, --debug     Enable debug output."
    echo "  -h, --help      Display this help message"
    echo "  -s, --single    Run a single test file"
    exit 1
}

main() {
    local debug=false
    local single_file=""

    while [[ $# -gt 0 ]]; do
        case $1 in
            -d|--debug) debug=true; shift ;;
            -h|--help) usage ;;
            -s|--single)
                single_file="$2"
                if [[ ! -f "$single_file" ]]; then
                    echo "Error: Test file $single_file not found."
                    exit 1
                fi
                shift 2
                ;;
            *) echo "Unknown option: $1"; usage ;;
        esac
    done

    local l_args=""
    local loaded_files=()

    # Check if we're running in single-file mode
    if [[ -n "$single_file" ]]; then
        echo "Running single test file: $single_file"
        # Only load mock vterm and the specified test file
        test_files=("$THIS_DIR"/tests/test-mock-vterm.el "$single_file")

        for file in "${test_files[@]}"; do
            if [[ -f "$file" ]]; then
                if [ "$debug" = true ]; then
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
                if [ "$debug" = true ]; then
                    echo "Loading source: $file"
                fi
                l_args="$l_args -l $file"
                loaded_files+=("$file")
            fi
        done

        # Load mock files first
        for file in "${MOCK_FILES[@]}"; do
            if [[ -f "$file" ]]; then
                if [ "$debug" = true ]; then
                    echo "Loading mock: $file"
                fi
                l_args="$l_args -l $file"
                loaded_files+=("$file")
            fi
        done

        # Load test files
        for file in "${TEST_FILES[@]}"; do
            if [[ -f "$file" ]]; then
                if [ "$debug" = true ]; then
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
    # load_paths="$load_paths(message \"Load paths: %s\" load-path) "

    # if [ "${#loaded_files[@]}" -eq 0 ]; then
    #     echo "No test files found"
    #     exit 1
    # fi

    # # Create require expressions
    # local require_exprs=""
    # for pkg in "${REQUIRE_PACKAGES[@]}"; do
    #     require_exprs="$require_exprs(require '$pkg)"
    # done

    # # Check if load paths were correctly added
    # if [ "$debug" = true ]; then
    #     echo "Load paths: $load_paths"
    #     echo "Files to load: $l_args"
    # fi

    # # Run emacs with the specified files
    # local exit_code=0
    # emacs -batch \
    #       -l ert \
    #       -l package \
    #       --eval "(progn \
    # (package-initialize) \
    # $load_paths \
    # $require_exprs \
    # (require 'ert))" \
    #       $l_args \
    #       -f ert-run-tests-batch-and-exit || exit_code=$?

    # In the emacs command section, update with error redirection
    emacs -batch \
          -l ert \
          -l package \
          --eval "(progn \
    (package-initialize) \
    $load_paths \
    $require_exprs \
    (require 'ert) \
    (setq load-file-rep-p nil) \
    (setq message-log-max nil) \
    (defun display-warning (&rest _) nil) \
    (defadvice load (around quiet-load activate) \
      (let ((load-read-function \
            (lambda (file) \
              (let ((inhibit-message t)) \
                (load file nil t)))) \
            (inhibit-message t)) \
        ad-do-it)))" \
          $l_args \
          -f ert-run-tests-batch-and-exit 2> >(grep -v "load-path\|Loading\|Cannot open" >&2) || exit_code=$?

    # Return the exit code
    return $exit_code
}

check_global_success() {
    local exit_code=$1
    local log_content=$(cat "$LOG_PATH")

    # Check for any type of module loading errors
    if echo "$log_content" | grep -q -E "Cannot open load file|Loading file.*failed to provide feature|Package.*(not found|not available)"; then
        echo -e "${RED}⨯ Module loading error detected!${NC}"
        echo "The following modules failed to load:"
        echo "$log_content" | grep -E -A 2 "Cannot open load file|Loading file.*failed to provide feature|Package.*(not found|not available)"
        return 1
    fi

    # Check for test failures
    if grep "Ran [0-9]\\+ tests" "$LOG_PATH" | tail -1 | grep -v "0 unexpected" > /dev/null; then
        echo -e "${RED}⨯ Tests FAILED!${NC}"
        # Only show actual test failures, not expected "Aborted" messages
        grep -A 3 -B 3 "FAILED:" "$LOG_PATH" --color=always || true
        return 1
    elif [ $exit_code -ne 0 ]; then
        echo -e "${RED}⨯ Tests did not complete successfully (exit code: $exit_code)${NC}"
        return 1
    else
        # Only show success if we actually ran some tests
        if grep -q "Ran [0-9]\\+ tests" "$LOG_PATH"; then
            echo -e "${GREEN}✓ All tests PASSED!${NC}"
            grep "Ran [0-9]\\+ tests" "$LOG_PATH" | tail -1
            return 0
        else
            echo -e "${YELLOW}⚠ No tests were run!${NC}"
            return 1
        fi
    fi
}

# Run the main function and store its exit code
main "$@" 2>&1 | tee "$LOG_PATH"
exit_code=$?

# Check success based on the log file and exit code
check_global_success $exit_code

# EOF