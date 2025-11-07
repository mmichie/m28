#!/bin/bash
# Run official CPython regression tests from stdlib
# Tracks which tests M28 can pass from CPython's test suite

set -e

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Find M28 binary
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
M28_BIN="$SCRIPT_DIR/../bin/m28"

if [[ ! -f "$M28_BIN" ]]; then
    echo -e "${RED}Error: M28 binary not found at $M28_BIN${NC}"
    echo "Please run 'make build' first"
    exit 1
fi

# Detect Python stdlib location
PYTHON_STDLIB=""
if command -v python3 &> /dev/null; then
    PYTHON_STDLIB=$(python3 -c "import sysconfig; print(sysconfig.get_path('stdlib'))" 2>/dev/null)
fi

if [[ -z "$PYTHON_STDLIB" || ! -d "$PYTHON_STDLIB/test" ]]; then
    echo -e "${RED}Error: Could not find Python stdlib test directory${NC}"
    echo "Expected at: $PYTHON_STDLIB/test"
    exit 1
fi

echo -e "${BLUE}======================================${NC}"
echo -e "${BLUE}  CPython Regression Test Suite      ${NC}"
echo -e "${BLUE}======================================${NC}"
echo -e "Python stdlib: $PYTHON_STDLIB"
echo ""

# Test counters
TOTAL=0
PASSED=0
FAILED=0
SKIPPED=0
FAILED_TESTS=()

# Function to run a test
run_test() {
    local test_name=$1
    local expected_status=$2  # "pass", "fail", or "skip"
    local test_file="$PYTHON_STDLIB/test/${test_name}.py"

    TOTAL=$((TOTAL + 1))

    # Show test name with padding
    printf "%-45s" "  $test_name..."

    if [[ "$expected_status" == "skip" ]]; then
        echo -e "${YELLOW}⊘ skipped${NC}"
        SKIPPED=$((SKIPPED + 1))
        return
    fi

    if [[ ! -f "$test_file" ]]; then
        echo -e "${YELLOW}⊘ not found${NC}"
        SKIPPED=$((SKIPPED + 1))
        return
    fi

    # Run with timeout
    if timeout 30s "$M28_BIN" "$test_file" > /tmp/m28_regression_test.txt 2>&1; then
        if [[ "$expected_status" == "pass" ]]; then
            echo -e "${GREEN}✓ passed${NC}"
            PASSED=$((PASSED + 1))
        else
            echo -e "${YELLOW}✓ passed (unexpected!)${NC}"
            PASSED=$((PASSED + 1))
        fi
    else
        EXIT_CODE=$?
        if [[ "$expected_status" == "fail" ]]; then
            echo -e "${YELLOW}✗ failed (expected)${NC}"
            FAILED=$((FAILED + 1))
        else
            echo -e "${RED}✗ FAILED${NC}"
            FAILED=$((FAILED + 1))
            FAILED_TESTS+=("$test_name")
            # Show first error line
            head -n 3 /tmp/m28_regression_test.txt 2>/dev/null | sed 's/^/    /'
        fi
    fi
}

# Define test categories and expected status
# Format: test_name expected_status
# expected_status: "pass" (should pass), "fail" (known to fail), "skip" (not ready to test)

echo -e "${BLUE}Tier 1: Basic Types${NC}"
echo "─────────────────────────"
run_test "test_bool" "pass"          # M28-9e56: ✓ PASSING
run_test "test_int" "skip"            # Not yet targeted
run_test "test_float" "skip"          # Not yet targeted
run_test "test_string" "skip"         # Not yet targeted

echo ""
echo -e "${BLUE}Tier 2: Data Structures${NC}"
echo "─────────────────────────"
run_test "test_list" "skip"           # Not yet targeted
run_test "test_tuple" "skip"          # Needs test.support
run_test "test_dict" "skip"           # Not yet targeted
run_test "test_set" "skip"            # Not yet targeted

echo ""
echo -e "${BLUE}Tier 3: Language Features${NC}"
echo "─────────────────────────"
run_test "test_class" "fail"          # M28-1c83: Core working, unittest.main() issue
run_test "test_augassign" "fail"      # M28-17e5: Blocked by unittest.main()
run_test "test_dictcomps" "fail"      # M28-7ed5: 4/6 passing, unittest.main() issue
run_test "test_contains" "fail"       # M28-a985: Blocked by unittest.main()
run_test "test_compare" "skip"        # Not yet targeted
run_test "test_slice" "skip"          # Not yet targeted

echo ""
echo -e "${BLUE}Tier 4: Advanced Features${NC}"
echo "─────────────────────────"
run_test "test_binop" "skip"          # Complex, needs work
run_test "test_operators" "skip"      # Not yet targeted
run_test "test_exceptions" "skip"     # Not yet targeted

# Summary
echo ""
echo -e "${BLUE}======================================${NC}"
echo -e "${BLUE}           TEST SUMMARY               ${NC}"
echo -e "${BLUE}======================================${NC}"
echo -e "Total Tests:    $TOTAL"
echo -e "Passed:         ${GREEN}$PASSED${NC}"
echo -e "Failed:         ${RED}$FAILED${NC}"
echo -e "Skipped:        ${YELLOW}$SKIPPED${NC}"

if [[ $TOTAL -gt 0 ]]; then
    TESTABLE=$((TOTAL - SKIPPED))
    if [[ $TESTABLE -gt 0 ]]; then
        PERCENT=$((PASSED * 100 / TESTABLE))
        echo -e "Success Rate:   ${GREEN}${PERCENT}%${NC} (of testable)"
    fi
fi

# Failed test details
if [[ ${#FAILED_TESTS[@]} -gt 0 ]]; then
    echo ""
    echo -e "${RED}Newly Failed Tests:${NC}"
    for test in "${FAILED_TESTS[@]}"; do
        echo "  - $test"
    done
fi

echo ""
echo -e "${BLUE}Next Steps:${NC}"
echo "  1. Fix M28-e4b4: unittest.main() exception issue"
echo "  2. Target M28-7ed5: test_dictcomps (4/6 passing!)"
echo "  3. See: ./bin/m28 -e 'import beads; beads.show(\"M28-877c\")'"

# Exit code based on unexpected failures
if [[ ${#FAILED_TESTS[@]} -eq 0 ]]; then
    echo ""
    echo -e "${GREEN}✅ All targeted tests behaving as expected${NC}"
    exit 0
else
    echo ""
    echo -e "${RED}❌ Some tests regressed${NC}"
    exit 1
fi
