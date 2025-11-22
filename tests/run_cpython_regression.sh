#!/bin/bash
# Run official CPython regression tests from stdlib
# Uses conformance.json to track which tests M28 can pass

set -e

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

# Find paths
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
M28_BIN="$SCRIPT_DIR/../bin/m28"
CONFORMANCE_JSON="$SCRIPT_DIR/cpython/conformance.json"

if [[ ! -f "$M28_BIN" ]]; then
    echo -e "${RED}Error: M28 binary not found at $M28_BIN${NC}"
    echo "Please run 'make build' first"
    exit 1
fi

if [[ ! -f "$CONFORMANCE_JSON" ]]; then
    echo -e "${RED}Error: conformance.json not found at $CONFORMANCE_JSON${NC}"
    exit 1
fi

# Check for jq
if ! command -v jq &> /dev/null; then
    echo -e "${YELLOW}Warning: jq not found, installing via brew...${NC}"
    if command -v brew &> /dev/null; then
        brew install jq
    else
        echo -e "${RED}Error: jq is required. Install with: brew install jq${NC}"
        exit 1
    fi
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
echo -e "Python stdlib: ${CYAN}$PYTHON_STDLIB${NC}"
echo -e "Conformance:   ${CYAN}$CONFORMANCE_JSON${NC}"
echo ""

# Test counters
TOTAL=0
PASSED=0
FAILED=0
NOT_FOUND=0
FAILED_TESTS=()
PASSED_TESTS=()
NOT_FOUND_TESTS=()

# Function to run a test
run_test() {
    local test_name=$1
    local test_file="$PYTHON_STDLIB/test/${test_name}.py"

    TOTAL=$((TOTAL + 1))

    # Show test name with padding
    printf "    %-40s" "$test_name..."

    if [[ ! -f "$test_file" ]]; then
        echo -e "${YELLOW}⊘ not found${NC}"
        NOT_FOUND=$((NOT_FOUND + 1))
        NOT_FOUND_TESTS+=("$test_name")
        return
    fi

    # Run with timeout
    if timeout 30s "$M28_BIN" "$test_file" > /tmp/m28_regression_test.txt 2>&1; then
        echo -e "${GREEN}✓ passed${NC}"
        PASSED=$((PASSED + 1))
        PASSED_TESTS+=("$test_name")
    else
        EXIT_CODE=$?
        if [[ $EXIT_CODE -eq 124 ]]; then
            echo -e "${YELLOW}⏱ timeout${NC}"
        else
            echo -e "${RED}✗ failed${NC}"
        fi
        FAILED=$((FAILED + 1))
        FAILED_TESTS+=("$test_name")
        # Show first error line
        head -n 2 /tmp/m28_regression_test.txt 2>/dev/null | sed 's/^/        /'
    fi
}

# Get list of tiers sorted by priority
TIERS=$(jq -r '.tiers | to_entries | sort_by(.value.priority) | .[].key' "$CONFORMANCE_JSON")

# Run tests tier by tier
for tier in $TIERS; do
    TIER_DESC=$(jq -r ".tiers.${tier}.description" "$CONFORMANCE_JSON")
    TIER_PRIORITY=$(jq -r ".tiers.${tier}.priority" "$CONFORMANCE_JSON")

    echo -e "${BLUE}Tier ${TIER_PRIORITY}: ${TIER_DESC}${NC}"
    echo "─────────────────────────"

    # Get tests in this tier
    TEST_NAMES=$(jq -r ".tiers.${tier}.tests | keys[]" "$CONFORMANCE_JSON")

    if [[ -z "$TEST_NAMES" ]]; then
        echo "  (no tests defined)"
        echo ""
        continue
    fi

    for test_name in $TEST_NAMES; do
        run_test "$test_name"
    done

    echo ""
done

# Summary
echo -e "${BLUE}======================================${NC}"
echo -e "${BLUE}           TEST SUMMARY               ${NC}"
echo -e "${BLUE}======================================${NC}"
echo -e "Total Tests:    $TOTAL"
echo -e "Passed:         ${GREEN}$PASSED${NC}"
echo -e "Failed:         ${RED}$FAILED${NC}"
echo -e "Not Found:      ${YELLOW}$NOT_FOUND${NC}"

if [[ $TOTAL -gt 0 ]]; then
    TESTABLE=$((TOTAL - NOT_FOUND))
    if [[ $TESTABLE -gt 0 ]]; then
        PERCENT=$((PASSED * 100 / TESTABLE))
        echo -e "Success Rate:   ${GREEN}${PERCENT}%${NC} (of testable)"
    fi
fi

# Show passed tests
if [[ ${#PASSED_TESTS[@]} -gt 0 ]]; then
    echo ""
    echo -e "${GREEN}Passed Tests (${#PASSED_TESTS[@]}):${NC}"
    for test in "${PASSED_TESTS[@]}"; do
        echo "  ✓ $test"
    done
fi

# Show failed tests
if [[ ${#FAILED_TESTS[@]} -gt 0 ]]; then
    echo ""
    echo -e "${RED}Failed Tests (${#FAILED_TESTS[@]}):${NC}"
    for test in "${FAILED_TESTS[@]}"; do
        echo "  ✗ $test"
    done
fi

# Show not found tests
if [[ ${#NOT_FOUND_TESTS[@]} -gt 0 ]]; then
    echo ""
    echo -e "${YELLOW}Not Found in CPython stdlib (${#NOT_FOUND_TESTS[@]}):${NC}"
    for test in "${NOT_FOUND_TESTS[@]}"; do
        echo "  ⊘ $test"
    done
fi

echo ""
if [[ $PASSED -gt 0 ]]; then
    echo -e "${GREEN}✅ $PASSED tests passing!${NC}"
    exit 0
else
    echo -e "${YELLOW}No tests passing yet${NC}"
    exit 0
fi
