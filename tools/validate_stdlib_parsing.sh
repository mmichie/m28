#!/bin/bash
# Batch Python Parsing Validator
# Tests that CPython stdlib files can be parsed

set -e

GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m'

STDLIB_PATH="${1:-$HOME/.pyenv/versions/3.12.0/lib/python3.12}"

if [[ ! -d "$STDLIB_PATH" ]]; then
    echo -e "${RED}Error: Stdlib directory not found: $STDLIB_PATH${NC}"
    exit 1
fi

echo -e "${BLUE}CPython Standard Library Parsing Validation${NC}"
echo "============================================="
echo ""
echo "Testing files in: $STDLIB_PATH"
echo ""

# Build the validator first
./tools/validate_python_parsing.sh "$STDLIB_PATH/os.py" > /dev/null 2>&1 || true

# Files to test (common stdlib modules)
TEST_FILES=(
    "os.py"
    "sys.py" 
    "re.py"
    "json/__init__.py"
    "collections/__init__.py"
    "collections/abc.py"
    "itertools.py"
    "functools.py"
    "pathlib.py"
    "datetime.py"
    "random.py"
    "string.py"
    "math.py"
    "threading.py"
    "io.py"
    "abc.py"
    "_collections_abc.py"
    "_weakrefset.py"
    "test/test_bool.py"
    "test/test_dict.py"
    "test/test_list.py"
)

PASSED=0
FAILED=0
SKIPPED=0
FAILED_FILES=()

for file in "${TEST_FILES[@]}"; do
    full_path="$STDLIB_PATH/$file"
    
    # Format file name for display
    display_name=$(basename "$file" .py)
    if [[ "$file" == *"/"* ]]; then
        display_name="$file"
    fi
    
    printf "%-40s" "Testing $display_name..."
    
    if [[ ! -f "$full_path" ]]; then
        echo -e "${YELLOW}⚠ skipped${NC}"
        SKIPPED=$((SKIPPED + 1))
        continue
    fi
    
    # Try to parse
    if timeout 2s /tmp/test_parse_only "$full_path" > /tmp/parse_result.txt 2>&1; then
        echo -e "${GREEN}✓ passed${NC}"
        PASSED=$((PASSED + 1))
    else
        EXIT_CODE=$?
        if [ $EXIT_CODE -eq 124 ]; then
            echo -e "${RED}✗ timeout${NC}"
        else
            echo -e "${RED}✗ failed${NC}"
            # Show first error line
            head -n 1 /tmp/parse_result.txt 2>/dev/null | sed 's/^/    /'
        fi
        FAILED=$((FAILED + 1))
        FAILED_FILES+=("$display_name")
    fi
done

# Summary
echo ""
echo -e "${BLUE}=============================================${NC}"
echo -e "${BLUE}                 SUMMARY${NC}"
echo -e "${BLUE}=============================================${NC}"
TOTAL=$((PASSED + FAILED))
echo "Total tested: $TOTAL"
echo -e "Passed:       ${GREEN}$PASSED${NC}"
echo -e "Failed:       ${RED}$FAILED${NC}"
echo -e "Skipped:      ${YELLOW}$SKIPPED${NC}"

if [ $FAILED -eq 0 ]; then
    echo -e "\n${GREEN}✓ All files parse successfully!${NC}"
else
    echo -e "\n${RED}✗ Failed files:${NC}"
    for file in "${FAILED_FILES[@]}"; do
        echo "  - $file"
    done
fi

[ $FAILED -eq 0 ]
