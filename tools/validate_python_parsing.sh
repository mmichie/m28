#!/bin/bash
# Python Parsing Validator
# Tests that Python files can be parsed without executing/importing them

set -e

GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo -e "${BLUE}Python Parsing Validation Tool${NC}"
echo "================================"
echo ""

# Test file or default to CPython test_bool.py
TEST_FILE="${1:-$HOME/.pyenv/versions/3.12.0/lib/python3.12/test/test_bool.py}"

if [[ ! -f "$TEST_FILE" ]]; then
    echo -e "${RED}Error: File not found: $TEST_FILE${NC}"
    exit 1
fi

echo "Testing file: $TEST_FILE"
echo ""

# Create a test program that just parses without executing
cat > /tmp/test_parse_only.go << 'GOEOF'
package main

import (
    "fmt"
    "os"
    "time"
    "io/ioutil"
    
    "github.com/mmichie/m28/parser"
)

func main() {
    if len(os.Args) < 2 {
        fmt.Println("Usage: test_parse_only <file.py>")
        os.Exit(1)
    }
    
    filePath := os.Args[1]
    
    // Read file
    start := time.Now()
    content, err := ioutil.ReadFile(filePath)
    if err != nil {
        fmt.Printf("Error reading file: %v\n", err)
        os.Exit(1)
    }
    readTime := time.Since(start)
    
    // Tokenize
    start = time.Now()
    tokenizer := parser.NewPythonTokenizer(string(content))
    tokens, err := tokenizer.Tokenize()
    tokenizeTime := time.Since(start)

    if err != nil {
        fmt.Printf("Tokenization error: %v\n", err)
        os.Exit(1)
    }

    // Parse
    start = time.Now()
    pythonParser := parser.NewPythonParser(tokens, filePath, string(content))
    astNodes, err := pythonParser.Parse()
    parseTime := time.Since(start)

    if err != nil {
        fmt.Printf("Parse error: %v\n", err)
        os.Exit(1)
    }

    if astNodes == nil {
        fmt.Println("Error: AST is nil")
        os.Exit(1)
    }
    
    fmt.Printf("✓ Successfully parsed\n")
    fmt.Printf("  Read time:      %v\n", readTime)
    fmt.Printf("  Tokenize time:  %v\n", tokenizeTime)
    fmt.Printf("  Parse time:     %v\n", parseTime)
    fmt.Printf("  Total time:     %v\n", readTime+tokenizeTime+parseTime)
    fmt.Printf("  Tokens:         %d\n", len(tokens))
}
GOEOF

# Build the parse-only tool
echo -e "${YELLOW}Building parse-only validator...${NC}"
cd /Users/mim/src/m28
if go build -o /tmp/test_parse_only /tmp/test_parse_only.go 2>&1; then
    echo -e "${GREEN}✓ Build successful${NC}"
else
    echo -e "${RED}✗ Build failed${NC}"
    exit 1
fi
echo ""

# Test the file
echo -e "${YELLOW}Parsing: $TEST_FILE${NC}"
if timeout 5s /tmp/test_parse_only "$TEST_FILE"; then
    echo -e "\n${GREEN}✓ File parses successfully${NC}"
    exit 0
else
    EXIT_CODE=$?
    if [ $EXIT_CODE -eq 124 ]; then
        echo -e "\n${RED}✗ Parsing timed out (>5s)${NC}"
    else
        echo -e "\n${RED}✗ Parsing failed${NC}"
    fi
    exit 1
fi
