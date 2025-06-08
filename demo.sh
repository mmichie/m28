#!/bin/bash
# M28 Demo Script - Quick showcase of language features
# Usage: ./demo.sh

set -e

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[0;33m'
NC='\033[0m'

echo -e "${BLUE}======================================${NC}"
echo -e "${BLUE}         M28 Language Demo            ${NC}"
echo -e "${BLUE}======================================${NC}"
echo ""

# Function to run demo
run_demo() {
    local title=$1
    local code=$2
    
    echo -e "${YELLOW}$title${NC}"
    echo -e "${GREEN}Code:${NC}"
    echo "$code" | sed 's/^/  /'
    echo -e "${GREEN}Output:${NC}"
    echo "$code" | ./bin/m28 2>&1 | sed 's/^/  /'
    echo ""
}

# Build first
echo -e "${YELLOW}Building M28...${NC}"
make build > /dev/null 2>&1
echo -e "${GREEN}✓ Ready${NC}\n"

# Demo 1: Basic syntax
run_demo "1. Basic Syntax - S-expressions with Python semantics" '
(print "Hello from M28!")
(= x 42)
(print (+ "The answer is: " (str x)))'

# Demo 2: Data structures
run_demo "2. Data Structures - Python-style literals" '
(= lst [1, 2, 3, 4, 5])
(print "List:" lst)
(= person {"name": "Alice", "age": 30})
(print "Dict:" person)'

# Demo 3: Functions
run_demo "3. Functions - Clean and powerful" '
(def greet (name)
  (+ "Hello, " name "!"))

(print (greet "Meta Engineer"))

(= double (lambda (x) (* x 2)))
(print "Double 21 =" (double 21))'

# Demo 4: Higher-order functions
run_demo "4. Functional Programming" '
(= numbers [1, 2, 3, 4, 5])
(= squared (list (map (lambda (x) (* x x)) numbers)))
(print "Squared:" squared)
(= evens (list (filter (lambda (x) (== (% x 2) 0)) numbers)))
(print "Evens:" evens)'

# Demo 5: Classes
run_demo "5. Object-Oriented Programming" '
(class Dog
  (def __init__ (self name)
    (= self.name name))
  
  (def bark (self)
    (+ self.name " says woof!")))

(= dog (Dog "Rex"))
(print (dog.bark))'

# Demo 6: Control flow
run_demo "6. Control Flow - Python keywords" '
(for i (range 5)
  (if (== (% i 2) 0)
    (print (+ (str i) " is even"))
    (print (+ (str i) " is odd"))))'

# Demo 7: Error handling
run_demo "7. Exception Handling" '
(def safe_divide (a b)
  (try
    (/ a b)
  except
    "Cannot divide by zero"))

(print "10/2 =" (safe_divide 10 2))
(print "10/0 =" (safe_divide 10 0))'

# Demo 8: Recursion
run_demo "8. Recursion - Fibonacci" '
(def fib (n)
  (if (<= n 1) n
    (+ (fib (- n 1)) (fib (- n 2)))))

(print "Fibonacci sequence:")
(for i (range 10)
  (print (+ "fib(" (str i) ") = " (str (fib i)))))'

echo -e "${BLUE}======================================${NC}"
echo -e "${GREEN}✨ Demo complete!${NC}"
echo ""
echo "Try the REPL: ${YELLOW}./bin/m28${NC}"
echo "Run tests: ${YELLOW}./test.sh${NC}"
echo "See examples: ${YELLOW}ls examples/${NC}"