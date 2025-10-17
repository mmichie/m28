# Python Compatibility Improvement Plan

## Executive Summary

M28's Python frontend currently supports basic Python syntax but has several limitations that prevent running most real-world Python code. This document analyzes the gaps and provides a roadmap for increasing compatibility.

## Current State

### ✅ What Works
- Variables and assignment
- Function definitions (def, parameters, return)
- Control flow (if/elif/else)
- Loops (for with range(), while, break, continue)
- List comprehensions with conditions
- Empty data structures ([], {})
- All operators (arithmetic, comparison, logical)
- Classes and methods
- Exception handling (try/except/finally)

### ❌ Critical Gaps
1. **List/dict/set literals with elements** - `[1, 2, 3]`, `{"a": 1}`, `{1, 2}`
2. **Import statements** - `import time`, `from math import sqrt`
3. **Default parameters** - `def func(x=5):`

### ❌ High Priority Gaps
4. **F-strings** - `f"Hello, {name}!"`
5. **Tuple literals** - `(1, 2, 3)`
6. **Multiple assignment** - `x, y = 1, 2`

### ❌ Medium Priority Gaps
7. **Chained assignment** - `x = y = z = 0`
8. **Binary/octal/hex literals** - `0b1010`, `0o755`, `0xFF`
9. **String prefixes** - `r"raw"`, `b"bytes"`

## Root Cause Analysis

### Issue #1: List Literals with Elements

**Problem:** `x = [1, 2, 3]` causes "TypeError: function call: expected callable, got int"

**Investigation Needed:**
- Parser creates: `Literal(ListValue{elem.ToIR(), ...})`
- Calls `ToIR()` during parsing (line 717 of python_parser.go)
- Need to trace what IR is generated and why evaluation fails

**Debugging Steps:**
```bash
# Test progressively
./bin/m28 -c 'x = []'              # Works
./bin/m28 test-list.py             # x = [1] fails
./bin/m28 -c 'x = [1]'             # Does this work?
```

**Hypothesis:** Python parser may be generating different IR than M28 parser for the same construct.

**Solution Approach:**
1. Add debug logging to parseListLiteral() to see what's generated
2. Compare IR from Python parser vs M28 parser for same list
3. Check if issue is in parsing, lowering, or evaluation
4. Fix the mismatch

**Files to Modify:**
- `parser/python_parser.go` - parseListLiteral() function (line 684)
- Possibly `core/ast/nodes.go` - if Literal node handling is wrong

### Issue #2: Dict/Set Literals

**Problem:** Similar to lists - `{"a": 1}` and `{1, 2, 3}` fail

**Solution:** Same approach as lists, check parseDictOrSetLiteral() (line 723)

### Issue #3: Default Parameters

**Problem:** `def func(x=5):` - parameter with default value

**Investigation:**
- Check how DefForm parses parameters
- Check how function calls handle missing arguments
- Check if defaults are stored and used

**Files to Check:**
- `parser/python_parser.go` - parseFunctionDef()
- `core/ast/nodes.go` - DefForm struct
- `eval/evaluator.go` - function call evaluation

### Issue #4: Import Statements

**Problem:** No import statement support in Python parser

**Current Workaround:** M28's module system uses `(import "module")` syntax

**Solution Options:**
1. **Parse to M28 import:** `import time` → `(import "time")`
2. **Add Python import nodes:** New ImportForm, FromImportForm AST nodes
3. **Module aliasing:** `import time as t` → context binding

**Implementation:**
- Add TOKEN_IMPORT, TOKEN_FROM to tokenizer
- Add parseImportStatement() to parser
- Lower to M28's import form
- Handle `as` aliasing
- Handle `from X import Y` syntax

### Issue #5: F-Strings

**Problem:** `f"Hello, {name}!"` not supported

**M28 Already Has:** F-strings in M28 syntax work

**Solution:**
- Check if PythonTokenizer recognizes f-string prefix
- Add FSTRING token type if needed
- Parse interpolations `{expr}`
- Lower to string concatenation or format() calls
- Reuse M28's f-string infrastructure

**Files to Modify:**
- `parser/python_tokenizer.go` - scanString() method
- `parser/python_parser.go` - parse f-string interpolations

### Issue #6: Tuple Literals

**Problem:** `(1, 2, 3)` currently parsed as expression, not tuple

**Python Semantics:**
- `(1)` is an int
- `(1,)` is a 1-tuple
- `(1, 2)` is a 2-tuple
- `()` is empty tuple

**Solution:**
- Modify parseParenthesized() to detect comma-separated values
- Create tuple when comma is present
- Handle trailing comma case

### Issue #7: Multiple Assignment

**Problem:** `x, y = 1, 2` (tuple unpacking)

**Requires:**
- Tuple literal support (Issue #6)
- Unpack logic in assignment
- Pattern matching on left side

**Solution:**
- Parse left side as tuple: `(x, y)`
- Parse right side as tuple: `(1, 2)`
- Lower to multiple assignments or unpacking special form

## Implementation Priority

### Phase 1: Fix Critical Bugs (1-2 weeks)
**Goal:** Make basic Python programs work

1. **Debug and fix list literals** ⭐ HIGHEST PRIORITY
   - `[1, 2, 3]` must work for any Python code to run
   - Estimated: 1-3 days
   - Test: Run benchmark suite, test-python-grammar.py

2. **Fix dict literals**
   - `{"key": "value"}` is extremely common
   - Estimated: 1-2 days (similar to lists)

3. **Fix set literals**
   - `{1, 2, 3}` less common but should be easy once dicts work
   - Estimated: 1 day

### Phase 2: Import Support (1 week)
**Goal:** Enable using M28's standard library from Python

4. **Add import statement parsing**
   - `import time` → `(import "time")`
   - `from math import sqrt` → selective import
   - Estimated: 3-5 days
   - Enables: Python code can use M28 modules

### Phase 3: Common Patterns (1-2 weeks)
**Goal:** Support common Python idioms

5. **Default parameters**
   - `def func(x=5, y=10):`
   - Very common in Python APIs
   - Estimated: 2-3 days

6. **F-strings**
   - `f"Value: {x}"`
   - Modern Python standard
   - Estimated: 3-5 days (if starting from scratch)
   - May be quick if reusing M28's f-string code

7. **Tuple literals**
   - `(1, 2, 3)` and `()`
   - Required for multiple return values
   - Estimated: 2-3 days

### Phase 4: Polish (1 week)
**Goal:** Handle edge cases and rare patterns

8. **Multiple assignment**
   - `x, y = 1, 2`
   - Depends on tuples
   - Estimated: 2-3 days

9. **Chained assignment**
   - `x = y = z = 0`
   - Simple parser change
   - Estimated: 1 day

10. **Number literal prefixes**
    - `0x`, `0b`, `0o` prefixes
    - Tokenizer change
    - Estimated: 1 day

## Testing Strategy

### 1. Unit Tests
Create Go unit tests for parser:
```go
func TestParseLis

tLiteral(t *testing.T) {
    // Test [1, 2, 3]
    // Test nested lists
    // Test mixed types
}
```

### 2. Integration Tests
Expand `tests/test-python-grammar.py`:
- Uncomment failing tests as features are fixed
- Add new test categories for each feature
- Ensure 100% pass rate before committing

### 3. Real-World Code
Test with actual Python code:
```python
# Mini benchmark
def fib(n):
    if n <= 1:
        return n
    return fib(n-1) + fib(n-2)

numbers = [fib(i) for i in range(10)]
print("Fibonacci:", numbers)
```

### 4. Compatibility Matrix
Track which Python features work:
| Feature | Status | Notes |
|---------|--------|-------|
| List literals | ❌ | Issue #1 |
| Dict literals | ❌ | Issue #2 |
| F-strings | ❌ | Issue #4 |
| ... | | |

## Success Metrics

### MVP (Minimum Viable Python)
- ✅ All test-python-grammar.py tests pass
- ✅ Can run fibonacci example with list literals
- ✅ Can import and use M28 modules from Python
- ✅ Benchmark suite runs without modifications

### Good Python Support
- ✅ MVP +
- ✅ F-strings work
- ✅ Tuple unpacking works
- ✅ Default parameters work
- ✅ Can run simple Flask/FastAPI examples (with M28 WSGI)

### Great Python Support
- ✅ Good support +
- ✅ 80%+ of Python 3 syntax works
- ✅ Can run pytest tests
- ✅ Can use numpy-style syntax (with M28 array library)

## Recommended Approach

### Week 1-2: Fix the Blocker
**Focus:** Make list/dict/set literals work
- This is blocking everything else
- Dedicate focused time to debugging
- Don't move on until this works

**Debugging Process:**
1. Add logging to parseListLiteral()
2. Print AST structure
3. Print IR generated
4. Compare with M28 parser output
5. Find the mismatch
6. Fix it
7. Test extensively

### Week 3: Import Support
**Focus:** Python can use M28 modules
- Parse import statements
- Lower to M28 import forms
- Test with real modules

### Week 4-5: Common Patterns
**Focus:** Default params, f-strings, tuples
- Makes Python code feel natural
- Each feature independently useful

### Week 6: Polish
**Focus:** Edge cases and cleanup
- Multiple assignment
- Chained assignment
- Number prefixes
- Documentation updates

## Next Steps

1. **Immediate:** Debug list literal issue
   ```bash
   cd /Users/mim/src/m28
   # Add debug output to parser/python_parser.go:parseListLiteral()
   make build
   ./bin/m28 test-list.py
   ```

2. **Create tracking issue:**
   - List all compatibility gaps
   - Track progress on each
   - Update ROADMAP.md as features complete

3. **Set up test infrastructure:**
   - Go unit tests for parser
   - Expand test-python-grammar.py
   - CI/CD runs Python compatibility tests

4. **Document supported features:**
   - Update README with Python support level
   - List what works and doesn't work
   - Provide workarounds for unsupported features

## Conclusion

M28's Python frontend is 70-80% complete. The main blocker is list/dict/set literals, which appears to be a parser or lowering bug. Once fixed, adding remaining features is straightforward:

- **Critical path:** Fix literals → Add imports → Add common patterns
- **Timeline:** 4-6 weeks for "Good Python Support"
- **Effort:** 1 developer working part-time
- **Risk:** Low - most features are parser/lowering changes, not runtime changes

The architecture is sound (tokenizer → AST → IR → eval). We just need to complete the parser implementation for remaining Python constructs.
