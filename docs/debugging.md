# Debugging M28

M28 includes a powerful structured logging system built on Go's `log/slog` with subsystem-based filtering for targeted debugging.

## Table of Contents

- [Quick Start](#quick-start)
- [CLI Flags Reference](#cli-flags-reference)
- [Subsystems](#subsystems)
- [Log Levels](#log-levels)
- [Common Debugging Scenarios](#common-debugging-scenarios)
- [Log Output Format](#log-output-format)
- [Adding Logging to Your Code](#adding-logging-to-your-code)
- [Troubleshooting Guide](#troubleshooting-guide)

## Quick Start

```bash
# Enable debug logging for a specific subsystem
m28 --debug parser script.py

# Maximum verbosity for investigation
m28 -vvv script.py

# Export debug logs as JSON for analysis
m28 --log-format json --debug all script.py > debug.jsonl
```

## CLI Flags Reference

### Basic Debug Flags

- `--debug SUBSYSTEMS` - Enable debug logging for specific subsystems
  - Can be a single subsystem: `--debug parser`
  - Multiple subsystems (comma-separated): `--debug parser,import,eval`
  - All subsystems: `--debug all`

- `--debug-level LEVEL` - Set the minimum log level (default: `error`)
  - Levels: `debug`, `info`, `warn`, `error`
  - Example: `--debug-level debug`
  - Note: Default is `error` (silent except for errors, following Unix philosophy)

- `--log-format FORMAT` - Set the output format (default: `text`)
  - `text` - Human-readable colored text output
  - `json` - Machine-readable JSON lines (JSONL)

- `--trace` - Enable trace-level logging
  - Shows function entry/exit with `→` and `←` markers
  - Very verbose - use for deep debugging
  - Best combined with specific subsystems

### Verbosity Shortcuts

- `-v` - Verbose mode
  - Equivalent to: `--debug-level debug`
  - Sets log level to debug

- `-vv` - More verbose
  - Equivalent to: `--debug-level debug --debug all`
  - Enables debug logging for all subsystems

- `-vvv` - Maximum verbosity
  - Equivalent to: `--debug-level debug --debug all --trace`
  - Enables trace-level logging for everything
  - ⚠️ Very verbose output

## Subsystems

M28's logging is organized into subsystems, each representing a major component:

### Available Subsystems

| Subsystem | Description | Common Use Cases |
|-----------|-------------|------------------|
| `parser` | Tokenization, AST parsing, desugaring | Syntax errors, parse failures, AST structure |
| `import` | Module resolution, loading, caching | Import errors, module not found, circular imports |
| `eval` | Expression evaluation, runtime operations | Runtime errors, unexpected behavior, value inspection |
| `builtin` | Built-in function calls, type operations | Function errors, type coercion issues |
| `scope` | Variable resolution, scope management | Variable not found, scope leakage |

### Subsystem Details

#### `parser`
- Token scanning and classification
- AST node construction
- Pythonic syntax desugaring (function calls, assignments, def statements)
- Parse-time validation

Key log attributes:
- `file` - Source file being parsed
- `tokens` - Number of tokens scanned
- `nodes` - Number of AST nodes created
- `parse_time` - Time spent parsing

#### `import`
- Module path resolution
- Package vs. module detection
- Import caching (tracks cache hits/misses)
- Circular dependency detection
- CPython stdlib integration

Key log attributes:
- `module` - Module name being imported
- `path` - Resolved file path
- `source_type` - `builtin`, `python`, or `m28`
- `cache_status` - `hit`, `miss`, `sys.modules_hit`
- `is_package` - Whether module is a package

#### `eval`
- Expression evaluation
- Function calls
- Operator execution
- Value type checking

Key log attributes:
- `expr` - Expression type
- `result` - Evaluation result
- `error` - Error details

#### `builtin`
- Built-in function invocation
- Type conversions
- Protocol method dispatch

Key log attributes:
- `function` - Built-in function name
- `args` - Number of arguments
- `result` - Return value

#### `scope`
- Variable lookups
- Scope creation/destruction
- Context management

Key log attributes:
- `variable` - Variable name
- `scope` - Scope type (local, global, builtin)
- `found` - Whether variable was found

## Log Levels

M28 uses hierarchical log levels (each includes all higher levels):

1. **TRACE** (via `--trace`)
   - Fine-grained operations
   - Function entry/exit markers (`→` / `←`)
   - Token-level parsing details
   - Every cache hit/miss

2. **DEBUG** (via `--debug-level debug` or `-v`)
   - Detailed diagnostic information
   - Resolution steps
   - Intermediate results
   - Performance metrics

3. **INFO** (default)
   - High-level events
   - File parsing started/completed
   - Module loaded successfully
   - Import resolution started

4. **WARN**
   - Issues that don't prevent execution
   - Circular imports detected
   - Fallback behaviors
   - Deprecated features

5. **ERROR**
   - Failures and error conditions
   - Parse errors
   - Import failures
   - Evaluation errors

## Common Debugging Scenarios

### Parser Issues

```bash
# Debug syntax errors or parse failures
m28 --debug parser script.py

# See detailed token scanning (very verbose)
m28 --debug parser --trace script.py

# Debug desugaring transformations
m28 --debug parser -v pythonic_syntax.py
```

**What to look for:**
- Parse error messages with line/column numbers
- Token count vs. node count ratios
- Desugaring transformations applied
- Parse time for performance issues

### Import Problems

```bash
# Debug module not found errors
m28 --debug import script.py

# See all import resolution attempts
m28 --debug import -v script.py

# Track cache behavior
m28 --debug import --log-format json script.py | grep cache_status
```

**What to look for:**
- Import resolution paths tried
- Cache hits/misses
- Circular import warnings
- Package vs. module detection
- Source type (builtin, python, m28)

### Runtime Errors

```bash
# Debug evaluation and execution
m28 --debug eval script.py

# Debug both evaluation and built-in calls
m28 --debug eval,builtin script.py

# Maximum verbosity for complex issues
m28 -vvv script.py 2> debug.log
```

**What to look for:**
- Expression types being evaluated
- Function call sequences
- Error propagation paths
- Value types at each step

### Performance Investigation

```bash
# JSON output for analysis
m28 --log-format json --debug all script.py > perf.jsonl

# Analyze with jq
cat perf.jsonl | jq 'select(.parse_time != null) | {file, parse_time, tokens, nodes}'

# Find slow operations
cat perf.jsonl | jq 'select(.total_time != null and (.total_time | tonumber) > 100)'
```

### Test Failures

```bash
# Debug specific test with targeted subsystems
m28 --debug parser,eval tests/test_classes.m28

# Full trace of test execution
m28 -vvv tests/test_feature.py 2>&1 | tee test_debug.log

# Compare with reference implementation
diff <(python3 test.py 2>&1) <(m28 --debug eval test.py 2>&1)
```

## Log Output Format

### Text Format (Default)

```
time=2025-11-25T12:00:00.000-08:00 level=INFO msg="Parsing file started" subsystem=parser file=/path/to/script.py
time=2025-11-25T12:00:00.001-08:00 level=DEBUG msg="Token scanned" subsystem=parser token=LPAREN line=5 col=10
time=2025-11-25T12:00:00.010-08:00 level=INFO msg="Parsing file completed successfully" subsystem=parser file=/path/to/script.py total_time=10ms nodes=42 tokens=150
```

**Format:** `time=<timestamp> level=<LEVEL> msg="<message>" subsystem=<subsystem> <key>=<value> ...`

### JSON Format

```json
{"time":"2025-11-25T12:00:00.000-08:00","level":"INFO","msg":"Parsing file started","subsystem":"parser","file":"/path/to/script.py"}
{"time":"2025-11-25T12:00:00.010-08:00","level":"INFO","msg":"Parsing file completed successfully","subsystem":"parser","file":"/path/to/script.py","total_time":"10ms","nodes":42,"tokens":150}
```

**Processing with jq:**

```bash
# Extract all errors
jq 'select(.level == "ERROR")' debug.jsonl

# Find slow parse operations
jq 'select(.subsystem == "parser" and .total_time != null) | {file, total_time, nodes}' debug.jsonl

# Track import cache efficiency
jq 'select(.subsystem == "import" and .cache_status != null) | .cache_status' debug.jsonl | sort | uniq -c
```

### Common Log Attributes

| Attribute | Type | Description |
|-----------|------|-------------|
| `time` | timestamp | When the log was generated |
| `level` | string | Log level (DEBUG, INFO, WARN, ERROR) |
| `msg` | string | Human-readable message |
| `subsystem` | string | Which subsystem produced the log |
| `file` | string | Source file path |
| `module` | string | Module name |
| `line`, `col` | number | Source location (errors) |
| `error` | string | Error details |
| `*_time` | duration | Timing metrics (parse_time, total_time, eval_time) |
| `*_count` | number | Counts (token_count, node_count) |
| `cache_status` | string | Cache hit/miss indicators |

## Adding Logging to Your Code

### Go Code

Import the logging system:

```go
import "github.com/mmichie/m28/core"
```

### Log Levels

```go
// Info: High-level events
core.Log.Info(core.SubsystemParser, "Parsing file started", "file", filename)

// Debug: Detailed diagnostics
core.Log.Debug(core.SubsystemImport, "Cache hit", "module", name, "cache_status", "hit")

// Warn: Non-critical issues
core.Log.Warn(core.SubsystemEval, "Deprecated feature used", "feature", featureName)

// Error: Failures
core.Log.Error(core.SubsystemParser, "Parse failed", "file", filename, "error", err)

// Trace: Function-level tracing
core.Log.Trace(core.SubsystemParser, "Parsing token", "token", tokenType)
```

### Function Tracing

Use `defer` for automatic entry/exit logging:

```go
func parseExpression(expr string) (Value, error) {
    defer core.Log.TraceFunc(core.SubsystemParser, "parseExpression", "expr", expr)()
    
    // Function body
    // ...
}
```

**Output:**
```
→ parseExpression expr="x + 1"
  ... function execution ...
← parseExpression expr="x + 1"
```

### Best Practices

1. **Use structured attributes** instead of string formatting:
   ```go
   // Good
   core.Log.Debug(core.SubsystemEval, "Evaluating expression", "type", exprType, "line", lineNum)
   
   // Avoid
   core.Log.Debug(core.SubsystemEval, fmt.Sprintf("Evaluating %s at line %d", exprType, lineNum))
   ```

2. **Choose appropriate log levels:**
   - `Info` - User-visible events (module loaded, file parsed)
   - `Debug` - Developer debugging (intermediate values, decisions)
   - `Trace` - Very detailed (every token, every iteration)
   - `Error` - Actual failures

3. **Add context attributes:**
   - File paths, line numbers, module names
   - Timing metrics for performance-sensitive operations
   - Cache status for import/lookup operations

4. **Use consistent attribute names:**
   - `file` for file paths
   - `module` for module names
   - `*_time` for durations
   - `*_count` for counts
   - `cache_status` for cache indicators

### Adding New Subsystems

To add a new subsystem:

1. Define the constant in `core/logging.go`:
```go
const (
    SubsystemParser  Subsystem = "parser"
    SubsystemImport  Subsystem = "import"
    SubsystemMyNew   Subsystem = "mynew"  // Add here
)
```

2. Add to `AllSubsystems()`:
```go
func AllSubsystems() []Subsystem {
    return []Subsystem{
        SubsystemParser,
        SubsystemImport,
        SubsystemMyNew,  // Add here
    }
}
```

3. Use in code:
```go
core.Log.Debug(core.SubsystemMyNew, "New feature initialized")
```

## Troubleshooting Guide

### "No output when using --debug"

**Problem:** Debug flags don't produce any output.

**Solutions:**
1. Check that you're using the correct subsystem name:
   ```bash
   # Wrong
   m28 --debug parsing script.py
   
   # Correct
   m28 --debug parser script.py
   ```

2. Ensure debug level is set:
   ```bash
   m28 --debug parser --debug-level debug script.py
   # Or use shortcut
   m28 --debug parser -v script.py
   ```

3. Debug output goes to stderr by default:
   ```bash
   # Capture debug output
   m28 --debug parser script.py 2> debug.log
   ```

### "Too much output / can't find relevant logs"

**Problem:** `-vvv` produces overwhelming output.

**Solutions:**
1. Target specific subsystems:
   ```bash
   # Instead of
   m28 -vvv script.py
   
   # Use
   m28 --debug import,eval -v script.py
   ```

2. Use grep to filter:
   ```bash
   m28 -vv script.py 2>&1 | grep "ERROR\|WARN"
   ```

3. Use JSON output for filtering:
   ```bash
   m28 --log-format json -vv script.py | jq 'select(.level == "ERROR" or .level == "WARN")'
   ```

### "Performance issues"

**Problem:** Need to identify performance bottlenecks.

**Solutions:**
1. Use JSON output to analyze timing:
   ```bash
   m28 --log-format json --debug all script.py | \
     jq 'select(.total_time != null) | {msg, total_time, file}' | \
     sort -k2 -nr
   ```

2. Enable import subsystem to see module load times:
   ```bash
   m28 --debug import --debug-level info script.py 2>&1 | grep "total_time"
   ```

3. Use trace mode to identify hot paths:
   ```bash
   m28 --debug parser --trace expensive_script.py 2>&1 | \
     grep "←" | \
     sort | uniq -c | sort -nr
   ```

### "Module import failures"

**Problem:** Mysterious import errors.

**Solutions:**
1. Enable import debugging:
   ```bash
   m28 --debug import -v script.py
   ```

2. Check resolution paths:
   ```bash
   m28 --debug import script.py 2>&1 | grep "resolution started\|located"
   ```

3. Verify cache behavior:
   ```bash
   m28 --debug import --log-format json script.py | \
     jq 'select(.cache_status != null)'
   ```

### "Parse errors without clear cause"

**Problem:** Syntax error but unclear what's wrong.

**Solutions:**
1. Enable parser tracing:
   ```bash
   m28 --debug parser --trace failing_script.py 2> parse.log
   ```

2. Check token stream:
   ```bash
   m28 --debug parser -v script.py 2>&1 | grep "token"
   ```

3. Compare with working code:
   ```bash
   diff <(m28 --debug parser -v working.py 2>&1) \
        <(m28 --debug parser -v broken.py 2>&1)
   ```

### "Redirect stderr without losing debug output"

**Problem:** Need both program output and debug logs.

**Solutions:**
```bash
# Save debug to file, see stdout
m28 --debug all script.py 2> debug.log

# See both stdout and debug on screen, save debug to file
m28 --debug all script.py 2>&1 | tee combined.log

# Separate stdout and debug logs
m28 --debug all script.py > output.txt 2> debug.log

# JSON debug with human-readable stdout
m28 --log-format json --debug all script.py 2> debug.jsonl
```

## Tips and Tricks

### Quick Debug Patterns

```bash
# Debug specific file in test suite
m28 --debug parser tests/test_comprehensions.py

# Compare M28 vs Python behavior
diff <(python3 script.py) <(m28 script.py)

# Time each import
m28 --debug import --log-format json script.py | \
  jq -r 'select(.total_time != null) | "\(.total_time) \(.module)"' | \
  sort -nr

# Find all errors in test run
m28 -vv tests/*.py 2>&1 | grep "level=ERROR" | tee errors.log

# Monitor cache hit rate
m28 --debug import --log-format json script.py | \
  jq -r .cache_status | grep -v null | \
  awk '{count[$1]++} END {for (status in count) print status, count[status]}'
```

### Environment Variables

None currently - all configuration is via CLI flags.

### Logging in REPL

The REPL uses the same logging system. Enable debug mode when starting:

```bash
m28 -vv
```

Or use the default (no debug output) and investigate specific issues by running files.

---

**See Also:**
- [Language Guide](language-guide.md) - M28 language features and syntax
- [Testing Guide](testing-guide.md) - Running and writing tests
- [Contributing](../CONTRIBUTING.md) - Development guidelines

For questions or issues with debugging, please [open an issue](https://github.com/mmichie/m28/issues).
