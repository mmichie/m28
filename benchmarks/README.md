# M28 Performance Benchmarks

This directory contains benchmarks for comparing M28's Python frontend performance against CPython.

## Available Benchmarks

### `python_bench_light.py`
A lightweight benchmark suitable for comparing Python 3 and M28. Tests:
- Recursive Fibonacci (function call overhead)
- Iterative Factorial (loops and arithmetic)
- List comprehensions
- Nested loops
- Simple function calls
- Class method calls

## Running Benchmarks

### Python 3
```bash
time python3 benchmarks/python_bench_light.py
```

### M28
```bash
time ./bin/m28 benchmarks/python_bench_light.py
```

## Sample Results

**Environment:** macOS, Python 3.12.0, M28 0.1.0-fresh-start

### Python 3
```
real    0m0.146s
user    0m0.022s
sys     0m0.013s
```

### M28
```
real    0m0.051s
user    0m0.049s
sys     0m0.006s
```

**M28 is ~2.9x faster** on this lightweight benchmark suite. This is surprising given M28 is a tree-walking interpreter! The performance advantage likely comes from:

1. **Go's runtime efficiency** - M28's evaluator is written in Go
2. **Reduced overhead** - M28's AST-to-IR pipeline is very direct
3. **Small benchmark size** - Python's startup cost dominates these short benchmarks
4. **No GIL** - M28 doesn't have Python's Global Interpreter Lock

## Notes

- These benchmarks measure raw execution speed, not language features
- Python 3 has much richer standard library and ecosystem
- M28 is optimized for embedding and scripting use cases
- Larger, more complex programs may show different performance characteristics
- Python 3 would likely perform better on I/O-heavy or numpy-based workloads

## Creating Your Own Benchmarks

Benchmarks should:
1. Work on both Python 3 and M28 (use supported features only)
2. Focus on core language operations
3. Be deterministic (same output every run)
4. Use `time` command for measurement (M28 doesn't have time module yet)

## Limitations

M28's Python frontend currently doesn't support:
- List/dict/set literals with elements: `[1, 2, 3]`
- F-strings: `f"Hello, {name}"`
- Chained assignment: `x = y = z = 0`
- Binary/octal/hex literals: `0b1010`, `0o755`, `0xFF`
- time module for internal timing

See `tests/test-python-grammar.py` for a complete list of supported features.
