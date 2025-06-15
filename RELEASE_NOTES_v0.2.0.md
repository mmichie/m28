# M28 v0.2.0 Release Notes

We're excited to announce the release of M28 v0.2.0! This release brings significant enhancements to the language, focusing on extensibility through Python-style protocols and major improvements to the type system.

## 🎉 Highlights

### Protocol System
M28 now supports Python-style protocols, enabling you to create custom types that integrate seamlessly with the language:

- **Iterator Protocol** (`__iter__`, `__next__`) - Make your objects iterable in for loops
- **Container Protocols** (`__getitem__`, `__setitem__`, `__delitem__`) - Custom indexing and item manipulation  
- **Operator Overloading** - Define how operators work with your custom types
- **And many more** - Boolean, length, contains, comparison, and string protocols

### Enhanced Type System
- 40-50% code reduction in type-heavy operations
- Better error messages with source location tracking
- Protocol-based dispatch for all operators
- Comprehensive validation framework

### New Language Features
- **Multiple assignment**: `(= x y 10)` assigns to both variables
- **Tuple unpacking in loops**: `(for [k v] in dict.items() ...)`
- **Lazy ranges**: Ranges are now memory-efficient iterators
- **Custom exceptions**: Full support for `raise` and exception handling

## 📊 By the Numbers

- **84 commits** since v0.1.0
- **100% test pass rate** (37/37 tests)
- **36% code reduction** through architectural improvements
- **69 builtins** (reduced from 110 by eliminating duplicates)

## 🔧 Developer Experience

- Full readline support in REPL with history
- Better error messages with contextual information
- Modular architecture for easier contributions
- Lazy module loading for faster startup

## 💔 Breaking Changes

- Lists are now immutable by default (use dicts for mutable collections)
- Range objects are lazy (use `list(range(n))` for eager evaluation)

## 🚀 Getting Started

```bash
# Update to v0.2.0
git pull
make clean && make build

# Try the new protocols
./bin/m28 examples/protocols_demo.m28
```

## Example: Custom Iterator

```lisp
(class Fibonacci
  (def __init__ (self)
    (= self.a 0)
    (= self.b 1))
  
  (def __iter__ (self)
    self)
  
  (def __next__ (self)
    (= result self.a)
    (= self.a self.b)
    (= self.b (+ result self.b))
    result))

# First 10 Fibonacci numbers
(= fib (Fibonacci))
(for i in (range 10)
  (print (next fib)))
```

## 🙏 Acknowledgments

Thanks to all contributors who made this release possible! Special thanks to the Lisp and Python communities for inspiration.

## 📚 Resources

- [Full Changelog](CHANGELOG.md)
- [Protocol Documentation](docs/protocols.md)
- [Migration Guide](docs/migration_v0.2.md)

Happy coding with M28! 🎈