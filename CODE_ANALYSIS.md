# M28 Codebase Analysis Report

## 1. Code Smells - Long Functions

### Critical Issues
- **`core/type_registry.go`**: `InitializeTypeRegistry()` is a massive 1131-line function
  - This is a severe god function that should be refactored
  - Contains repetitive registration patterns for each type
  - Should be split into multiple focused functions

### Other Long Functions (>50 lines)
- `core/doc_registry.go`: `FormatDocEntry()` - 81 lines
- `core/context_manager.go`: 
  - `SimpleContextManager.GetAttr()` - 65 lines
  - `FileContextManager.GetAttr()` - 99 lines
- `core/module_loader_enhanced.go`: `LoadModule()` - 107 lines
- `core/repr.go`: `Repr()` - 76 lines
- `core/generator.go`: `Generator.GetAttr()` - 61 lines
- `core/range.go`: `RangeValue.GetAttr()` - 61 lines

## 2. Non-Idiomatic Go Code

### Panic Usage
- `core/list_methods.go`, `dict_methods.go`, `set_methods.go`: Using `panic()` for initialization failures
  - Should return errors instead of panicking
  - Example: `panic("list type not found in registry")`

### Inconsistent Error Handling
- Mix of wrapped and unwrapped errors throughout the codebase
- Some functions return bare errors while others use `core.WrapEvalError()`
- No consistent error type hierarchy

### Import Organization
- Some files have unorganized imports (stdlib mixed with third-party)
- Should follow Go convention: stdlib first, then third-party, then local packages

## 3. Stubbed/Unimplemented Functions

### TODO Comments Found
- `core/type_registry.go:925`: Dict constructor doesn't support initialization from pairs
- `core/protocols/iterable.go:273`: DunderIterator wrapper not implemented
- `core/protocols/numeric.go:181`: DunderNumeric wrapper not implemented
- `core/protocols/indexable.go:251`: DunderIndexable wrapper not implemented
- `core/protocols/container.go:176`: Missing __len__ and __contains__ method checks
- `builtin/iteration.go:65`: Sentinel iterator not implemented in `iter()`

### Debug Code
- `parser/parser.go`: Contains commented-out debug print statements (lines 88, 98)
  - Should be removed or converted to proper logging

## 4. Duplicative Code

### Type Registration Pattern
- The entire `InitializeTypeRegistry()` function is highly repetitive
- Each type registration follows the same pattern:
  ```go
  RegisterType(&TypeDescriptor{
      Name: "typename",
      PythonName: "typename",
      BaseType: Type("typename"),
      Methods: map[string]*MethodDescriptor{...},
  })
  ```
- Should be refactored to use a data-driven approach

### GetAttr Method Pattern
- Multiple types implement nearly identical `GetAttr()` methods:
  - `SimpleContextManager.GetAttr()`
  - `FileContextManager.GetAttr()`
  - `Generator.GetAttr()`
  - `RangeValue.GetAttr()`
  - `Task.GetAttr()`
  - `Channel.GetAttr()`
- These could use a common helper or embed a base type

### Error Wrapping Pattern
- Repeated pattern throughout:
  ```go
  if err != nil {
      return nil, core.WrapEvalError(err, "context", ctx)
  }
  ```
- Could benefit from a helper function

## 5. Inconsistent Patterns

### Naming Conventions
- Mix of naming styles for similar concepts:
  - `NewTask()` vs `NewChannel()` vs `NewAsyncFunction()` (consistent)
  - But method names vary: `Wait()`, `Send()`, `Receive()` vs `GetChan()`
  - Some use `Get` prefix inconsistently

### Method Registration
- Some types register methods inline in `GetAttr()`
- Others use the type registry
- Should be consistent across all types

### Iterator Implementation
- Multiple iterator types with similar structure:
  - `listIterator`
  - `tupleIterator`
  - `setIterator` (implied)
- Could use a generic iterator implementation

## Recommendations

1. **Refactor `InitializeTypeRegistry()`**:
   - Split into separate functions per type or type category
   - Use a data-driven approach with type definitions in a slice
   - Consider using code generation for repetitive patterns

2. **Create Common Base Types**:
   - Implement a base type with common `GetAttr()` logic
   - Use embedding to reduce duplication

3. **Standardize Error Handling**:
   - Define clear error types in `core/error.go`
   - Use consistent error wrapping throughout
   - Replace panics with proper error returns

4. **Clean Up Debug Code**:
   - Remove commented debug statements
   - Implement proper logging if needed

5. **Implement Missing Features**:
   - Complete the protocol wrappers (DunderIterator, etc.)
   - Implement sentinel iterator
   - Add dict initialization from pairs

6. **Improve Code Organization**:
   - Group related functionality together
   - Consider splitting large packages (core is doing too much)
   - Standardize import ordering

7. **Use Go Idioms**:
   - Replace panic with error returns
   - Use interfaces more effectively
   - Consider using generics (Go 1.18+) for common patterns