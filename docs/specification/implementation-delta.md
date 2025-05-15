# M28 Implementation Delta

This document identifies the differences between the current implementation of M28 and the updated language specification. It provides a detailed roadmap for bringing the codebase into alignment with the specification.

## 1. Comment Syntax

**Current Implementation**:
- Supports both `#` and `;` for line comments
- Comment removal is done in `parser/parser.go` with the `removeComments` function

**Required Changes**:
- Modify `removeComments` in `parser/parser.go` to only recognize `#` as a comment character
- Update all example code and tests to use `#` for comments instead of `;`
- Update parser tests to verify semicolons are treated as regular characters

## 2. Function Definition with `def`

**Current Implementation**:
- `def` is used for both function and variable definitions in `special_forms/definition.go`
- The function `EvalDef` checks the first argument to determine if it's a function or variable definition

**Required Changes**:
- Modify `EvalDef` to only accept function definitions
- Return an error if a variable definition is attempted with `def`
- Add parsing checks to ensure `def` is used correctly
- Create examples showing the correct usage pattern
- Update all tests to use `=` for variable assignment

## 3. Type Checking System

**Current Implementation**:
- Incomplete implementation of `isinstance` and `issubclass` in `builtin/type.go`
- Placeholder functions that return "not implemented" errors
- Various type predicates scattered throughout the codebase

**Required Changes**:
- Implement `isinstance(obj, class_or_tuple)` function fully
- Implement `issubclass(class, class_or_tuple)` function
- Define type constants (int, float, str, etc.) accessible in M28 code
- Implement logic for type hierarchy and inheritance checks
- Add support for optional type annotations

## 4. Concurrency Model

**Current Implementation**:
- No existing concurrency support
- No goroutines, channels, or select statement

**Required Changes**:
- Implement the `go` special form for spawning goroutines
- Create `channel` built-in function for creating communication channels
- Implement channel operators (`<-` for send/receive)
- Create `select` special form for multiplexing channel operations
- Implement synchronization primitives (mutex, waitgroup, etc.)
- Add concurrency examples and tests

## 5. Implementation Priorities and Timeline

### Phase 1: Core Syntax Updates (1-2 weeks)

1. **Comment Syntax Update**
   - Update parser to only recognize `#` for comments
   - Update all examples and tests
   - Add regression tests

2. **Def Restriction**
   - Modify `EvalDef` to only accept function definitions
   - Update error messages and parsing checks
   - Update all examples and tests

### Phase 2: Type System Enhancements (2-3 weeks)

1. **Type Constants**
   - Define type constants in the core namespace
   - Add type access in M28 code

2. **Type Check Functions**
   - Implement `isinstance`
   - Implement `issubclass`
   - Update object system to work with type checks

3. **Type Annotations**
   - Add support for optional type annotations
   - Implement parser support for type annotations

### Phase 3: Concurrency Model (3-4 weeks)

1. **Goroutine Support**
   - Implement `go` special form
   - Add goroutine scheduling and lifecycle management

2. **Channel Implementation**
   - Create channel data type
   - Implement send/receive operations
   - Add channel state tracking

3. **Select Statement**
   - Implement `select` special form
   - Add support for timeouts and default case

4. **Synchronization Primitives**
   - Implement mutex, waitgroup, etc.
   - Add helper functions for common patterns

### Phase 4: Testing and Documentation (1-2 weeks)

1. **Test Coverage**
   - Create comprehensive tests for new features
   - Add concurrency stress tests
   - Add regression tests for syntax changes

2. **Documentation Updates**
   - Update language reference
   - Add examples for new features
   - Create tutorials for concurrency

## 6. Detailed Implementation Guidelines

### 6.1 Comment Syntax Update

```go
// In parser/parser.go, update removeComments function
func removeComments(input string) (string, map[int]int) {
    var result strings.Builder
    lines := strings.Split(input, "\n")
    lineMap := make(map[int]int)

    charPos := 0
    for lineNum, line := range lines {
        inString := false
        commentPos := -1

        for i, ch := range line {
            lineMap[charPos+i] = lineNum + 1

            if ch == '"' && (i == 0 || line[i-1] != '\\') {
                inString = !inString
            } else if ch == '#' && !inString { // Only recognize # for comments
                commentPos = i
                break
            }
        }

        if commentPos >= 0 {
            line = line[:commentPos]
        }

        result.WriteString(line)
        result.WriteString("\n")
        charPos += len(line) + 1
    }

    return result.String(), lineMap
}
```

### 6.2 Def Restriction

```go
// In special_forms/definition.go, update EvalDef function
func EvalDef(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
    if len(args) < 2 {
        return nil, fmt.Errorf("def requires at least a name and a body")
    }

    // Unwrap the first argument if it's a LocatedValue
    firstArg := args[0]
    if located, isLocated := firstArg.(core.LocatedValue); isLocated {
        firstArg = located.Value
    }

    // Check if we're defining a function
    funcDef, isFuncDef := firstArg.(core.LispList)
    if !isFuncDef || len(funcDef) == 0 {
        return nil, fmt.Errorf("def must define a function, use '=' for variable assignment")
    }

    // Function definition logic remains the same...
    // ...
}
```

### 6.3 Type Checking Implementation

```go
// In builtin/type.go, implement isinstance
func isinstanceFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
    if len(args) < 2 {
        return nil, fmt.Errorf("isinstance() takes at least 2 arguments")
    }

    obj := args[0]
    class := args[1]

    // Handle tuple of classes
    if classTuple, isTuple := class.(core.LispTuple); isTuple {
        for _, cls := range classTuple {
            result, err := checkInstanceOf(obj, cls, env)
            if err != nil {
                return nil, err
            }
            if result == core.PythonicBool(true) {
                return core.PythonicBool(true), nil
            }
        }
        return core.PythonicBool(false), nil
    }

    return checkInstanceOf(obj, class, env)
}

func checkInstanceOf(obj, class core.LispValue, env core.Environment) (core.LispValue, error) {
    // Handle built-in types
    switch class {
    case env.Get("int"):
        _, isInt := obj.(float64)
        return core.PythonicBool(isInt && isInteger(obj)), nil
    case env.Get("float"):
        _, isFloat := obj.(float64)
        return core.PythonicBool(isFloat), nil
    case env.Get("str"):
        _, isStr := obj.(string)
        return core.PythonicBool(isStr), nil
    case env.Get("list"):
        _, isList := obj.(core.LispList)
        return core.PythonicBool(isList), nil
    // ... other type checks ...
    }

    // Handle custom classes
    objClass, hasClass := getObjectClass(obj)
    if !hasClass {
        return core.PythonicBool(false), nil
    }

    return issubclassFunc([]core.LispValue{objClass, class}, env)
}
```

### 6.4 Concurrency Model Implementation

```go
// In special_forms/concurrency.go, implement go statement
func EvalGo(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
    if len(args) == 0 {
        return nil, fmt.Errorf("go requires at least one expression to evaluate")
    }

    // Create a copy of the environment to ensure closure capture
    goEnv := env.Copy()

    // Spawn a goroutine to evaluate the expression
    go func() {
        _, err := e.Eval(args[0], goEnv)
        if err != nil {
            // Log error since we can't return it from the goroutine
            fmt.Printf("Error in goroutine: %v\n", err)
        }
    }()

    return core.None, nil
}

// In builtin/concurrency.go, implement channel creation
func channelFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
    var bufferSize int = 0 // Default to unbuffered channel
    
    if len(args) > 0 {
        // If a buffer size is provided, use it
        if size, ok := args[0].(float64); ok {
            bufferSize = int(size)
        } else {
            return nil, fmt.Errorf("channel buffer size must be a number")
        }
    }
    
    return core.NewPythonicChannel(bufferSize), nil
}
```

## 7. Test Plan

### 7.1 Comment Syntax Tests

- Test that `#` is recognized as comment starter
- Test that `;` is treated as a regular character
- Test comment handling in various contexts (beginning of line, middle, inside strings)

### 7.2 Function Definition Tests

- Test that `def` works correctly for function definitions
- Test that attempting to use `def` for variable definitions produces an error
- Test variable assignment with `=`

### 7.3 Type System Tests

- Test `isinstance` with various types (built-in and custom)
- Test `issubclass` with class hierarchies
- Test type constants and their behavior

### 7.4 Concurrency Tests

- Test goroutine creation and execution
- Test channel communication
- Test select statement with various cases
- Test synchronization primitives
- Test concurrency patterns (worker pools, pipelines, etc.)

## 8. Deployment Strategy

### 8.1 Backward Compatibility

- Deprecate semicolon comments in documentation
- Provide migration guide for code using `def` for variable definitions
- Add warnings for deprecated syntax during transition period

### 8.2 Release Plan

1. **v0.9.0**: Comment syntax and def restriction changes
2. **v0.9.5**: Type system enhancements
3. **v1.0.0-beta**: Initial concurrency model
4. **v1.0.0**: Full specification implementation

### 8.3 Documentation Updates

- Update all examples to use new syntax
- Create dedicated guides for the type system and concurrency model
- Update language reference to reflect changes

## 9. Conclusion

The implementation delta between the current M28 codebase and the updated specification is significant but manageable. By focusing on the four key areas (comment syntax, def restriction, type system, and concurrency model), the codebase can be brought into alignment with the specification in a structured way.

The highest priorities are:
1. Restricting `def` to function definitions only
2. Implementing a proper type checking system
3. Adding concurrency with goroutines and channels

These changes will make M28 a more consistent, powerful language with better Pythonic semantics and powerful concurrency features.