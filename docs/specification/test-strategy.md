# M28 Testing Strategy

This document outlines a comprehensive testing strategy for the M28 language to ensure that implementation follows the specification and remains stable over time.

## 1. Testing Philosophy

### 1.1 Principles

- **Specification-Driven**: Tests should validate conformance to the language specification.
- **Comprehensive Coverage**: Tests should cover all language features and edge cases.
- **Regression Prevention**: Tests should detect regressions when changes are made.
- **Clear Failures**: When tests fail, they should provide clear information about what failed and why.
- **Documentation by Example**: Tests should serve as executable documentation of language behavior.

### 1.2 Test Categories

1. **Unit Tests**: Test individual components of the interpreter.
2. **Feature Tests**: Test specific language features across components.
3. **Integration Tests**: Test interactions between language features.
4. **Conformance Tests**: Verify compliance with the language specification.
5. **Performance Tests**: Measure and track interpreter performance.
6. **Concurrency Tests**: Verify correct behavior in concurrent contexts.

## 2. Test Organization

### 2.1 Directory Structure

```
/tests
├── unit/             # Tests for interpreter components
│   ├── lexer/        # Lexer tests
│   ├── parser/       # Parser tests
│   ├── evaluator/    # Evaluator tests
│   └── ...
├── features/         # Feature-specific tests
│   ├── arithmetic/   # Arithmetic operations
│   ├── collections/  # Lists, dictionaries, etc.
│   ├── functions/    # Function definition and calling
│   ├── objects/      # Object system and methods
│   ├── modules/      # Module system and imports
│   ├── concurrency/  # Goroutines, channels, etc.
│   └── ...
├── conformance/      # Specification conformance tests
│   ├── core/         # Core language features
│   ├── stdlib/       # Standard library behavior
│   └── ...
├── integration/      # Feature interaction tests
├── performance/      # Performance benchmarks
└── regression/       # Tests for specific bug fixes
```

### 2.2 Test File Naming

- **Unit Tests**: `test_component_behavior.m28`
- **Feature Tests**: `test_feature_aspect.m28`
- **Conformance Tests**: `test_spec_section_number.m28`
- **Integration Tests**: `test_feature1_with_feature2.m28`
- **Regression Tests**: `test_issue_number.m28`

## 3. Testing Approaches

### 3.1 Unit Testing

Unit tests focus on individual components of the interpreter:

- **Lexer Tests**: Verify token recognition and error handling.
- **Parser Tests**: Verify correct AST construction.
- **Evaluator Tests**: Verify expression evaluation.
- **Object System Tests**: Verify object and type behavior.

Example of a unit test for the evaluator:

```lisp
# test_evaluator_arithmetic.m28
(= result1 (+ 2 3))
(assert (== result1 5) "Addition failed")

(= result2 (- 10 4))
(assert (== result2 6) "Subtraction failed")

(= result3 (* 3 4))
(assert (== result3 12) "Multiplication failed")

(= result4 (/ 10 2))
(assert (== result4 5) "Division failed")
```

### 3.2 Feature Testing

Feature tests verify specific language features work correctly:

- **Arithmetic Tests**: Verify correct operator behavior.
- **String Tests**: Verify string operations and methods.
- **Collection Tests**: Verify list, dictionary, and set operations.
- **Control Flow Tests**: Verify if, for, while, etc.
- **Function Tests**: Verify function definition, calling, and closures.
- **Object System Tests**: Verify class definition, inheritance, methods.
- **Concurrency Tests**: Verify goroutines, channels, select, etc.

Example of a feature test for list operations:

```lisp
# test_list_operations.m28
(= my_list (list 1 2 3))

# Test append
(append my_list 4)
(assert (== my_list (list 1 2 3 4)) "Append failed")

# Test get
(= item (get my_list 2))
(assert (== item 3) "Get index failed")

# Test slice
(= slice (slice my_list 1 3))
(assert (== slice (list 2 3)) "Slice failed")

# Test length
(assert (== (len my_list) 4) "Length calculation failed")
```

### 3.3 Conformance Testing

Conformance tests verify the implementation follows the language specification:

- Tests organized by specification section
- Each test validates specific requirements from the spec
- Edge cases and error conditions explicitly tested
- Both positive and negative tests included

Example of a conformance test for type checking:

```lisp
# test_spec_2_2_1_type_checking.m28
# Test type function
(= t1 (type 42))
(assert (== t1 int) "Type of integer incorrect")

(= t2 (type "hello"))
(assert (== t2 str) "Type of string incorrect")

# Test isinstance function
(assert (isinstance 42 int) "isinstance with integer failed")
(assert (isinstance "hello" str) "isinstance with string failed")
(assert (isinstance 3.14 (tuple int float)) "isinstance with tuple of types failed")
(assert (not (isinstance "hello" int)) "isinstance negative test failed")

# Test issubclass function
(class Parent ())
(class Child (Parent))
(assert (issubclass Child Parent) "issubclass with direct inheritance failed")
(assert (not (issubclass Parent Child)) "issubclass negative test failed")
```

### 3.4 Integration Testing

Integration tests verify interactions between different language features:

- Tests combining multiple features
- Focus on feature interactions that could cause issues
- Verify features work correctly in combination

Example of an integration test for objects and exceptions:

```lisp
# test_object_exception_integration.m28
(class MyError (Exception)
  (def (init self message)
    (super.init self message)))

(class ResourceManager ()
  (def (init self name)
    (= self.name name)
    (= self.initialized False))
    
  (def (initialize self)
    (= self.initialized True))
    
  (def (use_resource self)
    (if (not self.initialized)
      (raise (MyError (+ "Resource not initialized: " self.name)))
      "Resource used successfully")))

(= resource (ResourceManager "test"))

# Test exception is raised when resource not initialized
(try
  (resource.use_resource)
  (except MyError as err
    (= error_message (str err)))
  (else
    (assert False "Exception not raised")))

(assert (in "Resource not initialized: test" error_message) "Wrong error message")

# Test resource works after initialization
(resource.initialize)
(= result (resource.use_resource))
(assert (== result "Resource used successfully") "Resource use failed after initialization")
```

### 3.5 Performance Testing

Performance tests measure interpreter performance:

- Execution time benchmarks
- Memory usage measurements
- Scaling behavior with input size
- Concurrency performance

Example of a performance test:

```lisp
# test_performance_fibonacci.m28
(def (fibonacci n)
  (if (< n 2)
    n
    (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(= start_time (time))
(= result (fibonacci 20))
(= end_time (time))
(= elapsed (- end_time start_time))

(print "Fibonacci(20) = " result)
(print "Time elapsed: " elapsed " seconds")

# Compare to previous benchmark results
(assert (< elapsed 2.0) "Performance regression: fibonacci too slow")
```

### 3.6 Concurrency Testing

Concurrency tests verify correct behavior of concurrent features:

- Goroutine creation and execution
- Channel communication
- Select statement behavior
- Synchronization primitives
- Race condition detection
- Deadlock detection

Example of a concurrency test:

```lisp
# test_concurrency_channels.m28
(= ch (channel))
(= results (list))

# Producer goroutine
(go
  (for i (range 5)
    (ch <- i)
    (time.sleep 0.01))
  (close ch))

# Consumer
(for value done (<- ch)
  (if done
    (append results value)
    (break)))

# Verify results
(assert (== results (list 0 1 2 3 4)) "Channel communication failed")
```

## 4. Test Execution

### 4.1 Test Runners

1. **Unit Test Runner**: Fast, focused tests for individual components.
2. **Feature Test Runner**: Runs feature-specific tests.
3. **Conformance Test Runner**: Validates specification compliance.
4. **Integration Test Runner**: Tests feature interactions.
5. **Performance Test Runner**: Runs benchmarks and compares to baselines.
6. **Full Test Suite**: Runs all tests, with coverage reporting.

### 4.2 Continuous Integration

- **Pre-commit Tests**: Run unit and feature tests before commits.
- **Pull Request Tests**: Run full test suite on pull requests.
- **Nightly Tests**: Run extended tests including performance.
- **Release Tests**: Run complete test suite before releases.

### 4.3 Coverage Tracking

- Track test coverage by component and feature.
- Identify untested or undertested areas.
- Set minimum coverage requirements.
- Generate coverage reports for review.

## 5. Test Development Workflow

### 5.1 Specification to Test Process

1. **Specification Review**: Identify testable requirements.
2. **Test Planning**: Design tests to verify requirements.
3. **Test Implementation**: Write test cases.
4. **Test Execution**: Run tests and verify results.
5. **Test Refinement**: Improve tests based on results.

### 5.2 Bug Fix Workflow

1. **Reproduce**: Create test that reproduces the bug.
2. **Fix**: Implement the fix.
3. **Verify**: Run the test to verify the fix.
4. **Regression Test**: Add the test to the regression suite.

### 5.3 Feature Development Workflow

1. **Specification**: Define the feature in the language specification.
2. **Test-Driven Development**: Write tests before implementation.
3. **Implementation**: Implement the feature.
4. **Verification**: Run tests to verify implementation.
5. **Documentation**: Update documentation with examples from tests.

## 6. Test Quality

### 6.1 Test Reviews

- Review tests for correctness, completeness, and clarity.
- Ensure tests verify specification requirements.
- Check for both positive and negative test cases.
- Verify error handling is tested.

### 6.2 Test Maintenance

- Update tests when the specification changes.
- Remove obsolete tests.
- Refactor tests to improve clarity and maintainability.
- Monitor test performance and optimize slow tests.

### 6.3 Metrics

- Test coverage percentage by component and feature.
- Number of tests per feature.
- Test execution time.
- Number of test failures over time.
- Bug detection effectiveness.

## 7. Sample Test Templates

### 7.1 Conformance Test Template

```lisp
# test_spec_X_Y_Z_feature.m28
# Tests for specification section X.Y.Z: [Section Title]

# Setup
(= setup_value 42)

# Test case 1: [Requirement description]
(= result1 (feature_function setup_value))
(assert (== result1 expected1) "Test case 1 failed: [Expected behavior]")

# Test case 2: [Requirement description]
(= result2 (feature_function2 setup_value))
(assert (== result2 expected2) "Test case 2 failed: [Expected behavior]")

# Error case 1: [Error condition description]
(try
  (feature_function invalid_input)
  (except ExpectedError as err
    (= error_message (str err)))
  (else
    (assert False "Error case 1 failed: Expected exception not raised")))

(assert (in "Expected error message" error_message) "Error message incorrect")

# Cleanup (if needed)
(cleanup_function)
```

### 7.2 Feature Test Template

```lisp
# test_feature_aspect.m28
# Tests for [Feature] - [Aspect]

# Basic functionality
(= result1 (feature_function normal_input))
(assert condition "Basic functionality failed: [Explanation]")

# Edge cases
(= result2 (feature_function edge_case))
(assert condition "Edge case failed: [Explanation]")

# Error handling
(try
  (feature_function invalid_input)
  (except ExpectedError
    # This is expected
    (= caught True))
  (else
    (assert False "Error handling failed: Exception not raised")))

(assert caught "Error was not caught properly")
```

### 7.3 Concurrency Test Template

```lisp
# test_concurrency_feature.m28
# Tests for concurrent behavior of [Feature]

# Setup synchronization primitives
(= ch (channel))
(= done_ch (channel))
(= shared_data (dict))
(= mu (mutex))

# Worker goroutine function
(def (worker id)
  (for i (range 10)
    # Do concurrent operations
    (mu.lock)
    (= shared_data.id (+ (get shared_data id 0) 1))
    (mu.unlock)
    
    # Communicate via channel
    (ch <- (+ "Worker " (str id) " iteration " (str i))))
  
  # Signal completion
  (done_ch <- id))

# Launch workers
(for id (range 3)
  (go (worker id)))

# Collect channel messages
(= messages (list))
(= counter 0)
(while (< counter 30)  # 3 workers * 10 iterations
  (append messages (<- ch))
  (= counter (+ counter 1)))

# Wait for workers to complete
(for i (range 3)
  (<- done_ch))

# Verify results
(assert (== (len messages) 30) "Wrong number of messages")
(assert (== (sum (dict.values shared_data)) 30) "Wrong total count")
```

## 8. Conclusion

A comprehensive testing strategy is essential for ensuring that the M28 language implementation follows the specification and remains stable over time. This strategy provides a framework for developing, organizing, and maintaining tests that verify the correctness, performance, and reliability of the language.

By following this testing strategy, the M28 project can:

1. **Maintain Specification Compliance**: Ensure the implementation follows the language specification.
2. **Prevent Regressions**: Detect when changes break existing functionality.
3. **Document Behavior**: Provide executable examples of language features.
4. **Guide Development**: Use tests to drive the implementation of new features.
5. **Build Confidence**: Give users confidence in the stability and correctness of the language.

The test suite should evolve alongside the language, with new tests added as features are added or changed, and existing tests updated to reflect specification changes.