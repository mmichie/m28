# Exception Handling in M28

M28 provides a comprehensive exception handling system similar to Python's, with support for try/except/finally blocks, custom exception types, and enhanced traceback information.

## Basic Exception Handling

M28 uses a `try`/`except` mechanism for handling exceptions:

```lisp
(try
  ; Code that might raise an exception
  (/ 10 0)  ; Division by zero error
  (except
    ; Exception handler
    (print "Caught an exception")))
```

## Exception Types

You can catch specific types of exceptions using typed except blocks:

```lisp
(try
  (raise ValueError "Invalid value")
  (except ValueError
    (print "Caught a ValueError"))
  (except TypeError
    (print "Caught a TypeError"))
  (except
    (print "Caught some other exception")))
```

## Accessing Exception Objects

You can capture the exception object for inspection using the `as` syntax:

```lisp
(try
  (raise KeyError "Missing key")
  (except KeyError as e
    (print "Caught exception:" e)))
```

## Finally Blocks

The `finally` block always executes, regardless of whether an exception occurred:

```lisp
(try
  (print "In try block")
  (raise "An error occurred")
  (except
    (print "Caught the exception"))
  (finally
    (print "This always runs")))
```

## Custom Exception Types

You can define custom exception types and create an exception hierarchy:

```lisp
(defexception CustomError)  ; Inherits from Exception
(defexception DatabaseError)
(defexception NetworkError DatabaseError)  ; Inherits from DatabaseError
```

## Nested Exception Handling

M28 supports proper traceback propagation through nested try/except blocks:

```lisp
(try
  (try
    (raise ValueError "Inner exception")
    (except
      (print "Inner handler")
      (raise TypeError "Re-raised exception")))
  (except
    (print "Outer handler caught re-raised exception")))
```

### Re-raising Exceptions

You can re-raise the current exception with `(raise)`:

```lisp
(try
  (/ 10 0)
  (except (e)
    (print "Caught:" e)
    (raise)))  ; Re-raise the same exception
```

### Exception Chaining

You can create exception chains to track cause and effect:

```lisp
(try
  (raise ValueError "Original error")
  (except ValueError (e)
    (raise RuntimeError "New error" e)))  ; e becomes the cause
```

## Traceback Information

Exceptions include detailed traceback information showing the call stack at the point where the exception occurred. This is especially useful for debugging nested function calls and exceptions.

When exceptions propagate through nested contexts, the full traceback is preserved, allowing you to see the complete chain of calls that led to the exception.

## Advanced Features

### Exception Context Preservation

When exceptions are raised in except blocks, they maintain a reference to the original exception as their "cause". This helps track the root cause of problems, even through multiple layers of exception handling.

### Bare Re-raise

A bare `(raise)` statement with no arguments re-raises the current exception, preserving its type, message, and traceback. This is useful for exception handlers that need to perform some action but still want to propagate the exception.

### Environment Integration

The active exception is automatically stored in the environment during exception handling (in the `__active_exception__` variable). This allows for complex error recovery strategies and proper re-raising of exceptions.

## Best Practices

1. **Be Specific**: Catch specific exception types rather than using a bare except block.
2. **Clean Up**: Use finally blocks to ensure resources are properly cleaned up.
3. **Preserve Context**: When converting exceptions, pass the original as the cause.
4. **Informative Messages**: Provide clear error messages that explain what went wrong.
5. **Exception Hierarchy**: Design a sensible exception hierarchy for your application.