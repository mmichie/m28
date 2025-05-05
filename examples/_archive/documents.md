# M28 Object System Documentation

This document explains how to implement object-oriented programming in M28 using closures.

## Current Limitations

While M28 supports closures and they correctly maintain state between calls, there are some limitations:

1. Each function definition creates a new environment
2. Variables in the environment can be accessed and modified
3. The same code may behave differently when executed in different contexts

## Working Patterns

### 1. Simple Counter (Works Well)

```lisp
(def (make-counter)
  (= count 0)
  (lambda ()
    (= count (+ count 1))
    count))

(= counter1 (make-counter))
(= counter2 (make-counter))

(counter1)  # Returns 1
(counter1)  # Returns 2
(counter2)  # Returns 1 (separate state)
```

This pattern works because:
- Each call to `make-counter` creates a new environment with its own `count` variable
- The lambda captures this environment and can access/modify `count`
- Each lambda maintains its own state between calls

### 2. Single-Method Objects with Arguments (Works Well)

```lisp
(def (make-adder base)
  (lambda (x)
    (+ base x)))

(= add5 (make-adder 5))
(= add10 (make-adder 10))

(add5 3)  # Returns 8
(add10 3) # Returns 13
```

### 3. Dispatch Object Pattern (Partially Works)

```lisp
(def (make-account balance)
  (def (dispatch op)
    (if (== op "balance")
      balance
      (if (== op "deposit")
        (lambda (amount)
          (= balance (+ balance amount))
          balance)
        "unknown")))
  dispatch)

(= acct (make-account 100))
(acct "balance")               # Returns 100
((acct "deposit") 50)          # Returns 150
(acct "balance")               # Returns 150
```

## Best Practices

1. Keep objects simple with minimal state
2. Use the counter pattern for simple state tracking
3. For more complex objects:
   - Use a dispatch function
   - Return different functions based on the operation
   - Keep state in variables captured by the dispatch function
   - Access methods via (obj "method-name")
4. If methods need arguments, have the method return a function:
   ```lisp
   ((account "deposit") 50)
   ```
5. Test extensively - variable capture behavior may be unpredictable in complex cases

## Future Improvements

Future versions of M28 should improve:

1. Better variable capture in nested closures
2. More consistent state management
3. Better error handling for methods with arguments
4. Potentially a dedicated object syntax