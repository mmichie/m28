# List Syntax Guide for M28

## Comma-Separated List Syntax

M28 now supports Python-style comma-separated list literals using square brackets. This allows for more readable list declarations, especially for complex nested data structures.

## Basic Syntax

Lists can be created using square brackets with comma-separated elements:

```lisp
[1, 2, 3, 4, 5]  ; A simple list of numbers
["a", "b", "c"]  ; A list of strings
[1, "two", 3.0]  ; Mixed element types
```

## Using Lists in Files

When working in .m28 files, you can use comma-separated list syntax directly:

```lisp
(= my_list [1, 2, 3, 4, 5])
(print my_list)  ; Will print [1, 2, 3, 4, 5]

(= nested [[1, 2], [3, 4], [5, 6]])
(print nested)   ; Will print [[1, 2], [3, 4], [5, 6]]
```

## Using Lists in the REPL

In the REPL, when using complex data structures like lists with assignment, 
it's recommended to use one of these approaches:

1. Quoted list syntax:
```lisp
(= a '(1 2 3))
```

2. List function:
```lisp
(= a (list 1 2 3))
```

These alternatives ensure proper evaluation in the REPL context.

## List Operations

Lists support the following operations:

```lisp
(nth list 0)         ; Access element by index (0-based)
(len list)           ; Get the length of a list
(+ list1 list2)      ; Concatenate lists
(for item list ...)  ; Iterate over a list
```

## List Comprehensions

M28 also supports list comprehensions:

```lisp
[x for x [1, 2, 3, 4, 5]]            ; Simple mapping
[x for x [1, 2, 3, 4, 5] if (> x 2)] ; With filter condition
```

By using comma-separated list syntax, your M28 code becomes more readable and familiar to those with Python experience while maintaining the powerful Lisp foundation.