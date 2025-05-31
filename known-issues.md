# Known Issues in M28

## Empty List/Dict Literals in Class `__init__`

**Issue**: Using `[]` or `{}` literals directly in class `__init__` methods causes an error.

**Example**:
```m28
(class MyClass
  (def __init__ (self)
    (= self.items [])  # This fails
    (= self.data {}))) # This also fails
```

**Workaround**: Use `(list)` or `(dict)` functions instead:
```m28
(class MyClass
  (def __init__ (self)
    (= self.items (list))  # Works!
    (= self.data (dict)))) # Works!
```

**Note**: This issue only occurs within `__init__` methods. Empty literals work fine:
- In regular code
- In class variables
- In other methods
- When returned from functions

**Status**: Known limitation, workaround available.

## File Opening in `with` Statements

**Issue**: Using `open` directly in a `with` statement sometimes fails with "'function' object does not support the context manager protocol".

**Example**:
```m28
# This might fail:
(with (open "file.txt" "r") as f
  (print (f.read)))
```

**Workaround**: Pre-open the file:
```m28
# This works:
(= f (open "file.txt" "r"))
(with f as file
  (print (file.read)))
```

**Note**: The File type does implement the context manager protocol correctly. This appears to be an evaluation order issue with certain function calls in the `with` expression position.

**Status**: Under investigation, workaround available.