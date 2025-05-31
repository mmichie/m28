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