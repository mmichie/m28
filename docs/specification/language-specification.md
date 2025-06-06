# M28 Language Specification

## 1. Core Language Semantics

### 1.1 Syntax

#### 1.1.1 Lexical Structure

**Character Set**
- M28 source code is encoded in UTF-8.
- All Unicode characters are allowed in strings and comments.
- For identifiers, only ASCII alphanumeric characters and underscore (`_`) are permitted.

**Tokens**
- *Identifiers*: Sequences of letters, digits, and underscores that don't start with a digit. (e.g., `x`, `count`, `my_var`)
- *Keywords*: Reserved words with special meaning (e.g., `def`, `if`, `for`, `import`).
- *Literals*: 
  - Numeric literals: integers (`42`) and floating-point numbers (`3.14`).
  - String literals: sequences of characters enclosed in double quotes (`"hello"`) or single quotes (`'world'`).
  - Boolean literals: `True` and `False`.
  - None literal: `None`.
- *Operators*: Symbols like `+`, `-`, `*`, `/`, `%`, etc.
- *Delimiters*: Parentheses `()`, brackets `[]`, braces `{}`, commas `,`, colons `:`, etc.

**Comments**
- Line comments start with `#` and continue to the end of the line.
- M28 does not support multi-line comments.

**Whitespace**
- Whitespace (spaces, tabs, newlines) is used to separate tokens.
- Whitespace is not significant except for separating tokens and line termination.
- Multiple consecutive whitespace characters are treated as a single separator.

#### 1.1.2 Grammar

**S-expression Syntax**

M28 uses Lisp-style S-expressions as its primary syntax structure:

- **Atom**: A single token (identifier, literal, etc.)
- **List**: A sequence of expressions enclosed in parentheses: `(expr1 expr2 ... exprN)`
- **Expression**: Either an atom or a list

The first element of a list typically determines the operation to be performed:
```lisp
(operator operand1 operand2 ... operandN)
```

**Special Forms**

Special forms are expressions with special evaluation rules:

- **Function Definition**: `(def function-name (param1 param2 ...) body...)`
- **Assignment**: `(= variable expression)`
- **Conditional**: `(if condition true-expr false-expr)`
- **Loops**: `(for item collection body...)` or `(while condition body...)`
- **Function calls**: `(function-name arg1 arg2 ...)`

**Python-Style Syntax Extensions**

M28 includes Python-style syntactic elements:

- **Dictionary literals**: `{"key1": value1, "key2": value2, ...}`
- **Keyword arguments**: `(function arg1 arg2 {"key1": value1, "key2": value2})`

**Expression Evaluation Rules**

1. Atoms evaluate to themselves for literals, or to their bound values for identifiers.
2. Lists are evaluated as:
   - If the first element is a special form, apply its special evaluation rules.
   - Otherwise, evaluate all elements of the list, then apply the value of the first element (which should be a function) to the values of the remaining elements (the arguments).

#### 1.1.3 Naming Conventions

**Valid Identifiers**
- Must start with a letter (a-z, A-Z) or underscore (_).
- Can contain letters, digits, and underscores.
- Cannot be a reserved keyword.

**Reserved Keywords**
```
and        break     class      continue   def
elif       else      except     finally    for
from       if        import     in         lambda
none       not       or         pass       raise
return     try       while      with       yield
go         async     await      select     channel
```

**Case Sensitivity**
- M28 is case-sensitive. `variable`, `Variable`, and `VARIABLE` are three distinct identifiers.

**Private Symbols**
- By convention, identifiers starting with an underscore (`_`) are considered private.
- In modules, symbols starting with underscore are not exported by default.

### 1.2 Evaluation Model

#### 1.2.1 Execution Model

**Read-Eval-Print Loop**
- M28 processes code in a Read-Eval-Print Loop (REPL) cycle:
  1. **Read**: Parse input into expressions
  2. **Evaluate**: Execute expressions according to language rules
  3. **Print**: Display the result
  4. **Loop**: Return to step 1

**Expression Evaluation Order**
- Expressions are evaluated from left to right.
- Subexpressions are evaluated before the expressions that contain them.
- Arguments to functions are evaluated before the function is called.

**Tail Call Optimization**
- M28 implements tail call optimization to prevent stack overflow for recursive calls in tail position.
- A tail call is a function call that is the last operation in a function.
- When a function call is in tail position, M28 reuses the current stack frame instead of creating a new one.

**Example**:
```lisp
# Tail-recursive factorial
(def factorial (n acc)
  (if (<= n 1)
      acc
      (factorial (- n 1) (* n acc))))
```

#### 1.2.2 Scope Rules

**Lexical Scoping**
- M28 uses lexical (static) scoping, where variable references are resolved based on the static structure of the program.
- A variable is visible within the block where it is defined and in nested blocks.

**Scope Types**
- **Global scope**: Variables defined at the top level of a module.
- **Function scope**: Variables defined within a function, including parameters.
- **Block scope**: Variables defined within a block (e.g., if, for, while).

**Variable Lookup Rules**
1. Look in the current local scope.
2. If not found, look in the enclosing scope, and continue up the scope chain.
3. If not found in any enclosing scope, look in the global scope.
4. If still not found, report a NameError.

**Closures**
- Functions in M28 capture their lexical environment, forming closures.
- A closure allows a function to access variables from the scope where it was defined, even after that scope has exited.

**Example**:
```lisp
(def make-counter ()
  (= count 0)
  (def increment ()
    (= count (+ count 1))
    count)
  increment)

(= counter (make-counter))
(counter)  # Returns: 1
(counter)  # Returns: 2
```

**Variable Shadowing**
- A variable defined in an inner scope can shadow (hide) a variable with the same name in an outer scope.
- The inner variable takes precedence within its scope.

**Example**:
```lisp
(= x 10)
(def test ()
  (= x 20)
  (print x))  # Prints: 20
(test)
(print x)     # Prints: 10
```

#### 1.2.3 Binding Semantics

**Assignment Semantics**
- The `=` operator binds a value to a variable name.
- If the variable exists in the current scope, its value is updated.
- If the variable doesn't exist, it is created in the current scope.

**Variable Declarations**
- Variables are implicitly declared upon first assignment.
- The `def` special form is strictly for function definitions, not variable declarations.

**Value Semantics vs. Reference Semantics**
- **Primitive types** (numbers, booleans, None) use value semantics - they are passed by value.
- **Composite types** (lists, dictionaries, objects) use reference semantics - they are passed by reference.

**Example**:
```lisp
# Primitive type (passed by value)
(= x 10)
(= y x)
(= x 20)
(print y)  # Prints: 10

# Composite type (passed by reference)
(= a (list 1 2 3))
(= b a)
(= (get a 0) 99)
(print b)  # Prints: (99 2 3)
```

**Immutability and Mutability**
- **Immutable types**: Numbers, strings, booleans, None, tuples.
- **Mutable types**: Lists, dictionaries, sets, objects.
- Operations on immutable types create new values.
- Operations on mutable types modify the existing value.

## 2. Type System

### 2.1 Type Hierarchy

#### 2.1.1 Primitive Types

**Numbers**
- **Integers**: Whole numbers with arbitrary precision (e.g., `42`, `-7`)
- **Floating-Point**: Decimal numbers with IEEE 754 double precision (e.g., `3.14`, `-0.5`)
- Numeric literals are interpreted as integers unless they contain a decimal point.

**Strings**
- Immutable sequences of Unicode characters.
- Defined with single (`'text'`) or double (`"text"`) quotes.
- Support escape sequences: `\n` (newline), `\t` (tab), `\"` (quote), `\\` (backslash).
- Strings can be concatenated with the `+` operator.

**Booleans**
- Two values: `True` and `False`.
- Used for logical operations and conditionals.
- Boolean operations: `and`, `or`, `not`.

**None**
- Represents the absence of a value.
- Similar to Python's `None` or Lisp's `nil`.
- Used as the default return value for functions that don't explicitly return a value.

**Symbols**
- Identifiers that represent names in the program.
- Used to reference variables, functions, and other named entities.
- Internally represented as strings but treated specially during evaluation.

#### 2.1.2 Collection Types

**Lists**
- Ordered, mutable sequences of elements.
- Created with the `list` function or as literals: `(list 1 2 3)`.
- Support indexing, slicing, and various operations (append, remove, etc.).
- Elements can be of any type, including mixed types.

**Tuples**
- Ordered, immutable sequences of elements.
- Created with the `tuple` function: `(tuple 1 2 3)`.
- Similar to lists but cannot be modified after creation.
- Used for fixed collections where values shouldn't change.

**Dictionaries**
- Mutable mappings of keys to values.
- Created with the `dict` function or as literals: `{"key1": value1, "key2": value2}`.
- Keys must be immutable (strings, numbers, tuples of immutables).
- Values can be of any type.
- Support lookup, insertion, deletion, and various dictionary operations.

**Sets**
- Unordered collections of unique elements.
- Created with the `set` function: `(set 1 2 3)`.
- Support set operations: union, intersection, difference, etc.
- Elements must be immutable (hashable).

#### 2.1.3 Callable Types

**Functions**
- First-class values that can be called with arguments.
- Defined with the `def` special form.
- Can be passed as arguments, returned from functions, and stored in variables.

**Example**:
```lisp
(def (add a b)
  (+ a b))
```

**Lambdas**
- Anonymous functions defined with the `lambda` special form.
- Used for simple functions or functions that don't need a name.

**Example**:
```lisp
(= square (lambda (x) (* x x)))
```

**Methods**
- Functions that are bound to objects.
- Accessed through dot notation or dictionary lookup.
- Always take the object itself as the first parameter (conventionally named `self`).

**Example**:
```lisp
(def make-person (name)
  (= person {})
  (def person.greet (self)
    (print (+ "Hello, my name is " name)))
  person)
```

**Special Forms**
- Expressions with special evaluation rules.
- Not first-class values (cannot be passed as arguments or stored in variables).
- Include core language constructs like `if`, `def`, `=`, `for`, etc.

#### 2.1.4 User-Defined Types

**Classes**
- Templates for creating objects with common structure and behavior.
- Defined with the `class` special form.
- Can have methods, properties, and inheritance relationships.

**Example**:
```lisp
(class Person ()
  (def __init__ (self name)
    (= self.name name))
  
  (def greet (self)
    (print (+ "Hello, my name is " self.name))))
```

**Objects**
- Instances of classes or custom data structures.
- Can have properties (attributes) and methods.
- Accessed through dot notation or dictionary-like access.

**Interfaces/Protocols**
- M28 uses duck typing rather than formal interfaces.
- Objects are compatible with operations if they support the required methods.
- Common protocols include Iterable, Callable, Context Manager, etc.

### 2.2 Type Operations

#### 2.2.1 Type Checking

**Type Functions**
- `(type obj)`: Returns the type of an object
- `(isinstance obj class_or_tuple)`: Checks if object is an instance of a class or any class in a tuple
- `(issubclass class class_or_tuple)`: Checks if a class is a subclass of another class

**Type Constants**
M28 provides built-in type objects that can be used with `isinstance` and `issubclass`:
- `int`, `float`, `str`, `bool`, `list`, `dict`, `tuple`, `set`, `function`, etc.

**Examples**:
```lisp
# Check if x is an integer
(isinstance x int)

# Check if x is either an integer or a float
(isinstance x (tuple int float))

# Check if x's type is dict
(== (type x) dict)

# Check if one class is a subclass of another
(issubclass ChildClass ParentClass)
```

**Optional Type Annotations**
```lisp
# Function with type annotations
(def (add [a int] [b int]) -> int
  (+ a b))

# Class with type annotations
(class Person ()
  (def (init [self Person] [name str] [age int]) -> None
    (= self.name name)
    (= self.age age)))
```

#### 2.2.2 Type Conversion

**Explicit Conversion Functions**
- `int` - Converts a value to an integer.
- `float` - Converts a value to a floating-point number.
- `str` - Converts a value to a string.
- `list` - Converts an iterable to a list.
- `tuple` - Converts an iterable to a tuple.
- `dict` - Creates a dictionary from key-value pairs.
- `set` - Converts an iterable to a set.
- `bool` - Converts a value to a boolean.

**Conversion Rules**
- **String to Number**: Strings containing numeric literals can be converted to numbers.
  - Example: `(int "42")` returns `42`.
  - Fails if the string is not a valid numeric literal.
- **Number to String**: Numbers can always be converted to strings.
  - Example: `(str 42)` returns `"42"`.
- **Between Numeric Types**: Conversion between integer and float is always possible.
  - Example: `(float 42)` returns `42.0`.
  - Example: `(int 3.14)` returns `3` (truncation).
- **Collections**: Conversion between collection types (list, tuple, set) preserves elements.
  - Example: `(list (tuple 1 2 3))` returns `(1 2 3)`.
  - Dictionaries convert to lists of key-value pairs.

**Conversion Failure Behavior**
- Failed conversions raise appropriate exceptions.
- `ValueError` for invalid literal format.
- `TypeError` for incompatible types.

#### 2.2.3 Operator Behavior

**Arithmetic Operators**
- `+`: Addition for numbers, concatenation for strings and lists.
- `-`: Subtraction for numbers.
- `*`: Multiplication for numbers, repetition for strings and lists with integers.
- `/`: Division for numbers (always returns a float).
- `//`: Integer division for numbers (returns an integer).
- `%`: Modulo for numbers.
- `**`: Exponentiation for numbers.

**Comparison Operators**
- `==`: Equality comparison.
- `!=`: Inequality comparison.
- `<`, `>`, `<=`, `>=`: Ordering comparisons.

**Type-Specific Behavior**
- **Numbers**: Standard mathematical operations.
- **Strings**: Concatenation with `+`, repetition with `*`.
- **Lists**: Concatenation with `+`, repetition with `*`.
- **Dictionaries**: No operator overloading, use methods instead.
- **Sets**: No operator overloading, use set operation functions.

**Coercion Rules for Mixed Types**
- **Number + Number**: If one operand is float, the result is float; otherwise, integer.
- **String + Any**: Converts the other operand to string and concatenates.
- **List + List**: Concatenates the lists.
- **Other combinations**: Type error if not compatible.

**Example**:
```lisp
(+ 2 3)        # Returns: 5
(+ 2.0 3)      # Returns: 5.0
(+ "Hello, " "world!")  # Returns: "Hello, world!"
(+ (list 1 2) (list 3 4))  # Returns: (1 2 3 4)
(* 3 "abc")    # Returns: "abcabcabc"
```

## 3. Object Protocol

### 3.1 Object Model

#### 3.1.1 Object Structure

**Object Representation**
- Objects in M28 are represented as collections of properties and methods.
- An object has:
  - **Properties**: Named values stored in the object.
  - **Methods**: Functions associated with the object.
  - **Class**: The template from which the object was created (for class instances).

**Property Access Mechanisms**
1. **Dot Notation**: `object.property` to get a property value.
2. **Dictionary-Like Access**: `(get object "property")` to get a property value.
3. **Assignment with Dot**: `(= object.property value)` to set a property.
4. **Dictionary Set**: `(dict.set object "property" value)` to set a property.

**Attribute Storage**
- Properties are stored in a dictionary-like structure internal to the object.
- Methods are stored separately for efficiency but accessed through the same mechanisms.
- Class attributes are stored in the class and inherited by instances.

**Method Representation**
- A method is a function that takes the object itself as its first parameter.
- Methods are bound to their objects when accessed, creating a bound method object.
- A bound method automatically provides the object as the first argument when called.

#### 3.1.2 Object Protocol Interface

**ObjectProtocol Interface**
- The core interface that all objects implement either directly or through adapters.
- Provides a unified way to access properties and methods across different object types.

**Core Methods of ObjectProtocol**:
```go
// In Go implementation
type ObjectProtocol interface {
    // Get an attribute (property or method)
    GetAttribute(name string, eval Evaluator, env Environment) (LispValue, error)
    
    // Set an attribute value
    SetAttribute(name string, value LispValue, eval Evaluator, env Environment) error
    
    // Call a method
    CallMethod(name string, args []LispValue, eval Evaluator, env Environment) (LispValue, error)
    
    // Check if an attribute exists
    HasAttribute(name string) bool
}
```

**Error Handling**
- **AttributeError**: Raised when accessing a non-existent property.
- **TypeError**: Raised when an object doesn't support the operation.
- **Exception Propagation**: Errors from method calls propagate normally.

#### 3.1.3 Method Dispatch

**Method Lookup Rules**
1. Check the object's own methods.
2. If not found and the object has a class, check the class's methods.
3. If the class has a parent class, continue up the inheritance chain.
4. If not found, raise an AttributeError.

**Self Parameter Binding**
- The first parameter of a method (conventionally named `self`) is automatically bound to the object when the method is called.
- This binding happens when the method is accessed, creating a bound method object.

**Method Resolution Order**
- For inheritance, M28 uses a depth-first search of the inheritance graph.
- If multiple inheritance is supported, a C3 linearization algorithm is used to determine the method resolution order.

**Example**:
```lisp
# Defining a method
(def (person.greet self)
  (print (+ "Hello, I am " self.name)))

# Calling a method
(person.greet person)  # self is bound to person
```

### 3.2 Class System

#### 3.2.1 Class Definition

**Class Declaration Syntax**
```lisp
(class ClassName (ParentClass1 ParentClass2 ...)
  # Class body - methods and class attributes
  (def (init self arg1 arg2)
    # Constructor body
    (= self.attr1 arg1)
    (= self.attr2 arg2))
  
  (def (method1 self)
    # Method body
    )
  
  (= class-attr value))
```

**Constructor Semantics**
- The `init` method is called when a new instance is created.
- It initializes the instance's properties.
- Arguments to the class call are passed to the `init` method.

**Class Attributes vs. Instance Attributes**
- **Class attributes**: Defined directly in the class body with `=`.
- **Instance attributes**: Defined in methods (typically in `init`) with `self.attr`.
- Class attributes are shared among all instances.
- Instance attributes are specific to each instance.

**Static Methods and Properties**
- **Static methods**: Methods that don't require an instance (`self`).
- **Class methods**: Methods that take the class as the first parameter (instead of an instance).
- Defined using special decorators or by directly assigning functions to class attributes.

#### 3.2.2 Inheritance

**Inheritance Syntax**
```lisp
(class ChildClass (ParentClass1 ParentClass2 ...)
  # Child class body
)
```

**Multiple Inheritance**
- M28 supports multiple inheritance.
- A class can inherit from multiple parent classes.
- Method resolution uses C3 linearization to determine which method to call.

**Method Overriding**
- A child class can redefine methods inherited from parent classes.
- The child's method takes precedence over the parent's method.

**Super Method Access**
- The `super` function allows calling methods from parent classes.
- It bypasses the normal method resolution to call the parent's version of a method.

**Example**:
```lisp
(class Animal ()
  (def (init self name)
    (= self.name name))
  
  (def (speak self)
    (print "Generic animal sound")))

(class Dog (Animal)
  (def (init self name breed)
    (super.init self name)  # Call parent constructor
    (= self.breed breed))
  
  (def (speak self)
    (print "Woof!")))
```

#### 3.2.3 Object Creation

**Instantiation Process**
1. Allocate a new empty object.
2. Link the object to its class.
3. Call the class's `init` method with the new object as `self` and any provided arguments.
4. Return the initialized object.

**Constructor Arguments**
- Arguments provided when creating an instance are passed to the `init` method.
- Named arguments can be passed using a dictionary as the last argument.

**Default Attributes**
- Instance attributes are typically set in the `init` method.
- Class attributes provide defaults that can be overridden by instance attributes.

**Example**:
```lisp
(= dog (Dog "Rex" "Labrador"))  # Creates a new Dog instance
```

### 3.3 Special Object Types

#### 3.3.1 Metaclasses

**Metaclass Concept**
- A metaclass is a class whose instances are classes.
- It allows customizing class creation and behavior.

**Class Customization Points**
- Class creation
- Method definition
- Attribute access
- Instance creation

**Metaclass Hierarchy**
- `type` is the default metaclass for all classes.
- Custom metaclasses inherit from `type`.
- A class's metaclass can be specified during class definition.

#### 3.3.2 Protocol Objects

**Iterator Protocol**
- Classes that implement `__iter__` and `__next__` methods.
- Used with `for` loops and other iteration contexts.
- `__iter__` returns the iterator object.
- `__next__` returns the next value or raises `StopIteration` when done.

**Context Manager Protocol**
- Classes that implement `__enter__` and `__exit__` methods.
- Used with the `with` statement for resource management.
- `__enter__` is called when entering the context and returns a value.
- `__exit__` is called when exiting the context (even if an exception occurred).

**Example**:
```lisp
(class FileHandler ()
  (def (init self filename mode)
    (= self.filename filename)
    (= self.mode mode)
    (= self.file None))
  
  (def (__enter__ self)
    (= self.file (open self.filename self.mode))
    self.file)
  
  (def (__exit__ self exc-type exc-val exc-tb)
    (if self.file
        (self.file.close))))

(with (FileHandler "example.txt" "w") as file
  (file.write "Hello, world!"))
```

**Callable Protocol**
- Objects that implement the `__call__` method can be called like functions.
- The `__call__` method takes arguments and returns a value.

**Container Protocols**
- **Sequence**: `__len__` and `__getitem__` for indexed access.
- **Mapping**: `__getitem__`, `__setitem__`, and `__delitem__` for key-based access.
- **Set**: `__contains__`, `__iter__`, and `__len__` for membership testing and iteration.

## 4. Standard Library

### 4.1 Built-in Functions

#### 4.1.1 Core Functions

**Arithmetic Functions**
- `(+ a b ...)`: Addition
- `(- a b ...)`: Subtraction
- `(* a b ...)`: Multiplication
- `(/ a b)`: Division (float result)
- `(// a b)`: Integer division
- `(% a b)`: Modulo
- `(** a b)`: Exponentiation
- `(abs x)`: Absolute value
- `(round x [n])`: Round to n digits

**String Functions**
- `(str x)`: Convert to string
- `(len s)`: String length
- `(slice s start [end])`: Substring
- `(split s [sep])`: Split string
- `(join sep seq)`: Join sequence with separator
- `(strip s)`: Remove whitespace
- `(replace s old new)`: Replace substrings
- `(lower s)`: Lowercase
- `(upper s)`: Uppercase

**Collection Functions**
- `(len collection)`: Collection size
- `(get collection key [default])`: Get item at key/index
- `(set collection key value)`: Set item at key/index
- `(del collection key)`: Delete item at key/index
- `(in element collection)`: Membership test
- `(append list item)`: Add to list
- `(insert list index item)`: Insert at position
- `(pop list [index])`: Remove and return item
- `(keys dict)`: Dictionary keys
- `(values dict)`: Dictionary values
- `(items dict)`: Dictionary key-value pairs

**Type Conversion Functions**
- `(int x)`: Convert to integer
- `(float x)`: Convert to float
- `(str x)`: Convert to string
- `(list x)`: Convert to list
- `(tuple x)`: Convert to tuple
- `(dict x)`: Convert to dictionary
- `(set x)`: Convert to set
- `(bool x)`: Convert to boolean

**I/O Functions**
- `(print x...)`: Print to stdout
- `(input [prompt])`: Read from stdin
- `(open filename [mode])`: Open file
- `(read file [size])`: Read from file
- `(write file data)`: Write to file
- `(close file)`: Close file

#### 4.1.2 Higher-Order Functions

**Map**
- `(map function sequence)`: Apply function to each item
- Example: `(map (lambda (x) (* x 2)) (list 1 2 3))` → `(2 4 6)`

**Filter**
- `(filter predicate sequence)`: Keep items where predicate is true
- Example: `(filter (lambda (x) (> x 0)) (list -1 0 1 2))` → `(1 2)`

**Reduce**
- `(reduce function sequence [initial])`: Accumulate result by applying function
- Example: `(reduce + (list 1 2 3 4))` → `10`

**Function Composition**
- `(compose f g ...)`: Return function that applies g, then f, etc.
- Example: `(= double-square (compose (lambda (x) (* x 2)) (lambda (x) (* x x))))`

**Partial Application**
- `(partial function arg1 arg2 ...)`: Return function with some arguments fixed
- Example: `(= add5 (partial + 5))`

#### 4.1.3 Utility Functions

**Debug Functions**
- `(debug x)`: Print debug information
- `(trace x)`: Print with trace information
- `(assert condition [message])`: Verify condition or raise error

**System Functions**
- `(time expr)`: Execute expression and return time
- `(gc)`: Force garbage collection
- `(exit [code])`: Exit program
- `(system command)`: Execute shell command
- `(getenv name)`: Get environment variable
- `(setenv name value)`: Set environment variable

**Reflection Functions**
- `(type x)`: Get type of value
- `(dir object)`: List object attributes
- `(globals)`: Get global symbol table
- `(locals)`: Get local symbol table
- `(eval expr)`: Evaluate expression string

### 4.2 Module System

#### 4.2.1 Module Structure

**Module Definition**
- A module is simply an M28 file with definitions.
- All top-level definitions are module attributes.
- Modules can define functions, variables, classes, etc.

**Export Mechanisms**
- By default, all top-level definitions are exported.
- A module can define a list `__exports__` to explicitly control exports.
- Names starting with underscore (`_`) are considered private and not exported by default.

**Example**:
```lisp
# In module my_module.m28
(= x 10)
(= _private 20)

(def (public_function)
  (print "This is public"))

(def (_private_function)
  (print "This is private"))

# Optional explicit exports
(= __exports__ (list "x" "public_function"))
```

**Symbol Visibility**
- **Public symbols**: Accessible when the module is imported.
- **Private symbols**: Only accessible within the module.
- **Internal symbols**: Used for implementation details.

**Module Attributes**
- **`__name__`**: The name of the module.
- **`__file__`**: The file path of the module.
- **`__exports__`**: List of explicitly exported symbols (if defined).

#### 4.2.2 Import Mechanics

**Import Syntax**
- Basic import: `(import "module_name")`
- Aliased import: `(import "module_name" as alias)`
- Selective import: `(from "module_name" import symbol1 symbol2 ...)` (not yet supported)
- Wildcard import: `(from "module_name" import *)` (not yet supported)

**Module Resolution Algorithm**
1. Check if the module is already loaded in cache.
2. If not, search for the module in:
   - Current directory
   - Directories in `PYTHONPATH` environment variable
   - Standard library directories
3. Load and execute the module file.
4. Cache the module for future imports.

**Circular Import Handling**
- Detection of circular imports during module loading.
- Partial initialization to break circular dependencies.
- Warning when circular imports are detected.

**Namespace Management**
- Each import creates a new namespace in the current scope.
- Imported names are accessed through the module namespace: `module.symbol`.
- Selective imports bring specific symbols into the current namespace.
- Wildcard imports bring all exported symbols into the current namespace.

**Example**:
```lisp
# Basic import
(import "math")
(print math.pi)

# Aliased import
(import "very_long_module_name" as short)
(print short.function)

# Note: from-import syntax not yet supported in v0.1.0
# Use module.attribute notation instead:
(import "math")
(print math.pi)
(print (math.sin 0))
```

#### 4.2.3 Standard Modules

**Core Library Modules**
- `math`: Mathematical functions and constants.
- `string`: String manipulation functions.
- `os`: Operating system interfaces.
- `io`: Input/output facilities.
- `re`: Regular expression operations.
- `datetime`: Date and time handling.
- `random`: Random number generation.
- `json`: JSON encoding and decoding.
- `sys`: System-specific parameters and functions.

**Extension Module Mechanism**
- Modules can be implemented in Go and registered with the interpreter.
- Extension modules provide access to system functionality.
- Extension modules follow the same import mechanics as M28 modules.

**Standard Module Behavior**
- Consistent interface across implementations.
- Well-defined error handling.
- Complete documentation of functions and behavior.
- Platform-independent where possible.

## 5. Control Flow

### 5.1 Conditionals

#### 5.1.1 Conditional Expressions

**If/Else Syntax**
```lisp
(if condition
    true-expression
    false-expression)
```

**If/Elif/Else Pattern**
```lisp
(if condition1
    result1
    (if condition2
        result2
        result3))
```

**Truthiness Rules**
- The following values are considered false:
  - `False`
  - `None`
  - Zero of any numeric type (`0`, `0.0`)
  - Empty collections (`""`, `()`, `[]`, `{}`, `set()`)
- All other values are considered true.

**Short-Circuit Evaluation**
- Logical operators evaluate only as many operands as needed.
- `(and x y)`: If `x` is false, `y` is not evaluated; returns `x`.
- `(or x y)`: If `x` is true, `y` is not evaluated; returns `x`.
- `(not x)`: Returns `True` if `x` is false, `False` otherwise.

**Example**:
```lisp
(if (> x 0)
    (print "x is positive")
    (print "x is zero or negative"))

(if (and (> x 0) (< x 10))
    (print "x is between 0 and 10"))
```

#### 5.1.2 Pattern Matching

**Match Expression**
```lisp
(match value
  (pattern1 expression1)
  (pattern2 expression2)
  ...
  (else default-expression))
```

**Pattern Types**
- **Literal patterns**: Match exact values.
- **Variable patterns**: Bind a variable to the matched value.
- **Wildcard pattern**: Matches any value.
- **Sequence patterns**: Match sequences with specific structures.
- **Mapping patterns**: Match dictionaries with specific keys.
- **Class patterns**: Match instances of specific classes.

**Example**:
```lisp
(match shape
  (("circle" radius)
   (* pi radius radius))
  
  (("rectangle" width height)
   (* width height))
  
  (("square" side)
   (* side side))
  
  (else
   (print "Unknown shape")
   0))
```

### 5.2 Iteration

#### 5.2.1 Loops

**For Loop Syntax**
```lisp
(for item collection
  body-expression1
  body-expression2
  ...)
```

**While Loop Syntax**
```lisp
(while condition
  body-expression1
  body-expression2
  ...)
```

**Loop Control**
- `(break)`: Exit the loop immediately.
- `(continue)`: Skip to the next iteration of the loop.

**Iteration Protocol**
- Objects are iterable if they implement the iterator protocol.
- The `for` loop calls `__iter__` to get an iterator, then repeatedly calls `__next__` until `StopIteration` is raised.
- Built-in collections (lists, strings, dictionaries, sets) are iterable.

**Example**:
```lisp
# For loop
(for x (range 5)
  (print x))

# While loop
(= i 0)
(while (< i 5)
  (print i)
  (= i (+ i 1)))

# Break and continue
(for x (range 10)
  (if (== x 3)
    (continue))
  (if (== x 8)
    (break))
  (print x))
```

#### 5.2.2 Comprehensions

**List Comprehensions**
```lisp
# List comprehensions are supported in M28 v0.1.0
# Note: expressions inside must use prefix notation
(= squares [(* x x) for x in (range 10)])
(= evens [(* x 2) for x in (range 10) if (== (% x 2) 0)])
```

**Example**:
```lisp
# List of squares
(= squares
  (for (x (range 10))
    (* x x)))
# Result: (0 1 4 9 16 25 36 49 64 81)

# Filtered list comprehension
(= even-squares
  (for (x (range 10))
    (if (== (% x 2) 0)
      (* x x)
      (continue))))
# Result: (0 4 16 36 64)
```

**Set Comprehensions**
```lisp
(= result
  (set
    (for (x collection)
      (transform x))))
```

**Dictionary Comprehensions**
```lisp
(= result
  (dict
    (for (x collection)
      (key-expr x) (value-expr x))))
```

**Generator Expressions**
- Similar to list comprehensions but produce values on-demand.
- Created implicitly when a comprehension is used in iteration context.

### 5.3 Exceptions

#### 5.3.1 Exception Handling

**Try/Except Syntax**
```lisp
(try
  body-expression1
  body-expression2
  ...
  (except [ExceptionType [as variable]]
    handler-expression1
    handler-expression2
    ...)
  ...
  (finally
    cleanup-expression1
    cleanup-expression2
    ...))
```

**Exception Propagation**
- When an exception occurs, it propagates up the call stack.
- Unless caught by an `except` clause, it will terminate the program.
- Multiple `except` clauses can handle different exception types.

**Resource Cleanup with Finally**
- The `finally` clause is always executed, whether an exception occurred or not.
- Used for cleanup operations like closing files.

**Example**:
```lisp
(try
  (= x (/ 10 0))
  (except ZeroDivisionError as err
    (print "Cannot divide by zero:" err))
  (except TypeError
    (print "Type error occurred"))
  (finally
    (print "Cleanup code here")))
```

#### 5.3.2 Exception Hierarchy

**Standard Exceptions**
- `Exception`: Base class for all exceptions.
- `SyntaxError`: Invalid syntax.
- `NameError`: Name not found.
- `TypeError`: Operation or function applied to inappropriate type.
- `ValueError`: Operation or function received argument of correct type but inappropriate value.
- `AttributeError`: Attribute not found.
- `IndexError`: Index out of range.
- `KeyError`: Key not found in mapping.
- `ZeroDivisionError`: Division by zero.
- `IOError`: I/O operation failure.

**Custom Exceptions**
- New exception types can be defined by inheriting from `Exception`.
- Custom exceptions can have custom attributes and methods.

**Example**:
```lisp
(class CustomError (Exception)
  (def (init self message)
    (super.init self message)
    (= self.message message)))

(def (risky_function)
  (if (< (random) 0.5)
    (raise (CustomError "Something bad happened"))))
```

**Best Practices**
1. Catch specific exceptions, not all exceptions.
2. Keep the try block small.
3. Release resources in finally blocks.
4. Use custom exceptions for domain-specific errors.
5. Include meaningful error messages.

## 6. Advanced Features

### 6.1 Generators and Coroutines

#### 6.1.1 Generator Functions

**Yield Statement**
- The `yield` statement suspends a function's execution and sends a value back.
- When the generator function is called again, it resumes where it left off.

**Generator Objects**
- Created when a generator function is called.
- Implement the iterator protocol.
- `__next__` method resumes the function until the next `yield`.
- `__iter__` method returns the generator itself.

**Example**:
```lisp
(def (count-up-to n)
  (= i 0)
  (while (< i n)
    (yield i)
    (= i (+ i 1))))

(for x (count-up-to 5)
  (print x))
# Prints: 0 1 2 3 4
```

**Generator Methods**
- `(generator.send value)`: Send a value into the generator.
- `(generator.throw exception)`: Throw an exception into the generator.
- `(generator.close)`: Close the generator.

**Example**:
```lisp
(def (echo)
  (= received (yield "Ready"))
  (while True
    (= received (yield received))))

(= gen (echo))
(print (gen.__next__))  # Prints: "Ready"
(print (gen.send "Hello"))  # Prints: "Hello"
(print (gen.send "World"))  # Prints: "World"
```

#### 6.1.2 Asynchronous Programming

**Async/Await**
- The `async` keyword defines a coroutine function.
- The `await` expression suspends the coroutine until the awaited coroutine completes.
- Coroutines are executed by an event loop.

**Example**:
```lisp
(async (def fetch-data url)
  (= response (await (http.get url)))
  (= data (await (response.json)))
  data)

(async (def main)
  (= result (await (fetch-data "https://example.com/api")))
  (print result))

(event-loop.run (main))
```

**Coroutine Objects**
- Created when an async function is called.
- Not executed immediately; must be awaited or scheduled on an event loop.
- Return future/promise-like objects representing the eventual result.

**Event Loop Integration**
- Standard event loop for scheduling coroutines.
- Functions for creating and running tasks.
- Support for timeouts, callbacks, and other async patterns.

**Asynchronous Generators**
- Combine async functions and generators.
- Created with `async` and `yield`.
- Consumed with `async for` loops.

### 6.2 Context Managers

#### 6.2.1 Context Manager Protocol

**With Statement**
```lisp
(with context-expr [as variable]
  body-expr1
  body-expr2
  ...)
```

**Required Methods**
- `__enter__(self)`: Called when entering the context.
- `__exit__(self, exc_type, exc_value, traceback)`: Called when exiting the context.

**Resource Management**
- Context managers ensure proper acquisition and release of resources.
- Common use cases: file handling, locks, database connections.
- The `__exit__` method is called even if an exception occurs.

**Exception Handling**
- If an exception occurs in the body, it's passed to `__exit__`.
- If `__exit__` returns True, the exception is suppressed.
- Otherwise, the exception propagates outside the context.

**Example**:
```lisp
(class FileLock ()
  (def (init self filename)
    (= self.filename filename)
    (= self.lock-file None))
  
  (def (__enter__ self)
    (= self.lock-file (open (+ self.filename ".lock") "w"))
    self)
  
  (def (__exit__ self exc-type exc-val exc-tb)
    (if self.lock-file
      (do
        (self.lock-file.close)
        (os.remove (+ self.filename ".lock"))))))

(with (FileLock "data.txt") as lock
  (print "Resource acquired")
  (process-data))
```

#### 6.2.2 Built-in Context Managers

**File Context Manager**
```lisp
(with (open "filename.txt" "r") as file
  (print (file.read)))
```

**Lock Context Manager**
```lisp
(with (threading.Lock) as lock
  (print "Critical section"))
```

**Creating Custom Context Managers**
- Define a class with `__enter__` and `__exit__` methods.
- Use for resource management, transaction control, temporary state changes, etc.

**Example**:
```lisp
(class TempDirectory ()
  (def (init self prefix)
    (= self.prefix prefix)
    (= self.path None))
  
  (def (__enter__ self)
    (= self.path (tempfile.mkdtemp self.prefix))
    self.path)
  
  (def (__exit__ self exc-type exc-val exc-tb)
    (if self.path
      (shutil.rmtree self.path))))

(with (TempDirectory "test-") as temp-dir
  (print (+ "Working in " temp-dir))
  (process-files temp-dir))
```

### 6.3 Concurrency Model

#### 6.3.1 Goroutine-based Concurrency

**Go Statement**
- The `go` statement spawns a lightweight thread (goroutine).
- Runs concurrently with other goroutines.
- Maps directly to Go's goroutines in the implementation.

**Example**:
```lisp
# Spawn a goroutine
(go
  (do-work data))

# Main execution continues immediately
(print "Started worker")
```

#### 6.3.2 Channel Communication

**Channel Creation**
- Create channels for communication between goroutines.
- Buffered and unbuffered channels are supported.

**Example**:
```lisp
# Unbuffered channel
(= ch (channel))

# Buffered channel with capacity 5
(= buf-ch (channel 5))
```

**Channel Operations**
- Send value to channel: `(ch <- value)`
- Receive from channel: `(<- ch)`
- Close channel: `(close ch)`

**Example**:
```lisp
# Send operation
(ch <- 42)

# Receive operation
(= result (<- ch))

# Receive with done indicator
(= value done (<- ch))
# done will be False when channel is closed
```

#### 6.3.3 Select Statement

**Select Syntax**
- Used to wait on multiple channel operations.
- Handles whichever operation can proceed first.
- Can include a default case for non-blocking behavior.

**Example**:
```lisp
(select
  ((ch1 -> value)    # Receive from ch1
   (process value))
  
  ((ch2 <- item)     # Send to ch2
   (print "Sent item"))
  
  (timeout 5.0       # Timeout after 5 seconds
   (print "Operation timed out"))
  
  (default           # Non-blocking fallback
   (print "No operations ready")))
```

#### 6.3.4 Advanced Concurrency Patterns

**Worker Pool**
```lisp
# Create a worker pool with 5 workers
(= pool (worker-pool 5))

# Submit tasks
(for item items
  (pool.submit (lambda () 
                 (process item))))

# Wait for completion
(pool.wait)
```

**Parallel Map**
```lisp
# Apply function to items in parallel
(= results (parallel-map process-func items))
```

**Fan-out/Fan-in Pattern**
```lisp
(def (fan-out-fan-in input-items worker-func num-workers)
  # Create channels
  (= jobs (channel))
  (= results (channel))
  
  # Start workers
  (for i (range num-workers)
    (go (worker jobs results worker-func)))
  
  # Send jobs
  (go
    (for item input-items
      (jobs <- item))
    (close jobs))
  
  # Collect results
  (= output (list))
  (for i (range (len input-items))
    (append output (<- results)))
  
  output)
```

#### 6.3.5 Context and Cancellation

**Context Creation**
- Create contexts for cancellation and timeouts.
- Propagate cancellation signals through call chains.

**Example**:
```lisp
# Create a context with cancellation
(= ctx (context))
(= cancel-func (ctx.cancel-func))

# Create a context with timeout
(= timeout-ctx (timeout-context 10.0))  # 10 second timeout

# Use context in functions
(def (worker ctx data)
  (if (ctx.done)
    (return "Cancelled")
    (process data)))
```

#### 6.3.6 Memory Model

**Memory Ordering Guarantees**
- Channel sends happen-before corresponding receives.
- Mutex unlock happens-before subsequent lock.
- Write to a variable happens-before another goroutine reads it through a channel.

**Synchronization Tools**
- **Mutex**: Mutual exclusion lock
- **RWMutex**: Reader/writer lock
- **WaitGroup**: Wait for multiple goroutines to finish
- **Once**: Execute function exactly once
- **Cond**: Condition variable

**Example**:
```lisp
# Mutex for shared state
(= mu (mutex))
(= shared-state {})

(def (update-state key value)
  (mu.lock)
  (= shared-state.key value)
  (mu.unlock))

# WaitGroup for coordination
(= wg (waitgroup))
(wg.add 3)  # Wait for 3 goroutines

(for i (range 3)
  (go
    (do-work i)
    (wg.done)))

(wg.wait)  # Block until all goroutines complete
```

### 6.4 Metaprogramming

#### 6.4.1 Macros

**Macro Definition**
```lisp
(defmacro name (param1 param2 ...)
  body)
```

**Macro Expansion**
- Macros transform code at compile time.
- They receive the unevaluated expressions as arguments.
- They return a new expression to be evaluated.

**Example**:
```lisp
(defmacro unless (condition body)
  `(if (not ,condition)
       ,body
       None))

(unless (< x 0)
  (print "x is non-negative"))
# Expands to:
# (if (not (< x 0))
#     (print "x is non-negative")
#     None)
```

**Hygiene**
- Macros must avoid variable capture.
- Use gensym to create unique symbols.
- Follow conventions like appending `-` to parameter names.

#### 6.4.2 Reflection

**Runtime Introspection**
- Examine and modify the program at runtime.
- Get information about variables, functions, and modules.
- Access and modify attributes dynamically.

**Dynamic Code Evaluation**
- The `eval` function evaluates a string as code.
- The `exec` function executes a string as code.
- Useful for plugins, configuration, and dynamic generation.

**Example**:
```lisp
(= code "(+ 2 3)")
(print (eval code))  # Prints: 5

(= func-code "(def (dynamic x) (* x 2))")
(exec func-code)
(print (dynamic 5))  # Prints: 10
```

**Symbol Manipulation**
- Get and set attributes with `getattr`, `setattr`, `delattr`.
- List symbols with `dir`.
- Access symbol tables with `globals` and `locals`.

**Example**:
```lisp
(= obj (SomeClass))
(= attr-name "some_method")

(if (hasattr obj attr-name)
  (do
    (= method (getattr obj attr-name))
    (method)))
```

**Code Generation**
- Generate code as strings.
- Compile code with `compile`.
- Create functions dynamically with `exec` and `eval`.

**Example**:
```lisp
(def (make-adder n)
  (= func-code (+ "(def (add-" (str n) " x) (+ x " (str n) "))"))
  (exec func-code)
  (getattr (globals) (+ "add-" (str n))))

(= add5 (make-adder 5))
(print (add5 10))  # Prints: 15
```

## 7. Implementation Constraints

### 7.1 Performance Expectations

#### 7.1.1 Computational Complexity

**Standard Operation Complexity**
- List access by index: O(1)
- List search: O(n)
- Dictionary access/insertion/deletion: O(1) average case
- Set operations: O(1) average case for membership, union, intersection
- String concatenation: O(n) where n is the length of the result

**Collection Method Complexity**
- `list.append`: O(1) amortized
- `list.insert`: O(n) worst case
- `list.remove`: O(n) worst case
- `sort`: O(n log n)
- `dict.keys/values/items`: O(1) for creation, O(n) for iteration

**Algorithm Expectations**
- Implementations should use efficient algorithms appropriate for the task.
- Space-time tradeoffs should favor reasonable memory usage.
- Avoid quadratic or worse algorithms for common operations.

#### 7.1.2 Memory Management

**Garbage Collection**
- M28 uses Go's garbage collector.
- Memory is automatically reclaimed when objects are no longer reachable.
- Circular references are handled correctly.

**Resource Lifecycle**
- Resources like files, network connections, etc. should be explicitly closed.
- Use context managers (`with`) for automatic resource management.
- Finalizers are not guaranteed to run, so don't rely on them for cleanup.

**Memory Efficiency**
- Avoid unnecessary copies of large data structures.
- Reuse objects where possible.
- Use generators for large sequences when only iteration is needed.
- Consider memory usage when designing algorithms and data structures.

### 7.2 Platform Interaction

#### 7.2.1 Host Environment

**Go Interoperability**
- M28 is implemented in Go and can integrate with Go code.
- Go functions can be exposed to M28 code.
- M28 values can be passed to and from Go functions.

**OS Interaction**
- File system access through built-in functions.
- Environment variables through `os` module.
- Command execution through `system` function.
- Path manipulation through `path` module.

**Environment Variables**
- Read with `os.getenv`.
- Set with `os.setenv`.
- Environment affects module loading, execution, etc.

**File System Access**
- File I/O through `open`, `read`, `write`, `close`.
- Directory operations through `os` module.
- Path manipulation through `path` module.
- Temporary files through `tempfile` module.

#### 7.2.2 Concurrency Model

**Thread Safety**
- The M28 interpreter is thread-safe for most operations.
- Multiple goroutines can interact with the interpreter safely.

**Parallelism Support**
- Built-in support for parallel execution via goroutines.
- Native integration with Go's concurrency primitives.
- High-level abstractions for common concurrency patterns.

**Synchronization Primitives**
- Basic synchronization primitives: mutex, rwmutex, waitgroup, etc.
- Higher-level concurrency utilities: worker pools, concurrent collections, etc.
- Concurrency patterns: fan-out/fan-in, pipeline, worker pool, etc.

**Memory Model Guarantees**
- Channel operations provide synchronization and memory ordering guarantees.
- Lock operations provide synchronization and memory ordering guarantees.
- M28 follows Go's memory model for concurrent access to shared memory.

## 8. Appendices

### 8.1 Grammar Reference

**EBNF Grammar**
```ebnf
program         ::= expression*
expression      ::= atom | list
atom            ::= number | string | symbol | boolean | none
number          ::= integer | float
integer         ::= ["-"] digit+
float           ::= ["-"] digit+ "." digit+
string          ::= '"' char* '"' | "'" char* "'"
symbol          ::= (letter | "_") (letter | digit | "_")*
boolean         ::= "True" | "False"
none            ::= "None"
list            ::= "(" expression* ")"
letter          ::= "a"..."z" | "A"..."Z"
digit           ::= "0"..."9"
char            ::= any character except " or ' (depending on string delimiter) unless escaped
```

### 8.2 Standard Library Reference

**Built-in Functions**
- Complete list of all functions with signatures, descriptions, examples, and notes.
- Categorized by functionality: arithmetic, string, collection, etc.

**Standard Types and Methods**
- Comprehensive reference for all standard types.
- Methods and properties for each type.
- Examples of usage.

**Error Codes and Messages**
- List of standard error types.
- Common error messages and their causes.
- Troubleshooting guidelines.

### 8.3 Differences from Python/Lisp

**Syntax Differences**
- M28 uses Lisp's parenthesized syntax vs. Python's indented blocks.
- Python's `x = 10` becomes `(= x 10)` in M28.
- Function calls are parenthesized: `(func arg1 arg2)` vs. `func(arg1, arg2)`.

**Semantic Differences**
- Evaluation rules follow Lisp more than Python.
- Symbol resolution and scoping have Lisp influences.
- Some operators behave differently.

**Feature Differences**
- Some Python features may not be implemented.
- Some Lisp features may not be implemented.
- M28 has unique features not in either Python or Lisp.

**Migration Considerations**
- Tips for moving code from Python to M28.
- Tips for moving code from Lisp to M28.
- Common issues and their solutions.

### 8.4 Concurrency Patterns and Examples

**Basic Patterns**
- Worker Pool
- Fan-out/Fan-in
- Pipeline
- Rate-limiting
- Timeout and Cancellation
- Load Balancing

**Complete Example: Parallel Web Crawler**
```lisp
(def (crawler start-url max-depth)
  (= visited (set))
  (= result-chan (channel))
  (= wg (waitgroup))
  
  (def (crawl url depth)
    (if (or (>= depth max-depth) (in url visited))
      (return))
    
    (visited.add url)
    (= links (extract-links url))
    (result-chan <- (dict "url" url "links" (len links)))
    
    (for link links
      (wg.add 1)
      (go
        (crawl link (+ depth 1))
        (wg.done))))
  
  # Start crawling
  (wg.add 1)
  (go
    (crawl start-url 0)
    (wg.done))
  
  # Collect results
  (go
    (wg.wait)
    (close result-chan))
  
  # Return results as a list
  (= results (list))
  (for result (<- result-chan)
    (append results result))
  
  results)
```

### 8.5 Implementation Notes

**Interpreter Architecture**
- Lexer and parser
- AST representation
- Evaluator
- Runtime environment
- Memory management
- Concurrency model

**Performance Considerations**
- Tail call optimization
- Memoization
- Lazy evaluation
- Concurrency patterns
- Resource management

**Integration Guidelines**
- Embedding M28 in Go applications
- Extending M28 with custom Go functions
- Interoperability with existing libraries
- Debugging and profiling