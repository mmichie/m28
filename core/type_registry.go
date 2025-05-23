package core

import (
	"fmt"
	"math"
	"strings"
)

// InitializeTypeRegistry sets up type descriptors for all built-in types
func InitializeTypeRegistry() {
	// Register number type
	RegisterType(&TypeDescriptor{
		Name:       "number",
		PythonName: "int", // We use int for Python compatibility
		BaseType:   NumberType,
		Methods: map[string]*MethodDescriptor{
			"abs": {
				Name:    "abs",
				Arity:   0,
				Doc:     "Return the absolute value of the number",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					n := float64(receiver.(NumberValue))
					return NumberValue(math.Abs(n)), nil
				},
			},
			"__neg__": {
				Name:    "__neg__",
				Arity:   0,
				Doc:     "Return the negation of the number",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					n := float64(receiver.(NumberValue))
					return NumberValue(-n), nil
				},
			},
			"__add__": {
				Name:    "__add__",
				Arity:   1,
				Doc:     "Add another number",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					a := float64(receiver.(NumberValue))
					b, ok := args[0].(NumberValue)
					if !ok {
						return nil, fmt.Errorf("unsupported operand type(s) for +: 'int' and '%s'", args[0].Type())
					}
					return NumberValue(a + float64(b)), nil
				},
			},
		},
		Constructor: func(args []Value, ctx *Context) (Value, error) {
			if len(args) == 0 {
				return NumberValue(0), nil
			}
			// Type conversion logic
			switch v := args[0].(type) {
			case NumberValue:
				return v, nil
			case StringValue:
				// Parse string to number
				var n float64
				_, err := fmt.Sscanf(string(v), "%f", &n)
				if err != nil {
					return nil, fmt.Errorf("invalid literal for int() with base 10: '%s'", string(v))
				}
				return NumberValue(n), nil
			case BoolValue:
				if bool(v) {
					return NumberValue(1), nil
				}
				return NumberValue(0), nil
			default:
				return nil, fmt.Errorf("int() argument must be a string or a number, not '%s'", v.Type())
			}
		},
		Repr: defaultRepr,
		Str:  defaultStr,
		Doc:  "int(x=0) -> integer\n\nConvert a number or string to an integer.",
	})

	// Register string type
	RegisterType(&TypeDescriptor{
		Name:       "string",
		PythonName: "str",
		BaseType:   StringType,
		Methods: map[string]*MethodDescriptor{
			"upper": {
				Name:    "upper",
				Arity:   0,
				Doc:     "Return a copy of the string converted to uppercase",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					s := string(receiver.(StringValue))
					return StringValue(strings.ToUpper(s)), nil
				},
			},
			"lower": {
				Name:    "lower",
				Arity:   0,
				Doc:     "Return a copy of the string converted to lowercase",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					s := string(receiver.(StringValue))
					return StringValue(strings.ToLower(s)), nil
				},
			},
			"strip": {
				Name:    "strip",
				Arity:   0,
				Doc:     "Return a copy of the string with leading and trailing whitespace removed",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					s := string(receiver.(StringValue))
					return StringValue(strings.TrimSpace(s)), nil
				},
			},
			"split": {
				Name:    "split",
				Arity:   -1, // Variadic
				Doc:     "Return a list of the words in the string, using sep as the delimiter",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					s := string(receiver.(StringValue))
					sep := " "
					if len(args) > 0 {
						if sepStr, ok := args[0].(StringValue); ok {
							sep = string(sepStr)
						} else {
							return nil, fmt.Errorf("sep must be a string")
						}
					}
					parts := strings.Split(s, sep)
					result := make(ListValue, len(parts))
					for i, part := range parts {
						result[i] = StringValue(part)
					}
					return result, nil
				},
			},
			"__len__": {
				Name:    "__len__",
				Arity:   0,
				Doc:     "Return the length of the string",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					s := string(receiver.(StringValue))
					return NumberValue(len(s)), nil
				},
			},
		},
		Properties: map[string]*PropertyDescriptor{
			"length": {
				Name:     "length",
				ReadOnly: true,
				Doc:      "The length of the string",
				Getter: func(v Value) (Value, error) {
					s := string(v.(StringValue))
					return NumberValue(len(s)), nil
				},
			},
		},
		Constructor: func(args []Value, ctx *Context) (Value, error) {
			if len(args) == 0 {
				return StringValue(""), nil
			}
			// Convert to string
			return StringValue(PrintValueWithoutQuotes(args[0])), nil
		},
		Repr: func(v Value) string {
			return fmt.Sprintf("%q", string(v.(StringValue)))
		},
		Str: func(v Value) string {
			return string(v.(StringValue))
		},
		Doc: "str(object='') -> string\n\nReturn a string representation of object.",
	})

	// Register bool type
	RegisterType(&TypeDescriptor{
		Name:       "bool",
		PythonName: "bool",
		BaseType:   BoolType,
		Methods: map[string]*MethodDescriptor{
			"__and__": {
				Name:    "__and__",
				Arity:   1,
				Doc:     "Logical AND",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					a := bool(receiver.(BoolValue))
					b := IsTruthy(args[0])
					return BoolValue(a && b), nil
				},
			},
			"__or__": {
				Name:    "__or__",
				Arity:   1,
				Doc:     "Logical OR",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					a := bool(receiver.(BoolValue))
					b := IsTruthy(args[0])
					return BoolValue(a || b), nil
				},
			},
		},
		Constructor: func(args []Value, ctx *Context) (Value, error) {
			if len(args) == 0 {
				return False, nil
			}
			return BoolValue(IsTruthy(args[0])), nil
		},
		Repr: defaultRepr,
		Str:  defaultStr,
		Doc:  "bool(x) -> bool\n\nReturns True when the argument x is true, False otherwise.",
	})

	// Register nil/None type
	RegisterType(&TypeDescriptor{
		Name:       "nil",
		PythonName: "NoneType",
		BaseType:   NilType,
		Methods:    make(map[string]*MethodDescriptor), // No methods
		Properties: make(map[string]*PropertyDescriptor),
		Constructor: func(args []Value, ctx *Context) (Value, error) {
			return None, nil
		},
		Repr: func(v Value) string { return "None" },
		Str:  func(v Value) string { return "None" },
		Doc:  "The type of None.",
	})

	// Register symbol type
	RegisterType(&TypeDescriptor{
		Name:       "symbol",
		PythonName: "symbol",
		BaseType:   SymbolType,
		Methods:    make(map[string]*MethodDescriptor),
		Properties: make(map[string]*PropertyDescriptor),
		Constructor: func(args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("symbol() takes exactly 1 argument")
			}
			if s, ok := args[0].(StringValue); ok {
				return SymbolValue(s), nil
			}
			return nil, fmt.Errorf("symbol() argument must be a string")
		},
		Repr: defaultRepr,
		Str:  defaultStr,
		Doc:  "symbol(name) -> symbol\n\nCreate a symbol from a string.",
	})

	// Register list type (basic for now)
	RegisterType(&TypeDescriptor{
		Name:       "list",
		PythonName: "list",
		BaseType:   ListType,
		Methods: map[string]*MethodDescriptor{
			"append": {
				Name:    "append",
				Arity:   1,
				Doc:     "Append an element to the list",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					// For now, lists are immutable in the functional style
					// This returns a new list with the element appended
					list := receiver.(ListValue)
					newList := make(ListValue, len(list)+1)
					copy(newList, list)
					newList[len(list)] = args[0]
					return newList, nil
				},
			},
			"__len__": {
				Name:    "__len__",
				Arity:   0,
				Doc:     "Return the length of the list",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					list := receiver.(ListValue)
					return NumberValue(len(list)), nil
				},
			},
		},
		Properties: map[string]*PropertyDescriptor{
			"length": {
				Name:     "length",
				ReadOnly: true,
				Doc:      "The length of the list",
				Getter: func(v Value) (Value, error) {
					list := v.(ListValue)
					return NumberValue(len(list)), nil
				},
			},
		},
		Constructor: func(args []Value, ctx *Context) (Value, error) {
			// Create list from arguments
			return ListValue(args), nil
		},
		Repr: defaultRepr,
		Str:  defaultStr,
		Doc:  "list() -> new empty list\nlist(iterable) -> new list initialized from iterable's items",
	})
}

// Helper to initialize types on startup
func init() {
	InitializeTypeRegistry()
}
