package core

import (
	"fmt"
	"math"
	"sort"
	"strings"
)

// InitializeTypeRegistry sets up type descriptors for all built-in types
func InitializeTypeRegistry() {
	// Register task type
	RegisterType(&TypeDescriptor{
		Name:       "task",
		PythonName: "Task",
		BaseType:   Type("task"),
		Methods: map[string]*MethodDescriptor{
			"result": {
				Name:    "result",
				Arity:   0,
				Doc:     "Get the result of the task (blocks until complete)",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					task := receiver.(*Task)
					return task.Wait()
				},
			},
			"done": {
				Name:    "done",
				Arity:   0,
				Doc:     "Check if the task is finished",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					task := receiver.(*Task)
					return BoolValue(task.IsFinished()), nil
				},
			},
		},
	})

	// Register channel type
	RegisterType(&TypeDescriptor{
		Name:       "channel",
		PythonName: "Channel",
		BaseType:   Type("channel"),
		Methods: map[string]*MethodDescriptor{
			"send": {
				Name:    "send",
				Arity:   1,
				Doc:     "Send a value to the channel",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					if len(args) != 1 {
						return nil, fmt.Errorf("send() takes exactly one argument")
					}
					ch := receiver.(*Channel)
					err := ch.Send(args[0])
					if err != nil {
						return nil, err
					}
					return Nil, nil
				},
			},
			"receive": {
				Name:    "receive",
				Arity:   0,
				Doc:     "Receive a value from the channel",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					ch := receiver.(*Channel)
					return ch.Receive()
				},
			},
			"close": {
				Name:    "close",
				Arity:   0,
				Doc:     "Close the channel",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					ch := receiver.(*Channel)
					err := ch.Close()
					if err != nil {
						return nil, err
					}
					return Nil, nil
				},
			},
			"__len__": {
				Name:    "__len__",
				Arity:   0,
				Doc:     "Get the number of values in the channel buffer",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					ch := receiver.(*Channel)
					return NumberValue(len(ch.GetChan())), nil
				},
			},
		},
	})

	// Register generator type
	RegisterType(&TypeDescriptor{
		Name:       "generator",
		PythonName: "generator",
		BaseType:   Type("generator"),
		Methods: map[string]*MethodDescriptor{
			"__next__": {
				Name:    "__next__",
				Arity:   0,
				Doc:     "Retrieve the next value from the generator",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					gen := receiver.(*Generator)
					return gen.Next()
				},
			},
			"send": {
				Name:    "send",
				Arity:   1,
				Doc:     "Send a value into the generator",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					if len(args) != 1 {
						return nil, fmt.Errorf("send() takes exactly one argument")
					}
					gen := receiver.(*Generator)
					return gen.Send(args[0])
				},
			},
			"close": {
				Name:    "close",
				Arity:   0,
				Doc:     "Close the generator",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					gen := receiver.(*Generator)
					return gen.Close()
				},
			},
		},
	})

	// Register class type
	RegisterType(&TypeDescriptor{
		Name:       "class",
		PythonName: "type",
		BaseType:   Type("class"),
		Methods: map[string]*MethodDescriptor{
			"__call__": {
				Name:    "__call__",
				Arity:   -1, // Variable args
				Doc:     "Create an instance of the class",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					class := receiver.(*Class)
					return class.Call(args, ctx)
				},
			},
			"__name__": {
				Name:    "__name__",
				Arity:   0,
				Doc:     "Get the class name",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					class := receiver.(*Class)
					return StringValue(class.Name), nil
				},
			},
		},
	})

	// Register module type
	RegisterType(&TypeDescriptor{
		Name:       "module",
		PythonName: "module",
		BaseType:   Type("module"),
		Methods: map[string]*MethodDescriptor{
			"__getattr__": {
				Name:    "__getattr__",
				Arity:   1,
				Doc:     "Get an attribute from the module",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					module := receiver.(*Module)
					if len(args) != 1 {
						return nil, fmt.Errorf("__getattr__ expects 1 argument")
					}
					
					name, ok := args[0].(StringValue)
					if !ok {
						return nil, fmt.Errorf("attribute name must be a string")
					}
					
					if val, ok := module.GetExport(string(name)); ok {
						return val, nil
					}
					
					return nil, fmt.Errorf("module '%s' has no attribute '%s'", module.Name, string(name))
				},
			},
			"__dir__": {
				Name:    "__dir__",
				Arity:   0,
				Doc:     "List all exported names",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					module := receiver.(*Module)
					exports := module.GetAllExports()
					result := make(ListValue, len(exports))
					for i, name := range exports {
						result[i] = StringValue(name)
					}
					return result, nil
				},
			},
		},
	})

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

	// Register list type with comprehensive methods
	RegisterType(&TypeDescriptor{
		Name:       "list",
		PythonName: "list",
		BaseType:   ListType,
		Methods: map[string]*MethodDescriptor{
			"append": {
				Name:    "append",
				Arity:   1,
				Doc:     "Append an element to the list (returns new list)",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					// For now, lists are immutable in the functional style
					list := receiver.(ListValue)
					newList := make(ListValue, len(list)+1)
					copy(newList, list)
					newList[len(list)] = args[0]
					return newList, nil
				},
			},
			"extend": {
				Name:    "extend",
				Arity:   1,
				Doc:     "Extend list by appending elements from another list",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					list := receiver.(ListValue)
					other, ok := args[0].(ListValue)
					if !ok {
						return nil, fmt.Errorf("extend expects a list, got %s", args[0].Type())
					}
					newList := make(ListValue, len(list)+len(other))
					copy(newList, list)
					copy(newList[len(list):], other)
					return newList, nil
				},
			},
			"insert": {
				Name:    "insert",
				Arity:   2,
				Doc:     "Insert element at index",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					list := receiver.(ListValue)
					index, ok := args[0].(NumberValue)
					if !ok {
						return nil, fmt.Errorf("insert expects index as first argument")
					}
					idx := int(index)
					if idx < 0 {
						idx = len(list) + idx + 1
					}
					if idx < 0 {
						idx = 0
					}
					if idx > len(list) {
						idx = len(list)
					}
					
					newList := make(ListValue, len(list)+1)
					copy(newList[:idx], list[:idx])
					newList[idx] = args[1]
					copy(newList[idx+1:], list[idx:])
					return newList, nil
				},
			},
			"pop": {
				Name:    "pop",
				Arity:   -1, // 0 or 1 args
				Doc:     "Remove and return element at index (default last)",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					list := receiver.(ListValue)
					if len(list) == 0 {
						return nil, fmt.Errorf("pop from empty list")
					}
					
					idx := len(list) - 1
					if len(args) > 0 {
						index, ok := args[0].(NumberValue)
						if !ok {
							return nil, fmt.Errorf("pop expects index as argument")
						}
						idx = int(index)
						if idx < 0 {
							idx = len(list) + idx
						}
					}
					
					if idx < 0 || idx >= len(list) {
						return nil, &IndexError{Index: idx, Length: len(list)}
					}
					
					return list[idx], nil
				},
			},
			"index": {
				Name:    "index",
				Arity:   1,
				Doc:     "Return index of first occurrence of value",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					list := receiver.(ListValue)
					for i, item := range list {
						if EqualValues(item, args[0]) {
							return NumberValue(i), nil
						}
					}
					return nil, fmt.Errorf("value not found in list")
				},
			},
			"count": {
				Name:    "count",
				Arity:   1,
				Doc:     "Return number of occurrences of value",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					list := receiver.(ListValue)
					count := 0
					for _, item := range list {
						if EqualValues(item, args[0]) {
							count++
						}
					}
					return NumberValue(count), nil
				},
			},
			"reverse": {
				Name:    "reverse",
				Arity:   0,
				Doc:     "Return reversed list",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					list := receiver.(ListValue)
					newList := make(ListValue, len(list))
					for i, j := 0, len(list)-1; i < len(list); i, j = i+1, j-1 {
						newList[i] = list[j]
					}
					return newList, nil
				},
			},
			"sort": {
				Name:    "sort",
				Arity:   0,
				Doc:     "Return sorted list",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					list := receiver.(ListValue)
					newList := make(ListValue, len(list))
					copy(newList, list)
					// Simple sort for numbers and strings
					sort.Slice(newList, func(i, j int) bool {
						return Compare(newList[i], newList[j]) < 0
					})
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
			"__getitem__": {
				Name:    "__getitem__",
				Arity:   1,
				Doc:     "Get item by index",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					list := receiver.(ListValue)
					index, ok := args[0].(NumberValue)
					if !ok {
						return nil, fmt.Errorf("list indices must be integers")
					}
					return list.GetItem(int(index))
				},
			},
			"__setitem__": {
				Name:    "__setitem__",
				Arity:   2,
				Doc:     "Set item by index",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					list := receiver.(ListValue)
					index, ok := args[0].(NumberValue)
					if !ok {
						return nil, fmt.Errorf("list indices must be integers")
					}
					// Since lists are immutable, return a new list
					idx := int(index)
					if idx < 0 {
						idx = len(list) + idx
					}
					if idx < 0 || idx >= len(list) {
						return nil, &IndexError{Index: idx, Length: len(list)}
					}
					newList := make(ListValue, len(list))
					copy(newList, list)
					newList[idx] = args[1]
					return newList, nil
				},
			},
			"__contains__": {
				Name:    "__contains__",
				Arity:   1,
				Doc:     "Check if value is in list",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					list := receiver.(ListValue)
					for _, item := range list {
						if EqualValues(item, args[0]) {
							return True, nil
						}
					}
					return False, nil
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
			if len(args) == 0 {
				return EmptyList, nil
			}
			if len(args) == 1 {
				// Try to convert from iterable
				if iter, ok := args[0].(Iterable); ok {
					result := make(ListValue, 0)
					it := iter.Iterator()
					for {
						val, ok := it.Next()
						if !ok {
							break
						}
						result = append(result, val)
					}
					return result, nil
				}
			}
			// Create list from arguments
			return ListValue(args), nil
		},
		Repr: defaultRepr,
		Str:  defaultStr,
		Doc:  "list() -> new empty list\nlist(iterable) -> new list initialized from iterable's items",
	})

	// Register dict type
	RegisterType(&TypeDescriptor{
		Name:       "dict",
		PythonName: "dict",
		BaseType:   DictType,
		Methods: map[string]*MethodDescriptor{
			"get": {
				Name:    "get",
				Arity:   -1, // 1 or 2 args
				Doc:     "Get value by key, with optional default",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					dict := receiver.(*DictValue)
					if len(args) < 1 || len(args) > 2 {
						return nil, fmt.Errorf("get expects 1 or 2 arguments")
					}
					
					key, ok := args[0].(StringValue)
					if !ok {
						return nil, fmt.Errorf("dict key must be a string")
					}
					
					val, found := dict.Get(string(key))
					if found {
						return val, nil
					}
					
					if len(args) > 1 {
						return args[1], nil
					}
					return None, nil
				},
			},
			"set": {
				Name:    "set",
				Arity:   2,
				Doc:     "Set value by key (returns new dict)",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					dict := receiver.(*DictValue)
					key, ok := args[0].(StringValue)
					if !ok {
						return nil, fmt.Errorf("dict key must be a string")
					}
					
					// Create new dict with the update
					newDict := NewDict()
					for _, k := range dict.Keys() {
						v, _ := dict.Get(k)
						newDict.Set(k, v)
					}
					newDict.Set(string(key), args[1])
					return newDict, nil
				},
			},
			"keys": {
				Name:    "keys",
				Arity:   0,
				Doc:     "Return list of keys",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					dict := receiver.(*DictValue)
					keys := dict.Keys()
					result := make(ListValue, len(keys))
					for i, k := range keys {
						result[i] = StringValue(k)
					}
					return result, nil
				},
			},
			"values": {
				Name:    "values",
				Arity:   0,
				Doc:     "Return list of values",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					dict := receiver.(*DictValue)
					keys := dict.Keys()
					result := make(ListValue, len(keys))
					for i, k := range keys {
						v, _ := dict.Get(k)
						result[i] = v
					}
					return result, nil
				},
			},
			"items": {
				Name:    "items",
				Arity:   0,
				Doc:     "Return list of (key, value) tuples",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					dict := receiver.(*DictValue)
					keys := dict.Keys()
					result := make(ListValue, len(keys))
					for i, k := range keys {
						v, _ := dict.Get(k)
						result[i] = TupleValue{StringValue(k), v}
					}
					return result, nil
				},
			},
			"__len__": {
				Name:    "__len__",
				Arity:   0,
				Doc:     "Return number of items",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					dict := receiver.(*DictValue)
					return NumberValue(len(dict.Keys())), nil
				},
			},
			"__contains__": {
				Name:    "__contains__",
				Arity:   1,
				Doc:     "Check if key exists",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					dict := receiver.(*DictValue)
					key, ok := args[0].(StringValue)
					if !ok {
						return False, nil
					}
					_, found := dict.Get(string(key))
					return BoolValue(found), nil
				},
			},
		},
		Constructor: func(args []Value, ctx *Context) (Value, error) {
			dict := NewDict()
			// TODO: Support initialization from pairs
			return dict, nil
		},
		Repr: defaultRepr,
		Str:  defaultStr,
		Doc:  "dict() -> new empty dictionary",
	})

	// Register tuple type
	RegisterType(&TypeDescriptor{
		Name:       "tuple",
		PythonName: "tuple",
		BaseType:   TupleType,
		Methods: map[string]*MethodDescriptor{
			"count": {
				Name:    "count",
				Arity:   1,
				Doc:     "Return number of occurrences of value",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					tuple := receiver.(TupleValue)
					count := 0
					for _, item := range tuple {
						if EqualValues(item, args[0]) {
							count++
						}
					}
					return NumberValue(count), nil
				},
			},
			"index": {
				Name:    "index",
				Arity:   1,
				Doc:     "Return index of first occurrence of value",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					tuple := receiver.(TupleValue)
					for i, item := range tuple {
						if EqualValues(item, args[0]) {
							return NumberValue(i), nil
						}
					}
					return nil, fmt.Errorf("value not found in tuple")
				},
			},
			"__len__": {
				Name:    "__len__",
				Arity:   0,
				Doc:     "Return the length of the tuple",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					tuple := receiver.(TupleValue)
					return NumberValue(len(tuple)), nil
				},
			},
			"__getitem__": {
				Name:    "__getitem__",
				Arity:   1,
				Doc:     "Get item by index",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					tuple := receiver.(TupleValue)
					index, ok := args[0].(NumberValue)
					if !ok {
						return nil, fmt.Errorf("tuple indices must be integers")
					}
					return tuple.GetItem(int(index))
				},
			},
		},
		Properties: map[string]*PropertyDescriptor{
			"length": {
				Name:     "length",
				ReadOnly: true,
				Doc:      "The length of the tuple",
				Getter: func(v Value) (Value, error) {
					tuple := v.(TupleValue)
					return NumberValue(len(tuple)), nil
				},
			},
		},
		Constructor: func(args []Value, ctx *Context) (Value, error) {
			if len(args) == 0 {
				return EmptyTuple, nil
			}
			if len(args) == 1 {
				// Try to convert from iterable
				if iter, ok := args[0].(Iterable); ok {
					result := make(TupleValue, 0)
					it := iter.Iterator()
					for {
						val, ok := it.Next()
						if !ok {
							break
						}
						result = append(result, val)
					}
					return result, nil
				}
			}
			return TupleValue(args), nil
		},
		Repr: defaultRepr,
		Str:  defaultStr,
		Doc:  "tuple() -> empty tuple\ntuple(iterable) -> tuple initialized from iterable's items",
	})

	// Register set type
	RegisterType(&TypeDescriptor{
		Name:       "set",
		PythonName: "set",
		BaseType:   SetType,
		Methods: map[string]*MethodDescriptor{
			"add": {
				Name:    "add",
				Arity:   1,
				Doc:     "Add element to set (returns new set)",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					set := receiver.(*SetValue)
					newSet := NewSet()
					// Copy existing items
					for k, v := range set.items {
						newSet.items[k] = v
					}
					newSet.Add(args[0])
					return newSet, nil
				},
			},
			"remove": {
				Name:    "remove",
				Arity:   1,
				Doc:     "Remove element from set",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					set := receiver.(*SetValue)
					if !set.Contains(args[0]) {
						return nil, &KeyError{Key: args[0]}
					}
					newSet := NewSet()
					// Copy without the removed item
					key := PrintValue(args[0])
					for k, v := range set.items {
						if k != key {
							newSet.items[k] = v
						}
					}
					return newSet, nil
				},
			},
			"discard": {
				Name:    "discard",
				Arity:   1,
				Doc:     "Remove element if present",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					set := receiver.(*SetValue)
					newSet := NewSet()
					key := PrintValue(args[0])
					for k, v := range set.items {
						if k != key {
							newSet.items[k] = v
						}
					}
					return newSet, nil
				},
			},
			"__len__": {
				Name:    "__len__",
				Arity:   0,
				Doc:     "Return number of elements",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					set := receiver.(*SetValue)
					return NumberValue(set.Size()), nil
				},
			},
			"__contains__": {
				Name:    "__contains__",
				Arity:   1,
				Doc:     "Check if value is in set",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					set := receiver.(*SetValue)
					return BoolValue(set.Contains(args[0])), nil
				},
			},
		},
		Constructor: func(args []Value, ctx *Context) (Value, error) {
			set := NewSet()
			if len(args) == 1 {
				// Try to convert from iterable
				if iter, ok := args[0].(Iterable); ok {
					it := iter.Iterator()
					for {
						val, ok := it.Next()
						if !ok {
							break
						}
						set.Add(val)
					}
					return set, nil
				}
			}
			// Add all arguments
			for _, arg := range args {
				set.Add(arg)
			}
			return set, nil
		},
		Repr: defaultRepr,
		Str:  defaultStr,
		Doc:  "set() -> new empty set\nset(iterable) -> new set object",
	})
}

// Helper to initialize types on startup
func init() {
	InitializeTypeRegistry()
}
