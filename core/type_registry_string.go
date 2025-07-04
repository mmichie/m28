package core

import (
	"fmt"
	"strings"
)

// registerStringType registers the string type descriptor with all its methods
func registerStringType() {
	RegisterType(&TypeDescriptor{
		Name:       "string",
		PythonName: "str",
		BaseType:   StringType,
		Methods:    getStringMethods(),
		Constructor: func(args []Value, ctx *Context) (Value, error) {
			if len(args) == 0 {
				return StringValue(""), nil
			}
			if len(args) == 1 {
				return StringValue(PrintValue(args[0])), nil
			}
			return nil, fmt.Errorf("str() takes at most 1 argument (%d given)", len(args))
		},
		Str: func(v Value) string {
			return string(v.(StringValue))
		},
		Repr: func(v Value) string {
			return fmt.Sprintf("%q", string(v.(StringValue)))
		},
		Doc: "str(object='') -> str\nstr(bytes_or_buffer[, encoding[, errors]]) -> str\n\nCreate a new string object from the given object.",
	})
}

// getStringMethods returns all string methods
func getStringMethods() map[string]*MethodDescriptor {
	return map[string]*MethodDescriptor{
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
			Arity:   -1,
			Doc:     "Return a list of the words in the string, using sep as the delimiter",
			Builtin: true,
			Handler: stringMethodSplit,
		},
		"join": {
			Name:    "join",
			Arity:   1,
			Doc:     "Return a string which is the concatenation of the strings in the iterable",
			Builtin: true,
			Handler: stringMethodJoin,
		},
		"replace": {
			Name:    "replace",
			Arity:   -1,
			Doc:     "Return a copy with all occurrences of old replaced by new",
			Builtin: true,
			Handler: stringMethodReplace,
		},
		"startswith": {
			Name:    "startswith",
			Arity:   1,
			Doc:     "Return True if string starts with the prefix, otherwise False",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("startswith() takes exactly one argument")
				}
				s := string(receiver.(StringValue))
				prefix, ok := args[0].(StringValue)
				if !ok {
					return nil, fmt.Errorf("startswith() argument must be str")
				}
				return BoolValue(strings.HasPrefix(s, string(prefix))), nil
			},
		},
		"endswith": {
			Name:    "endswith",
			Arity:   1,
			Doc:     "Return True if string ends with the suffix, otherwise False",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("endswith() takes exactly one argument")
				}
				s := string(receiver.(StringValue))
				suffix, ok := args[0].(StringValue)
				if !ok {
					return nil, fmt.Errorf("endswith() argument must be str")
				}
				return BoolValue(strings.HasSuffix(s, string(suffix))), nil
			},
		},
		"find": {
			Name:    "find",
			Arity:   1,
			Doc:     "Return the lowest index where substring is found, or -1 if not found",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("find() takes exactly one argument")
				}
				s := string(receiver.(StringValue))
				sub, ok := args[0].(StringValue)
				if !ok {
					return nil, fmt.Errorf("find() argument must be str")
				}
				return NumberValue(strings.Index(s, string(sub))), nil
			},
		},
		"count": {
			Name:    "count",
			Arity:   1,
			Doc:     "Return the number of non-overlapping occurrences of substring",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("count() takes exactly one argument")
				}
				s := string(receiver.(StringValue))
				sub, ok := args[0].(StringValue)
				if !ok {
					return nil, fmt.Errorf("count() argument must be str")
				}
				return NumberValue(strings.Count(s, string(sub))), nil
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
		"__getitem__": {
			Name:    "__getitem__",
			Arity:   1,
			Doc:     "Return character at given index",
			Builtin: true,
			Handler: stringMethodGetItem,
		},
		"__contains__": {
			Name:    "__contains__",
			Arity:   1,
			Doc:     "Return True if substring is in string",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("__contains__ takes exactly one argument")
				}
				s := string(receiver.(StringValue))
				sub, ok := args[0].(StringValue)
				if !ok {
					return nil, fmt.Errorf("'in <string>' requires string as left operand")
				}
				return BoolValue(strings.Contains(s, string(sub))), nil
			},
		},
		"__add__": {
			Name:    "__add__",
			Arity:   1,
			Doc:     "Return self+value",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("__add__ takes exactly one argument")
				}
				s1 := string(receiver.(StringValue))
				s2, ok := args[0].(StringValue)
				if !ok {
					return nil, fmt.Errorf("can only concatenate str to str")
				}
				return StringValue(s1 + string(s2)), nil
			},
		},
		"__mul__": {
			Name:    "__mul__",
			Arity:   1,
			Doc:     "Return self*n",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("__mul__ takes exactly one argument")
				}
				s := string(receiver.(StringValue))
				n, ok := args[0].(NumberValue)
				if !ok {
					// Match the expected error format
					return nil, fmt.Errorf("TypeError: *: unsupported operand type(s) for *: 'string'")
				}
				count := int(n)
				if count <= 0 {
					return StringValue(""), nil
				}
				return StringValue(strings.Repeat(s, count)), nil
			},
		},
		"encode": {
			Name:    "encode",
			Arity:   -1, // Variable args
			Doc:     "Encode the string using the specified encoding",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				s := string(receiver.(StringValue))
				encoding := "utf-8"
				if len(args) > 0 {
					enc, ok := args[0].(StringValue)
					if !ok {
						return nil, fmt.Errorf("encode() argument 1 must be str, not %s", args[0].Type())
					}
					encoding = string(enc)
				}
				if encoding != "utf-8" {
					return nil, fmt.Errorf("only utf-8 encoding is currently supported")
				}
				return BytesValue([]byte(s)), nil
			},
		},
	}
}

// Helper functions for complex string methods

func stringMethodSplit(receiver Value, args []Value, ctx *Context) (Value, error) {
	s := string(receiver.(StringValue))
	sep := " "
	maxSplit := -1

	if len(args) > 0 {
		if sepStr, ok := args[0].(StringValue); ok {
			sep = string(sepStr)
		} else if args[0] == Nil {
			// None means split on whitespace
			fields := strings.Fields(s)
			result := make([]Value, len(fields))
			for i, field := range fields {
				result[i] = StringValue(field)
			}
			return ListValue(result), nil
		} else {
			return nil, fmt.Errorf("sep must be a string")
		}
	}

	if len(args) > 1 {
		if n, ok := args[1].(NumberValue); ok {
			maxSplit = int(n)
		} else {
			return nil, fmt.Errorf("maxsplit must be an integer")
		}
	}

	var parts []string
	if maxSplit < 0 {
		parts = strings.Split(s, sep)
	} else {
		parts = strings.SplitN(s, sep, maxSplit+1)
	}

	result := make([]Value, len(parts))
	for i, part := range parts {
		result[i] = StringValue(part)
	}
	return ListValue(result), nil
}

func stringMethodJoin(receiver Value, args []Value, ctx *Context) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("join() takes exactly one argument")
	}

	sep := string(receiver.(StringValue))

	// Convert iterable to list of strings
	var parts []string
	switch v := args[0].(type) {
	case ListValue:
		parts = make([]string, len(v))
		for i, item := range v {
			if s, ok := item.(StringValue); ok {
				parts[i] = string(s)
			} else {
				return nil, fmt.Errorf("join() requires string elements")
			}
		}
	case TupleValue:
		parts = make([]string, len(v))
		for i, item := range v {
			if s, ok := item.(StringValue); ok {
				parts[i] = string(s)
			} else {
				return nil, fmt.Errorf("join() requires string elements")
			}
		}
	default:
		return nil, fmt.Errorf("join() argument must be an iterable")
	}

	return StringValue(strings.Join(parts, sep)), nil
}

func stringMethodReplace(receiver Value, args []Value, ctx *Context) (Value, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("replace() takes at least 2 arguments")
	}

	s := string(receiver.(StringValue))

	old, ok := args[0].(StringValue)
	if !ok {
		return nil, fmt.Errorf("replace() old must be str")
	}

	new, ok := args[1].(StringValue)
	if !ok {
		return nil, fmt.Errorf("replace() new must be str")
	}

	count := -1
	if len(args) > 2 {
		if n, ok := args[2].(NumberValue); ok {
			count = int(n)
		} else {
			return nil, fmt.Errorf("replace() count must be an integer")
		}
	}

	result := strings.Replace(s, string(old), string(new), count)
	return StringValue(result), nil
}

func stringMethodGetItem(receiver Value, args []Value, ctx *Context) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("__getitem__ takes exactly one argument")
	}

	s := string(receiver.(StringValue))

	// Handle slice
	if slice, ok := args[0].(*SliceValue); ok {
		start, stop := 0, len(s)

		if slice.Start != nil && slice.Start != Nil {
			if n, ok := slice.Start.(NumberValue); ok {
				start = int(n)
				if start < 0 {
					start = len(s) + start
				}
				if start < 0 {
					start = 0
				}
			}
		}

		if slice.Stop != nil && slice.Stop != Nil {
			if n, ok := slice.Stop.(NumberValue); ok {
				stop = int(n)
				if stop < 0 {
					stop = len(s) + stop
				}
			}
		}

		if stop > len(s) {
			stop = len(s)
		}
		if start > stop {
			start = stop
		}

		// TODO: Handle step
		return StringValue(s[start:stop]), nil
	}

	// Handle index
	idx, ok := args[0].(NumberValue)
	if !ok {
		return nil, fmt.Errorf("string indices must be integers")
	}

	i := int(idx)
	if i < 0 {
		i = len(s) + i
	}

	if i < 0 || i >= len(s) {
		return nil, &IndexError{Index: i, Length: len(s)}
	}

	return StringValue(s[i : i+1]), nil
}
