package core

import (
	"fmt"
	"strings"
)

// InitStringMethods adds additional string methods to the string type descriptor
func InitStringMethods() {
	// Get the string type descriptor
	td := GetTypeDescriptor("string")
	if td == nil {
		return
	}

	// Add __len__ method that properly counts Unicode characters
	td.Methods["__len__"] = &MethodDescriptor{
		Name:    "__len__",
		Arity:   0,
		Doc:     "Return the number of characters in the string",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			s := string(receiver.(StringValue))
			// Count runes (Unicode characters) instead of bytes
			return NumberValue(len([]rune(s))), nil
		},
	}

	// Add missing methods
	td.Methods["replace"] = &MethodDescriptor{
		Name:    "replace",
		Arity:   -1,
		Doc:     "Return a copy with all occurrences of substring old replaced by new",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) < 2 {
				return nil, fmt.Errorf("replace() takes at least 2 arguments (%d given)", len(args))
			}
			
			s := string(receiver.(StringValue))
			
			old, ok := args[0].(StringValue)
			if !ok {
				return nil, fmt.Errorf("replace() argument 1 must be a string")
			}
			
			new, ok := args[1].(StringValue)
			if !ok {
				return nil, fmt.Errorf("replace() argument 2 must be a string")
			}
			
			if len(args) >= 3 {
				if count, ok := args[2].(NumberValue); ok {
					return StringValue(strings.Replace(s, string(old), string(new), int(count))), nil
				}
				return nil, fmt.Errorf("replace() argument 3 must be a number")
			}
			
			return StringValue(strings.ReplaceAll(s, string(old), string(new))), nil
		},
	}

	td.Methods["startswith"] = &MethodDescriptor{
		Name:    "startswith",
		Arity:   1,
		Doc:     "Return True if string starts with the prefix, otherwise return False",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("startswith() takes exactly one argument (%d given)", len(args))
			}
			
			s := string(receiver.(StringValue))
			prefix, ok := args[0].(StringValue)
			if !ok {
				return nil, fmt.Errorf("startswith() argument must be a string")
			}
			
			return BoolValue(strings.HasPrefix(s, string(prefix))), nil
		},
	}

	td.Methods["endswith"] = &MethodDescriptor{
		Name:    "endswith",
		Arity:   1,
		Doc:     "Return True if string ends with the suffix, otherwise return False",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("endswith() takes exactly one argument (%d given)", len(args))
			}
			
			s := string(receiver.(StringValue))
			suffix, ok := args[0].(StringValue)
			if !ok {
				return nil, fmt.Errorf("endswith() argument must be a string")
			}
			
			return BoolValue(strings.HasSuffix(s, string(suffix))), nil
		},
	}

	td.Methods["find"] = &MethodDescriptor{
		Name:    "find",
		Arity:   1,
		Doc:     "Return the lowest index where substring is found, or -1 if not found",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("find() takes exactly one argument (%d given)", len(args))
			}
			
			s := string(receiver.(StringValue))
			sub, ok := args[0].(StringValue)
			if !ok {
				return nil, fmt.Errorf("find() argument must be a string")
			}
			
			return NumberValue(strings.Index(s, string(sub))), nil
		},
	}

	td.Methods["join"] = &MethodDescriptor{
		Name:    "join",
		Arity:   1,
		Doc:     "Return a string which is the concatenation of the strings in the iterable",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("join() takes exactly one argument (%d given)", len(args))
			}
			
			sep := string(receiver.(StringValue))
			
			// Handle different iterable types
			var parts []string
			switch v := args[0].(type) {
			case ListValue:
				parts = make([]string, len(v))
				for i, item := range v {
					if s, ok := item.(StringValue); ok {
						parts[i] = string(s)
					} else {
						parts[i] = PrintValueWithoutQuotes(item)
					}
				}
			case TupleValue:
				parts = make([]string, len(v))
				for i, item := range v {
					if s, ok := item.(StringValue); ok {
						parts[i] = string(s)
					} else {
						parts[i] = PrintValueWithoutQuotes(item)
					}
				}
			default:
				return nil, fmt.Errorf("join() argument must be an iterable")
			}
			
			return StringValue(strings.Join(parts, sep)), nil
		},
	}

	td.Methods["capitalize"] = &MethodDescriptor{
		Name:    "capitalize",
		Arity:   0,
		Doc:     "Return a capitalized version of the string",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			s := string(receiver.(StringValue))
			if len(s) == 0 {
				return receiver, nil
			}
			// Capitalize first letter, lowercase the rest
			return StringValue(strings.ToUpper(s[:1]) + strings.ToLower(s[1:])), nil
		},
	}

	td.Methods["title"] = &MethodDescriptor{
		Name:    "title",
		Arity:   0,
		Doc:     "Return a titlecased version of the string",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			s := string(receiver.(StringValue))
			return StringValue(strings.Title(s)), nil
		},
	}

	td.Methods["count"] = &MethodDescriptor{
		Name:    "count",
		Arity:   1,
		Doc:     "Return the number of non-overlapping occurrences of substring",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("count() takes exactly one argument (%d given)", len(args))
			}
			
			s := string(receiver.(StringValue))
			sub, ok := args[0].(StringValue)
			if !ok {
				return nil, fmt.Errorf("count() argument must be a string")
			}
			
			return NumberValue(strings.Count(s, string(sub))), nil
		},
	}

	td.Methods["format"] = &MethodDescriptor{
		Name:    "format",
		Arity:   -1,
		Doc:     "Return a formatted version of the string",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			// Simple implementation - just replace {} with args in order
			s := string(receiver.(StringValue))
			result := s
			
			for i, arg := range args {
				placeholder := "{}"
				if idx := strings.Index(result, placeholder); idx >= 0 {
					argStr := PrintValueWithoutQuotes(arg)
					result = result[:idx] + argStr + result[idx+len(placeholder):]
				} else {
					// Try numbered placeholders
					placeholder = fmt.Sprintf("{%d}", i)
					result = strings.ReplaceAll(result, placeholder, PrintValueWithoutQuotes(arg))
				}
			}
			
			return StringValue(result), nil
		},
	}

	td.Methods["lstrip"] = &MethodDescriptor{
		Name:    "lstrip",
		Arity:   -1,
		Doc:     "Return a copy of the string with leading characters removed",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			s := string(receiver.(StringValue))
			if len(args) == 0 {
				return StringValue(strings.TrimLeft(s, " \t\n\r")), nil
			}
			if chars, ok := args[0].(StringValue); ok {
				return StringValue(strings.TrimLeft(s, string(chars))), nil
			}
			return nil, fmt.Errorf("lstrip() argument must be a string")
		},
	}

	td.Methods["rstrip"] = &MethodDescriptor{
		Name:    "rstrip",
		Arity:   -1,
		Doc:     "Return a copy of the string with trailing characters removed",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			s := string(receiver.(StringValue))
			if len(args) == 0 {
				return StringValue(strings.TrimRight(s, " \t\n\r")), nil
			}
			if chars, ok := args[0].(StringValue); ok {
				return StringValue(strings.TrimRight(s, string(chars))), nil
			}
			return nil, fmt.Errorf("rstrip() argument must be a string")
		},
	}

	td.Methods["isdigit"] = &MethodDescriptor{
		Name:    "isdigit",
		Arity:   0,
		Doc:     "Return True if all characters in the string are digits",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			s := string(receiver.(StringValue))
			if len(s) == 0 {
				return False, nil
			}
			for _, r := range s {
				if r < '0' || r > '9' {
					return False, nil
				}
			}
			return True, nil
		},
	}

	td.Methods["isalpha"] = &MethodDescriptor{
		Name:    "isalpha",
		Arity:   0,
		Doc:     "Return True if all characters in the string are alphabetic",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			s := string(receiver.(StringValue))
			if len(s) == 0 {
				return False, nil
			}
			for _, r := range s {
				if !((r >= 'a' && r <= 'z') || (r >= 'A' && r <= 'Z')) {
					return False, nil
				}
			}
			return True, nil
		},
	}

	td.Methods["isspace"] = &MethodDescriptor{
		Name:    "isspace",
		Arity:   0,
		Doc:     "Return True if all characters in the string are whitespace",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			s := string(receiver.(StringValue))
			if len(s) == 0 {
				return False, nil
			}
			for _, r := range s {
				if r != ' ' && r != '\t' && r != '\n' && r != '\r' {
					return False, nil
				}
			}
			return True, nil
		},
	}

	td.Methods["index"] = &MethodDescriptor{
		Name:    "index",
		Arity:   1,
		Doc:     "Like find(), but raises ValueError when the substring is not found",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("index() takes exactly one argument (%d given)", len(args))
			}
			
			s := string(receiver.(StringValue))
			sub, ok := args[0].(StringValue)
			if !ok {
				return nil, fmt.Errorf("index() argument must be a string")
			}
			
			idx := strings.Index(s, string(sub))
			if idx == -1 {
				return nil, fmt.Errorf("substring not found")
			}
			return NumberValue(idx), nil
		},
	}

	// Update strip to accept optional argument
	td.Methods["strip"] = &MethodDescriptor{
		Name:    "strip",
		Arity:   -1,
		Doc:     "Return a copy of the string with leading and trailing characters removed",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			s := string(receiver.(StringValue))
			if len(args) == 0 {
				return StringValue(strings.TrimSpace(s)), nil
			}
			if chars, ok := args[0].(StringValue); ok {
				return StringValue(strings.Trim(s, string(chars))), nil
			}
			return nil, fmt.Errorf("strip() argument must be a string")
		},
	}

	// Update split to support maxsplit
	td.Methods["split"] = &MethodDescriptor{
		Name:    "split",
		Arity:   -1,
		Doc:     "Return a list of the words in the string, using sep as the delimiter",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			s := string(receiver.(StringValue))
			sep := " "
			limit := -1
			
			if len(args) > 0 {
				if sepStr, ok := args[0].(StringValue); ok {
					sep = string(sepStr)
				} else {
					return nil, fmt.Errorf("sep must be a string")
				}
			}
			if len(args) > 1 {
				if maxSplit, ok := args[1].(NumberValue); ok {
					limit = int(maxSplit) + 1
				} else {
					return nil, fmt.Errorf("maxsplit must be a number")
				}
			}
			
			var parts []string
			if limit == -1 {
				parts = strings.Split(s, sep)
			} else {
				parts = strings.SplitN(s, sep, limit)
			}
			
			result := make(ListValue, len(parts))
			for i, part := range parts {
				result[i] = StringValue(part)
			}
			return result, nil
		},
	}
}