// Package builtin provides standard library functions for the M28 language.
package builtin

import (
	"m28/core"
)

// RegisterAll registers all builtin functions in the global context
func RegisterAll(ctx *core.Context) {
	// Register basic arithmetic functions
	RegisterArithmeticFunctions(ctx)
	
	// Register comparison functions
	RegisterComparisonFunctions(ctx)
	
	// Register list/container functions
	RegisterContainerFunctions(ctx)
	
	// Register string functions
	RegisterStringFunctions(ctx)
	
	// Register conversion functions
	RegisterConversionFunctions(ctx)
	
	// Register utility functions
	RegisterUtilityFunctions(ctx)
}

// RegisterUtilityFunctions registers common utility functions
func RegisterUtilityFunctions(ctx *core.Context) {
	// Type checking functions
	ctx.Define("type", core.NewBuiltinFunction(TypeFunc))
	ctx.Define("instanceof", core.NewBuiltinFunction(InstanceofFunc))
	
	// Length function for containers
	ctx.Define("len", core.NewBuiltinFunction(LenFunc))
}

// TypeFunc returns the type of a value
func TypeFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, core.NewError(ctx, "type requires exactly 1 argument")
	}
	
	return core.StringValue(args[0].Type().Name()), nil
}

// InstanceofFunc checks if a value is an instance of a given type
func InstanceofFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, core.NewError(ctx, "instanceof requires exactly 2 arguments")
	}
	
	// Get the type name
	typeName, ok := args[1].(core.StringValue)
	if !ok {
		return nil, core.NewError(ctx, "instanceof requires a type name (string) as second argument")
	}
	
	// Check if the value's type matches or is a subtype
	valueType := args[0].Type()
	return core.BoolValue(valueType.Name() == string(typeName) || valueType.IsSubtypeOf(args[1].Type())), nil
}

// LenFunc returns the length of a container
func LenFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, core.NewError(ctx, "len requires exactly 1 argument")
	}
	
	switch v := args[0].(type) {
	case core.StringValue:
		return core.NumberValue(len(v)), nil
	case core.ListValue:
		return core.NumberValue(len(v)), nil
	case core.TupleValue:
		return core.NumberValue(len(v)), nil
	case *core.DictValue:
		// Get all keys
		result, err := v.CallMethod("keys", nil, ctx)
		if err != nil {
			return nil, err
		}
		// Return length of keys list
		if keys, ok := result.(core.ListValue); ok {
			return core.NumberValue(len(keys)), nil
		}
		return core.NumberValue(0), nil
	default:
		return nil, core.NewError(ctx, "len not supported for %s", v.Type().Name())
	}
}

// RegisterContainerFunctions registers functions for manipulating containers
func RegisterContainerFunctions(ctx *core.Context) {
	// List functions
	ctx.Define("list", core.NewBuiltinFunction(ListFunc))
	ctx.Define("append", core.NewBuiltinFunction(AppendFunc))
	ctx.Define("extend", core.NewBuiltinFunction(ExtendFunc))
	
	// Dict functions
	ctx.Define("dict", core.NewBuiltinFunction(DictFunc))
	ctx.Define("get", core.NewBuiltinFunction(GetFunc))
	
	// Tuple functions
	ctx.Define("tuple", core.NewBuiltinFunction(TupleFunc))
}

// ListFunc creates a new list
func ListFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	return core.ListValue(args), nil
}

// AppendFunc appends an item to a list
func AppendFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 2 {
		return nil, core.NewError(ctx, "append requires at least 2 arguments")
	}
	
	list, ok := args[0].(core.ListValue)
	if !ok {
		return nil, core.NewError(ctx, "first argument to append must be a list")
	}
	
	result := make(core.ListValue, len(list))
	copy(result, list)
	
	return append(result, args[1:]...), nil
}

// ExtendFunc extends a list with another list
func ExtendFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, core.NewError(ctx, "extend requires exactly 2 arguments")
	}
	
	list1, ok := args[0].(core.ListValue)
	if !ok {
		return nil, core.NewError(ctx, "first argument to extend must be a list")
	}
	
	list2, ok := args[1].(core.ListValue)
	if !ok {
		return nil, core.NewError(ctx, "second argument to extend must be a list")
	}
	
	result := make(core.ListValue, len(list1))
	copy(result, list1)
	
	return append(result, list2...), nil
}

// DictFunc creates a new dictionary
func DictFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	dict := core.NewDict()
	
	// If called with no arguments, return empty dict
	if len(args) == 0 {
		return dict, nil
	}
	
	// If called with odd number of arguments, error
	if len(args)%2 != 0 {
		return nil, core.NewError(ctx, "dict requires an even number of arguments")
	}
	
	// Add key-value pairs
	for i := 0; i < len(args); i += 2 {
		// Keys must be strings
		key, ok := args[i].(core.StringValue)
		if !ok {
			return nil, core.NewError(ctx, "dict keys must be strings")
		}
		
		dict.Set(string(key), args[i+1])
	}
	
	return dict, nil
}

// GetFunc gets a value from a dictionary with an optional default
func GetFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 2 || len(args) > 3 {
		return nil, core.NewError(ctx, "get requires 2 or 3 arguments")
	}
	
	dict, ok := args[0].(*core.DictValue)
	if !ok {
		return nil, core.NewError(ctx, "first argument to get must be a dictionary")
	}
	
	key, ok := args[1].(core.StringValue)
	if !ok {
		return nil, core.NewError(ctx, "second argument to get must be a string")
	}
	
	// Get the value
	value, found := dict.Get(string(key))
	if found {
		return value, nil
	}
	
	// Return default if provided
	if len(args) == 3 {
		return args[2], nil
	}
	
	// Otherwise return nil
	return core.Nil, nil
}

// TupleFunc creates a new tuple
func TupleFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	return core.TupleValue(args), nil
}

// RegisterStringFunctions registers string manipulation functions
func RegisterStringFunctions(ctx *core.Context) {
	// String functions
	ctx.Define("upper", core.NewBuiltinFunction(UpperFunc))
	ctx.Define("lower", core.NewBuiltinFunction(LowerFunc))
	ctx.Define("split", core.NewBuiltinFunction(SplitFunc))
	ctx.Define("join", core.NewBuiltinFunction(JoinFunc))
}

// UpperFunc converts a string to uppercase
func UpperFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, core.NewError(ctx, "upper requires exactly 1 argument")
	}
	
	str, ok := args[0].(core.StringValue)
	if !ok {
		return nil, core.NewError(ctx, "upper requires a string argument")
	}
	
	// Call the string's upper method
	result, err := str.CallMethod("upper", nil, ctx)
	if err != nil {
		return nil, err
	}
	
	return result, nil
}

// LowerFunc converts a string to lowercase
func LowerFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, core.NewError(ctx, "lower requires exactly 1 argument")
	}
	
	str, ok := args[0].(core.StringValue)
	if !ok {
		return nil, core.NewError(ctx, "lower requires a string argument")
	}
	
	// Call the string's lower method
	result, err := str.CallMethod("lower", nil, ctx)
	if err != nil {
		return nil, err
	}
	
	return result, nil
}

// SplitFunc splits a string by a delimiter
func SplitFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, core.NewError(ctx, "split requires exactly 2 arguments")
	}
	
	str, ok := args[0].(core.StringValue)
	if !ok {
		return nil, core.NewError(ctx, "first argument to split must be a string")
	}
	
	delim, ok := args[1].(core.StringValue)
	if !ok {
		return nil, core.NewError(ctx, "second argument to split must be a string")
	}
	
	// Call the string's split method
	result, err := str.CallMethod("split", []core.Value{delim}, ctx)
	if err != nil {
		return nil, err
	}
	
	return result, nil
}

// JoinFunc joins a list of strings with a delimiter
func JoinFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, core.NewError(ctx, "join requires exactly 2 arguments")
	}
	
	delim, ok := args[0].(core.StringValue)
	if !ok {
		return nil, core.NewError(ctx, "first argument to join must be a string")
	}
	
	list, ok := args[1].(core.ListValue)
	if !ok {
		return nil, core.NewError(ctx, "second argument to join must be a list")
	}
	
	// Convert all list items to strings
	strs := make([]string, len(list))
	for i, item := range list {
		strs[i] = item.String()
	}
	
	// Build the joined string
	result := ""
	for i, s := range strs {
		if i > 0 {
			result += string(delim)
		}
		result += s
	}
	
	return core.StringValue(result), nil
}

// RegisterConversionFunctions registers type conversion functions
func RegisterConversionFunctions(ctx *core.Context) {
	// Conversion functions
	ctx.Define("str", core.NewBuiltinFunction(StrFunc))
	ctx.Define("num", core.NewBuiltinFunction(NumFunc))
	ctx.Define("bool", core.NewBuiltinFunction(BoolFunc))
}

// StrFunc converts a value to a string
func StrFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, core.NewError(ctx, "str requires exactly 1 argument")
	}
	
	return core.StringValue(args[0].String()), nil
}

// NumFunc converts a value to a number
func NumFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, core.NewError(ctx, "num requires exactly 1 argument")
	}
	
	switch v := args[0].(type) {
	case core.NumberValue:
		return v, nil
	case core.StringValue:
		// Parse string as float
		var result float64
		if _, err := fmt.Sscanf(string(v), "%f", &result); err != nil {
			return nil, core.NewError(ctx, "cannot convert string '%s' to number", v)
		}
		return core.NumberValue(result), nil
	case core.BoolValue:
		if bool(v) {
			return core.NumberValue(1), nil
		}
		return core.NumberValue(0), nil
	default:
		return nil, core.NewError(ctx, "cannot convert %s to number", v.Type().Name())
	}
}

// BoolFunc converts a value to a boolean
func BoolFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, core.NewError(ctx, "bool requires exactly 1 argument")
	}
	
	// Check truthiness based on type
	switch v := args[0].(type) {
	case core.BoolValue:
		return v, nil
	case core.NumberValue:
		return core.BoolValue(float64(v) != 0), nil
	case core.StringValue:
		return core.BoolValue(string(v) != ""), nil
	case core.ListValue:
		return core.BoolValue(len(v) > 0), nil
	case core.TupleValue:
		return core.BoolValue(len(v) > 0), nil
	case core.NilValue:
		return core.False, nil
	default:
		return core.True, nil // Other objects are truthy by default
	}
}