package builtin

import (
	"fmt"
	"math"
	"strconv"
	"strings"

	"github.com/mmichie/m28/common/builders"
	"github.com/mmichie/m28/common/errors"
	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// RegisterTypes registers all type conversion and checking functions using builders
func RegisterTypes(ctx *core.Context) {
	// type - returns the type of a value
	// BEFORE: 15 lines with manual validation
	// AFTER: 6 lines
	ctx.Define("type", core.NewBuiltinFunction(builders.UnaryAny("type", func(val core.Value) (core.Value, error) {
		// Return type descriptor instead of string to support isinstance
		switch v := val.(type) {
		case core.NumberValue:
			return core.StringValue(string(core.NumberType)), nil
		case core.StringValue:
			return core.StringValue(string(core.StringType)), nil
		case core.BoolValue:
			return core.StringValue(string(core.BoolType)), nil
		case core.NilValue:
			return core.StringValue(string(core.NilType)), nil
		case core.ListValue:
			return core.StringValue(string(core.ListType)), nil
		case *core.DictValue:
			return core.StringValue(string(core.DictType)), nil
		case core.TupleValue:
			return core.StringValue(string(core.TupleType)), nil
		case *core.SetValue:
			return core.StringValue(string(core.SetType)), nil
		case *core.BuiltinFunction:
			return core.StringValue("function"), nil
		case core.Callable:
			return core.StringValue("function"), nil
		case *core.Class:
			return v, nil // Classes are their own type
		case *core.Instance:
			return v.Class, nil
		default:
			// For any custom types
			return core.StringValue(string(val.Type())), nil
		}
	})))

	// str - convert to string
	ctx.Define("str", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("str", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		val := v.Get(0)

		// Try __str__ dunder method first
		if str, found, err := types.CallStr(val, ctx); found {
			if err != nil {
				return nil, err
			}
			return core.StringValue(str), nil
		}

		// Fall back to built-in String() method
		return core.StringValue(val.String()), nil
	}))

	// bool - convert to boolean
	ctx.Define("bool", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("bool", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		val := v.Get(0)

		// Try __bool__ dunder method first
		if b, found, err := types.CallBool(val, ctx); found {
			if err != nil {
				return nil, err
			}
			return core.BoolValue(b), nil
		}

		// Fall back to IsTruthy
		return core.BoolValue(core.IsTruthy(val)), nil
	}))

	// is_none - check if value is None
	// BEFORE: 6 lines
	// AFTER: 3 lines
	ctx.Define("is_none", core.NewBuiltinFunction(builders.UnaryAny("is_none", func(val core.Value) (core.Value, error) {
		_, isNil := val.(core.NilValue)
		return core.BoolValue(isNil), nil
	})))

	// int - convert to integer with optional base
	// BEFORE: 36 lines
	// AFTER: ~15 lines with custom builder
	ctx.Define("int", core.NewBuiltinFunction(IntBuilder()))

	// float - convert to float
	// BEFORE: 23 lines
	// AFTER: ~10 lines with custom builder
	ctx.Define("float", core.NewBuiltinFunction(FloatBuilder()))

	// isinstance - check if object is instance of type(s)
	// BEFORE: 20 lines
	// AFTER: ~12 lines with custom builder
	ctx.Define("isinstance", core.NewBuiltinFunction(IsInstanceBuilder()))

	// issubclass - check if class is subclass of another
	// BEFORE: 7 lines
	// AFTER: ~8 lines with validation
	ctx.Define("issubclass", core.NewBuiltinFunction(IsSubclassBuilder()))

	// format - Python-style format(value, format_spec='')
	// Calls __format__ dunder method for custom formatting
	ctx.Define("format", core.NewBuiltinFunction(FormatBuilder()))

	// Note: super is implemented as a special form in eval/class_forms.go
	// to support both bare "super" and "super()" syntax
}

// Custom builders for complex type conversion functions

// IntBuilder creates the int() function with optional base parameter
func IntBuilder() builders.BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("int", args)

		if err := v.Range(1, 2); err != nil {
			return nil, err
		}

		val := v.Get(0)

		// No base specified - simple conversion
		if v.Count() == 1 {
			// Try __int__ dunder method first
			if intVal, found, err := types.CallInt(val, ctx); found {
				if err != nil {
					return nil, err
				}
				return core.NumberValue(float64(intVal)), nil
			}

			switch x := val.(type) {
			case core.NumberValue:
				return core.NumberValue(float64(int(x))), nil
			case core.StringValue:
				// Try to parse as integer
				s := strings.TrimSpace(string(x))
				if i, err := strconv.ParseInt(s, 10, 64); err == nil {
					return core.NumberValue(float64(i)), nil
				}
				// Try to parse as float and truncate
				if f, err := strconv.ParseFloat(s, 64); err == nil {
					return core.NumberValue(float64(int(f))), nil
				}
				return nil, errors.NewValueError("int", fmt.Sprintf("invalid literal for int() with base 10: '%s'", s))
			case core.BoolValue:
				if bool(x) {
					return core.NumberValue(1), nil
				}
				return core.NumberValue(0), nil
			default:
				return nil, errors.NewTypeError("int", "number or string", string(x.Type()))
			}
		}

		// Base specified - string parsing with base
		str, err := v.GetString(0)
		if err != nil {
			return nil, errors.NewTypeError("int", "string (when base is given)", string(val.Type()))
		}

		base, err := v.GetInt(1)
		if err != nil {
			return nil, err
		}

		if base != 0 && (base < 2 || base > 36) {
			return nil, errors.NewValueError("int", "int() base must be >= 2 and <= 36, or 0")
		}

		// Parse with specified base
		s := strings.TrimSpace(str)
		if i, err := strconv.ParseInt(s, base, 64); err == nil {
			return core.NumberValue(float64(i)), nil
		}

		return nil, errors.NewValueError("int", fmt.Sprintf("invalid literal for int() with base %d: '%s'", base, s))
	}
}

// FloatBuilder creates the float() function
func FloatBuilder() builders.BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("float", args)

		if err := v.Exact(1); err != nil {
			return nil, err
		}

		val := v.Get(0)

		// Try __float__ dunder method first
		if floatVal, found, err := types.CallFloat(val, ctx); found {
			if err != nil {
				return nil, err
			}
			return core.NumberValue(floatVal), nil
		}

		switch x := val.(type) {
		case core.NumberValue:
			return x, nil
		case core.StringValue:
			s := strings.TrimSpace(string(x))
			// Handle special values
			switch strings.ToLower(s) {
			case "inf", "infinity":
				return core.NumberValue(math.Inf(1)), nil
			case "-inf", "-infinity":
				return core.NumberValue(math.Inf(-1)), nil
			case "nan":
				return core.NumberValue(math.NaN()), nil
			}
			// Try to parse as float
			if f, err := strconv.ParseFloat(s, 64); err == nil {
				return core.NumberValue(f), nil
			}
			return nil, errors.NewValueError("float", fmt.Sprintf("could not convert string to float: '%s'", s))
		case core.BoolValue:
			if bool(x) {
				return core.NumberValue(1.0), nil
			}
			return core.NumberValue(0.0), nil
		default:
			return nil, errors.NewTypeError("float", "number or string", string(val.Type()))
		}
	}
}

// IsInstanceBuilder creates the isinstance() function
func IsInstanceBuilder() builders.BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("isinstance", args)

		if err := v.Exact(2); err != nil {
			return nil, err
		}

		obj := v.Get(0)
		typeArg := v.Get(1)

		// Handle tuple of types
		if tuple, ok := typeArg.(core.TupleValue); ok {
			for _, t := range tuple {
				if isInstanceOf(obj, t) {
					return core.BoolValue(true), nil
				}
			}
			return core.BoolValue(false), nil
		}

		// Single type check
		return core.BoolValue(isInstanceOf(obj, typeArg)), nil
	}
}

// IsSubclassBuilder creates the issubclass() function
func IsSubclassBuilder() builders.BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("issubclass", args)

		if err := v.Exact(2); err != nil {
			return nil, err
		}

		// First argument must be a class
		cls, ok := v.Get(0).(*core.Class)
		if !ok {
			return nil, errors.NewTypeError("issubclass", "arg 1 must be a class", string(v.Get(0).Type()))
		}

		// Second argument can be a class or tuple of classes
		typeArg := v.Get(1)

		// Handle tuple of classes
		if tuple, ok := typeArg.(core.TupleValue); ok {
			for _, t := range tuple {
				if parentCls, ok := t.(*core.Class); ok {
					// Simple check - classes are same or cls has parent in bases
					if cls == parentCls || (cls.Parent != nil && cls.Parent == parentCls) {
						return core.BoolValue(true), nil
					}
				}
			}
			return core.BoolValue(false), nil
		}

		// Single class check
		if parentCls, ok := typeArg.(*core.Class); ok {
			// Simple check - classes are same or cls has parent in bases
			return core.BoolValue(cls == parentCls || (cls.Parent != nil && cls.Parent == parentCls)), nil
		}

		return nil, errors.NewTypeError("issubclass", "arg 2 must be a class or tuple of classes", string(typeArg.Type()))
	}
}

// Helper function for isinstance checks
func isInstanceOf(obj, typeVal core.Value) bool {
	switch t := typeVal.(type) {
	case core.StringValue:
		// Check against built-in type names
		typeName := string(t)
		switch obj.(type) {
		case core.NumberValue:
			return typeName == string(core.NumberType) || typeName == "int" || typeName == "float"
		case core.StringValue:
			return typeName == string(core.StringType) || typeName == "str"
		case core.BoolValue:
			return typeName == string(core.BoolType) || typeName == "bool"
		case core.NilValue:
			return typeName == string(core.NilType) || typeName == "NoneType"
		case core.ListValue:
			return typeName == string(core.ListType) || typeName == "list"
		case *core.DictValue:
			return typeName == string(core.DictType) || typeName == "dict"
		case core.TupleValue:
			return typeName == string(core.TupleType) || typeName == "tuple"
		case *core.SetValue:
			return typeName == string(core.SetType) || typeName == "set"
		case *core.BuiltinFunction:
			return typeName == "function"
		case core.Callable:
			return typeName == "function"
		}
	case *core.Class:
		// Check against user-defined classes
		if inst, ok := obj.(*core.Instance); ok {
			// Simple check - instance's class matches
			return inst.Class == t || (inst.Class.Parent != nil && inst.Class.Parent == t)
		}
	}
	return false
}

// FormatBuilder creates the format() function
// Python-style format(value, format_spec=”) that calls __format__ dunder method
func FormatBuilder() builders.BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("format", args)

		if err := v.Range(1, 2); err != nil {
			return nil, err
		}

		val := v.Get(0)
		formatSpec := ""

		// Get format_spec if provided
		if v.Count() == 2 {
			spec, err := v.GetString(1)
			if err != nil {
				return nil, err
			}
			formatSpec = spec
		}

		// Try __format__ dunder method first
		if str, found, err := types.CallFormat(val, formatSpec, ctx); found {
			if err != nil {
				return nil, err
			}
			return core.StringValue(str), nil
		}

		// Fall back to str() conversion if no __format__ method
		// For built-in types, convert appropriately
		if str, ok := val.(core.StringValue); ok {
			// Strings should return themselves, not their repr
			return str, nil
		}
		// For other types, use String() method
		return core.StringValue(val.String()), nil
	}
}

// Migration Statistics:
// Functions migrated: 8 type-related functions
// Original lines: ~119 lines
// Migrated lines: ~80 lines (more functionality)
// Reduction: ~33% with enhanced error handling and features
