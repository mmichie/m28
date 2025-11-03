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

// Type classes for built-in types
var (
	NumberTypeClass   *core.Class // Single number type for M28's unified NumberValue
	BoolTypeClass     *core.Class
	NoneTypeClass     *core.Class
	ListTypeClass     *core.Class
	TupleTypeClass    *core.Class
	SetTypeClass      *core.Class
	FunctionTypeClass *core.Class
	MethodTypeClass   *core.Class // For bound methods
	TypeMetaclass     *TypeType   // The type metaclass (for class ABCMeta(type):)
	StrTypeClass      *StrType    // Global str class instance
	DictTypeClass     *DictType   // Global dict class instance (set by RegisterCollections)
	// Cache for dynamically created type classes
	typeClassCache = make(map[string]*core.Class)
)

// SetDictTypeClass is called by RegisterCollections to set the global dict class
func SetDictTypeClass(dictClass *DictType) {
	DictTypeClass = dictClass
}

// RegisterTypes registers all type conversion and checking functions using builders
func RegisterTypes(ctx *core.Context) {
	// Create type classes for built-in types
	NumberTypeClass = core.NewClass("number", nil) // M28 uses unified NumberValue for all numbers
	BoolTypeClass = core.NewClass("bool", nil)
	NoneTypeClass = core.NewClass("NoneType", nil)
	ListTypeClass = core.NewClass("list", nil)
	TupleTypeClass = core.NewClass("tuple", nil)
	SetTypeClass = core.NewClass("set", nil)
	FunctionTypeClass = core.NewClass("function", nil)
	MethodTypeClass = core.NewClass("method", nil) // For bound methods

	// Register type descriptor for NoneType to handle __class__
	noneTypeDesc := &core.TypeDescriptor{
		Name:       "NoneType",
		PythonName: "NoneType",
		BaseType:   core.NilType,
		Properties: map[string]*core.PropertyDescriptor{
			"__class__": {
				Name:     "__class__",
				ReadOnly: true,
				Doc:      "The class of None",
				Getter: func(v core.Value) (core.Value, error) {
					return NoneTypeClass, nil
				},
			},
		},
	}
	core.RegisterType(noneTypeDesc)

	// Add __new__ classmethod to tuple
	tupleNew := core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// tuple.__new__(cls, iterable=())
		// First arg is cls (the class), second is optional iterable
		if len(args) < 1 {
			return nil, fmt.Errorf("tuple.__new__() missing 1 required positional argument: 'cls'")
		}

		cls := args[0]

		// Convert iterable to tuple data
		var tupleData core.TupleValue

		if len(args) == 1 {
			// If only cls provided, create empty tuple
			tupleData = core.TupleValue{}
		} else {
			// Convert iterable to tuple
			iterable := args[1]

			// Handle different iterable types
			switch v := iterable.(type) {
			case core.TupleValue:
				tupleData = v
			case *core.ListValue:
				tupleData = core.TupleValue(v.Items())
			case core.Iterable:
				// Iterate and collect values
				result := make([]core.Value, 0)
				iter := v.Iterator()
				for {
					val, hasNext := iter.Next()
					if !hasNext {
						break
					}
					result = append(result, val)
				}
				tupleData = core.TupleValue(result)
			default:
				return nil, fmt.Errorf("tuple.__new__() argument must be an iterable")
			}
		}

		// If cls is a subclass of tuple (like a namedtuple), create a TupleInstance
		// Otherwise, return a plain TupleValue
		if class, ok := cls.(*core.Class); ok {
			// Check if it's a tuple subclass (not the base tuple class)
			if class != TupleTypeClass && class.Name != "tuple" {
				return core.NewTupleInstance(class, tupleData), nil
			}
		}

		// Return plain tuple for base tuple class
		return tupleData, nil
	})
	tupleNew.SetAttr("__name__", core.StringValue("__new__"))
	TupleTypeClass.SetMethod("__new__", &core.ClassMethodValue{Function: tupleNew})

	// Add __new__ classmethod to list
	listNew := core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// list.__new__(cls, iterable=())
		// First arg is cls (the class), second is optional iterable
		if len(args) < 1 {
			return nil, fmt.Errorf("list.__new__() missing 1 required positional argument: 'cls'")
		}

		cls := args[0]

		// Convert iterable to list data
		var listData *core.ListValue

		if len(args) == 1 {
			// If only cls provided, create empty list
			listData = core.NewList()
		} else {
			// Convert iterable to list
			iterable := args[1]

			// Handle different iterable types
			switch v := iterable.(type) {
			case *core.ListValue:
				// Copy the list
				listData = core.NewList(v.Items()...)
			case core.TupleValue:
				listData = core.NewList(v...)
			case core.Iterable:
				// Iterate and collect values
				result := make([]core.Value, 0)
				iter := v.Iterator()
				for {
					val, hasNext := iter.Next()
					if !hasNext {
						break
					}
					result = append(result, val)
				}
				listData = core.NewList(result...)
			default:
				return nil, fmt.Errorf("list.__new__() argument must be an iterable")
			}
		}

		// If cls is a subclass of list (like StackSummary), create a ListInstance
		// Otherwise, return a plain ListValue
		if class, ok := cls.(*core.Class); ok {
			// Check if it's a list subclass (not the base list class)
			if class != ListTypeClass && class.Name != "list" {
				return core.NewListInstance(class, listData), nil
			}
		}

		// Return plain list for base list class
		return listData, nil
	})
	listNew.SetAttr("__name__", core.StringValue("__new__"))
	ListTypeClass.SetMethod("__new__", &core.ClassMethodValue{Function: listNew})

	// Add __code__ and __globals__ as class attributes on function type
	// These are descriptor objects that Python uses to access function attributes
	FunctionTypeClass.SetClassAttr("__code__", core.NewCodeObject(nil))
	FunctionTypeClass.SetClassAttr("__globals__", core.NewDict())

	// type - the type metaclass (can be inherited from)
	// Create as a class with __call__ method so it can be used as:
	// 1. type(obj) - returns the type of obj
	// 2. class Meta(type): - can be inherited from for metaclasses
	TypeMetaclass = createTypeMetaclass()
	ctx.Define("type", TypeMetaclass)

	// str - Python str class
	// Create as a class so str.join and other class methods can be accessed
	StrTypeClass = createStrClass()
	ctx.Define("str", StrTypeClass)

	// is_none - check if value is None
	// BEFORE: 6 lines
	// AFTER: 3 lines
	ctx.Define("is_none", core.NewBuiltinFunction(builders.UnaryAny("is_none", func(val core.Value) (core.Value, error) {
		_, isNil := val.(core.NilValue)
		return core.BoolValue(isNil), nil
	})))

	// object - Base class for all Python objects
	// Must be defined before other types that inherit from it
	objectClass := core.NewClass("object", nil)

	// Note: We set __signature__ instead of __text_signature__ because M28's
	// _signature_fromstr doesn't fully support text signature parsing yet.
	// The __signature__ will be set later after inspect module is available.

	// Add __new__ method to object
	// object.__new__(cls) - creates a new instance of the class
	// This is a static method that's called before __init__
	objectNew := core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("__new__() missing 1 required positional argument: 'cls'")
		}
		// First argument should be the class
		cls, ok := args[0].(*core.Class)
		if !ok {
			return nil, fmt.Errorf("__new__() argument 1 must be a class, not %T", args[0])
		}
		// Create a new instance of the class
		return core.NewInstance(cls), nil
	})
	// Note: We don't add __text_signature__ to object.__new__ because M28's
	// _signature_fromstr doesn't fully support it yet.
	objectClass.SetMethod("__new__", objectNew)

	// Add __init__ method to object
	// object.__init__(self, *args, **kwargs) - does nothing, accepts any arguments
	// We need to create a special builtin that supports keyword arguments
	objectInit := &core.BuiltinFunctionWithKwargs{
		BaseObject: *core.NewBaseObject(core.FunctionType),
		Name:       "__init__",
		Fn: func(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
			// object.__init__ accepts self and any arguments/kwargs, returns None
			return core.None, nil
		},
	}

	// Create a custom __code__ object with proper signature: (self, *args, **kwargs)
	// This allows inspect.signature() to work correctly
	codeObj := core.NewCodeObject(objectInit)
	codeObj.SetAttr("co_argcount", core.NumberValue(1)) // 1 positional arg: self
	codeObj.SetAttr("co_posonlyargcount", core.NumberValue(0))
	codeObj.SetAttr("co_kwonlyargcount", core.NumberValue(0))
	codeObj.SetAttr("co_nlocals", core.NumberValue(1))
	codeObj.SetAttr("co_flags", core.NumberValue(0x04|0x08)) // CO_VARARGS | CO_VARKEYWORDS
	codeObj.SetAttr("co_varnames", core.TupleValue{core.StringValue("self"), core.StringValue("args"), core.StringValue("kwargs")})
	codeObj.SetAttr("co_code", core.StringValue(""))
	codeObj.SetAttr("co_consts", core.TupleValue{})
	codeObj.SetAttr("co_names", core.TupleValue{})
	codeObj.SetAttr("co_freevars", core.TupleValue{})
	codeObj.SetAttr("co_cellvars", core.TupleValue{})
	codeObj.SetAttr("co_filename", core.StringValue("<builtin>"))
	codeObj.SetAttr("co_name", core.StringValue("__init__"))
	codeObj.SetAttr("co_firstlineno", core.NumberValue(0))
	codeObj.SetAttr("co_lnotab", core.StringValue(""))
	objectInit.SetAttr("__code__", codeObj)

	// Note: We don't add __text_signature__ to object.__init__ because M28's
	// _signature_fromstr doesn't fully support it yet. Instead, we rely on __code__.

	objectClass.SetMethod("__init__", objectInit)

	// Add __str__ method to object
	// object.__str__(self) - returns string representation
	objectClass.SetMethod("__str__", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("__str__() missing 1 required positional argument: 'self'")
		}
		// Return a simple string representation
		// For instances, format directly without calling String() to avoid recursion
		if inst, ok := args[0].(*core.Instance); ok {
			return core.StringValue(fmt.Sprintf("<%s instance at %p>", inst.Class.Name, inst)), nil
		}
		// For other types, use String() method
		return core.StringValue(args[0].String()), nil
	}))

	// Add __init_subclass__ as a classmethod
	// This is called when a subclass is created
	// Python's object.__init_subclass__(cls) is a no-op, but must exist
	// NOTE: In Python, __init_subclass__ is implicitly a classmethod
	// We'll handle it specially in class creation
	objectClass.SetMethod("__init_subclass__", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Accept any number of arguments for compatibility
		// The first arg should be the new subclass
		if len(args) > 0 {
			if cls, ok := args[0].(*core.Class); ok {
				// Initialize unittest-required class attributes
				// These would normally be set by unittest.TestCase.__init_subclass__
				// but since we don't have full __init_subclass__ support yet, set them here
				cls.Attributes["_classSetupFailed"] = core.False
				if _, hasCleanups := cls.Attributes["_class_cleanups"]; !hasCleanups {
					cls.Attributes["_class_cleanups"] = core.NewList()
				}
			}
		}
		return core.None, nil
	}))

	ctx.Define("object", objectClass)

	// int - Python int constructor
	// Defined after object so it can inherit from it
	intClass := createIntClass(objectClass)
	ctx.Define("int", intClass)

	// bool - Python bool class that inherits from int
	// Defined after int so it can inherit from it
	ctx.Define("bool", createBoolClass(intClass))

	// float - Python float constructor
	ctx.Define("float", createFloatClass())

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

		if err := v.Range(0, 2); err != nil {
			return nil, err
		}

		// Python int() with no args returns 0
		if v.Count() == 0 {
			return core.NumberValue(0), nil
		}

		val := v.Get(0)

		// No base specified - simple conversion
		if v.Count() == 1 {
			// Try __int__ dunder method first
			if result, found, err := types.CallDunder(val, "__int__", []core.Value{}, ctx); found {
				if err != nil {
					return nil, err
				}
				// __int__ can return NumberValue or BigIntValue
				switch r := result.(type) {
				case core.NumberValue:
					return core.NumberValue(float64(int(r))), nil
				case core.BigIntValue:
					return r, nil // Return BigIntValue as-is
				default:
					return nil, errors.NewTypeError("int", "int or bigint", string(result.Type()))
				}
			}

			switch x := val.(type) {
			case core.NumberValue:
				return core.NumberValue(float64(int(x))), nil
			case core.BigIntValue:
				return x, nil // Already a BigInt, return as-is
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

		if err := v.Max(1); err != nil {
			return nil, err
		}

		// Python float() with no args returns 0.0
		if v.Count() == 0 {
			return core.NumberValue(0.0), nil
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

		subArg := v.Get(0)
		baseArg := v.Get(1)

		// Handle builtin type constructors (e.g., bool, int, str)
		if subBF, ok := subArg.(*core.BuiltinFunction); ok {
			if baseBF, ok := baseArg.(*core.BuiltinFunction); ok {
				// Get the names of both builtin types
				subName := ""
				baseName := ""
				if nameAttr, hasName := subBF.GetAttr("__name__"); hasName {
					if nameStr, ok := nameAttr.(core.StringValue); ok {
						subName = string(nameStr)
					}
				}
				if nameAttr, hasName := baseBF.GetAttr("__name__"); hasName {
					if nameStr, ok := nameAttr.(core.StringValue); ok {
						baseName = string(nameStr)
					}
				}

				// Check if they're the same type
				if subName == baseName {
					return core.BoolValue(true), nil
				}

				// Special case: bool is a subclass of int in Python
				if subName == "bool" && baseName == "int" {
					return core.BoolValue(true), nil
				}

				return core.BoolValue(false), nil
			}
			// subclass is builtin but base is not - can't be a subclass
			return core.BoolValue(false), nil
		}

		// First argument must be a class if not a builtin
		cls, ok := subArg.(*core.Class)
		if !ok {
			return nil, errors.NewTypeError("issubclass", "arg 1 must be a class", string(subArg.Type()))
		}

		// Second argument can be a class or tuple of classes
		typeArg := baseArg

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
		case core.BigIntValue:
			return typeName == string(core.BigIntType) || typeName == "int"
		case core.StringValue:
			return typeName == string(core.StringType) || typeName == "str"
		case core.BoolValue:
			return typeName == string(core.BoolType) || typeName == "bool"
		case core.NilValue:
			return typeName == string(core.NilType) || typeName == "NoneType"
		case *core.ListValue:
			return typeName == string(core.ListType) || typeName == "list"
		case *core.DictValue:
			return typeName == string(core.DictType) || typeName == "dict"
		case core.TupleValue:
			return typeName == string(core.TupleType) || typeName == "tuple"
		case *core.SetValue:
			return typeName == string(core.SetType) || typeName == "set"
		case core.BytesValue:
			return typeName == string(core.BytesType) || typeName == "bytes"
		case *core.ByteArrayValue:
			return typeName == "bytearray"
		case *core.BuiltinFunction:
			return typeName == "function"
		case core.Callable:
			return typeName == "function"
		}
	case *TypeType:
		// isinstance(obj, type) checks if obj is a class
		// type is the metaclass, so instances of type are classes
		switch obj.(type) {
		case *core.Class:
			return true
		case *TypeType, *StrType, *DictType:
			// These are wrapper types that are also classes
			return true
		}
		return false
	case *core.Class:
		// Check against user-defined classes
		if inst, ok := obj.(*core.Instance); ok {
			// Simple check - instance's class matches
			return inst.Class == t || (inst.Class.Parent != nil && inst.Class.Parent == t)
		}
	case *core.BuiltinFunction:
		// Handle builtin type constructors like list, int, float, etc.
		// Check the function's name attribute if available
		if nameVal, ok := t.GetAttr("__name__"); ok {
			if nameStr, ok := nameVal.(core.StringValue); ok {
				typeName := string(nameStr)
				switch obj.(type) {
				case *core.ListValue:
					return typeName == "list"
				case core.NumberValue:
					return typeName == "int" || typeName == "float"
				case core.BigIntValue:
					return typeName == "int"
				case core.StringValue:
					return typeName == "str"
				case core.BoolValue:
					return typeName == "bool"
				case core.TupleValue:
					return typeName == "tuple"
				case *core.DictValue:
					return typeName == "dict"
				case *core.SetValue:
					return typeName == "set"
				case core.BytesValue:
					return typeName == "bytes"
				case *core.ByteArrayValue:
					return typeName == "bytearray"
				}
			}
		}
	case *IntType:
		// Special handling for int type to match both NumberValue and BigIntValue
		switch obj.(type) {
		case core.NumberValue, core.BigIntValue:
			return true
		case *core.Instance:
			// Also match int subclass instances
			inst := obj.(*core.Instance)
			return inst.Class == t.Class || (inst.Class.Parent != nil && inst.Class.Parent == t.Class)
		}
		return false
	default:
		// Handle wrapper types that have GetClass() method
		if wrapper, ok := typeVal.(interface{ GetClass() *core.Class }); ok {
			classVal := wrapper.GetClass()
			if inst, ok := obj.(*core.Instance); ok {
				return inst.Class == classVal || (inst.Class.Parent != nil && inst.Class.Parent == classVal)
			}
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

// StrType represents the str class that can be called and has class methods
type StrType struct {
	*core.Class
}

// GetClass returns the embedded Class for use as a parent class
func (s *StrType) GetClass() *core.Class {
	return s.Class
}

// Call implements the callable interface for str() conversion
func (s *StrType) Call(args []core.Value, ctx *core.Context) (core.Value, error) {
	// Python str() with no args returns ""
	if len(args) == 0 {
		return core.StringValue(""), nil
	}

	if len(args) > 1 {
		return nil, errors.NewTypeError("str", "str() takes at most 1 argument", fmt.Sprintf("%d given", len(args)))
	}

	val := args[0]

	// Special case: if val is a Class, don't try to call its __str__ method
	// Classes should use their own String() method for representation
	if _, isClass := val.(*core.Class); isClass {
		return core.StringValue(val.String()), nil
	}

	// For wrapper types that embed Class (like StrType, IntType, etc.)
	if _, hasGetClass := val.(interface{ GetClass() *core.Class }); hasGetClass {
		return core.StringValue(val.String()), nil
	}

	// Try __str__ dunder method first (for instances)
	if str, found, err := types.CallStr(val, ctx); found {
		if err != nil {
			return nil, err
		}
		return core.StringValue(str), nil
	}

	// Fall back to built-in String() method
	return core.StringValue(val.String()), nil
}

// CallWithKeywords overrides the embedded Class's CallWithKeywords
// to ensure our custom Call is used instead of the generic class instantiation
func (s *StrType) CallWithKeywords(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
	// For str, keyword arguments are not supported in the constructor
	// Just ignore kwargs and call our custom Call method
	if len(kwargs) > 0 {
		return nil, fmt.Errorf("str() does not accept keyword arguments")
	}
	return s.Call(args, ctx)
}

// TypeType represents the type metaclass that can be called and inherited from
type TypeType struct {
	*core.Class
}

// GetClass returns the embedded Class for use as a parent class
func (t *TypeType) GetClass() *core.Class {
	return t.Class
}

// CallWithKeywords implements keyword argument support for type()
// This takes precedence over Class.CallWithKeywords
func (t *TypeType) CallWithKeywords(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
	// For type(), keyword arguments are only valid when creating a new class (3+ args)
	// For type(obj), ignore kwargs
	if len(args) == 1 && len(kwargs) == 0 {
		return t.Call(args, ctx)
	} else if len(args) == 3 {
		// type(name, bases, dict, **kwargs) - delegate to type.__new__
		return t.Call(args, ctx)
	} else {
		return t.Call(args, ctx)
	}
}

// Call implements the callable interface for type() function
func (t *TypeType) Call(args []core.Value, ctx *core.Context) (core.Value, error) {
	// type() can be called in two ways:
	// 1. type(obj) - returns the type of obj (1 argument)
	// 2. type(name, bases, dict) - creates a new class (3 arguments)

	if len(args) == 1 {
		// type(obj) - return the type of the object
		val := args[0]

		// Return type class instead of string to support isinstance and attribute access
		switch v := val.(type) {
		case *TypeType:
			// type(type) or type(any TypeType wrapper) returns type
			return TypeMetaclass, nil
		case *StrType:
			// type(str) returns type
			return TypeMetaclass, nil
		case *DictType:
			// type(dict) returns type
			return TypeMetaclass, nil
		case core.NumberValue:
			return NumberTypeClass, nil
		case core.StringValue:
			if StrTypeClass != nil {
				return StrTypeClass, nil
			}
			return core.NewClass("str", nil), nil
		case core.BoolValue:
			return BoolTypeClass, nil
		case core.NilValue:
			return NoneTypeClass, nil
		case *core.ListValue:
			return ListTypeClass, nil
		case *core.DictValue:
			if DictTypeClass != nil {
				return DictTypeClass, nil
			}
			return core.NewClass("dict", nil), nil
		case core.TupleValue:
			return TupleTypeClass, nil
		case *core.SetValue:
			return SetTypeClass, nil
		case *core.BuiltinFunction:
			return FunctionTypeClass, nil
		case *core.BoundMethod:
			return MethodTypeClass, nil
		case *core.BoundInstanceMethod:
			return MethodTypeClass, nil
		case *core.Class:
			// type(SomeClass) returns type (the metaclass)
			return TypeMetaclass, nil
		case core.Callable:
			return FunctionTypeClass, nil
		case *core.Instance:
			return v.Class, nil
		case *core.GenericAlias:
			// type(list[int]) returns GenericAlias class
			// Create or get cached GenericAlias class with __new__ method
			if typeClassCache["GenericAlias"] == nil {
				cls := createGenericAliasClass()
				typeClassCache["GenericAlias"] = cls
			}
			return typeClassCache["GenericAlias"], nil
		default:
			// For any custom types, create or get cached class with that name
			typeName := string(val.Type())
			if typeClassCache[typeName] == nil {
				cls := core.NewClass(typeName, nil)
				// Set __name__ attribute so isinstance(type_obj, type) works
				cls.SetClassAttr("__name__", core.StringValue(typeName))
				typeClassCache[typeName] = cls
			}
			return typeClassCache[typeName], nil
		}
	} else if len(args) == 3 {
		// type(name, bases, dict) - create a new class
		// Delegate to type.__new__(type, name, bases, dict)
		newMethod, found := t.Class.GetMethod("__new__")
		if !found {
			return nil, fmt.Errorf("type.__new__ not found")
		}

		callable, ok := newMethod.(core.Callable)
		if !ok {
			return nil, fmt.Errorf("type.__new__ is not callable")
		}

		// Call with type (cls) as first argument, then the 3 provided args
		newArgs := make([]core.Value, 4)
		newArgs[0] = t       // cls argument
		newArgs[1] = args[0] // name
		newArgs[2] = args[1] // bases
		newArgs[3] = args[2] // dict
		return callable.Call(newArgs, ctx)
	} else {
		return nil, fmt.Errorf("type() takes 1 or 3 arguments, got %d", len(args))
	}
}

// createGenericAliasClass creates the GenericAlias class with __new__ method
func createGenericAliasClass() *core.Class {
	cls := core.NewClass("GenericAlias", nil)
	cls.SetClassAttr("__name__", core.StringValue("GenericAlias"))

	// Add __new__ method that accepts (cls, origin, args)
	cls.SetMethod("__new__", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// GenericAlias.__new__(cls, origin, args)
		if len(args) < 3 {
			return nil, fmt.Errorf("GenericAlias.__new__() missing required arguments")
		}

		// args[0] is cls
		// args[1] is origin (the base type like list, dict)
		// args[2] is args (the type parameters as a tuple or single value)

		origin := args[1]
		typeArgs := args[2]

		// Convert typeArgs to a slice
		var argsSlice []core.Value
		if tuple, ok := typeArgs.(core.TupleValue); ok {
			argsSlice = []core.Value(tuple)
		} else {
			argsSlice = []core.Value{typeArgs}
		}

		// Get the origin class
		var originClass *core.Class
		switch o := origin.(type) {
		case *core.Class:
			originClass = o
		case interface{ GetClass() *core.Class }:
			originClass = o.GetClass()
		default:
			return nil, fmt.Errorf("GenericAlias origin must be a class, got %T", origin)
		}

		// Create the GenericAlias
		return core.NewGenericAlias(originClass, argsSlice), nil
	}))

	return cls
}

// createTypeMetaclass creates the type metaclass
func createTypeMetaclass() *TypeType {
	class := core.NewClass("type", nil)

	// Add __call__ method to type metaclass
	// This is needed for inspect.signature() to work properly
	// type.__call__(cls, *args, **kwargs) - calls cls to create an instance
	// cls is the class being instantiated (e.g., MyClass)
	// This method represents what happens when you call a class: MyClass(...)
	typeCallFunc := core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// type.__call__(cls, *args, **kwargs)
		// args[0] is cls (the class being called)
		// args[1:] are the arguments to pass to the class constructor

		if len(args) < 1 {
			return nil, fmt.Errorf("__call__() missing required positional argument: 'cls'")
		}

		cls := args[0]
		ctorArgs := args[1:]

		// Get the class object - handle various class-like types
		var classObj *core.Class
		switch c := cls.(type) {
		case *core.Class:
			classObj = c
		case *TypeType, *StrType, *DictType, *IntType, *FloatType:
			// These wrapper types have a GetClass() method
			if wrapper, ok := cls.(interface{ GetClass() *core.Class }); ok {
				classObj = wrapper.GetClass()
			} else {
				return nil, fmt.Errorf("__call__() requires a class, got %T", cls)
			}
		default:
			return nil, fmt.Errorf("__call__() requires a class, got %T", cls)
		}

		// Call the class to create an instance
		// This delegates to the class's normal instantiation logic
		if callable, ok := cls.(core.Callable); ok {
			return callable.Call(ctorArgs, ctx)
		}

		// Fallback: manual instance creation using __new__ and __init__
		// 1. Call __new__ to create the instance
		newMethod, hasNew := classObj.GetMethod("__new__")
		var instance core.Value
		if hasNew {
			if newCallable, ok := newMethod.(core.Callable); ok {
				// Call __new__(cls, *args)
				newArgs := make([]core.Value, len(ctorArgs)+1)
				newArgs[0] = cls
				copy(newArgs[1:], ctorArgs)
				var err error
				instance, err = newCallable.Call(newArgs, ctx)
				if err != nil {
					return nil, err
				}
			}
		} else {
			// No __new__, create basic instance
			instance = core.NewInstance(classObj)
		}

		// 2. Call __init__ if it exists
		initMethod, hasInit := classObj.GetMethod("__init__")
		if hasInit {
			if initCallable, ok := initMethod.(core.Callable); ok {
				// Call __init__(self, *args)
				initArgs := make([]core.Value, len(ctorArgs)+1)
				initArgs[0] = instance
				copy(initArgs[1:], ctorArgs)
				_, err := initCallable.Call(initArgs, ctx)
				if err != nil {
					return nil, err
				}
			}
		}

		return instance, nil
	})
	typeCallFunc.SetAttr("__name__", core.StringValue("__call__"))
	class.SetMethod("__call__", typeCallFunc)

	// Add __new__ method to type metaclass
	// type.__new__(cls, name, bases, dict, **kwargs) creates a new class
	// Create a wrapper that supports keyword arguments
	typeNewFunc := NewKwargsBuiltinFunction("type.__new__", func(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
		// type.__new__(cls, name, bases, dict, **kwargs)
		// We need at least cls, name, bases, dict
		if len(args) < 4 {
			return nil, fmt.Errorf("type.__new__() takes at least 4 arguments (%d given)", len(args))
		}

		// args[0] is cls (the metaclass being used)
		// args[1] is name (string)
		// args[2] is bases (tuple of parent classes)
		// args[3] is dict/namespace (dict of class members)
		// kwargs contains any additional keyword arguments (e.g., metaclass params)

		nameVal, ok := args[1].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("type.__new__() argument 2 must be str, not %s", args[1].Type())
		}
		name := string(nameVal)

		basesVal, ok := args[2].(core.TupleValue)
		if !ok {
			return nil, fmt.Errorf("type.__new__() argument 3 must be tuple, not %s", args[2].Type())
		}

		namespace, ok := args[3].(*core.DictValue)
		if !ok {
			return nil, fmt.Errorf("type.__new__() argument 4 must be dict, not %s", args[3].Type())
		}

		// Extract parent classes from bases tuple
		var parentClasses []*core.Class
		for _, base := range basesVal {
			switch b := base.(type) {
			case *core.Class:
				parentClasses = append(parentClasses, b)
			case interface{ GetClass() *core.Class }:
				parentClasses = append(parentClasses, b.GetClass())
			case *core.BuiltinFunction:
				// Type constructors like object, str, etc. are callable but also valid bases
				// Try to get the class associated with this type name
				typeName := "object"
				if nameAttr, ok := b.GetAttr("__name__"); ok {
					if nameStr, ok := nameAttr.(core.StringValue); ok {
						typeName = string(nameStr)
					}
				}
				// Look up or create a class for this type
				baseClass := typeClassCache[typeName]
				if baseClass == nil {
					baseClass = core.NewClass(typeName, nil)
					baseClass.SetClassAttr("__name__", core.StringValue(typeName))
					typeClassCache[typeName] = baseClass
				}
				parentClasses = append(parentClasses, baseClass)
			default:
				return nil, fmt.Errorf("bases must be classes, not %T (name: %v)", base, base)
			}
		}

		// Create the new class
		var newClass *core.Class
		if len(parentClasses) > 1 {
			newClass = core.NewClassWithParents(name, parentClasses)
		} else if len(parentClasses) == 1 {
			newClass = core.NewClass(name, parentClasses[0])
		} else {
			newClass = core.NewClass(name, nil)
		}

		// Add methods and attributes from namespace
		for _, keyStr := range namespace.Keys() {
			value, _ := namespace.Get(keyStr)

			// Strip the "s:" prefix from string keys to get the actual attribute name
			keyName := keyStr
			if strings.HasPrefix(keyStr, "s:") {
				keyName = keyStr[2:] // Remove "s:" prefix
			}

			// Check if it's a descriptor (has __get__ method)
			// Descriptors should be stored as attributes, not methods
			isDescriptor := false
			if valueWithGetAttr, ok := value.(interface {
				GetAttr(string) (core.Value, bool)
			}); ok {
				if _, hasGet := valueWithGetAttr.GetAttr("__get__"); hasGet {
					isDescriptor = true
				}
			}

			if isDescriptor {
				// Add as attribute (descriptor)
				newClass.SetClassAttr(keyName, value)

				// Call __set_name__ if the descriptor has it (PEP 487)
				// This allows descriptors like cached_property to know their attribute name
				if valueWithGetAttr, ok := value.(interface {
					GetAttr(string) (core.Value, bool)
				}); ok {
					if setNameMethod, hasSetName := valueWithGetAttr.GetAttr("__set_name__"); hasSetName {
						if callable, ok := setNameMethod.(interface {
							Call([]core.Value, *core.Context) (core.Value, error)
						}); ok {
							// Call __set_name__(owner, name)
							// GetAttr already returns a bound method for Python instances
							_, err := callable.Call([]core.Value{newClass, core.StringValue(keyName)}, ctx)
							if err != nil {
								return nil, fmt.Errorf("error calling __set_name__ for %s: %v", keyName, err)
							}
						}
					}
				}
			} else if _, ok := value.(interface {
				Call([]core.Value, *core.Context) (core.Value, error)
			}); ok {
				// It's a callable, add as method
				newClass.SetMethod(keyName, value)
			} else {
				// Add as attribute
				newClass.SetClassAttr(keyName, value)
			}
		}

		return newClass, nil
	})
	class.SetMethod("__new__", typeNewFunc)

	return &TypeType{Class: class}
}

// createStrClass creates the str class with all string methods
func createStrClass() *StrType {
	class := core.NewClass("string", nil) // M28 uses "string" as the type name

	// Get the string type descriptor to access all string methods
	td := core.GetTypeDescriptor("string")
	if td != nil {
		// Add all methods from the type descriptor as class methods
		// Each method needs to be wrapped in a BuiltinFunction
		for name, methodDesc := range td.Methods {
			// Capture the handler in a closure
			handler := methodDesc.Handler
			// Create a BuiltinFunction that calls the handler
			// The handler expects (receiver, args, ctx) but will be called with all args
			// We need to make it an unbound method that can be called with str.method(...)
			fn := core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
				// For unbound methods, first arg should be the instance
				if len(args) < 1 {
					return nil, fmt.Errorf("%s() missing 1 required positional argument", name)
				}
				receiver := args[0]
				methodArgs := args[1:]
				return handler(receiver, methodArgs, ctx)
			})
			class.SetMethod(name, fn)
		}
	}

	// Add maketrans as a class method
	// maketrans(x[, y[, z]]) creates a translation table
	class.SetMethod("maketrans", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("maketrans", args)
		if err := v.Range(1, 3); err != nil {
			return nil, err
		}

		// Simple implementation: create a dict mapping
		table := core.NewDict()

		if v.Count() == 1 {
			// Single argument: must be a dict mapping chars to replacement strings or None
			if dict, ok := args[0].(*core.DictValue); ok {
				return dict, nil
			}
			return nil, core.NewTypeError("dict", args[0], "maketrans() argument")
		}

		// Two or three arguments
		x, okx := args[0].(core.StringValue)
		y, oky := args[1].(core.StringValue)
		if !okx || !oky {
			return nil, core.NewTypeError("string", args[0], "maketrans() arguments")
		}

		if len(x) != len(y) {
			return nil, fmt.Errorf("maketrans: the first two maketrans arguments must have equal length")
		}

		// Build the translation table
		for i, char := range string(x) {
			table.Set(string(char), core.StringValue(string([]rune(string(y))[i])))
		}

		// If there's a third argument, it's characters to delete (map to None)
		if v.Count() == 3 {
			if z, ok := args[2].(core.StringValue); ok {
				for _, char := range string(z) {
					table.Set(string(char), core.None)
				}
			}
		}

		return table, nil
	}))

	return &StrType{Class: class}
}

// IntType represents the int class that can be called and used with isinstance
type IntType struct {
	*core.Class
}

// GetClass returns the embedded Class for use as a parent class
func (i *IntType) GetClass() *core.Class {
	return i.Class
}

// Call implements the callable interface for int() construction
func (i *IntType) Call(args []core.Value, ctx *core.Context) (core.Value, error) {
	return IntBuilder()(args, ctx)
}

// CallWithKeywords delegates to Call since int() doesn't accept keyword arguments
// This prevents primitive ints from being wrapped in Instance objects
func (i *IntType) CallWithKeywords(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
	if len(kwargs) > 0 {
		return nil, fmt.Errorf("int() does not accept keyword arguments")
	}
	return i.Call(args, ctx)
}

// createIntClass creates the int class that can be used with isinstance
func createIntClass(objectClass *core.Class) *IntType {
	class := core.NewClass("int", objectClass)

	// Add __new__ method to int
	// int.__new__(cls, value=0, base=10) - creates a new int instance
	class.SetMethod("__new__", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("__new__() missing 1 required positional argument: 'cls'")
		}

		// First argument is the class - handle Class, IntType, and BoolType
		var cls *core.Class
		if c, ok := args[0].(*core.Class); ok {
			cls = c
		} else if it, ok := args[0].(*IntType); ok {
			cls = it.Class
		} else if _, ok := args[0].(*BoolType); ok {
			// Python doesn't allow int.__new__(bool, ...) - it raises TypeError
			return nil, &core.TypeError{
				Message: "int.__new__(bool) is not safe, use bool.__new__()",
			}
		} else {
			return nil, fmt.Errorf("__new__() argument 1 must be a class, not %T", args[0])
		}

		// For int subclasses, we need to handle the value argument
		// args[1] is the value (if provided)
		// args[2] is the base (if provided)

		var value core.NumberValue
		if len(args) > 1 {
			// Convert the value argument to an int
			result, err := IntBuilder()(args[1:], ctx)
			if err != nil {
				return nil, err
			}
			if num, ok := result.(core.NumberValue); ok {
				value = num
			}
		}

		// If this is exactly int, return the NumberValue directly
		if cls.Name == "int" {
			return value, nil
		}

		// For subclasses, create an instance and store the value
		instance := core.NewInstance(cls)
		instance.Attributes["__value__"] = value
		return instance, nil
	}))

	// Add __int__ method - needed for int() builtin and int subclasses
	class.SetMethod("__int__", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("__int__() takes exactly 1 argument (%d given)", len(args))
		}
		// The receiver is passed as the first argument
		receiver := args[0]

		// Handle NumberValue directly
		if n, ok := receiver.(core.NumberValue); ok {
			return core.NumberValue(float64(int(n))), nil
		}

		// Handle int subclass instances that store __value__
		if inst, ok := receiver.(*core.Instance); ok {
			if val, exists := inst.Attributes["__value__"]; exists {
				if n, ok := val.(core.NumberValue); ok {
					return core.NumberValue(float64(int(n))), nil
				}
			}
			return nil, fmt.Errorf("int subclass instance has no __value__ attribute")
		}

		return nil, fmt.Errorf("__int__() argument must be int, not %s", receiver.Type())
	}))

	// Add __index__ method - needed for slicing
	class.SetMethod("__index__", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("__index__() takes exactly 1 argument (%d given)", len(args))
		}
		receiver := args[0]

		// Handle NumberValue directly
		if n, ok := receiver.(core.NumberValue); ok {
			intVal := int(n)
			if float64(intVal) != float64(n) {
				return nil, fmt.Errorf("__index__ returned non-integer value")
			}
			return core.NumberValue(float64(n)), nil
		}

		// Handle int subclass instances that store __value__
		if inst, ok := receiver.(*core.Instance); ok {
			if val, exists := inst.Attributes["__value__"]; exists {
				if n, ok := val.(core.NumberValue); ok {
					intVal := int(n)
					if float64(intVal) != float64(n) {
						return nil, fmt.Errorf("__index__ returned non-integer value")
					}
					return core.NumberValue(float64(n)), nil
				}
			}
			return nil, fmt.Errorf("int subclass instance has no __value__ attribute")
		}

		return nil, fmt.Errorf("__index__() argument must be int, not %s", receiver.Type())
	}))

	// Add __add__ method - needed for arithmetic
	class.SetMethod("__add__", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, fmt.Errorf("__add__() takes exactly 2 arguments (%d given)", len(args))
		}
		receiver := args[0]
		other := args[1]

		// Extract numeric value from receiver
		var a float64
		if n, ok := receiver.(core.NumberValue); ok {
			a = float64(n)
		} else if inst, ok := receiver.(*core.Instance); ok {
			if val, exists := inst.Attributes["__value__"]; exists {
				if n, ok := val.(core.NumberValue); ok {
					a = float64(n)
				} else {
					return core.NotImplemented, nil
				}
			} else {
				return core.NotImplemented, nil
			}
		} else {
			return core.NotImplemented, nil
		}

		// Extract numeric value from other
		if b, ok := other.(core.NumberValue); ok {
			return core.NumberValue(a + float64(b)), nil
		} else if inst, ok := other.(*core.Instance); ok {
			if val, exists := inst.Attributes["__value__"]; exists {
				if b, ok := val.(core.NumberValue); ok {
					return core.NumberValue(a + float64(b)), nil
				}
			}
		}

		return core.NotImplemented, nil
	}))

	// Add from_bytes class method - converts bytes to int
	// int.from_bytes(bytes, byteorder, *, signed=False)
	class.SetMethod("from_bytes", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 2 {
			return nil, fmt.Errorf("int.from_bytes() missing required arguments")
		}

		// Get the bytes argument - can be bytes, str, or iterable of integers
		var bytesData []byte
		if bytesVal, ok := args[0].(core.BytesValue); ok {
			bytesData = []byte(bytesVal)
		} else if strVal, ok := args[0].(core.StringValue); ok {
			bytesData = []byte(strVal)
		} else if listVal, ok := args[0].(*core.ListValue); ok {
			// Handle list of integers (from map() or other iterables)
			items := listVal.Items()
			bytesData = make([]byte, len(items))
			for i, item := range items {
				if num, ok := item.(core.NumberValue); ok {
					bytesData[i] = byte(int(num))
				} else {
					return nil, fmt.Errorf("int.from_bytes() list elements must be integers")
				}
			}
		} else if tupleVal, ok := args[0].(core.TupleValue); ok {
			// Handle tuple of integers
			bytesData = make([]byte, len(tupleVal))
			for i, val := range tupleVal {
				if num, ok := val.(core.NumberValue); ok {
					bytesData[i] = byte(int(num))
				} else {
					return nil, fmt.Errorf("int.from_bytes() tuple elements must be integers")
				}
			}
		} else {
			return nil, fmt.Errorf("int.from_bytes() argument 1 must be bytes, str, or iterable of ints, not %s", args[0].Type())
		}

		// Get byteorder argument
		byteorder, ok := args[1].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("int.from_bytes() argument 2 must be str, not %s", args[1].Type())
		}

		// For simplicity, implement basic big-endian and little-endian conversion
		// Ignore the signed parameter for now
		var result uint64
		if string(byteorder) == "big" {
			for _, b := range bytesData {
				result = (result << 8) | uint64(b)
			}
		} else if string(byteorder) == "little" {
			for i := len(bytesData) - 1; i >= 0; i-- {
				result = (result << 8) | uint64(bytesData[i])
			}
		} else {
			return nil, fmt.Errorf("int.from_bytes() byteorder must be 'big' or 'little', not %s", byteorder)
		}

		return core.NumberValue(float64(result)), nil
	}))

	return &IntType{Class: class}
}

// BoolType represents the bool class that can be called and used with isinstance
type BoolType struct {
	*core.Class
}

// GetClass returns the embedded Class for use as a parent class
func (b *BoolType) GetClass() *core.Class {
	return b.Class
}

// Call implements the callable interface for bool() construction
func (b *BoolType) Call(args []core.Value, ctx *core.Context) (core.Value, error) {
	// Python bool() accepts at most 1 argument
	if len(args) > 1 {
		return nil, &core.TypeError{
			Message: fmt.Sprintf("bool expected at most 1 argument, got %d", len(args)),
		}
	}

	// Python bool() with no args returns False
	if len(args) == 0 {
		return core.BoolValue(false), nil
	}

	val := args[0]

	// Try __bool__ dunder method first
	if result, found, err := types.CallBool(val, ctx); found {
		if err != nil {
			return nil, err
		}
		return core.BoolValue(result), nil
	}

	// Try __len__ as fallback (if __bool__ not found)
	if length, found, err := types.CallLen(val, ctx); found {
		if err != nil {
			return nil, err
		}
		return core.BoolValue(length != 0), nil
	}

	// Fall back to IsTruthy for other types
	return core.BoolValue(core.IsTruthy(val)), nil
}

// CallWithKeywords delegates to Call since bool() doesn't accept keyword arguments
func (b *BoolType) CallWithKeywords(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
	if len(kwargs) > 0 {
		return nil, fmt.Errorf("bool() does not accept keyword arguments")
	}
	return b.Call(args, ctx)
}

// createBoolClass creates the bool class that inherits from int
func createBoolClass(intClass *IntType) *BoolType {
	class := core.NewClass("bool", intClass.Class)

	// Add __new__ method to bool
	// bool.__new__(cls, x=False) - creates a new bool instance
	// Note: In Python, bool is not actually subclassable, but we need __new__ for compatibility
	class.SetMethod("__new__", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("__new__() missing 1 required positional argument: 'cls'")
		}

		// First argument is the class
		var cls *core.Class
		if c, ok := args[0].(*core.Class); ok {
			cls = c
		} else if bt, ok := args[0].(*BoolType); ok {
			cls = bt.Class
		} else {
			return nil, fmt.Errorf("__new__() argument 1 must be a class, not %T", args[0])
		}

		// If trying to subclass bool, raise TypeError
		if cls.Name != "bool" {
			return nil, &core.TypeError{
				Message: "type 'bool' is not an acceptable base type",
			}
		}

		// Convert argument to bool
		var result core.BoolValue
		if len(args) > 1 {
			val := args[1]
			// Try __bool__ dunder method first
			if b, found, err := types.CallBool(val, ctx); found {
				if err != nil {
					return nil, err
				}
				result = core.BoolValue(b)
			} else {
				// Fall back to IsTruthy
				result = core.BoolValue(core.IsTruthy(val))
			}
		} else {
			result = core.BoolValue(false)
		}

		return result, nil
	}))

	// Add __repr__ method
	class.SetMethod("__repr__", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("__repr__() takes exactly 1 argument (%d given)", len(args))
		}
		if b, ok := args[0].(core.BoolValue); ok {
			if b {
				return core.StringValue("True"), nil
			}
			return core.StringValue("False"), nil
		}
		return nil, fmt.Errorf("__repr__() argument must be bool, not %s", args[0].Type())
	}))

	// Add __str__ method (same as __repr__ for bool)
	class.SetMethod("__str__", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("__str__() takes exactly 1 argument (%d given)", len(args))
		}
		if b, ok := args[0].(core.BoolValue); ok {
			if b {
				return core.StringValue("True"), nil
			}
			return core.StringValue("False"), nil
		}
		return nil, fmt.Errorf("__str__() argument must be bool, not %s", args[0].Type())
	}))

	return &BoolType{Class: class}
}

// FloatType represents the float class that can be called and used with isinstance
type FloatType struct {
	*core.Class
}

// GetClass returns the embedded Class for use as a parent class
func (f *FloatType) GetClass() *core.Class {
	return f.Class
}

// Call implements the callable interface for float() construction
func (f *FloatType) Call(args []core.Value, ctx *core.Context) (core.Value, error) {
	return FloatBuilder()(args, ctx)
}

// CallWithKeywords delegates to Call since float() doesn't accept keyword arguments
// This prevents primitive floats from being wrapped in Instance objects
func (f *FloatType) CallWithKeywords(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
	if len(kwargs) > 0 {
		return nil, fmt.Errorf("float() does not accept keyword arguments")
	}
	return f.Call(args, ctx)
}

// createFloatClass creates the float class that can be used with isinstance
func createFloatClass() *FloatType {
	class := core.NewClass("float", nil)

	// Add __getformat__ class method (used to detect IEEE 754 format)
	// Python's float.__getformat__("double") returns "IEEE, little-endian" or similar
	class.Methods["__getformat__"] = core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("__getformat__() takes exactly one argument")
		}

		typeStr, ok := args[0].(core.StringValue)
		if !ok {
			return nil, core.NewTypeError("str", args[0], "__getformat__() argument")
		}

		// Go uses IEEE 754 for float64
		// Return the expected format string
		formatType := string(typeStr)
		if formatType == "double" || formatType == "float" {
			return core.StringValue("IEEE, little-endian"), nil
		}

		return nil, fmt.Errorf("__getformat__() argument must be 'double' or 'float'")
	})

	return &FloatType{Class: class}
}

// Migration Statistics:
// Functions migrated: 8 type-related functions
// Original lines: ~119 lines
// Migrated lines: ~80 lines (more functionality)
// Reduction: ~33% with enhanced error handling and features
