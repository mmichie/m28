package builtin

import (
	"fmt"
	"reflect"

	"github.com/mmichie/m28/core"
)

func RegisterTypeBuiltins() {
	core.RegisterBuiltin("callable", callableFunc)
	core.RegisterBuiltin("isinstance", isinstanceFunc)
	core.RegisterBuiltin("issubclass", issubclassFunc)
	core.RegisterBuiltin("type", typeFunc)
	core.RegisterBuiltin("is", isFunc)
}

func callableFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("callable() takes exactly one argument")
	}
	_, ok := args[0].(*core.Lambda)
	return core.PythonicBool(ok), nil
}

func isinstanceFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("isinstance() takes exactly 2 arguments (object, class)")
	}

	obj := args[0]
	classArg := args[1]

	// Handle string type names
	if typeName, isString := classArg.(string); isString {
		switch typeName {
		case "int":
			// Check if it's a number that's an integer
			if num, isNum := obj.(float64); isNum {
				return core.PythonicBool(num == float64(int(num))), nil
			}
			return core.PythonicBool(false), nil
		case "float":
			_, isFloat := obj.(float64)
			return core.PythonicBool(isFloat), nil
		case "str":
			_, isStr := obj.(string)
			return core.PythonicBool(isStr), nil
		case "list":
			_, isList := obj.(core.LispList)
			return core.PythonicBool(isList), nil
		case "tuple":
			_, isTuple := obj.(core.LispTuple)
			return core.PythonicBool(isTuple), nil
		case "dict":
			_, isDict := obj.(*core.PythonicDict)
			return core.PythonicBool(isDict), nil
		case "bool":
			_, isBool := obj.(core.PythonicBool)
			return core.PythonicBool(isBool), nil
		case "function":
			_, isFunc := obj.(*core.Lambda)
			return core.PythonicBool(isFunc), nil
		case "none":
			_, isNone := obj.(core.PythonicNone)
			return core.PythonicBool(isNone), nil
		}
	}

	// Handle list of classes - isinstance(obj, [class1, class2, ...])
	if classes, isList := classArg.(core.LispList); isList {
		for _, class := range classes {
			result, err := checkInstanceOf(obj, class, env)
			if err != nil {
				return nil, err
			}
			if result == core.PythonicBool(true) {
				return core.PythonicBool(true), nil
			}
		}
		return core.PythonicBool(false), nil
	}

	// Handle list literal of classes - isinstance(obj, [class1, class2, ...])
	if classes, isListLit := classArg.(core.LispListLiteral); isListLit {
		for _, class := range classes {
			result, err := checkInstanceOf(obj, class, env)
			if err != nil {
				return nil, err
			}
			if result == core.PythonicBool(true) {
				return core.PythonicBool(true), nil
			}
		}
		return core.PythonicBool(false), nil
	}

	// Handle tuple of classes - isinstance(obj, (class1, class2, ...))
	if classes, isTuple := classArg.(core.LispTuple); isTuple {
		for _, class := range classes {
			result, err := checkInstanceOf(obj, class, env)
			if err != nil {
				return nil, err
			}
			if result == core.PythonicBool(true) {
				return core.PythonicBool(true), nil
			}
		}
		return core.PythonicBool(false), nil
	}

	// Single class check
	return checkInstanceOf(obj, classArg, env)
}

// checkInstanceOf checks if obj is an instance of the given class
func checkInstanceOf(obj, class core.LispValue, env core.Environment) (core.LispValue, error) {
	// Handle built-in types
	if typeId, isTypeId := class.(core.TypeIdentifier); isTypeId {
		switch typeId {
		case core.TypeIdentifier("int"):
			// Check if it's a number that's an integer
			if num, isNum := obj.(float64); isNum {
				return core.PythonicBool(num == float64(int(num))), nil
			}
			return core.PythonicBool(false), nil
		case core.TypeIdentifier("float"):
			_, isFloat := obj.(float64)
			return core.PythonicBool(isFloat), nil
		case core.TypeIdentifier("str"):
			_, isStr := obj.(string)
			return core.PythonicBool(isStr), nil
		case core.TypeIdentifier("list"):
			_, isList := obj.(core.LispList)
			return core.PythonicBool(isList), nil
		case core.TypeIdentifier("tuple"):
			_, isTuple := obj.(core.LispTuple)
			return core.PythonicBool(isTuple), nil
		case core.TypeIdentifier("dict"):
			_, isDict := obj.(*core.PythonicDict)
			return core.PythonicBool(isDict), nil
		case core.TypeIdentifier("bool"):
			_, isBool := obj.(core.PythonicBool)
			return core.PythonicBool(isBool), nil
		case core.TypeIdentifier("function"):
			_, isFunc := obj.(*core.Lambda)
			return core.PythonicBool(isFunc), nil
		case core.TypeIdentifier("none"):
			_, isNone := obj.(core.PythonicNone)
			return core.PythonicBool(isNone), nil
		}
	}

	// Handle string type names
	if typeName, isString := class.(string); isString {
		switch typeName {
		case "int":
			// Check if it's a number that's an integer
			if num, isNum := obj.(float64); isNum {
				return core.PythonicBool(num == float64(int(num))), nil
			}
			return core.PythonicBool(false), nil
		case "float":
			_, isFloat := obj.(float64)
			return core.PythonicBool(isFloat), nil
		case "str":
			_, isStr := obj.(string)
			return core.PythonicBool(isStr), nil
		case "list":
			_, isList := obj.(core.LispList)
			return core.PythonicBool(isList), nil
		case "tuple":
			_, isTuple := obj.(core.LispTuple)
			return core.PythonicBool(isTuple), nil
		case "dict":
			_, isDict := obj.(*core.PythonicDict)
			return core.PythonicBool(isDict), nil
		case "bool":
			_, isBool := obj.(core.PythonicBool)
			return core.PythonicBool(isBool), nil
		case "function":
			_, isFunc := obj.(*core.Lambda)
			return core.PythonicBool(isFunc), nil
		case "none":
			_, isNone := obj.(core.PythonicNone)
			return core.PythonicBool(isNone), nil
		}
	}

	// Handle custom classes
	objInstance, isObj := obj.(*core.PythonicObject)
	if !isObj {
		return core.PythonicBool(false), nil
	}

	classObj, isClass := class.(*core.PythonicClass)
	if !isClass {
		return nil, fmt.Errorf("isinstance() second argument must be a type or tuple of types")
	}

	// Check if the object's class is the given class or any of its parent classes
	return core.PythonicBool(isInstanceOfClass(objInstance.Class, classObj)), nil
}

// isInstanceOfClass checks if a class is the same as or a subclass of another class
func isInstanceOfClass(objClass, checkClass *core.PythonicClass) bool {
	if objClass == checkClass {
		return true
	}

	// Check all parent classes
	for _, parent := range objClass.Parents {
		if isInstanceOfClass(parent, checkClass) {
			return true
		}
	}

	return false
}

func issubclassFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("issubclass() takes exactly 2 arguments (class, classinfo)")
	}

	// Get the class argument
	class, isClass := args[0].(*core.PythonicClass)
	if !isClass {
		// Handle string type names
		if typeName, isString := args[0].(string); isString {
			// For built-in types, the only valid issubclass relationship is with themselves
			if typeNameB, isStringB := args[1].(string); isStringB {
				return core.PythonicBool(typeName == typeNameB), nil
			}

			// Handle tuple of types
			if classesB, isListB := args[1].(core.LispList); isListB {
				for _, cls := range classesB {
					if typeNameB, isStringB := cls.(string); isStringB && typeName == typeNameB {
						return core.PythonicBool(true), nil
					}
				}
			}
			return core.PythonicBool(false), nil
		}

		return nil, fmt.Errorf("issubclass() first argument must be a class")
	}

	// Handle list of classes for second argument
	if classes, isList := args[1].(core.LispList); isList {
		for _, cls := range classes {
			// Check if the class item is a PythonicClass
			checkClass, isCheckClass := cls.(*core.PythonicClass)
			if isCheckClass {
				if isSubclassOf(class, checkClass) {
					return core.PythonicBool(true), nil
				}
				continue
			}

			// Custom classes cannot be subclasses of built-in types represented as strings
			_, isString := cls.(string)
			if isString {
				continue
			}
		}
		return core.PythonicBool(false), nil
	}

	// Handle tuple of classes for second argument (Python's tuple)
	if classes, isTuple := args[1].(core.LispTuple); isTuple {
		for _, cls := range classes {
			// Check if the class item is a PythonicClass
			checkClass, isCheckClass := cls.(*core.PythonicClass)
			if isCheckClass {
				if isSubclassOf(class, checkClass) {
					return core.PythonicBool(true), nil
				}
				continue
			}

			// Custom classes cannot be subclasses of built-in types represented as strings
			_, isString := cls.(string)
			if isString {
				continue
			}
		}
		return core.PythonicBool(false), nil
	}

	// Check if second argument is a string type name
	_, isString := args[1].(string)
	if isString {
		// Custom classes cannot be subclasses of built-in types
		return core.PythonicBool(false), nil
	}

	// Single class check
	checkClass, isCheckClass := args[1].(*core.PythonicClass)
	if !isCheckClass {
		return nil, fmt.Errorf("issubclass() second argument must be a class, type name, or list of classes")
	}

	return core.PythonicBool(isSubclassOf(class, checkClass)), nil
}

// isSubclassOf checks if a class is a subclass of another class
func isSubclassOf(class, checkClass *core.PythonicClass) bool {
	if class == nil || checkClass == nil {
		return false
	}

	// Direct equality check
	if class == checkClass {
		return true
	}

	// Check all parent classes recursively
	for _, parent := range class.Parents {
		if parent == checkClass {
			return true
		}

		if isSubclassOf(parent, checkClass) {
			return true
		}
	}

	return false
}

func typeFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("type() takes exactly one argument")
	}

	// Special handling for tuples
	if _, isTuple := args[0].(core.LispTuple); isTuple {
		return "tuple", nil
	}

	// Special handling for lists
	if _, isList := args[0].(core.LispList); isList {
		return "list", nil
	}

	// Special handling for dictionaries
	if _, isDict := args[0].(*core.PythonicDict); isDict {
		return "dict", nil
	}

	// Special handling for sets
	if _, isSet := args[0].(*core.PythonicSet); isSet {
		return "set", nil
	}

	// Special handling for numbers - distinguish between int and float
	if num, isNum := args[0].(float64); isNum {
		// Check if it's an integer (no decimal part)
		if num == float64(int(num)) {
			return "int", nil
		}
		return "float", nil
	}

	// Default to Go's type system
	typeName := reflect.TypeOf(args[0]).String()

	// Map Go types to Python-like type names
	switch typeName {
	case "string":
		return "str", nil
	case "core.PythonicBool":
		return "bool", nil
	case "core.PythonicNone":
		return "none", nil
	case "*core.Lambda":
		return "function", nil
	}

	return typeName, nil
}

func isFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("is() takes exactly two arguments")
	}

	// Direct identity comparison
	return core.PythonicBool(core.EqValues(args[0], args[1])), nil
}
