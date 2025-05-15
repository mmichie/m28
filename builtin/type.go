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
	if len(args) < 2 {
		return nil, fmt.Errorf("isinstance() takes at least 2 arguments (object, class)")
	}

	obj := args[0]
	classArg := args[1]

	// Handle list of classes - isinstance(obj, [class1, class2, ...])
	if classes, isList := classArg.(core.LispList); isList {
		for _, class := range classes {
			// Evaluate the class item if needed
			var classItem core.LispValue = class
			if eval, ok := class.(core.Evaluable); ok {
				var err error
				classItem, err = eval.Eval(nil, env)
				if err != nil {
					return nil, fmt.Errorf("error evaluating class in list: %v", err)
				}
			}
			
			result, err := checkInstanceOf(obj, classItem, env)
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
	if len(args) < 2 {
		return nil, fmt.Errorf("issubclass() takes at least 2 arguments (class, classinfo)")
	}

	// Get the class argument
	class, isClass := args[0].(*core.PythonicClass)
	if !isClass {
		// Check if it's a type identifier
		typeId, isTypeId := args[0].(core.TypeIdentifier)
		if !isTypeId {
			return nil, fmt.Errorf("issubclass() first argument must be a class")
		}
		
		// Handle built-in type identifiers for first argument
		if typeId == core.TypeIdentifier("int") ||
		   typeId == core.TypeIdentifier("float") ||
		   typeId == core.TypeIdentifier("str") ||
		   typeId == core.TypeIdentifier("list") ||
		   typeId == core.TypeIdentifier("tuple") ||
		   typeId == core.TypeIdentifier("dict") ||
		   typeId == core.TypeIdentifier("bool") ||
		   typeId == core.TypeIdentifier("function") ||
		   typeId == core.TypeIdentifier("none") {
			
			// For built-in types, the only valid issubclass relationship is with themselves
			typeId2, isTypeId2 := args[1].(core.TypeIdentifier)
			if isTypeId2 && typeId == typeId2 {
				return core.PythonicBool(true), nil
			}
			
			// Handle list of classes for second argument (Python's tuple equivalent)
			if classes, isList := args[1].(core.LispList); isList {
				for _, cls := range classes {
					typeId2, isTypeId2 := cls.(core.TypeIdentifier)
					if isTypeId2 && typeId == typeId2 {
						return core.PythonicBool(true), nil
					}
				}
			}
			
			return core.PythonicBool(false), nil
		}
	}

	// Handle list of classes for second argument (Python's tuple equivalent)
	if classes, isList := args[1].(core.LispList); isList {
		for _, cls := range classes {
			// Evaluate the class item if needed
			var classItem core.LispValue = cls
			if eval, ok := cls.(core.Evaluable); ok {
				var err error
				classItem, err = eval.Eval(nil, env)
				if err != nil {
					return nil, fmt.Errorf("error evaluating class in list: %v", err)
				}
			}

			// Check if the class item is a PythonicClass
			checkClass, isCheckClass := classItem.(*core.PythonicClass)
			if isCheckClass {
				if isSubclassOf(class, checkClass) {
					return core.PythonicBool(true), nil
				}
				continue
			}
			
			// Check if the class item is a TypeIdentifier
			_, isTypeId := classItem.(core.TypeIdentifier)
			if !isTypeId {
				return nil, fmt.Errorf("issubclass() second argument list must contain only classes or type identifiers")
			}
			
			// Built-in types cannot be base classes of custom classes
			continue
		}
		return core.PythonicBool(false), nil
	}

	// Check if second argument is a TypeIdentifier
	_, isTypeId := args[1].(core.TypeIdentifier)
	if isTypeId {
		// Custom classes cannot be subclasses of built-in types
		return core.PythonicBool(false), nil
	}

	// Single class check
	checkClass, isCheckClass := args[1].(*core.PythonicClass)
	if !isCheckClass {
		return nil, fmt.Errorf("issubclass() second argument must be a class, type identifier, or list of classes")
	}

	return core.PythonicBool(isSubclassOf(class, checkClass)), nil
}

// isSubclassOf checks if a class is a subclass of another class
func isSubclassOf(class, checkClass *core.PythonicClass) bool {
	fmt.Printf("DEBUG isSubclassOf: Checking if %s is a subclass of %s\n", class.Name, checkClass.Name)
	
	if class == nil || checkClass == nil {
		fmt.Println("DEBUG isSubclassOf: One of the classes is nil")
		return false
	}

	// Direct equality check
	if class == checkClass {
		fmt.Println("DEBUG isSubclassOf: Direct match - classes are the same")
		return true
	}

	// Print parent classes for debugging
	fmt.Printf("DEBUG isSubclassOf: %s has %d parents: ", class.Name, len(class.Parents))
	for i, parent := range class.Parents {
		if parent == nil {
			fmt.Print("nil ")
		} else {
			fmt.Printf("%s ", parent.Name)
		}
		if i < len(class.Parents)-1 {
			fmt.Print(", ")
		}
	}
	fmt.Println()

	// Check all parent classes recursively
	for _, parent := range class.Parents {
		if parent == checkClass {
			fmt.Printf("DEBUG isSubclassOf: Found direct parent match: %s is a parent of %s\n", 
				checkClass.Name, class.Name)
			return true
		}
		
		if isSubclassOf(parent, checkClass) {
			fmt.Printf("DEBUG isSubclassOf: Found indirect parent match: %s is an ancestor of %s\n",
				checkClass.Name, class.Name)
			return true
		}
	}

	fmt.Printf("DEBUG isSubclassOf: No match found - %s is NOT a subclass of %s\n", 
		class.Name, checkClass.Name)
	return false
}

func typeFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("type() takes exactly one argument")
	}
	return reflect.TypeOf(args[0]).String(), nil
}

func isFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("is() takes exactly two arguments")
	}

	value := args[0]

	// Check if second argument is a function call that returns a type
	typeFunc, isList := args[1].(core.LispList)
	if isList && len(typeFunc) > 0 {
		if typeFunc[0] == core.LispSymbol("int") {
			_, isInt := value.(float64)
			return core.PythonicBool(isInt), nil
		} else if typeFunc[0] == core.LispSymbol("float") {
			_, isFloat := value.(float64)
			return core.PythonicBool(isFloat), nil
		} else if typeFunc[0] == core.LispSymbol("str") {
			_, isStr := value.(string)
			return core.PythonicBool(isStr), nil
		} else if typeFunc[0] == core.LispSymbol("list") {
			_, isList := value.(core.LispList)
			return core.PythonicBool(isList), nil
		} else if typeFunc[0] == core.LispSymbol("dict") {
			_, isDict := value.(*core.PythonicDict)
			return core.PythonicBool(isDict), nil
		} else if typeFunc[0] == core.LispSymbol("bool") {
			_, isBool := value.(core.PythonicBool)
			return core.PythonicBool(isBool), nil
		}
	}

	// Direct identity comparison
	return core.PythonicBool(core.EqValues(value, args[1])), nil
}
