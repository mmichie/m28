package core

import "fmt"

// registerClassType registers the class type descriptor
func registerClassType() {
	RegisterType(&TypeDescriptor{
		Name:       "class",
		PythonName: "type",
		BaseType:   Type("class"),
		Methods: map[string]*MethodDescriptor{
			"__call__": {
				Name:    "__call__",
				Arity:   -1,
				Doc:     "Call the class to create an instance",
				Builtin: true,
			},
			"__instancecheck__": {
				Name:    "__instancecheck__",
				Arity:   1,
				Doc:     "Check if an object is an instance of this class",
				Builtin: true,
			},
			"__subclasscheck__": {
				Name:    "__subclasscheck__",
				Arity:   1,
				Doc:     "Check if a class is a subclass of this class",
				Builtin: true,
			},
		},
		Properties: map[string]*PropertyDescriptor{
			"__name__": {
				Name: "__name__",
				Doc:  "Class name",
			},
			"__bases__": {
				Name: "__bases__",
				Doc:  "Base classes",
			},
			"__dict__": {
				Name: "__dict__",
				Doc:  "Class namespace",
			},
		},
	})
}

// registerModuleType registers the module type descriptor
func registerModuleType() {
	RegisterType(&TypeDescriptor{
		Name:       "module",
		PythonName: "module",
		BaseType:   Type("module"),
		Methods:    map[string]*MethodDescriptor{},
		Properties: map[string]*PropertyDescriptor{
			"__name__": {
				Name: "__name__",
				Doc:  "Module name",
			},
			"__file__": {
				Name: "__file__",
				Doc:  "Module file path",
			},
			"__dict__": {
				Name: "__dict__",
				Doc:  "Module namespace",
			},
			"__doc__": {
				Name: "__doc__",
				Doc:  "Module documentation",
			},
		},
		Constructor: func(args []Value, ctx *Context) (Value, error) {
			if len(args) < 1 {
				return nil, fmt.Errorf("module() missing required argument 'name' (pos 1)")
			}

			name, ok := args[0].(StringValue)
			if !ok {
				return nil, fmt.Errorf("module() argument 'name' must be str")
			}

			path := ""
			if len(args) > 1 {
				if p, ok := args[1].(StringValue); ok {
					path = string(p)
				}
			}

			return NewModule(string(name), path), nil
		},
	})
}
