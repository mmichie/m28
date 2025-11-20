package builtin

import (
	"fmt"
	"os"

	"github.com/mmichie/m28/core"
)

// SuperBuilder creates the super builtin function
func SuperBuilder() func([]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Python-style super() with 0 or 2 arguments
		switch len(args) {
		case 0:
			// super() with no arguments - automatically determine class and self
			// Look for __class__ and __self__ in the current context
			fmt.Fprintf(os.Stderr, "[DEBUG super()] Called with 0 arguments\n")
			selfVal, err := ctx.Lookup("self")
			if err != nil {
				return nil, fmt.Errorf("super(): no arguments given and cannot determine self")
			}

			instance, ok := selfVal.(*core.Instance)
			if !ok {
				return nil, fmt.Errorf("super(): self is not an instance")
			}
			fmt.Fprintf(os.Stderr, "[DEBUG super()] instance.Class=%s\n", instance.Class.Name)

			// Get the current method's class from context
			// This is set when a method is called
			classVal, err := ctx.Lookup("__class__")
			if err != nil {
				// Fallback: use instance's class
				// This isn't perfect but works for simple inheritance
				fmt.Fprintf(os.Stderr, "[DEBUG super()] __class__ not found, using instance.Class=%s\n", instance.Class.Name)
				return core.NewSuper(instance.Class, instance), nil
			}

			class, ok := classVal.(*core.Class)
			if !ok {
				return nil, fmt.Errorf("super(): __class__ is not a class")
			}

			fmt.Fprintf(os.Stderr, "[DEBUG super()] Found __class__=%s for instance.Class=%s\n", class.Name, instance.Class.Name)
			return core.NewSuper(class, instance), nil

		case 2:
			// super(class, instance) - explicit form
			class, ok := args[0].(*core.Class)
			if !ok {
				return nil, fmt.Errorf("super() first argument must be a class, not %s", args[0].Type())
			}

			instance, ok := args[1].(*core.Instance)
			if !ok {
				return nil, fmt.Errorf("super() second argument must be an instance, not %s", args[1].Type())
			}

			// Verify instance is an instance of class or its subclass
			if !isSubclassInstance(instance, class) {
				return nil, fmt.Errorf("super(type, obj): obj must be an instance or subtype of type")
			}

			return core.NewSuper(class, instance), nil

		default:
			return nil, fmt.Errorf("super() takes 0 or 2 arguments (%d given)", len(args))
		}
	}
}

// isSubclassInstance checks if instance is an instance of class or its subclass
func isSubclassInstance(instance *core.Instance, class *core.Class) bool {
	current := instance.Class
	for current != nil {
		if current == class {
			return true
		}
		// Check parents
		if len(current.Parents) > 0 {
			for _, parent := range current.Parents {
				if isSubclassOf(parent, class) {
					return true
				}
			}
		} else if current.Parent != nil {
			current = current.Parent
		} else {
			break
		}
	}
	return false
}

// isSubclassOf checks if child is a subclass of parent
func isSubclassOf(child, parent *core.Class) bool {
	if child == parent {
		return true
	}
	if len(child.Parents) > 0 {
		for _, p := range child.Parents {
			if isSubclassOf(p, parent) {
				return true
			}
		}
	} else if child.Parent != nil {
		return isSubclassOf(child.Parent, parent)
	}
	return false
}
