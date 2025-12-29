package core

import (
	"fmt"
)

// SlotDescriptor represents a __slots__ descriptor
// Slots are data descriptors that store values in a fixed array rather than __dict__
type SlotDescriptor struct {
	BaseObject
	Name      string // Name of the slot
	SlotIndex int    // Index in the instance's slot values array
}

// NewSlotDescriptor creates a new slot descriptor
func NewSlotDescriptor(name string, index int) *SlotDescriptor {
	return &SlotDescriptor{
		BaseObject: *NewBaseObject(Type("member_descriptor")),
		Name:       name,
		SlotIndex:  index,
	}
}

// Type returns the descriptor type
func (sd *SlotDescriptor) Type() Type {
	return Type("member_descriptor")
}

// String returns the string representation
func (sd *SlotDescriptor) String() string {
	return fmt.Sprintf("<member '%s'>", sd.Name)
}

// GetAttr implements the descriptor __get__ method
func (sd *SlotDescriptor) GetAttr(name string) (Value, bool) {
	switch name {
	case "__get__":
		// Return a callable that implements __get__(instance, owner)
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if len(args) < 1 || len(args) > 2 {
				return nil, fmt.Errorf("__get__ requires 1 or 2 arguments")
			}

			instance := args[0]

			// If called on the class (instance is None), return the descriptor itself
			if instance == None {
				return sd, nil
			}

			// Get the slot value from the instance
			if inst, ok := instance.(*Instance); ok {
				if inst.SlotValues != nil && sd.SlotIndex < len(inst.SlotValues) {
					value := inst.SlotValues[sd.SlotIndex]
					if value == nil {
						// Slot is not set - raise AttributeError
						return nil, &AttributeError{
							ObjType:  inst.Class.Name,
							AttrName: sd.Name,
						}
					}
					return value, nil
				}
				return nil, fmt.Errorf("slot index out of range")
			}

			return nil, fmt.Errorf("descriptor '__get__' requires an instance")
		}), true
	case "__set__":
		// Return a callable that implements __set__(instance, value)
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if len(args) != 2 {
				return nil, fmt.Errorf("__set__ requires exactly 2 arguments")
			}

			instance := args[0]
			value := args[1]

			// Set the slot value on the instance
			if inst, ok := instance.(*Instance); ok {
				if inst.SlotValues == nil {
					return nil, fmt.Errorf("'%s' object has no slots", inst.Class.Name)
				}
				if sd.SlotIndex >= len(inst.SlotValues) {
					return nil, fmt.Errorf("slot index out of range (index %d, len %d)", sd.SlotIndex, len(inst.SlotValues))
				}
				inst.SlotValues[sd.SlotIndex] = value
				return None, nil
			}

			return nil, fmt.Errorf("descriptor '__set__' requires an instance")
		}), true
	case "__delete__":
		// Return a callable that implements __delete__(instance)
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("__delete__ requires exactly 1 argument")
			}

			instance := args[0]

			// Delete the slot value from the instance (set to nil)
			if inst, ok := instance.(*Instance); ok {
				if inst.SlotValues == nil {
					return nil, fmt.Errorf("'%s' object has no slots", inst.Class.Name)
				}
				if sd.SlotIndex >= len(inst.SlotValues) {
					return nil, fmt.Errorf("slot index out of range")
				}
				inst.SlotValues[sd.SlotIndex] = nil
				return None, nil
			}

			return nil, fmt.Errorf("descriptor '__delete__' requires an instance")
		}), true
	case "__name__":
		return StringValue(sd.Name), true
	case "__objclass__":
		// This should return the class that owns this descriptor
		// For now, return None - we'd need to store a reference to the owning class
		return None, true
	}

	return sd.BaseObject.GetAttr(name)
}

// SetupSlots sets up slot descriptors for a class based on its __slots__ attribute
// Handles slot inheritance from parent classes
func SetupSlots(class *Class, slotsValue Value) error {

	// Start with parent class slots (if any)
	var allSlotNames []string
	var startIndex int

	// Inherit slots from parent classes
	if len(class.Parents) > 0 {
		parent := class.Parents[0] // Use first parent
		if parent.HasSlots() {
			// Inherit parent slots
			allSlotNames = make([]string, len(parent.SlotNames))
			copy(allSlotNames, parent.SlotNames)
			startIndex = len(allSlotNames)
		}
	}

	// Extract new slot names from this class's __slots__ attribute
	var newSlotNames []string

	switch v := slotsValue.(type) {
	case TupleValue:
		// __slots__ is a tuple
		for _, item := range v {
			if str, ok := item.(StringValue); ok {
				newSlotNames = append(newSlotNames, string(str))
			} else {
				return fmt.Errorf("__slots__ items must be strings, got %s", item.Type())
			}
		}
	case *ListValue:
		// __slots__ is a list
		for _, item := range v.Items() {
			if str, ok := item.(StringValue); ok {
				newSlotNames = append(newSlotNames, string(str))
			} else {
				return fmt.Errorf("__slots__ items must be strings, got %s", item.Type())
			}
		}
	case StringValue:
		// __slots__ is a single string
		newSlotNames = []string{string(v)}
	default:
		return fmt.Errorf("__slots__ must be a sequence of strings, got %s", slotsValue.Type())
	}

	// Add new slots to the inherited list
	allSlotNames = append(allSlotNames, newSlotNames...)

	// Create slot descriptors for the NEW slots only (parent slots already have descriptors)
	for i, name := range newSlotNames {
		descriptor := NewSlotDescriptor(name, startIndex+i)
		class.SetClassAttr(name, descriptor)
	}

	// Store ALL slot names on the class for later use (inherited + new)
	class.SlotNames = allSlotNames

	return nil
}
