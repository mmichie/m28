package protocols

import (
	"strings"

	"github.com/mmichie/m28/core"
)

// ContainerOps provides common container operations
type ContainerOps struct {
	getValue    func() core.Value
	getSize     func() int
	getContains func(core.Value) bool
}

// StringContainer adapts StringValue to Container protocol
type StringContainer struct {
	value string
}

// NewStringContainer creates a Container adapter for strings
func NewStringContainer(s core.StringValue) Container {
	return &StringContainer{value: string(s)}
}

// Size returns the length of the string
func (s *StringContainer) Size() int {
	return len(s.value)
}

// Contains checks if the string contains a substring
func (s *StringContainer) Contains(item core.Value) bool {
	if substr, ok := item.(core.StringValue); ok {
		return strings.Contains(s.value, string(substr))
	}
	return false
}

// IsEmpty checks if the string is empty
func (s *StringContainer) IsEmpty() bool {
	return len(s.value) == 0
}

// ListContainer adapts ListValue to Container protocol
type ListContainer struct {
	list *core.ListValue
}

// NewListContainer creates a Container adapter for lists
func NewListContainer(list *core.ListValue) Container {
	return &ListContainer{list: list}
}

// Size returns the number of elements
func (l *ListContainer) Size() int {
	return l.list.Len()
}

// Contains checks if the list contains an item
func (l *ListContainer) Contains(item core.Value) bool {
	for _, elem := range l.list.Items() {
		if core.EqualValues(elem, item) {
			return true
		}
	}
	return false
}

// IsEmpty checks if the list is empty
func (l *ListContainer) IsEmpty() bool {
	return l.list.Len() == 0
}

// DictContainer adapts DictValue to Container protocol
type DictContainer struct {
	dict *core.DictValue
}

// NewDictContainer creates a Container adapter for dicts
func NewDictContainer(dict *core.DictValue) Container {
	return &DictContainer{dict: dict}
}

// Size returns the number of key-value pairs
func (d *DictContainer) Size() int {
	return d.dict.Size()
}

// Contains checks if the dict contains a key
func (d *DictContainer) Contains(item core.Value) bool {
	if !core.IsHashable(item) {
		return false
	}
	_, exists := d.dict.GetValue(item)
	return exists
}

// IsEmpty checks if the dict is empty
func (d *DictContainer) IsEmpty() bool {
	return d.dict.Size() == 0
}

// SetContainer adapts SetValue to Container protocol
type SetContainer struct {
	set *core.SetValue
}

// NewSetContainer creates a Container adapter for sets
func NewSetContainer(set *core.SetValue) Container {
	return &SetContainer{set: set}
}

// Size returns the number of elements
func (s *SetContainer) Size() int {
	return s.set.Size()
}

// Contains checks if the set contains an item
func (s *SetContainer) Contains(item core.Value) bool {
	return s.set.Contains(item)
}

// IsEmpty checks if the set is empty
func (s *SetContainer) IsEmpty() bool {
	return s.set.Size() == 0
}

// TupleContainer adapts TupleValue to Container protocol
type TupleContainer struct {
	tuple core.TupleValue
}

// NewTupleContainer creates a Container adapter for tuples
func NewTupleContainer(tuple core.TupleValue) Container {
	return &TupleContainer{tuple: tuple}
}

// Size returns the number of elements
func (t *TupleContainer) Size() int {
	return len(t.tuple)
}

// Contains checks if the tuple contains an item
func (t *TupleContainer) Contains(item core.Value) bool {
	for _, elem := range t.tuple {
		if core.EqualValues(elem, item) {
			return true
		}
	}
	return false
}

// IsEmpty checks if the tuple is empty
func (t *TupleContainer) IsEmpty() bool {
	return len(t.tuple) == 0
}

// InstanceContainer adapts an Instance with __len__/__contains__ to Container protocol
type InstanceContainer struct {
	instance       *core.Instance
	lenMethod      core.Value
	hasContains    bool
	containsMethod core.Value
}

// NewInstanceContainer creates a Container adapter for instances with container methods
// Returns nil if the instance doesn't have __len__
func NewInstanceContainer(inst *core.Instance) Container {
	// Must have __len__ to be a container
	lenMethod, hasLen := inst.GetAttr("__len__")
	if !hasLen {
		return nil
	}

	containsMethod, hasContains := inst.GetAttr("__contains__")

	return &InstanceContainer{
		instance:       inst,
		lenMethod:      lenMethod,
		hasContains:    hasContains,
		containsMethod: containsMethod,
	}
}

// Size returns the result of calling __len__
func (ic *InstanceContainer) Size() int {
	if callable, ok := ic.lenMethod.(interface {
		Call([]core.Value, *core.Context) (core.Value, error)
	}); ok {
		result, err := callable.Call([]core.Value{}, nil)
		if err == nil {
			if num, ok := result.(core.NumberValue); ok {
				return int(num)
			}
		}
	}
	return 0
}

// Contains checks if the container contains an item using __contains__
func (ic *InstanceContainer) Contains(item core.Value) bool {
	if !ic.hasContains {
		// If no __contains__, fall back to iteration (not implemented here)
		return false
	}

	if callable, ok := ic.containsMethod.(interface {
		Call([]core.Value, *core.Context) (core.Value, error)
	}); ok {
		result, err := callable.Call([]core.Value{item}, nil)
		if err == nil {
			return core.IsTruthy(result)
		}
	}
	return false
}

// IsEmpty checks if the container is empty
func (ic *InstanceContainer) IsEmpty() bool {
	return ic.Size() == 0
}

// GetContainer returns a Container implementation for a value if possible
func GetContainer(v core.Value) (Container, bool) {
	switch val := v.(type) {
	case core.StringValue:
		return NewStringContainer(val), true
	case *core.ListValue:
		return NewListContainer(val), true
	case *core.DictValue:
		return NewDictContainer(val), true
	case *core.SetValue:
		return NewSetContainer(val), true
	case core.TupleValue:
		return NewTupleContainer(val), true
	case *core.Instance:
		// Check for __len__ method on the instance
		if container := NewInstanceContainer(val); container != nil {
			return container, true
		}
		return nil, false
	default:
		// Check if value implements Container directly
		if container, ok := v.(Container); ok {
			return container, true
		}
		return nil, false
	}
}
