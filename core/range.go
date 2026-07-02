package core

import (
	"fmt"
	"math"
)

// RangeValue represents a lazy range object like Python's range
type RangeValue struct {
	BaseObject
	Start    float64
	Stop     float64
	Step     float64
	registry *MethodRegistry
}

// NewRangeValue creates a new range value
func NewRangeValue(start, stop, step float64) (*RangeValue, error) {
	if step == 0 {
		return nil, fmt.Errorf("range() arg 3 must not be zero")
	}

	r := &RangeValue{
		BaseObject: *NewBaseObject(RangeType),
		Start:      start,
		Stop:       stop,
		Step:       step,
	}

	// Initialize the method registry
	r.registry = r.createRegistry()

	return r, nil
}

// Type implements Value.Type
func (r *RangeValue) Type() Type {
	return RangeType
}

// String implements Value.String
func (r *RangeValue) String() string {
	if r.Step == 1 {
		if r.Start == 0 {
			return fmt.Sprintf("range(%v)", r.Stop)
		}
		return fmt.Sprintf("range(%v, %v)", r.Start, r.Stop)
	}
	return fmt.Sprintf("range(%v, %v, %v)", r.Start, r.Stop, r.Step)
}

// Length returns the number of elements in the range
func (r *RangeValue) Length() int {
	if r.Step > 0 && r.Start >= r.Stop {
		return 0
	}
	if r.Step < 0 && r.Start <= r.Stop {
		return 0
	}

	// Calculate length based on start, stop, and step
	length := math.Ceil((r.Stop - r.Start) / r.Step)
	if length < 0 {
		return 0
	}
	return int(length)
}

// GetItem returns the item at the given index
func (r *RangeValue) GetItem(index int) (Value, error) {
	length := r.Length()

	// Handle negative indices
	if index < 0 {
		index = length + index
	}

	if index < 0 || index >= length {
		return nil, &IndexError{Index: index, Length: length}
	}

	// Calculate the value at the index
	value := r.Start + float64(index)*r.Step
	return BoxNumber(value), nil
}

// Contains checks if a value is in the range
func (r *RangeValue) Contains(value Value) bool {
	num, ok := value.(NumberValue)
	if !ok {
		return false
	}

	n := float64(num)

	// Check if n is within bounds
	if r.Step > 0 {
		if n < r.Start || n >= r.Stop {
			return false
		}
	} else {
		if n > r.Start || n <= r.Stop {
			return false
		}
	}

	// Check if n is reachable with the given step
	// (n - start) must be divisible by step
	diff := n - r.Start
	if math.Mod(diff, r.Step) != 0 {
		return false
	}

	return true
}

// Iterator implements Iterable.Iterator
func (r *RangeValue) Iterator() Iterator {
	return &rangeIterator{
		rang:    r,
		current: r.Start,
	}
}

// Implement the Iterable interface check
var _ Iterable = (*RangeValue)(nil)

// rangeIterator implements Iterator for RangeValue
type rangeIterator struct {
	rang    *RangeValue
	current float64
}

// iteratorState tracks the state when RangeValue acts as its own iterator
type rangeIteratorState struct {
	current float64
	started bool
}

// Next implements Iterator.Next
func (it *rangeIterator) Next() (Value, bool) {
	if it.rang.Step > 0 {
		if it.current >= it.rang.Stop {
			return nil, false
		}
	} else {
		if it.current <= it.rang.Stop {
			return nil, false
		}
	}

	// BoxNumber reuses the shared small-int box: loop counters are the
	// single hottest boxing site in the interpreter.
	val := BoxNumber(it.current)
	it.current += it.rang.Step
	return val, true
}

// Reset implements Iterator.Reset
func (it *rangeIterator) Reset() {
	it.current = it.rang.Start
}

// Type implements Value.Type for range iterator
func (it *rangeIterator) Type() Type {
	return "range_iterator"
}

// String implements Value.String for range iterator
func (it *rangeIterator) String() string {
	return "<range_iterator>"
}

// GetAttr implements Object interface for range iterator protocol
func (it *rangeIterator) GetAttr(name string) (Value, bool) {
	if name == "__iter__" {
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			return it, nil
		}), true
	}
	if name == "__next__" {
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			val, ok := it.Next()
			if !ok {
				return nil, &StopIteration{}
			}
			return val, nil
		}), true
	}
	if name == "__length_hint__" {
		// PEP 424: Return estimated remaining length
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			var remaining int
			if it.rang.Step > 0 {
				remaining = int((it.rang.Stop - it.current + it.rang.Step - 1) / it.rang.Step)
			} else {
				remaining = int((it.current - it.rang.Stop - it.rang.Step - 1) / (-it.rang.Step))
			}
			if remaining < 0 {
				remaining = 0
			}
			return NumberValue(remaining), nil
		}), true
	}
	return nil, false
}

// createRegistry sets up all methods and properties for range
func (r *RangeValue) createRegistry() *MethodRegistry {
	registry := NewMethodRegistry()

	// Register properties
	registry.RegisterProperties(
		MakeProperty("start", "Start value of the range", func(receiver Value) (Value, error) {
			r := receiver.(*RangeValue)
			return NumberValue(r.Start), nil
		}),
		MakeProperty("stop", "Stop value of the range", func(receiver Value) (Value, error) {
			r := receiver.(*RangeValue)
			return NumberValue(r.Stop), nil
		}),
		MakeProperty("step", "Step value of the range", func(receiver Value) (Value, error) {
			r := receiver.(*RangeValue)
			return NumberValue(r.Step), nil
		}),
	)

	// Register methods
	registry.RegisterMethods(
		// __len__ method
		MakeMethod("__len__", 0, "Return the length of the range", func(receiver Value, args []Value, ctx *Context) (Value, error) {
			r, err := TypedReceiver[*RangeValue](receiver, "__len__")
			if err != nil {
				return nil, err
			}
			if err := ValidateArity("__len__", args, 0); err != nil {
				return nil, err
			}
			return NumberValue(r.Length()), nil
		}),

		// __getitem__ method
		MakeMethod("__getitem__", 1, "Get item by index", func(receiver Value, args []Value, ctx *Context) (Value, error) {
			r, err := TypedReceiver[*RangeValue](receiver, "__getitem__")
			if err != nil {
				return nil, err
			}
			if err := ValidateArity("__getitem__", args, 1); err != nil {
				return nil, err
			}

			// Handle slice objects
			if slice, ok := args[0].(*SliceValue); ok {
				return r.getSlice(slice)
			}

			// Handle integer index (honors the __index__ protocol)
			idx, err := sequenceIndex(args[0], ctx, "range")
			if err != nil {
				return nil, err
			}

			return r.GetItem(idx)
		}),

		// __contains__ method
		MakeMethod("__contains__", 1, "Check if value is in range", func(receiver Value, args []Value, ctx *Context) (Value, error) {
			r, err := TypedReceiver[*RangeValue](receiver, "__contains__")
			if err != nil {
				return nil, err
			}
			if err := ValidateArity("__contains__", args, 1); err != nil {
				return nil, err
			}
			return BoolValue(r.Contains(args[0])), nil
		}),

		// index method - return the index of a value, or raise ValueError
		MakeMethod("index", 1, "Return the index of value", func(receiver Value, args []Value, ctx *Context) (Value, error) {
			r, err := TypedReceiver[*RangeValue](receiver, "index")
			if err != nil {
				return nil, err
			}
			if err := ValidateArity("index", args, 1); err != nil {
				return nil, err
			}
			num, ok := args[0].(NumberValue)
			if !ok || !r.Contains(args[0]) {
				return nil, &ValueError{Message: fmt.Sprintf("%s is not in range", Repr(args[0]))}
			}
			return NumberValue(int((float64(num) - r.Start) / r.Step)), nil
		}),

		// count method - return 1 if value is in the range, else 0
		MakeMethod("count", 1, "Return the number of occurrences of value", func(receiver Value, args []Value, ctx *Context) (Value, error) {
			r, err := TypedReceiver[*RangeValue](receiver, "count")
			if err != nil {
				return nil, err
			}
			if err := ValidateArity("count", args, 1); err != nil {
				return nil, err
			}
			if r.Contains(args[0]) {
				return NumberValue(1), nil
			}
			return NumberValue(0), nil
		}),

		// __iter__ method
		MakeIterMethod(),

		// __eq__ method
		MakeMethod("__eq__", 1, "Check equality with another range", func(receiver Value, args []Value, ctx *Context) (Value, error) {
			r, err := TypedReceiver[*RangeValue](receiver, "__eq__")
			if err != nil {
				return nil, err
			}
			if err := ValidateArity("__eq__", args, 1); err != nil {
				return nil, err
			}

			other, ok := args[0].(*RangeValue)
			if !ok {
				return BoolValue(false), nil
			}

			// Two ranges are equal if they produce the same sequence
			// This means same length and same elements
			if r.Length() != other.Length() {
				return BoolValue(false), nil
			}

			// Empty ranges are always equal
			if r.Length() == 0 {
				return BoolValue(true), nil
			}

			// Check if start and step produce same sequence
			return BoolValue(r.Start == other.Start && r.Step == other.Step), nil
		}),
	)

	return registry
}

// GetRegistry implements AttributeProvider
func (r *RangeValue) GetRegistry() *MethodRegistry {
	return r.registry
}

// GetBaseObject implements AttributeProvider
func (r *RangeValue) GetBaseObject() *BaseObject {
	return &r.BaseObject
}

// GetAttr implements the new simplified GetAttr pattern
func (r *RangeValue) GetAttr(name string) (Value, bool) {
	// Use the common implementation
	if val, ok := GetAttrWithRegistry(r, name); ok {
		return val, ok
	}

	// Check type descriptor for additional methods
	desc := GetTypeDescriptor(RangeType)
	if desc != nil {
		val, err := desc.GetAttribute(r, name)
		if err == nil {
			return val, true
		}
	}

	return nil, false
}

// getSlice returns a new range for a slice of this range
func (r *RangeValue) getSlice(slice *SliceValue) (Value, error) {
	length := r.Length()

	// Step (default 1, may be negative)
	step := 1
	if slice.Step != nil && slice.Step != Nil {
		if num, ok := slice.Step.(NumberValue); ok {
			step = int(num)
		}
	}
	if step == 0 {
		return nil, fmt.Errorf("slice step cannot be zero")
	}

	// Bounds for index clamping, per Python's slice.indices(length).
	lower, upper := 0, length
	if step < 0 {
		lower, upper = -1, length-1
	}

	clamp := func(v *Value, def int) int {
		if v == nil || *v == nil || *v == Nil {
			return def
		}
		num, ok := (*v).(NumberValue)
		if !ok {
			return def
		}
		idx := int(num)
		if idx < 0 {
			idx += length
			if idx < lower {
				idx = lower
			}
		} else if idx > upper {
			idx = upper
		}
		return idx
	}

	// start defaults to upper for negative step, lower otherwise; stop is the
	// opposite default.
	startDef, stopDef := lower, upper
	if step < 0 {
		startDef, stopDef = upper, lower
	}
	start := clamp(&slice.Start, startDef)
	stop := clamp(&slice.Stop, stopDef)

	// Number of elements the slice selects.
	slicelen := 0
	if step > 0 {
		if stop > start {
			slicelen = (stop-start-1)/step + 1
		}
	} else {
		if start > stop {
			slicelen = (start-stop-1)/(-step) + 1
		}
	}
	if slicelen <= 0 {
		return NewRangeValue(0, 0, 1) // empty range
	}

	// Map the slice's index space onto the range's actual values.
	newStart := r.Start + float64(start)*r.Step
	newStep := r.Step * float64(step)
	newStop := newStart + float64(slicelen)*newStep
	return NewRangeValue(newStart, newStop, newStep)
}
