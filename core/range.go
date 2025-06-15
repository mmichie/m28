package core

import (
	"fmt"
	"math"
)

// RangeValue represents a lazy range object like Python's range
type RangeValue struct {
	BaseObject
	Start float64
	Stop  float64
	Step  float64
}

// NewRangeValue creates a new range value
func NewRangeValue(start, stop, step float64) (*RangeValue, error) {
	if step == 0 {
		return nil, fmt.Errorf("range() arg 3 must not be zero")
	}
	return &RangeValue{
		BaseObject: *NewBaseObject(RangeType),
		Start:      start,
		Stop:       stop,
		Step:       step,
	}, nil
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
	return NumberValue(value), nil
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

	val := NumberValue(it.current)
	it.current += it.rang.Step
	return val, true
}

// Reset implements Iterator.Reset
func (it *rangeIterator) Reset() {
	it.current = it.rang.Start
}

// GetAttr implements Object.GetAttr for range methods
func (r *RangeValue) GetAttr(name string) (Value, bool) {
	switch name {
	case "start":
		return NumberValue(r.Start), true
	case "stop":
		return NumberValue(r.Stop), true
	case "step":
		return NumberValue(r.Step), true
	case "__len__":
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if len(args) != 0 {
				return nil, fmt.Errorf("__len__ takes no arguments")
			}
			return NumberValue(r.Length()), nil
		}), true
	case "__getitem__":
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("__getitem__ requires 1 argument")
			}

			// Handle slice objects
			if slice, ok := args[0].(*SliceValue); ok {
				return r.getSlice(slice)
			}

			// Handle integer index
			idx, ok := args[0].(NumberValue)
			if !ok {
				return nil, fmt.Errorf("range indices must be integers")
			}

			return r.GetItem(int(idx))
		}), true
	case "__contains__":
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("__contains__ requires 1 argument")
			}
			return BoolValue(r.Contains(args[0])), nil
		}), true
	case "__iter__":
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			// Return self - iter() builtin will handle creating the proper iterator
			return r, nil
		}), true
	}

	// Check type descriptor for other methods
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

	// Determine start index
	start := 0
	if slice.Start != nil {
		if num, ok := slice.Start.(NumberValue); ok {
			start = int(num)
			if start < 0 {
				start = length + start
			}
			if start < 0 {
				start = 0
			}
			if start > length {
				start = length
			}
		}
	}

	// Determine stop index
	stop := length
	if slice.Stop != nil {
		if num, ok := slice.Stop.(NumberValue); ok {
			stop = int(num)
			if stop < 0 {
				stop = length + stop
			}
			if stop < 0 {
				stop = 0
			}
			if stop > length {
				stop = length
			}
		}
	}

	// Determine step
	step := 1
	if slice.Step != nil {
		if num, ok := slice.Step.(NumberValue); ok {
			step = int(num)
			if step == 0 {
				return nil, fmt.Errorf("slice step cannot be zero")
			}
		}
	}

	// Calculate new range parameters
	newStart := r.Start + float64(start)*r.Step
	newStop := r.Start + float64(stop)*r.Step
	newStep := r.Step * float64(step)

	return NewRangeValue(newStart, newStop, newStep)
}
