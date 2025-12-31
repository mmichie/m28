package modules

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// InitBisectModule creates the _bisect C extension module
func InitBisectModule() *core.DictValue {
	bisectModule := core.NewDict()

	// bisect_left(a, x, lo=0, hi=len(a)) - locate leftmost insertion point
	bisectModule.Set("bisect_left", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 2 {
			return nil, fmt.Errorf("bisect_left() requires at least 2 arguments")
		}

		list, ok := args[0].(*core.ListValue)
		if !ok {
			return nil, fmt.Errorf("bisect_left() first argument must be a list")
		}

		x := args[1]
		items := list.Items()

		lo := 0
		hi := len(items)

		if len(args) >= 3 {
			if loVal, ok := args[2].(core.NumberValue); ok {
				lo = int(loVal)
			}
		}
		if len(args) >= 4 {
			if hiVal, ok := args[3].(core.NumberValue); ok {
				hi = int(hiVal)
			}
		}

		// Binary search for leftmost position
		for lo < hi {
			mid := (lo + hi) / 2
			cmp := core.Compare(items[mid], x)
			if cmp < 0 { // items[mid] < x
				lo = mid + 1
			} else {
				hi = mid
			}
		}

		return core.NumberValue(lo), nil
	}))

	// bisect_right(a, x, lo=0, hi=len(a)) - locate rightmost insertion point
	bisectModule.Set("bisect_right", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 2 {
			return nil, fmt.Errorf("bisect_right() requires at least 2 arguments")
		}

		list, ok := args[0].(*core.ListValue)
		if !ok {
			return nil, fmt.Errorf("bisect_right() first argument must be a list")
		}

		x := args[1]
		items := list.Items()

		lo := 0
		hi := len(items)

		if len(args) >= 3 {
			if loVal, ok := args[2].(core.NumberValue); ok {
				lo = int(loVal)
			}
		}
		if len(args) >= 4 {
			if hiVal, ok := args[3].(core.NumberValue); ok {
				hi = int(hiVal)
			}
		}

		// Binary search for rightmost position
		for lo < hi {
			mid := (lo + hi) / 2
			cmp := core.Compare(x, items[mid])
			if cmp < 0 { // x < items[mid]
				hi = mid
			} else {
				lo = mid + 1
			}
		}

		return core.NumberValue(lo), nil
	}))

	// insort_left(a, x, lo=0, hi=len(a)) - insert x in list a in sorted order (left)
	bisectModule.Set("insort_left", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 2 {
			return nil, fmt.Errorf("insort_left() requires at least 2 arguments")
		}

		list, ok := args[0].(*core.ListValue)
		if !ok {
			return nil, fmt.Errorf("insort_left() first argument must be a list")
		}

		x := args[1]
		items := list.Items()

		lo := 0
		hi := len(items)

		if len(args) >= 3 {
			if loVal, ok := args[2].(core.NumberValue); ok {
				lo = int(loVal)
			}
		}
		if len(args) >= 4 {
			if hiVal, ok := args[3].(core.NumberValue); ok {
				hi = int(hiVal)
			}
		}

		// Find insertion point
		for lo < hi {
			mid := (lo + hi) / 2
			cmp := core.Compare(items[mid], x)
			if cmp < 0 { // items[mid] < x
				lo = mid + 1
			} else {
				hi = mid
			}
		}

		// Insert at position lo using slice manipulation
		// Create new slice with room for one more element
		newItems := make([]core.Value, len(items)+1)
		copy(newItems[:lo], items[:lo])
		newItems[lo] = x
		copy(newItems[lo+1:], items[lo:])

		// Use SetSlice to replace entire list
		start := 0
		end := len(items)
		list.SetSlice(&start, &end, core.NewList(newItems...))

		return core.NilValue{}, nil
	}))

	// insort_right(a, x, lo=0, hi=len(a)) - insert x in list a in sorted order (right)
	bisectModule.Set("insort_right", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 2 {
			return nil, fmt.Errorf("insort_right() requires at least 2 arguments")
		}

		list, ok := args[0].(*core.ListValue)
		if !ok {
			return nil, fmt.Errorf("insort_right() first argument must be a list")
		}

		x := args[1]
		items := list.Items()

		lo := 0
		hi := len(items)

		if len(args) >= 3 {
			if loVal, ok := args[2].(core.NumberValue); ok {
				lo = int(loVal)
			}
		}
		if len(args) >= 4 {
			if hiVal, ok := args[3].(core.NumberValue); ok {
				hi = int(hiVal)
			}
		}

		// Find insertion point
		for lo < hi {
			mid := (lo + hi) / 2
			cmp := core.Compare(x, items[mid])
			if cmp < 0 { // x < items[mid]
				hi = mid
			} else {
				lo = mid + 1
			}
		}

		// Insert at position lo using slice manipulation
		newItems := make([]core.Value, len(items)+1)
		copy(newItems[:lo], items[:lo])
		newItems[lo] = x
		copy(newItems[lo+1:], items[lo:])

		// Use SetSlice to replace entire list
		start := 0
		end := len(items)
		list.SetSlice(&start, &end, core.NewList(newItems...))

		return core.NilValue{}, nil
	}))

	return bisectModule
}
