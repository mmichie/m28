package modules

import (
	"container/heap"

	"github.com/mmichie/m28/common/errors"
	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// InitHeapqModule creates and returns the heapq module
func InitHeapqModule() *core.DictValue {
	heapqModule := core.NewDict()

	// heappush - Push item onto heap
	heapqModule.Set("heappush", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("heappush", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		list, ok := types.AsList(v.Get(0))
		if !ok {
			return nil, errors.NewTypeError("heappush", "list", string(v.Get(0).Type()))
		}

		item := v.Get(1)

		// Convert to heap and push
		h := &minHeap{items: list}
		heap.Push(h, item)

		// Update the original list (modify in place)
		// Note: In Go this modifies the slice backing array
		return core.Nil, nil
	}))

	// heappop - Pop smallest item from heap
	heapqModule.Set("heappop", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("heappop", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		list, ok := types.AsList(v.Get(0))
		if !ok {
			return nil, errors.NewTypeError("heappop", "list", string(v.Get(0).Type()))
		}

		if list.Len() == 0 {
			return nil, errors.NewRuntimeError("heappop", "heap is empty")
		}

		// Convert to heap and pop
		h := &minHeap{items: list}
		item := heap.Pop(h)

		return item.(core.Value), nil
	}))

	// heapify - Transform list into a heap in-place
	heapqModule.Set("heapify", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("heapify", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		list, ok := types.AsList(v.Get(0))
		if !ok {
			return nil, errors.NewTypeError("heapify", "list", string(v.Get(0).Type()))
		}

		// Convert to heap
		h := &minHeap{items: list}
		heap.Init(h)

		return core.Nil, nil
	}))

	// heappushpop - Push item then pop smallest (more efficient than separate ops)
	heapqModule.Set("heappushpop", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("heappushpop", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		list, ok := types.AsList(v.Get(0))
		if !ok {
			return nil, errors.NewTypeError("heappushpop", "list", string(v.Get(0).Type()))
		}

		item := v.Get(1)

		// If heap is empty or item is smaller than smallest, just return item
		if list.Len() == 0 {
			return item, nil
		}

		if compareValues(item, list.Items()[0]) < 0 {
			return item, nil
		}

		// Otherwise push and pop
		h := &minHeap{items: list}
		heap.Push(h, item)
		return heap.Pop(h).(core.Value), nil
	}))

	// heapreplace - Pop smallest then push item (more efficient than separate ops)
	heapqModule.Set("heapreplace", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("heapreplace", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		list, ok := types.AsList(v.Get(0))
		if !ok {
			return nil, errors.NewTypeError("heapreplace", "list", string(v.Get(0).Type()))
		}

		if list.Len() == 0 {
			return nil, errors.NewRuntimeError("heapreplace", "heap is empty")
		}

		item := v.Get(1)

		// Pop first, then push
		h := &minHeap{items: list}
		returnItem := heap.Pop(h)
		heap.Push(h, item)

		return returnItem.(core.Value), nil
	}))

	// nlargest - Return n largest elements
	heapqModule.Set("nlargest", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("nlargest", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		n, err := v.GetNumber(0)
		if err != nil {
			return nil, err
		}

		iterable, err := types.RequireIterable(v.Get(1), "nlargest() second argument")
		if err != nil {
			return nil, err
		}

		// Collect all items
		items := make([]core.Value, 0)
		iter := iterable.Iterator()
		for {
			val, hasNext := iter.Next()
			if !hasNext {
				break
			}
			items = append(items, val)
		}

		// Sort descending
		sortedItems := sortValues(items, true)

		// Return first n
		nInt := int(n)
		if nInt > len(sortedItems) {
			nInt = len(sortedItems)
		}

		return core.NewList(sortedItems[:nInt]...), nil
	}))

	// nsmallest - Return n smallest elements
	heapqModule.Set("nsmallest", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("nsmallest", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		n, err := v.GetNumber(0)
		if err != nil {
			return nil, err
		}

		iterable, err := types.RequireIterable(v.Get(1), "nsmallest() second argument")
		if err != nil {
			return nil, err
		}

		// Collect all items
		items := make([]core.Value, 0)
		iter := iterable.Iterator()
		for {
			val, hasNext := iter.Next()
			if !hasNext {
				break
			}
			items = append(items, val)
		}

		// Sort ascending
		sortedItems := sortValues(items, false)

		// Return first n
		nInt := int(n)
		if nInt > len(sortedItems) {
			nInt = len(sortedItems)
		}

		return core.NewList(sortedItems[:nInt]...), nil
	}))

	return heapqModule
}

// minHeap implements heap.Interface for M28 values
type minHeap struct {
	items *core.ListValue
}

func (h minHeap) Len() int { return h.items.Len() }

func (h minHeap) Less(i, j int) bool {
	items := h.items.Items()
	return compareValues(items[i], items[j]) < 0
}

func (h minHeap) Swap(i, j int) {
	items := h.items.Items()
	items[i], items[j] = items[j], items[i]
}

func (h *minHeap) Push(x interface{}) {
	h.items.Append(x.(core.Value))
}

func (h *minHeap) Pop() interface{} {
	items := h.items.Items()
	n := len(items)
	x := items[n-1]
	// Truncate the list by setting items to a new slice
	h.items = core.NewList(items[0 : n-1]...)
	return x
}

// compareValues compares two M28 values
// Returns: -1 if a < b, 0 if a == b, 1 if a > b
func compareValues(a, b core.Value) int {
	// Try numeric comparison first
	if aNum, aOk := types.AsNumber(a); aOk {
		if bNum, bOk := types.AsNumber(b); bOk {
			if aNum < bNum {
				return -1
			} else if aNum > bNum {
				return 1
			}
			return 0
		}
	}

	// Try string comparison
	if aStr, aOk := types.AsString(a); aOk {
		if bStr, bOk := types.AsString(b); bOk {
			if aStr < bStr {
				return -1
			} else if aStr > bStr {
				return 1
			}
			return 0
		}
	}

	// Default: compare string representations
	aStr := a.String()
	bStr := b.String()
	if aStr < bStr {
		return -1
	} else if aStr > bStr {
		return 1
	}
	return 0
}

// sortValues sorts a list of values
func sortValues(items []core.Value, descending bool) []core.Value {
	// Create a copy to avoid modifying original
	sorted := make([]core.Value, len(items))
	copy(sorted, items)

	// Simple bubble sort (good enough for nlargest/nsmallest)
	n := len(sorted)
	for i := 0; i < n; i++ {
		for j := 0; j < n-i-1; j++ {
			cmp := compareValues(sorted[j], sorted[j+1])
			shouldSwap := (descending && cmp < 0) || (!descending && cmp > 0)
			if shouldSwap {
				sorted[j], sorted[j+1] = sorted[j+1], sorted[j]
			}
		}
	}

	return sorted
}
