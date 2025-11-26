// Package modules provides the _heapq C extension module for M28
// Implements heap queue algorithm (priority queue) using Go's container/heap
package modules

import (
	"container/heap"
	"fmt"

	"github.com/mmichie/m28/core"
)

// pythonListHeap wraps a Python list to implement Go's heap.Interface
type pythonListHeap struct {
	list *core.ListValue
}

func (h pythonListHeap) Len() int {
	return h.list.Len()
}

func (h pythonListHeap) Less(i, j int) bool {
	// Get elements at positions i and j
	items := h.list.Items()
	if i >= len(items) || j >= len(items) {
		return false
	}

	// Use core.Compare for Python-compatible comparison
	// Returns -1 if items[i] < items[j]
	return core.Compare(items[i], items[j]) < 0
}

func (h pythonListHeap) Swap(i, j int) {
	// Use GetItem and SetItem to properly swap elements in the list
	if i >= h.list.Len() || j >= h.list.Len() || i < 0 || j < 0 {
		return
	}

	itemI, errI := h.list.GetItem(i)
	itemJ, errJ := h.list.GetItem(j)
	if errI != nil || errJ != nil {
		return
	}

	h.list.SetItem(i, itemJ)
	h.list.SetItem(j, itemI)
}

func (h *pythonListHeap) Push(x interface{}) {
	// Add element to the list
	if val, ok := x.(core.Value); ok {
		h.list.Append(val)
	}
}

func (h *pythonListHeap) Pop() interface{} {
	// Remove and return last element
	items := h.list.Items()
	n := len(items)
	if n == 0 {
		return core.None
	}
	item := items[n-1]
	*h.list = *core.NewList(items[:n-1]...)
	return item
}

// pythonListMaxHeap wraps a Python list to implement a max heap (reverses comparisons)
type pythonListMaxHeap struct {
	list *core.ListValue
}

func (h pythonListMaxHeap) Len() int {
	return h.list.Len()
}

func (h pythonListMaxHeap) Less(i, j int) bool {
	// Reverse comparison for max heap
	items := h.list.Items()
	if i >= len(items) || j >= len(items) {
		return false
	}
	// Returns true if items[i] > items[j] (reversed from min heap)
	return core.Compare(items[i], items[j]) > 0
}

func (h pythonListMaxHeap) Swap(i, j int) {
	if i >= h.list.Len() || j >= h.list.Len() || i < 0 || j < 0 {
		return
	}

	itemI, errI := h.list.GetItem(i)
	itemJ, errJ := h.list.GetItem(j)
	if errI != nil || errJ != nil {
		return
	}

	h.list.SetItem(i, itemJ)
	h.list.SetItem(j, itemI)
}

func (h *pythonListMaxHeap) Push(x interface{}) {
	if val, ok := x.(core.Value); ok {
		h.list.Append(val)
	}
}

func (h *pythonListMaxHeap) Pop() interface{} {
	items := h.list.Items()
	n := len(items)
	if n == 0 {
		return core.None
	}
	item := items[n-1]
	*h.list = *core.NewList(items[:n-1]...)
	return item
}

// Init_HeapqModule creates and returns the _heapq module
func Init_HeapqModule() *core.DictValue {
	heapqModule := core.NewDict()

	// heappush(heap, item) - Push item onto heap, maintaining heap invariant
	heapqModule.Set("heappush", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 2 {
			return nil, fmt.Errorf("heappush expected 2 arguments, got %d", len(args))
		}

		list, ok := args[0].(*core.ListValue)
		if !ok {
			return nil, fmt.Errorf("heappush argument must be a list")
		}

		item := args[1]

		// Create heap wrapper and push
		h := &pythonListHeap{list: list}
		heap.Push(h, item)

		return core.None, nil
	}))

	// heappop(heap) - Pop and return smallest item from heap
	heapqModule.Set("heappop", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("heappop expected 1 argument, got %d", len(args))
		}

		list, ok := args[0].(*core.ListValue)
		if !ok {
			return nil, fmt.Errorf("heappop argument must be a list")
		}

		if list.Len() == 0 {
			return nil, fmt.Errorf("IndexError: index out of range")
		}

		// Create heap wrapper and pop
		h := &pythonListHeap{list: list}
		item := heap.Pop(h)

		if val, ok := item.(core.Value); ok {
			return val, nil
		}
		return core.None, nil
	}))

	// heapify(x) - Transform list into heap in-place, in O(n) time
	heapqModule.Set("heapify", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("heapify expected 1 argument, got %d", len(args))
		}

		list, ok := args[0].(*core.ListValue)
		if !ok {
			return nil, fmt.Errorf("heapify argument must be a list")
		}

		// Create heap wrapper and heapify
		h := &pythonListHeap{list: list}
		heap.Init(h)

		return core.None, nil
	}))

	// heapreplace(heap, item) - Pop smallest, then push new item (more efficient than pop + push)
	heapqModule.Set("heapreplace", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 2 {
			return nil, fmt.Errorf("heapreplace expected 2 arguments, got %d", len(args))
		}

		list, ok := args[0].(*core.ListValue)
		if !ok {
			return nil, fmt.Errorf("heapreplace argument must be a list")
		}

		if list.Len() == 0 {
			return nil, fmt.Errorf("IndexError: index out of range")
		}

		item := args[1]

		// Get the smallest item (at index 0)
		items := list.Items()
		returnItem := items[0]

		// Replace it with the new item and fix the heap
		items[0] = item
		h := &pythonListHeap{list: list}
		heap.Fix(h, 0)

		return returnItem, nil
	}))

	// heappushpop(heap, item) - Push item, then pop and return smallest (more efficient than push + pop)
	heapqModule.Set("heappushpop", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 2 {
			return nil, fmt.Errorf("heappushpop expected 2 arguments, got %d", len(args))
		}

		list, ok := args[0].(*core.ListValue)
		if !ok {
			return nil, fmt.Errorf("heappushpop argument must be a list")
		}

		item := args[1]

		// If heap is empty or item is smaller than smallest, just return item
		if list.Len() == 0 {
			return item, nil
		}

		items := list.Items()
		if core.Compare(item, items[0]) < 0 {
			return item, nil
		}

		// Otherwise, replace smallest with item and fix heap
		returnItem := items[0]
		items[0] = item
		h := &pythonListHeap{list: list}
		heap.Fix(h, 0)

		return returnItem, nil
	}))

	// _heappop_max(heap) - Pop and return largest item from max heap
	heapqModule.Set("_heappop_max", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("_heappop_max expected 1 argument, got %d", len(args))
		}

		list, ok := args[0].(*core.ListValue)
		if !ok {
			return nil, fmt.Errorf("_heappop_max argument must be a list")
		}

		if list.Len() == 0 {
			return nil, fmt.Errorf("IndexError: index out of range")
		}

		// Create max heap wrapper and pop
		h := &pythonListMaxHeap{list: list}
		item := heap.Pop(h)

		if val, ok := item.(core.Value); ok {
			return val, nil
		}
		return core.None, nil
	}))

	// _heapify_max(x) - Transform list into max heap in-place
	heapqModule.Set("_heapify_max", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("_heapify_max expected 1 argument, got %d", len(args))
		}

		list, ok := args[0].(*core.ListValue)
		if !ok {
			return nil, fmt.Errorf("_heapify_max argument must be a list")
		}

		// Create max heap wrapper and heapify
		h := &pythonListMaxHeap{list: list}
		heap.Init(h)

		return core.None, nil
	}))

	// _heapreplace_max(heap, item) - Pop largest from max heap, then push new item
	heapqModule.Set("_heapreplace_max", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 2 {
			return nil, fmt.Errorf("_heapreplace_max expected 2 arguments, got %d", len(args))
		}

		list, ok := args[0].(*core.ListValue)
		if !ok {
			return nil, fmt.Errorf("_heapreplace_max argument must be a list")
		}

		if list.Len() == 0 {
			return nil, fmt.Errorf("IndexError: index out of range")
		}

		item := args[1]

		// Get the largest item (at index 0 in max heap)
		items := list.Items()
		returnItem := items[0]

		// Replace it with the new item and fix the heap
		items[0] = item
		h := &pythonListMaxHeap{list: list}
		heap.Fix(h, 0)

		return returnItem, nil
	}))

	return heapqModule
}
