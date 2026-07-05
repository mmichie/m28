package modules

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// bisectLen returns len(a) for any sequence (list, tuple, or an object with
// __len__), matching what CPython's bisect accepts.
func bisectLen(a core.Value, ctx *core.Context) (int, error) {
	switch v := a.(type) {
	case *core.ListValue:
		return len(v.Items()), nil
	case core.TupleValue:
		return len(v), nil
	}
	if obj, ok := a.(core.Object); ok {
		if m, found := obj.GetAttr("__len__"); found {
			if c, ok := m.(core.Callable); ok {
				r, err := c.Call(nil, ctx)
				if err != nil {
					return 0, err
				}
				if n, ok := r.(core.NumberValue); ok {
					return int(n), nil
				}
			}
		}
	}
	return 0, fmt.Errorf("object of type '%s' has no len()", a.Type())
}

// bisectGetItem returns a[i] for any sequence (list, tuple, or an object with
// __getitem__).
func bisectGetItem(a core.Value, i int, ctx *core.Context) (core.Value, error) {
	switch v := a.(type) {
	case *core.ListValue:
		return v.Items()[i], nil
	case core.TupleValue:
		return v[i], nil
	}
	if obj, ok := a.(core.Object); ok {
		if m, found := obj.GetAttr("__getitem__"); found {
			if c, ok := m.(core.Callable); ok {
				return c.Call([]core.Value{core.NumberValue(i)}, ctx)
			}
		}
	}
	return nil, fmt.Errorf("'%s' object is not subscriptable", a.Type())
}

// bisectApplyKey returns key(v), or v when key is absent (nil or None).
func bisectApplyKey(key, v core.Value, ctx *core.Context) (core.Value, error) {
	if key == nil {
		return v, nil
	}
	if _, isNil := key.(core.NilValue); isNil {
		return v, nil
	}
	if c, ok := key.(core.Callable); ok {
		return c.Call([]core.Value{v}, ctx)
	}
	return nil, fmt.Errorf("'%s' object is not callable", key.Type())
}

// bisectParams resolves the (a, x, lo, hi, key) arguments shared by all four
// bisect functions. a and x may be positional or keyword; lo/hi are optional
// (positional or keyword); key is keyword-only.
type bisectParams struct {
	a, x, key core.Value
	lo, hi    int
}

func parseBisectArgs(name string, args []core.Value, kwargs *core.Kwargs, ctx *core.Context) (*bisectParams, error) {
	p := &bisectParams{}

	get := func(idx int, kw string) (core.Value, bool) {
		if idx < len(args) {
			return args[idx], true
		}
		if v, ok := kwargs.Get(kw); ok {
			return v, true
		}
		return nil, false
	}

	var ok bool
	if p.a, ok = get(0, "a"); !ok {
		return nil, fmt.Errorf("%s() missing required argument 'a'", name)
	}
	if p.x, ok = get(1, "x"); !ok {
		return nil, fmt.Errorf("%s() missing required argument 'x'", name)
	}

	n, err := bisectLen(p.a, ctx)
	if err != nil {
		return nil, err
	}

	p.lo = 0
	if v, ok := get(2, "lo"); ok {
		if num, ok := v.(core.NumberValue); ok {
			p.lo = int(num)
		}
	}
	if p.lo < 0 {
		return nil, fmt.Errorf("lo must be non-negative")
	}

	p.hi = n
	if v, ok := get(3, "hi"); ok {
		if _, isNil := v.(core.NilValue); !isNil {
			if num, ok := v.(core.NumberValue); ok {
				p.hi = int(num)
			}
		}
	}
	if p.hi > n {
		p.hi = n
	}

	p.key, _ = kwargs.Get("key")
	return p, nil
}

// bisectSearch returns the insertion index. left=true finds the leftmost
// position (bisect_left), left=false the rightmost (bisect_right).
func bisectSearch(p *bisectParams, left bool, ctx *core.Context) (int, error) {
	lo, hi := p.lo, p.hi
	for lo < hi {
		mid := (lo + hi) / 2
		item, err := bisectGetItem(p.a, mid, ctx)
		if err != nil {
			return 0, err
		}
		item, err = bisectApplyKey(p.key, item, ctx)
		if err != nil {
			return 0, err
		}
		if left {
			// a[mid] < x  ->  go right
			if core.Compare(item, p.x) < 0 {
				lo = mid + 1
			} else {
				hi = mid
			}
		} else {
			// x < a[mid]  ->  go left
			if core.Compare(p.x, item) < 0 {
				hi = mid
			} else {
				lo = mid + 1
			}
		}
	}
	return lo, nil
}

// bisectInsert inserts x at position pos into a (a list, or any object with an
// insert(index, item) method, e.g. CPython's bisect supports both).
func bisectInsert(a core.Value, pos int, x core.Value, ctx *core.Context) error {
	if list, ok := a.(*core.ListValue); ok {
		items := list.Items()
		newItems := make([]core.Value, len(items)+1)
		copy(newItems[:pos], items[:pos])
		newItems[pos] = x
		copy(newItems[pos+1:], items[pos:])
		start, end := 0, len(items)
		list.SetSlice(&start, &end, core.NewList(newItems...))
		return nil
	}
	if obj, ok := a.(core.Object); ok {
		if m, found := obj.GetAttr("insert"); found {
			if c, ok := m.(core.Callable); ok {
				_, err := c.Call([]core.Value{core.NumberValue(pos), x}, ctx)
				return err
			}
		}
	}
	return fmt.Errorf("'%s' object has no insert() method", a.Type())
}

func newBisectFunc(name string, left, insort bool) *core.BuiltinFunctionWithKwargs {
	return &core.BuiltinFunctionWithKwargs{
		BaseObject: *core.NewBaseObject(core.BuiltinFunctionType),
		Name:       name,
		Fn: func(args []core.Value, kwargs *core.Kwargs, ctx *core.Context) (core.Value, error) {
			p, err := parseBisectArgs(name, args, kwargs, ctx)
			if err != nil {
				return nil, err
			}
			if insort {
				// insort compares the keyed value of x against keyed elements,
				// but inserts the original x. Search with x replaced by key(x).
				searchX, err := bisectApplyKey(p.key, p.x, ctx)
				if err != nil {
					return nil, err
				}
				orig := p.x
				p.x = searchX
				pos, err := bisectSearch(p, left, ctx)
				if err != nil {
					return nil, err
				}
				if err := bisectInsert(p.a, pos, orig, ctx); err != nil {
					return nil, err
				}
				return core.None, nil
			}
			pos, err := bisectSearch(p, left, ctx)
			if err != nil {
				return nil, err
			}
			return core.NumberValue(pos), nil
		},
	}
}

// InitBisectModule creates the _bisect C extension module
func InitBisectModule() *core.DictValue {
	bisectModule := core.NewDict()
	bisectModule.Set("bisect_left", newBisectFunc("bisect_left", true, false))
	bisectModule.Set("bisect_right", newBisectFunc("bisect_right", false, false))
	bisectModule.Set("insort_left", newBisectFunc("insort_left", true, true))
	bisectModule.Set("insort_right", newBisectFunc("insort_right", false, true))
	return bisectModule
}
