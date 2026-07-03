package core

import (
	"fmt"
)

// setOperand is a uniform view over set/frozenset operands: entry iteration
// with cached hashes plus hashed membership, so set algebra never rehashes
// and both types compare interchangeably (Python semantics).
type setOperand struct {
	forEachEntry   func(fn func(hash uint64, elem Value) bool)
	containsHashed func(elem Value, hash uint64, ctx *Context) (bool, error)
	size           int
}

// asSetOperand adapts a set or frozenset. ok is false for anything else.
func asSetOperand(v Value) (setOperand, bool) {
	switch s := v.(type) {
	case *SetValue:
		return setOperand{s.forEachEntry, s.containsHashed, s.Size()}, true
	case *FrozenSetValue:
		return setOperand{s.forEachEntry, s.containsHashed, s.Size()}, true
	}
	return setOperand{}, false
}

// frozensetFromOperandOrIterable builds a frozenset operand from a
// set/frozenset (cached hashes) or any iterable (full hashing with ctx).
func operandFromIterable(arg Value, method string, ctx *Context) (setOperand, error) {
	if op, ok := asSetOperand(arg); ok {
		return op, nil
	}
	if s, ok := arg.(StringValue); ok {
		// Strings iterate as characters.
		tmp := NewSet()
		for _, ch := range string(s) {
			if err := tmp.AddWithError(StringValue(string(ch)), ctx); err != nil {
				return setOperand{}, err
			}
		}
		return setOperand{tmp.forEachEntry, tmp.containsHashed, tmp.Size()}, nil
	}
	if iterable, ok := arg.(Iterable); ok {
		tmp := NewSet()
		iter := iterable.Iterator()
		for {
			val, hasNext := iter.Next()
			if !hasNext {
				break
			}
			if err := tmp.AddWithError(val, ctx); err != nil {
				return setOperand{}, err
			}
		}
		return setOperand{tmp.forEachEntry, tmp.containsHashed, tmp.Size()}, nil
	}
	return setOperand{}, fmt.Errorf("%s() argument must be an iterable", method)
}

// copyEntriesInto adds every element of op into any add-by-entry target.
func copyEntriesInto(op setOperand, add func(elem Value, hash uint64, ctx *Context) error, ctx *Context) error {
	var err error
	op.forEachEntry(func(h uint64, elem Value) bool {
		err = add(elem, h, ctx)
		return err == nil
	})
	return err
}

// registerFrozenSetType registers the frozenset type descriptor with all its methods
func registerFrozenSetType() {
	RegisterType(&TypeDescriptor{
		Name:       "frozenset",
		PythonName: "frozenset",
		BaseType:   FrozenSetType,
		Methods:    getFrozenSetMethods(),
		Constructor: func(args []Value, ctx *Context) (Value, error) {
			if len(args) == 0 {
				return NewFrozenSet(), nil
			}
			if len(args) == 1 {
				arg := args[0]
				result := NewFrozenSet()

				// Sets/frozensets copy with cached hashes.
				if op, ok := asSetOperand(arg); ok {
					if err := copyEntriesInto(op, result.addEntry, ctx); err != nil {
						return nil, err
					}
					return result, nil
				}

				switch v := arg.(type) {
				case *ListValue:
					for _, item := range v.Items() {
						if err := result.AddWithError(item, ctx); err != nil {
							return nil, err
						}
					}
				case TupleValue:
					for _, item := range v {
						if err := result.AddWithError(item, ctx); err != nil {
							return nil, err
						}
					}
				case StringValue:
					// String is iterable, each character becomes an element
					for _, ch := range string(v) {
						if err := result.AddWithError(StringValue(string(ch)), ctx); err != nil {
							return nil, err
						}
					}
				default:
					if iterable, ok := arg.(Iterable); ok {
						iter := iterable.Iterator()
						for {
							val, ok := iter.Next()
							if !ok {
								break
							}
							if err := result.AddWithError(val, ctx); err != nil {
								return nil, err
							}
						}
					} else {
						return nil, fmt.Errorf("frozenset() argument must be an iterable")
					}
				}
				return result, nil
			}
			return nil, fmt.Errorf("frozenset() takes at most 1 argument (%d given)", len(args))
		},
		Repr: func(v Value) string {
			return v.(*FrozenSetValue).String()
		},
		Str: func(v Value) string {
			return v.(*FrozenSetValue).String()
		},
		Doc: "frozenset() -> new empty frozenset object\nfrozenset(iterable) -> new frozenset object\n\nBuild an immutable unordered collection of unique elements.",
	})
}

// frozensetBinaryOp implements the shared shape of __sub__/__and__/__or__/
// __xor__ and the named set-algebra methods.
func frozensetSubsetOf(fs *FrozenSetValue, other setOperand, ctx *Context) (bool, error) {
	result := true
	var walkErr error
	fs.forEachEntry(func(h uint64, elem Value) bool {
		in, err := other.containsHashed(elem, h, ctx)
		if err != nil {
			walkErr = err
			return false
		}
		if !in {
			result = false
			return false
		}
		return true
	})
	return result, walkErr
}

func frozensetSupersetOf(fs *FrozenSetValue, other setOperand, ctx *Context) (bool, error) {
	result := true
	var walkErr error
	other.forEachEntry(func(h uint64, elem Value) bool {
		in, err := fs.containsHashed(elem, h, ctx)
		if err != nil {
			walkErr = err
			return false
		}
		if !in {
			result = false
			return false
		}
		return true
	})
	return result, walkErr
}

// getFrozenSetMethods returns all frozenset methods (read-only operations only)
func getFrozenSetMethods() map[string]*MethodDescriptor {
	requireSetOperand := func(arg Value, opName string) (setOperand, error) {
		op, ok := asSetOperand(arg)
		if !ok {
			return setOperand{}, fmt.Errorf("unsupported operand type(s) for %s: 'frozenset' and '%s'", opName, arg.Type())
		}
		return op, nil
	}

	return map[string]*MethodDescriptor{
		"copy": {
			Name:    "copy",
			Arity:   0,
			Doc:     "Return a shallow copy of the frozenset",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				fs := receiver.(*FrozenSetValue)
				result := NewFrozenSet()
				op, _ := asSetOperand(fs)
				if err := copyEntriesInto(op, result.addEntry, ctx); err != nil {
					return nil, err
				}
				return result, nil
			},
		},
		"__le__": {
			Name: "__le__", Arity: 1, Doc: "Subset test (self <= other)", Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				fs := receiver.(*FrozenSetValue)
				other, ok := asSetOperand(args[0])
				if !ok {
					return nil, NewTypeError("set or frozenset", args[0], "__le__ argument")
				}
				sub, err := frozensetSubsetOf(fs, other, ctx)
				if err != nil {
					return nil, err
				}
				return BoolValue(sub), nil
			},
		},
		"__lt__": {
			Name: "__lt__", Arity: 1, Doc: "Proper-subset test (self < other)", Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				fs := receiver.(*FrozenSetValue)
				other, ok := asSetOperand(args[0])
				if !ok {
					return nil, NewTypeError("set or frozenset", args[0], "__lt__ argument")
				}
				if fs.Size() >= other.size {
					return False, nil
				}
				sub, err := frozensetSubsetOf(fs, other, ctx)
				if err != nil {
					return nil, err
				}
				return BoolValue(sub), nil
			},
		},
		"__ge__": {
			Name: "__ge__", Arity: 1, Doc: "Superset test (self >= other)", Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				fs := receiver.(*FrozenSetValue)
				other, ok := asSetOperand(args[0])
				if !ok {
					return nil, NewTypeError("set or frozenset", args[0], "__ge__ argument")
				}
				super, err := frozensetSupersetOf(fs, other, ctx)
				if err != nil {
					return nil, err
				}
				return BoolValue(super), nil
			},
		},
		"__gt__": {
			Name: "__gt__", Arity: 1, Doc: "Proper-superset test (self > other)", Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				fs := receiver.(*FrozenSetValue)
				other, ok := asSetOperand(args[0])
				if !ok {
					return nil, NewTypeError("set or frozenset", args[0], "__gt__ argument")
				}
				if fs.Size() <= other.size {
					return False, nil
				}
				super, err := frozensetSupersetOf(fs, other, ctx)
				if err != nil {
					return nil, err
				}
				return BoolValue(super), nil
			},
		},
		"union": {
			Name:    "union",
			Arity:   -1,
			Doc:     "Return the union of frozensets",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				fs := receiver.(*FrozenSetValue)
				result := NewFrozenSet()
				self, _ := asSetOperand(fs)
				if err := copyEntriesInto(self, result.addEntry, ctx); err != nil {
					return nil, err
				}
				for _, arg := range args {
					op, ok := asSetOperand(arg)
					if !ok {
						return nil, fmt.Errorf("union() argument must be a set or frozenset")
					}
					if err := copyEntriesInto(op, result.addEntry, ctx); err != nil {
						return nil, err
					}
				}
				return result, nil
			},
		},
		"intersection": {
			Name:    "intersection",
			Arity:   -1,
			Doc:     "Return the intersection of frozensets",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				fs := receiver.(*FrozenSetValue)
				result := NewFrozenSet()
				self, _ := asSetOperand(fs)
				if len(args) == 0 {
					if err := copyEntriesInto(self, result.addEntry, ctx); err != nil {
						return nil, err
					}
					return result, nil
				}

				others := make([]setOperand, len(args))
				for i, arg := range args {
					op, ok := asSetOperand(arg)
					if !ok {
						return nil, fmt.Errorf("intersection() argument must be a set or frozenset")
					}
					others[i] = op
				}

				var walkErr error
				self.forEachEntry(func(h uint64, elem Value) bool {
					for _, other := range others {
						in, err := other.containsHashed(elem, h, ctx)
						if err != nil {
							walkErr = err
							return false
						}
						if !in {
							return true
						}
					}
					walkErr = result.addEntry(elem, h, ctx)
					return walkErr == nil
				})
				if walkErr != nil {
					return nil, walkErr
				}
				return result, nil
			},
		},
		"difference": {
			Name:    "difference",
			Arity:   -1,
			Doc:     "Return the difference of frozensets",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				fs := receiver.(*FrozenSetValue)
				result := NewFrozenSet()

				others := make([]setOperand, len(args))
				for i, arg := range args {
					op, ok := asSetOperand(arg)
					if !ok {
						return nil, fmt.Errorf("difference() argument must be a set or frozenset")
					}
					others[i] = op
				}

				var walkErr error
				fs.forEachEntry(func(h uint64, elem Value) bool {
					for _, other := range others {
						in, err := other.containsHashed(elem, h, ctx)
						if err != nil {
							walkErr = err
							return false
						}
						if in {
							return true
						}
					}
					walkErr = result.addEntry(elem, h, ctx)
					return walkErr == nil
				})
				if walkErr != nil {
					return nil, walkErr
				}
				return result, nil
			},
		},
		"symmetric_difference": {
			Name:    "symmetric_difference",
			Arity:   1,
			Doc:     "Return the symmetric difference of two frozensets",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				fs := receiver.(*FrozenSetValue)
				other, ok := asSetOperand(args[0])
				if !ok {
					return nil, fmt.Errorf("symmetric_difference() argument must be a set or frozenset")
				}
				result := NewFrozenSet()
				var walkErr error

				fs.forEachEntry(func(h uint64, elem Value) bool {
					in, err := other.containsHashed(elem, h, ctx)
					if err != nil {
						walkErr = err
						return false
					}
					if !in {
						walkErr = result.addEntry(elem, h, ctx)
						return walkErr == nil
					}
					return true
				})
				if walkErr != nil {
					return nil, walkErr
				}

				other.forEachEntry(func(h uint64, elem Value) bool {
					in, err := fs.containsHashed(elem, h, ctx)
					if err != nil {
						walkErr = err
						return false
					}
					if !in {
						walkErr = result.addEntry(elem, h, ctx)
						return walkErr == nil
					}
					return true
				})
				if walkErr != nil {
					return nil, walkErr
				}
				return result, nil
			},
		},
		"issubset": {
			Name:    "issubset",
			Arity:   1,
			Doc:     "Check if this frozenset is a subset of another",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				fs := receiver.(*FrozenSetValue)
				other, err := operandFromIterable(args[0], "issubset", ctx)
				if err != nil {
					return nil, err
				}
				sub, err := frozensetSubsetOf(fs, other, ctx)
				if err != nil {
					return nil, err
				}
				return BoolValue(sub), nil
			},
		},
		"issuperset": {
			Name:    "issuperset",
			Arity:   1,
			Doc:     "Check if this frozenset is a superset of another",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				fs := receiver.(*FrozenSetValue)
				other, err := operandFromIterable(args[0], "issuperset", ctx)
				if err != nil {
					return nil, err
				}
				super, err := frozensetSupersetOf(fs, other, ctx)
				if err != nil {
					return nil, err
				}
				return BoolValue(super), nil
			},
		},
		"isdisjoint": {
			Name:    "isdisjoint",
			Arity:   1,
			Doc:     "Check if two frozensets have no elements in common",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				fs := receiver.(*FrozenSetValue)
				other, ok := asSetOperand(args[0])
				if !ok {
					return nil, fmt.Errorf("isdisjoint() argument must be a set or frozenset")
				}
				disjoint := true
				var walkErr error
				fs.forEachEntry(func(h uint64, elem Value) bool {
					in, err := other.containsHashed(elem, h, ctx)
					if err != nil {
						walkErr = err
						return false
					}
					if in {
						disjoint = false
						return false
					}
					return true
				})
				if walkErr != nil {
					return nil, walkErr
				}
				return BoolValue(disjoint), nil
			},
		},
		"__len__": {
			Name:    "__len__",
			Arity:   0,
			Doc:     "Return the number of elements in the frozenset",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				fs := receiver.(*FrozenSetValue)
				return NumberValue(fs.Size()), nil
			},
		},
		"__contains__": {
			Name:    "__contains__",
			Arity:   1,
			Doc:     "Check if value is in frozenset",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("__contains__ takes exactly one argument")
				}
				fs := receiver.(*FrozenSetValue)
				in, err := fs.ContainsWithError(args[0], ctx)
				if err != nil {
					return nil, err
				}
				return BoolValue(in), nil
			},
		},
		"__iter__": {
			Name:    "__iter__",
			Arity:   0,
			Doc:     "Return an iterator for the frozenset",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				fs := receiver.(*FrozenSetValue)
				return fs.Iterator().(*setIterator), nil
			},
		},
		"__hash__": {
			Name:    "__hash__",
			Arity:   0,
			Doc:     "Return hash value for the frozenset",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				fs := receiver.(*FrozenSetValue)
				h, err := fs.hashWithCtx(ctx)
				if err != nil {
					return nil, err
				}
				return NumberValue(float64(h)), nil
			},
		},
		"__sub__": {
			Name:    "__sub__",
			Arity:   1,
			Doc:     "Return the difference of two frozensets (self - other)",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				fs := receiver.(*FrozenSetValue)
				other, err := requireSetOperand(args[0], "-")
				if err != nil {
					return nil, err
				}
				result := NewFrozenSet()
				var walkErr error
				fs.forEachEntry(func(h uint64, elem Value) bool {
					in, cErr := other.containsHashed(elem, h, ctx)
					if cErr != nil {
						walkErr = cErr
						return false
					}
					if !in {
						walkErr = result.addEntry(elem, h, ctx)
						return walkErr == nil
					}
					return true
				})
				if walkErr != nil {
					return nil, walkErr
				}
				return result, nil
			},
		},
		"__or__": {
			Name:    "__or__",
			Arity:   1,
			Doc:     "Return the union of two frozensets (self | other)",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				fs := receiver.(*FrozenSetValue)
				other, err := requireSetOperand(args[0], "|")
				if err != nil {
					return nil, err
				}
				result := NewFrozenSet()
				self, _ := asSetOperand(fs)
				if err := copyEntriesInto(self, result.addEntry, ctx); err != nil {
					return nil, err
				}
				if err := copyEntriesInto(other, result.addEntry, ctx); err != nil {
					return nil, err
				}
				return result, nil
			},
		},
		"__and__": {
			Name:    "__and__",
			Arity:   1,
			Doc:     "Return the intersection of two frozensets (self & other)",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				fs := receiver.(*FrozenSetValue)
				other, err := requireSetOperand(args[0], "&")
				if err != nil {
					return nil, err
				}
				result := NewFrozenSet()
				var walkErr error
				fs.forEachEntry(func(h uint64, elem Value) bool {
					in, cErr := other.containsHashed(elem, h, ctx)
					if cErr != nil {
						walkErr = cErr
						return false
					}
					if in {
						walkErr = result.addEntry(elem, h, ctx)
						return walkErr == nil
					}
					return true
				})
				if walkErr != nil {
					return nil, walkErr
				}
				return result, nil
			},
		},
		"__xor__": {
			Name:    "__xor__",
			Arity:   1,
			Doc:     "Return the symmetric difference of two frozensets (self ^ other)",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				fs := receiver.(*FrozenSetValue)
				other, err := requireSetOperand(args[0], "^")
				if err != nil {
					return nil, err
				}
				result := NewFrozenSet()
				var walkErr error

				fs.forEachEntry(func(h uint64, elem Value) bool {
					in, cErr := other.containsHashed(elem, h, ctx)
					if cErr != nil {
						walkErr = cErr
						return false
					}
					if !in {
						walkErr = result.addEntry(elem, h, ctx)
						return walkErr == nil
					}
					return true
				})
				if walkErr != nil {
					return nil, walkErr
				}

				other.forEachEntry(func(h uint64, elem Value) bool {
					in, cErr := fs.containsHashed(elem, h, ctx)
					if cErr != nil {
						walkErr = cErr
						return false
					}
					if !in {
						walkErr = result.addEntry(elem, h, ctx)
						return walkErr == nil
					}
					return true
				})
				if walkErr != nil {
					return nil, walkErr
				}
				return result, nil
			},
		},
	}
}

// setOperandItems returns the elements, membership test and size of a set or
// frozenset operand, so subset/superset comparisons accept either type
// (compat shim over the engine API; new code should use asSetOperand).
func setOperandItems(v Value) (items []Value, contains func(Value) bool, size int, ok bool) {
	switch s := v.(type) {
	case *FrozenSetValue:
		return s.Items(), s.Contains, s.Size(), true
	case *SetValue:
		return s.Items(), s.Contains, s.Size(), true
	}
	return nil, nil, 0, false
}
