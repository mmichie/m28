package modules

import "github.com/mmichie/m28/core"

// InitAbcModule creates the _abc module
// This provides C-level support for abstract base classes
func InitAbcModule() *core.DictValue {
	module := core.NewDict()

	// Module docstring
	module.Set("__doc__", core.StringValue("Abstract Base Classes support"))

	// _abc_init - initialize ABC machinery and compute __abstractmethods__.
	// CPython's C _abc_init calls compute_abstract_methods(cls); we mirror that:
	// the frozenset of names still abstract (defined abstract in this class's
	// namespace, or inherited abstract and not concretely overridden).
	module.Set("_abc_init", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return core.NilValue{}, nil
		}
		cls, ok := args[0].(*core.Class)
		if !ok {
			return core.NilValue{}, nil
		}

		// getattr builtin: used to read __isabstractmethod__ through descriptors
		// (property / DynamicClassAttribute) and to resolve inherited names across
		// the MRO -- like CPython's compute_abstract_methods.
		var gaCall func(args []core.Value) (core.Value, error)
		if gaVal, err := ctx.Lookup("getattr"); err == nil {
			if ga, ok := gaVal.(interface {
				Call([]core.Value, *core.Context) (core.Value, error)
			}); ok {
				gaCall = func(args []core.Value) (core.Value, error) { return ga.Call(args, ctx) }
			}
		}
		isAbstract := func(v core.Value) bool {
			if gaCall == nil {
				return false
			}
			r, err := gaCall([]core.Value{v, core.StringValue("__isabstractmethod__"), core.BoolValue(false)})
			if err != nil {
				return false
			}
			return core.IsTruthy(r)
		}
		// getAttrOf returns getattr(obj, name, None) -- MRO-aware resolution.
		getAttrOf := func(obj core.Value, name string) core.Value {
			if gaCall == nil {
				return core.None
			}
			r, err := gaCall([]core.Value{obj, core.StringValue(name), core.None})
			if err != nil {
				return core.None
			}
			return r
		}

		abstracts := core.NewFrozenSet()

		// 1. Abstract methods defined directly in this class's namespace.
		for name, v := range cls.Methods {
			if isAbstract(v) {
				abstracts.Add(core.StringValue(name))
			}
		}
		for name, v := range cls.Attributes {
			if name == "__abstractmethods__" {
				continue
			}
			if isAbstract(v) {
				abstracts.Add(core.StringValue(name))
			}
		}

		// 2. Abstract methods inherited from bases and not yet implemented.
		//    A name in a base's (transitive) __abstractmethods__ stays abstract
		//    unless this class resolves it to a CONCRETE implementation anywhere
		//    in its MRO -- including a sibling base (e.g. ItemsView gets __len__
		//    from MappingView while Set still lists it abstract). getattr(cls,
		//    name) is MRO-aware: a still-abstract name resolves to a value whose
		//    __isabstractmethod__ is true, or (for an abstract DynamicClassAttribute
		//    on a class) returns None -- both are kept abstract.
		seen := map[string]bool{}
		for _, base := range cls.Parents {
			baseAbs, ok := base.Attributes["__abstractmethods__"]
			if !ok {
				continue
			}
			fs, ok := baseAbs.(*core.FrozenSetValue)
			if !ok {
				continue
			}
			for it := fs.Iterator(); ; {
				nv, ok := it.Next()
				if !ok {
					break
				}
				ns, ok := nv.(core.StringValue)
				if !ok {
					continue
				}
				name := string(ns)
				if seen[name] {
					continue
				}
				seen[name] = true
				// (a) Overridden in THIS class's own namespace: abstract iff the
				//     override is itself abstract. Checked directly (not via
				//     getattr) because a concrete DynamicClassAttribute resolves
				//     to None on class access, indistinguishable from an abstract
				//     one -- so e.g. Okay1's concrete color must be seen here.
				if own, found := cls.Methods[name]; found {
					if isAbstract(own) {
						abstracts.Add(ns)
					}
					continue
				}
				if own, found := cls.Attributes[name]; found {
					if isAbstract(own) {
						abstracts.Add(ns)
					}
					continue
				}
				// (b) Not overridden here: resolve across the MRO. A concrete
				//     implementation from a sibling base (e.g. ItemsView's __len__
				//     from MappingView) clears it; None (an inherited abstract
				//     DynamicClassAttribute) or an abstract value keeps it.
				resolved := getAttrOf(cls, name)
				if _, isNone := resolved.(core.NilValue); !isNone && !isAbstract(resolved) {
					continue
				}
				abstracts.Add(ns)
			}
		}

		_ = cls.SetAttr("__abstractmethods__", abstracts)
		return core.NilValue{}, nil
	}))

	// _abc_register(cls, subclass) - register subclass as a virtual subclass of
	// the ABC cls, then return subclass (so ABCMeta.register works as a
	// decorator). Recorded on cls so isinstance/issubclass recognize it.
	module.Set("_abc_register", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 2 {
			return nil, core.NewTypeError("_abc_register", nil, "_abc_register() takes exactly 2 arguments")
		}
		if cls, ok := args[0].(*core.Class); ok {
			if sub, ok := args[1].(*core.Class); ok {
				// User class: record on the ABC directly.
				cls.RegisterVirtualSubclass(sub)
			} else if typeName, ok := core.TypeObjectName(args[1]); ok {
				// Builtin type (int/float/complex/...): record by name in the
				// global builtin-type registry (numbers.py registers these).
				core.RegisterBuiltinVirtualSubclass(typeName, cls)
			}
		}
		return args[1], nil
	}))

	// _abc_instancecheck - check if an instance is of a type
	module.Set("_abc_instancecheck", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, core.NewTypeError("_abc_instancecheck", nil, "_abc_instancecheck() takes exactly 2 arguments")
		}
		// Simple implementation - just use isinstance
		// In full implementation, this would check ABC registries
		return core.BoolValue(false), nil
	}))

	// _abc_subclasscheck - check if a class is a subclass
	module.Set("_abc_subclasscheck", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, core.NewTypeError("_abc_subclasscheck", nil, "_abc_subclasscheck() takes exactly 2 arguments")
		}
		// Simple implementation - just return false
		// In full implementation, this would check ABC registries
		return core.BoolValue(false), nil
	}))

	// _get_dump - get ABC cache dump (for debugging)
	module.Set("_get_dump", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Return empty list for now (tuple-like)
		return core.NewList(), nil
	}))

	// get_cache_token - get current ABC cache token
	module.Set("get_cache_token", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Return a simple incrementing token (0 for now)
		return core.NumberValue(0), nil
	}))

	// _reset_registry - reset ABC registry cache
	module.Set("_reset_registry", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// No-op for now - would clear ABC caches
		return core.NilValue{}, nil
	}))

	// _reset_caches - reset ABC internal caches
	module.Set("_reset_caches", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// No-op for now - would clear ABC internal caches
		return core.NilValue{}, nil
	}))

	return module
}
