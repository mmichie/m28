package modules

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// simpleEnumWithKwargs wraps a BuiltinFunction to accept keyword arguments
type simpleEnumWithKwargs struct {
	*core.BuiltinFunction
}

// CallWithKeywords implements keyword argument support for _simple_enum
func (s *simpleEnumWithKwargs) CallWithKeywords(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
	// _simple_enum accepts keyword arguments but we ignore them in the stub
	// Just call the underlying function with positional args only
	return s.BuiltinFunction.Call(args, ctx)
}

// InitEnumModule initializes a stub enum module
// This provides minimal enum support for Python stdlib modules like re
func InitEnumModule() *core.DictValue {
	module := core.NewDict()

	// Create a simple Enum base class
	enumClass := core.NewClassWithParents("Enum", []*core.Class{})
	// Add __members__ as an empty dict (will be populated when enum members are created)
	enumClass.SetAttr("__members__", core.NewDict())
	module.Set("Enum", enumClass)

	// Create IntFlag class (subclass of Enum)
	intFlagClass := core.NewClassWithParents("IntFlag", []*core.Class{enumClass})

	// Add _convert_() classmethod for socket.py compatibility
	// socket.py calls IntFlag._convert_('MsgFlag', __name__, lambda_filter)
	intFlagClass.SetMethod("_convert_", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// For now, just return None - socket.py doesn't actually use the result
		// It just needs the method to exist to avoid AttributeError
		return core.None, nil
	}))

	module.Set("IntFlag", intFlagClass)

	// Create Flag class (subclass of Enum)
	flagClass := core.NewClassWithParents("Flag", []*core.Class{enumClass})
	module.Set("Flag", flagClass)

	// Create StrEnum class (subclass of Enum)
	strEnumClass := core.NewClassWithParents("StrEnum", []*core.Class{enumClass})
	module.Set("StrEnum", strEnumClass)

	// FlagBoundary enum values
	// For now, these are just string constants
	module.Set("STRICT", core.StringValue("STRICT"))
	module.Set("CONFORM", core.StringValue("CONFORM"))
	module.Set("EJECT", core.StringValue("EJECT"))
	module.Set("KEEP", core.StringValue("KEEP"))

	// Create FlagBoundary class
	flagBoundaryClass := core.NewClassWithParents("FlagBoundary", []*core.Class{strEnumClass})
	module.Set("FlagBoundary", flagBoundaryClass)

	// global_enum decorator - for now, just returns the class unchanged
	module.Set("global_enum", core.NewNamedBuiltinFunction("global_enum", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, core.NewTypeError("global_enum", nil, "global_enum() takes at least 1 argument")
		}
		// Just return the class unchanged
		return args[0], nil
	}))

	// global_flag_repr decorator
	module.Set("global_flag_repr", core.NewNamedBuiltinFunction("global_flag_repr", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, core.NewTypeError("global_flag_repr", nil, "global_flag_repr() takes at least 1 argument")
		}
		return args[0], nil
	}))

	// global_enum_repr decorator
	module.Set("global_enum_repr", core.NewNamedBuiltinFunction("global_enum_repr", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, core.NewTypeError("global_enum_repr", nil, "global_enum_repr() takes at least 1 argument")
		}
		return args[0], nil
	}))

	// global_str decorator
	module.Set("global_str", core.NewNamedBuiltinFunction("global_str", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, core.NewTypeError("global_str", nil, "global_str() takes at least 1 argument")
		}
		return args[0], nil
	}))

	// _simple_enum decorator factory
	// Returns a decorator that returns the class unchanged
	// _simple_enum(etype=Enum, *, boundary=None, use_args=None)
	simpleEnumFunc := core.NewNamedBuiltinFunction("_simple_enum", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Returns a decorator function
		decorator := core.NewNamedBuiltinFunction("_simple_enum_decorator", func(decoratorArgs []core.Value, decoratorCtx *core.Context) (core.Value, error) {
			if len(decoratorArgs) < 1 {
				return nil, core.NewTypeError("_simple_enum", nil, "_simple_enum decorator takes at least 1 argument")
			}
			// Just return the class unchanged
			return decoratorArgs[0], nil
		})
		return decorator, nil
	})
	// Create a wrapper that implements CallWithKeywords
	simpleEnumWrapper := &simpleEnumWithKwargs{
		BuiltinFunction: simpleEnumFunc,
	}
	module.Set("_simple_enum", simpleEnumWrapper)

	// auto() function - returns incrementing integers
	autoCounter := 0
	module.Set("auto", core.NewNamedBuiltinFunction("auto", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		autoCounter++
		return core.NumberValue(autoCounter), nil
	}))

	// nonmember decorator - marks a member as not part of the enum
	module.Set("nonmember", core.NewNamedBuiltinFunction("nonmember", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, core.NewTypeError("nonmember", nil, "nonmember() takes exactly 1 argument")
		}
		return args[0], nil
	}))

	// member decorator - marks a member as part of the enum
	module.Set("member", core.NewNamedBuiltinFunction("member", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, core.NewTypeError("member", nil, "member() takes exactly 1 argument")
		}
		return args[0], nil
	}))

	// property decorator for enum members
	module.Set("property", core.NewNamedBuiltinFunction("property", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, core.NewTypeError("property", nil, "property() takes at least 1 argument")
		}
		return args[0], nil
	}))

	// EnumType metaclass - implements enum member creation logic
	enumTypeClass := core.NewClassWithParents("EnumType", []*core.Class{})

	// Implement __new__ method for enum metaclass
	// This is called when creating an enum class: class MyEnum(IntEnum): A = 1
	enumTypeClass.SetMethod("__new__", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Args: (mcls, name, bases, namespace)
		if len(args) < 4 {
			return nil, fmt.Errorf("EnumType.__new__() takes at least 4 arguments")
		}

		className, ok := args[1].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("EnumType.__new__() class name must be a string")
		}

		bases, ok := args[2].(core.TupleValue)
		if !ok {
			return nil, fmt.Errorf("EnumType.__new__() bases must be a tuple")
		}

		namespace, ok := args[3].(*core.DictValue)
		if !ok {
			return nil, fmt.Errorf("EnumType.__new__() namespace must be a dict")
		}

		// Convert bases tuple to parent classes
		var parents []*core.Class
		for _, base := range bases {
			if baseClass, ok := base.(*core.Class); ok {
				parents = append(parents, baseClass)
			}
		}

		// Create the enum class
		enumClass := core.NewClassWithParents(string(className), parents)
		// Set the metaclass so that calling the enum class goes through EnumType.__call__
		enumClass.SetClassAttr("__class__", args[0]) // args[0] is the metaclass (EnumType)

		// Check if this is an IntEnum (inherits from IntEnum or has int in bases)
		isIntEnum := false
		for _, parent := range parents {
			if parent.Name == "IntEnum" || parent.Name == "Enum" {
				isIntEnum = true
				break
			}
		}

		// Create __members__ dict to store enum members
		members := core.NewDict()

		// Process namespace to create enum members
		memberCount := 0
		for _, keyStr := range namespace.Keys() {
			value, _ := namespace.Get(keyStr)

			// Skip dunder methods and regular methods
			if len(keyStr) >= 2 && keyStr[0] == '_' && keyStr[1] == '_' {
				// Copy dunder attributes to class
				enumClass.SetClassAttr(keyStr, value)
				continue
			}

			// Skip if it's a function/method
			if _, isFunc := value.(*core.BuiltinFunction); isFunc {
				enumClass.SetMethod(keyStr, value)
				continue
			}

			// This is an enum member - create an instance
			member := core.NewInstance(enumClass)

			// For IntEnum, use the provided value; for regular Enum, use auto-incrementing
			var memberValue core.Value
			if isIntEnum {
				// Try to extract integer value
				switch v := value.(type) {
				case core.NumberValue:
					memberValue = v
				case core.StringValue:
					// For _ParameterKind style where value is a description, use member count
					memberValue = core.NumberValue(float64(memberCount))
				default:
					memberValue = core.NumberValue(float64(memberCount))
				}
			} else {
				memberValue = value
			}

			// Set member attributes
			member.SetAttr("name", core.StringValue(keyStr))
			member.SetAttr("value", memberValue)
			member.SetAttr("_value_", memberValue)
			member.SetAttr("_name_", core.StringValue(keyStr))

			// For IntEnum, make it support int operations
			if isIntEnum {
				if numVal, ok := memberValue.(core.NumberValue); ok {
					// Add __int__ for int() conversion
					member.SetAttr("__int__", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
						return numVal, nil
					}))

					// Add __index__ for operator.index()
					member.SetAttr("__index__", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
						return numVal, nil
					}))

					// Add comparison operators
					// __lt__ (<)
					member.SetAttr("__lt__", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
						if len(args) < 1 {
							return nil, fmt.Errorf("__lt__() missing required argument")
						}
						// args[0] is other (self is already bound)
						other := args[0]
						// Try to get numeric value from other
						var otherNum float64
						if otherNumVal, ok := other.(core.NumberValue); ok {
							otherNum = float64(otherNumVal)
						} else if otherObj, ok := other.(core.Object); ok {
							if val, ok := otherObj.GetAttr("value"); ok {
								if valNum, ok := val.(core.NumberValue); ok {
									otherNum = float64(valNum)
								} else {
									return core.BoolValue(false), nil
								}
							} else {
								return core.BoolValue(false), nil
							}
						} else {
							return core.BoolValue(false), nil
						}
						return core.BoolValue(float64(numVal) < otherNum), nil
					}))

					// __le__ (<=)
					member.SetAttr("__le__", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
						if len(args) < 1 {
							return nil, fmt.Errorf("__le__() missing required argument")
						}
						other := args[0]
						var otherNum float64
						if otherNumVal, ok := other.(core.NumberValue); ok {
							otherNum = float64(otherNumVal)
						} else if otherObj, ok := other.(core.Object); ok {
							if val, ok := otherObj.GetAttr("value"); ok {
								if valNum, ok := val.(core.NumberValue); ok {
									otherNum = float64(valNum)
								} else {
									return core.BoolValue(false), nil
								}
							} else {
								return core.BoolValue(false), nil
							}
						} else {
							return core.BoolValue(false), nil
						}
						return core.BoolValue(float64(numVal) <= otherNum), nil
					}))

					// __gt__ (>)
					member.SetAttr("__gt__", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
						if len(args) < 1 {
							return nil, fmt.Errorf("__gt__() missing required argument")
						}
						other := args[0]
						var otherNum float64
						if otherNumVal, ok := other.(core.NumberValue); ok {
							otherNum = float64(otherNumVal)
						} else if otherObj, ok := other.(core.Object); ok {
							if val, ok := otherObj.GetAttr("value"); ok {
								if valNum, ok := val.(core.NumberValue); ok {
									otherNum = float64(valNum)
								} else {
									return core.BoolValue(false), nil
								}
							} else {
								return core.BoolValue(false), nil
							}
						} else {
							return core.BoolValue(false), nil
						}
						return core.BoolValue(float64(numVal) > otherNum), nil
					}))

					// __ge__ (>=)
					member.SetAttr("__ge__", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
						if len(args) < 1 {
							return nil, fmt.Errorf("__ge__() missing required argument")
						}
						other := args[0]
						var otherNum float64
						if otherNumVal, ok := other.(core.NumberValue); ok {
							otherNum = float64(otherNumVal)
						} else if otherObj, ok := other.(core.Object); ok {
							if val, ok := otherObj.GetAttr("value"); ok {
								if valNum, ok := val.(core.NumberValue); ok {
									otherNum = float64(valNum)
								} else {
									return core.BoolValue(false), nil
								}
							} else {
								return core.BoolValue(false), nil
							}
						} else {
							return core.BoolValue(false), nil
						}
						return core.BoolValue(float64(numVal) >= otherNum), nil
					}))

					// __eq__ (==)
					member.SetAttr("__eq__", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
						if len(args) < 1 {
							return nil, fmt.Errorf("__eq__() missing required argument")
						}
						other := args[0]
						var otherNum float64
						if otherNumVal, ok := other.(core.NumberValue); ok {
							otherNum = float64(otherNumVal)
						} else if otherObj, ok := other.(core.Object); ok {
							if val, ok := otherObj.GetAttr("value"); ok {
								if valNum, ok := val.(core.NumberValue); ok {
									otherNum = float64(valNum)
								} else {
									return core.BoolValue(false), nil
								}
							} else {
								return core.BoolValue(false), nil
							}
						} else {
							return core.BoolValue(false), nil
						}
						return core.BoolValue(float64(numVal) == otherNum), nil
					}))
				}
			}

			// Add the member to the class and __members__ dict
			enumClass.SetClassAttr(keyStr, member)
			members.Set(keyStr, member)
			memberCount++
		}

		// Set __members__ on the class
		enumClass.SetAttr("__members__", members)

		// Create a value-to-member map for singleton lookup
		// This enables Color(1) to return the existing Color.RED instance
		valueMap := core.NewDict()
		for _, keyStr := range members.Keys() {
			if member, ok := members.Get(keyStr); ok {
				if memberObj, ok := member.(core.Object); ok {
					if val, ok := memberObj.GetAttr("value"); ok {
						valueMap.SetValue(val, member)
					}
				}
			}
		}
		enumClass.SetClassAttr("_value2member_map_", valueMap)

		return enumClass, nil
	}))

	// Implement __call__ on EnumType metaclass to return singleton instances
	// When you call Color(1), Python calls EnumType.__call__(Color, 1)
	enumTypeClass.SetMethod("__call__", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Args: (cls, value)
		if len(args) < 2 {
			return nil, fmt.Errorf("enum() takes at least 1 argument")
		}

		cls := args[0]
		value := args[1]

		// If value is already an instance of this enum class, return it unchanged
		// This allows Parameter("self", POSITIONAL_OR_KEYWORD) to work
		if inst, ok := value.(*core.Instance); ok {
			if inst.Class == cls {
				return value, nil
			}
		}

		// Get the _value2member_map_ from the class
		if classObj, ok := cls.(core.Object); ok {
			if valueMapVal, ok := classObj.GetAttr("_value2member_map_"); ok {
				if valueMap, ok := valueMapVal.(*core.DictValue); ok {
					// Look up member by value
					if member, ok := valueMap.GetValue(value); ok {
						return member, nil
					}
				}
			}
		}

		return nil, fmt.Errorf("'%v' is not a valid enum value", value)
	}))

	module.Set("EnumType", enumTypeClass)

	// EnumMeta metaclass (alias for EnumType)
	module.Set("EnumMeta", enumTypeClass)

	// ReprEnum class
	reprEnumClass := core.NewClassWithParents("ReprEnum", []*core.Class{enumClass})
	module.Set("ReprEnum", reprEnumClass)

	// IntEnum class - uses EnumType as metaclass
	// IntEnum is a base class for creating integer-valued enums
	// When you do `class MyEnum(IntEnum): A = 1`, EnumType.__new__ is called
	intEnumClass := core.NewClassWithParents("IntEnum", []*core.Class{enumClass})
	// Mark this class as using EnumType as its metaclass
	// This will cause child classes to use EnumType.__new__
	intEnumClass.SetClassAttr("__class__", enumTypeClass)
	// Add __members__ as an empty dict (will be populated by subclasses)
	intEnumClass.SetAttr("__members__", core.NewDict())

	// Add _convert_() classmethod to IntEnum
	// This is used by signal.py to convert module-level constants into enum members
	// For our minimal stub, we just return None to indicate success
	intEnumClass.SetMethod("_convert_", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// signal.py calls: _IntEnum._convert_('Signals', __name__, lambda_filter)
		// We don't need to actually do anything - just return None to indicate success
		return core.None, nil
	}))

	module.Set("IntEnum", intEnumClass)

	// unique decorator - ensures all enum values are unique
	module.Set("unique", core.NewNamedBuiltinFunction("unique", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, core.NewTypeError("unique", nil, "unique() takes exactly 1 argument")
		}
		return args[0], nil
	}))

	// verify decorator - verifies enum constraints
	module.Set("verify", core.NewNamedBuiltinFunction("verify", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, core.NewTypeError("verify", nil, "verify() takes at least 1 argument")
		}
		// Returns a decorator
		decorator := core.NewNamedBuiltinFunction("verify_decorator", func(decoratorArgs []core.Value, decoratorCtx *core.Context) (core.Value, error) {
			if len(decoratorArgs) < 1 {
				return nil, core.NewTypeError("verify", nil, "verify decorator takes at least 1 argument")
			}
			return decoratorArgs[0], nil
		})
		return decorator, nil
	}))

	// CONTINUOUS, NAMED_FLAGS, UNIQUE - verification flags
	module.Set("CONTINUOUS", core.StringValue("CONTINUOUS"))
	module.Set("NAMED_FLAGS", core.StringValue("NAMED_FLAGS"))
	module.Set("UNIQUE", core.StringValue("UNIQUE"))

	return module
}
