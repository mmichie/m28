package modules

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// InitUnittestUtilModule provides a stub for unittest.util
// This avoids parsing issues with slice notation in the Python source
func InitUnittestUtilModule() *core.DictValue {
	utilModule := core.NewDict()

	// strclass returns a string representation of a class
	utilModule.SetWithKey("strclass", core.StringValue("strclass"),
		core.NewBuiltinFunction(strclass))

	// safe_repr returns a safe representation of an object
	utilModule.SetWithKey("safe_repr", core.StringValue("safe_repr"),
		core.NewBuiltinFunction(safeRepr))

	// _count_diff_all_purpose - count differences (stub)
	utilModule.SetWithKey("_count_diff_all_purpose", core.StringValue("_count_diff_all_purpose"),
		core.NewBuiltinFunction(countDiffAllPurpose))

	// _count_diff_hashable - count differences for hashable items (stub)
	utilModule.SetWithKey("_count_diff_hashable", core.StringValue("_count_diff_hashable"),
		core.NewBuiltinFunction(countDiffHashable))

	// _common_shorten_repr - shorten repr output (stub)
	utilModule.SetWithKey("_common_shorten_repr", core.StringValue("_common_shorten_repr"),
		core.NewBuiltinFunction(commonShortenRepr))

	return utilModule
}

func strclass(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("strclass() requires 1 argument")
	}

	// Try to get __module__ and __qualname__ from the class object
	cls, ok := args[0].(*core.DictValue)
	if !ok {
		// Not a dict, just return string representation
		return core.StringValue(fmt.Sprintf("%v", args[0])), nil
	}

	// Get __module__
	moduleVal, hasModule := cls.Get("__module__")
	if !hasModule {
		moduleVal, hasModule = cls.Get("s:__module__")
	}
	var moduleName string
	if hasModule {
		if modStr, ok := moduleVal.(core.StringValue); ok {
			moduleName = string(modStr)
		}
	}

	// Get __qualname__
	qualnameVal, hasQualname := cls.Get("__qualname__")
	if !hasQualname {
		qualnameVal, hasQualname = cls.Get("s:__qualname__")
	}
	var qualname string
	if hasQualname {
		if qnStr, ok := qualnameVal.(core.StringValue); ok {
			qualname = string(qnStr)
		}
	}

	// Format as "module.qualname"
	if moduleName != "" && qualname != "" {
		return core.StringValue(fmt.Sprintf("%s.%s", moduleName, qualname)), nil
	} else if qualname != "" {
		return core.StringValue(qualname), nil
	} else {
		return core.StringValue(fmt.Sprintf("%v", args[0])), nil
	}
}

func safeRepr(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("safe_repr() requires at least 1 argument")
	}

	obj := args[0]

	// Optional short parameter (default False)
	// If provided, limits the representation length
	// For now, we'll ignore it and just return the string representation

	// Return the string representation of the object
	return core.StringValue(obj.String()), nil
}

func countDiffAllPurpose(args []core.Value, ctx *core.Context) (core.Value, error) {
	// Stub: returns two dicts of item counts
	// In reality, this counts items in two sequences for diff comparison
	// For now, return empty dicts
	missing := core.NewDict()
	unexpected := core.NewDict()

	// Return tuple of (missing, unexpected)
	return core.TupleValue{missing, unexpected}, nil
}

func countDiffHashable(args []core.Value, ctx *core.Context) (core.Value, error) {
	// Stub: same as countDiffAllPurpose but for hashable items
	missing := core.NewDict()
	unexpected := core.NewDict()

	return core.TupleValue{missing, unexpected}, nil
}

func commonShortenRepr(args []core.Value, ctx *core.Context) (core.Value, error) {
	// Stub: returns shortened repr
	// In reality, this finds common prefix/suffix and shortens output
	if len(args) < 1 {
		return nil, fmt.Errorf("_common_shorten_repr() requires at least 1 argument")
	}

	// For now, just return the string as-is
	s1 := args[0].String()
	s2 := ""
	if len(args) >= 2 {
		s2 = args[1].String()
	}

	// Return tuple of (shortened_s1, shortened_s2)
	return core.TupleValue{core.StringValue(s1), core.StringValue(s2)}, nil
}
