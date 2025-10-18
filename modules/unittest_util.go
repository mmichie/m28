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
