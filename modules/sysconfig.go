package modules

import (
	"github.com/mmichie/m28/core"
)

// InitSysconfigModule creates a minimal sysconfig module stub
// This provides just enough functionality for test.support to work
func InitSysconfigModule() *core.DictValue {
	sysconfigModule := core.NewDict()

	// get_config_var - returns configuration variables
	sysconfigModule.SetWithKey("get_config_var", core.StringValue("get_config_var"),
		core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			// For now, return None for all config vars
			// This is safe - test.support checks these values with "or ''"
			return core.None, nil
		}))

	// is_python_build - returns False (we're not a python build)
	sysconfigModule.SetWithKey("is_python_build", core.StringValue("is_python_build"),
		core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.False, nil
		}))

	// customize_compiler - no-op for now
	sysconfigModule.SetWithKey("customize_compiler", core.StringValue("customize_compiler"),
		core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.None, nil
		}))

	return sysconfigModule
}
