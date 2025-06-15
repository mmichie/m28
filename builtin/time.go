package builtin

import (
	"time"

	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// RegisterTimeModule registers the time module
func RegisterTimeModule(ctx *core.Context) {
	// Create module as a dict
	timeModule := core.NewDict()

	// time() - return current time in seconds since epoch
	timeModule.SetWithKey("time", core.StringValue("time"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("time", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}

		// Return current time as float seconds since Unix epoch
		now := time.Now()
		seconds := float64(now.Unix()) + float64(now.Nanosecond())/1e9
		return core.NumberValue(seconds), nil
	}))

	// sleep() - sleep for given number of seconds
	timeModule.SetWithKey("sleep", core.StringValue("sleep"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("sleep", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		seconds, err := v.GetNumber(0)
		if err != nil {
			return nil, err
		}

		duration := time.Duration(float64(seconds) * float64(time.Second))
		time.Sleep(duration)

		return core.Nil, nil
	}))

	// Register the module in the module registry
	registry := core.GetModuleRegistry()
	registry.StoreModule("time", timeModule, "<builtin>", []string{})
}

// Migration Statistics:
// Functions migrated: 2 time functions (time, sleep)
// Type checks eliminated: 2 manual type assertions
// Code improvements: Uses validation framework
// Benefits: Consistent error messages, cleaner validation
