package builtin

import (
	"fmt"
	"time"

	"github.com/mmichie/m28/core"
)

// RegisterTimeModule registers the time module
func RegisterTimeModule(ctx *core.Context) {
	// Create module as a dict
	timeModule := core.NewDict()

	// time() - return current time in seconds since epoch
	timeModule.SetWithKey("time", core.StringValue("time"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 0 {
			return nil, fmt.Errorf("time() takes no arguments")
		}
		
		// Return current time as float seconds since Unix epoch
		now := time.Now()
		seconds := float64(now.Unix()) + float64(now.Nanosecond())/1e9
		return core.NumberValue(seconds), nil
	}))

	// sleep() - sleep for given number of seconds
	timeModule.SetWithKey("sleep", core.StringValue("sleep"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("sleep() takes exactly one argument")
		}
		
		seconds, ok := args[0].(core.NumberValue)
		if !ok {
			return nil, fmt.Errorf("sleep() argument must be a number")
		}
		
		duration := time.Duration(float64(seconds) * float64(time.Second))
		time.Sleep(duration)
		
		return core.Nil, nil
	}))

	// Register the module in the module registry
	registry := core.GetModuleRegistry()
	registry.StoreModule("time", timeModule, "<builtin>", []string{})
}