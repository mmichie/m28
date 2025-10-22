package modules

import (
	"time"

	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// RegisterTimeModule registers the time module
func InitTimeModule() *core.DictValue {
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

	// monotonic() - return monotonic time in seconds (Python 3.3+)
	// Used for measuring elapsed time, not affected by system clock adjustments
	var startTime = time.Now()
	timeModule.SetWithKey("monotonic", core.StringValue("monotonic"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("monotonic", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}

		// Return elapsed time since module load in seconds
		elapsed := time.Since(startTime)
		return core.NumberValue(elapsed.Seconds()), nil
	}))

	// strftime() - format time according to a format string
	timeModule.SetWithKey("strftime", core.StringValue("strftime"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("strftime", args)
		if err := v.Range(1, 2); err != nil {
			return nil, err
		}

		formatStr, err := v.GetString(0)
		if err != nil {
			return nil, err
		}

		// If no time tuple provided, use current time
		var t time.Time
		if v.Count() == 1 {
			t = time.Now()
		} else {
			// For now, just use current time
			// Full implementation would parse the time tuple argument
			t = time.Now()
		}

		// Simple conversion of common Python strftime formats to Go formats
		// This is a basic implementation - Python's strftime has many more format codes
		goFormat := formatStr
		goFormat = replaceStrftimeFormat(goFormat, "%Y", "2006")
		goFormat = replaceStrftimeFormat(goFormat, "%4Y", "2006") // glibc extension
		goFormat = replaceStrftimeFormat(goFormat, "%m", "01")
		goFormat = replaceStrftimeFormat(goFormat, "%d", "02")
		goFormat = replaceStrftimeFormat(goFormat, "%H", "15")
		goFormat = replaceStrftimeFormat(goFormat, "%M", "04")
		goFormat = replaceStrftimeFormat(goFormat, "%S", "05")

		result := t.Format(goFormat)
		return core.StringValue(result), nil
	}))

	return timeModule
}

// replaceStrftimeFormat is a helper to replace Python strftime format codes with Go format codes
func replaceStrftimeFormat(s, pythonFmt, goFmt string) string {
	// Simple string replacement - more sophisticated implementation would handle edge cases
	result := ""
	i := 0
	for i < len(s) {
		if i+len(pythonFmt) <= len(s) && s[i:i+len(pythonFmt)] == pythonFmt {
			result += goFmt
			i += len(pythonFmt)
		} else {
			result += string(s[i])
			i++
		}
	}
	return result
}

// Migration Statistics:
// Functions migrated: 2 time functions (time, sleep)
// Type checks eliminated: 2 manual type assertions
// Code improvements: Uses validation framework
// Benefits: Consistent error messages, cleaner validation
