// Package builtin provides standard library functions for the M28 language.
package modules

import (
	"fmt"
	"time"

	"github.com/mmichie/m28/core"
)

// DateTimeInstance represents a datetime object
type DateTimeInstance struct {
	*core.Instance
	Time time.Time
}

// RegisterDateTimeModule registers the datetime module
func InitDatetimeModule() *core.DictValue {
	// Create datetime module as a dict
	datetimeModule := core.NewDict()

	// Create datetime class
	datetimeClass := &core.Class{
		Name:       "datetime",
		Methods:    make(map[string]core.Value),
		Attributes: make(map[string]core.Value),
	}

	// Add class methods
	datetimeClass.Methods["now"] = core.NewBuiltinFunction(datetimeNow)
	datetimeClass.Methods["fromtimestamp"] = core.NewBuiltinFunction(datetimeFromTimestamp)

	// Add instance methods (these will be bound to instances)
	datetimeClass.Methods["timestamp"] = core.NewBuiltinFunction(datetimeTimestamp)
	datetimeClass.Methods["strftime"] = core.NewBuiltinFunction(datetimeStrftime)
	datetimeClass.Methods["__str__"] = core.NewBuiltinFunction(datetimeStr)

	// Register the class in the module
	datetimeModule.SetWithKey("datetime", core.StringValue("datetime"), datetimeClass)

	return datetimeModule
}

// datetimeNow creates a new datetime instance with current time
func datetimeNow(args []core.Value, ctx *core.Context) (core.Value, error) {
	// Create a new instance
	instance := core.NewInstance(&core.Class{Name: "datetime"})

	// Store the current time
	now := time.Now()
	instance.Attributes["_time"] = &timeValue{Time: now}

	// Add timestamp method that returns the Unix timestamp
	instance.Attributes["timestamp"] = core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.NumberValue(float64(now.Unix()) + float64(now.Nanosecond())/1e9), nil
	})

	// Add __str__ method
	instance.Attributes["__str__"] = core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.StringValue(now.Format("2006-01-02 15:04:05")), nil
	})

	return instance, nil
}

// datetimeFromTimestamp creates a datetime from a Unix timestamp
func datetimeFromTimestamp(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("datetime.fromtimestamp requires exactly 1 argument")
	}

	timestamp, ok := args[0].(core.NumberValue)
	if !ok {
		return nil, fmt.Errorf("datetime.fromtimestamp: timestamp must be a number")
	}

	// Create a new instance
	instance := core.NewInstance(&core.Class{Name: "datetime"})

	// Convert timestamp to time
	sec := int64(timestamp)
	nsec := int64((float64(timestamp) - float64(sec)) * 1e9)
	t := time.Unix(sec, nsec)
	instance.Attributes["_time"] = &timeValue{Time: t}

	// Add timestamp method
	instance.Attributes["timestamp"] = core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.NumberValue(float64(t.Unix()) + float64(t.Nanosecond())/1e9), nil
	})

	// Add __str__ method
	instance.Attributes["__str__"] = core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.StringValue(t.Format("2006-01-02 15:04:05")), nil
	})

	return instance, nil
}

// datetimeTimestamp returns the Unix timestamp
func datetimeTimestamp(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("timestamp method requires self argument")
	}

	// Extract the time from the instance
	instance, ok := args[0].(*core.Instance)
	if !ok {
		return nil, fmt.Errorf("timestamp: first argument must be a datetime instance")
	}

	timeVal, ok := instance.Attributes["_time"].(*timeValue)
	if !ok {
		return nil, fmt.Errorf("timestamp: invalid datetime instance")
	}

	return core.NumberValue(float64(timeVal.Time.Unix()) + float64(timeVal.Time.Nanosecond())/1e9), nil
}

// datetimeStrftime formats the datetime
func datetimeStrftime(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("strftime requires format argument")
	}

	// Extract the time from the instance
	instance, ok := args[0].(*core.Instance)
	if !ok {
		return nil, fmt.Errorf("strftime: first argument must be a datetime instance")
	}

	timeVal, ok := instance.Attributes["_time"].(*timeValue)
	if !ok {
		return nil, fmt.Errorf("strftime: invalid datetime instance")
	}

	format, ok := args[1].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("strftime: format must be a string")
	}

	// Convert Python format to Go format
	goFormat := pythonToGoTimeFormat(string(format))
	return core.StringValue(timeVal.Time.Format(goFormat)), nil
}

// datetimeStr returns string representation
func datetimeStr(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("__str__ method requires self argument")
	}

	// Extract the time from the instance
	instance, ok := args[0].(*core.Instance)
	if !ok {
		return nil, fmt.Errorf("__str__: first argument must be a datetime instance")
	}

	timeVal, ok := instance.Attributes["_time"].(*timeValue)
	if !ok {
		return nil, fmt.Errorf("__str__: invalid datetime instance")
	}

	return core.StringValue(timeVal.Time.Format("2006-01-02 15:04:05")), nil
}

// timeValue wraps a time.Time for storage in instance attributes
type timeValue struct {
	Time time.Time
}

func (t *timeValue) Type() core.Type {
	return core.Type("time")
}

func (t *timeValue) String() string {
	return t.Time.Format("2006-01-02 15:04:05")
}

// pythonToGoTimeFormat converts Python strftime format to Go format
func pythonToGoTimeFormat(pythonFormat string) string {
	// This is a simplified conversion - only handles common formats
	replacements := map[string]string{
		"%Y": "2006",
		"%y": "06",
		"%m": "01",
		"%d": "02",
		"%H": "15",
		"%I": "03",
		"%M": "04",
		"%S": "05",
		"%p": "PM",
		"%a": "Mon",
		"%A": "Monday",
		"%b": "Jan",
		"%B": "January",
		"%c": "Mon Jan 2 15:04:05 2006",
		"%x": "01/02/06",
		"%X": "15:04:05",
		"%%": "%",
	}

	result := pythonFormat
	for python, golang := range replacements {
		result = replaceAll(result, python, golang)
	}
	return result
}

// replaceAll replaces all occurrences of old with new in s
func replaceAll(s, old, new string) string {
	result := ""
	for i := 0; i < len(s); {
		if i+len(old) <= len(s) && s[i:i+len(old)] == old {
			result += new
			i += len(old)
		} else {
			result += string(s[i])
			i++
		}
	}
	return result
}
