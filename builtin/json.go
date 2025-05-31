// Package builtin provides standard library functions for the M28 language.
package builtin

import (
	"encoding/json"
	"fmt"
	"strings"

	"github.com/mmichie/m28/core"
)

// RegisterJSONModule registers the json module
func RegisterJSONModule(ctx *core.Context) {
	// Create module as a dict
	jsonModule := core.NewDict()

	// Register json functions
	jsonModule.SetWithKey("dumps", core.StringValue("dumps"), core.NewBuiltinFunction(jsonDumps))
	jsonModule.SetWithKey("loads", core.StringValue("loads"), core.NewBuiltinFunction(jsonLoads))
	jsonModule.SetWithKey("dump", core.StringValue("dump"), core.NewBuiltinFunction(jsonDump))
	jsonModule.SetWithKey("load", core.StringValue("load"), core.NewBuiltinFunction(jsonLoad))

	// Register the module in the module registry
	registry := core.GetModuleRegistry()
	registry.StoreModule("json", jsonModule, "<builtin>", []string{})
}

// jsonDumps converts an M28 value to a JSON string
func jsonDumps(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("json.dumps requires at least 1 argument")
	}

	// Check for indent parameter
	indent := 0
	if len(args) >= 2 {
		// Check if we have a named parameter for indent
		for i := 1; i < len(args); i++ {
			if num, ok := args[i].(core.NumberValue); ok {
				indent = int(num)
				break
			}
		}
	}

	// Convert M28 value to Go value
	goValue, err := m28ToGo(args[0])
	if err != nil {
		return nil, fmt.Errorf("json.dumps: %v", err)
	}

	// Marshal to JSON
	var jsonBytes []byte
	if indent > 0 {
		jsonBytes, err = json.MarshalIndent(goValue, "", strings.Repeat(" ", indent))
	} else {
		jsonBytes, err = json.Marshal(goValue)
	}
	if err != nil {
		return nil, fmt.Errorf("json.dumps: %v", err)
	}

	return core.StringValue(string(jsonBytes)), nil
}

// jsonLoads parses a JSON string into an M28 value
func jsonLoads(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("json.loads requires exactly 1 argument")
	}

	str, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("json.loads: argument must be a string")
	}

	// Parse JSON
	var goValue interface{}
	err := json.Unmarshal([]byte(str), &goValue)
	if err != nil {
		return nil, fmt.Errorf("json.loads: %v", err)
	}

	// Convert to M28 value
	return goToM28(goValue)
}

// jsonDump writes JSON to a file
func jsonDump(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("json.dump requires at least 2 arguments (obj, file)")
	}

	// Get the file object
	file, ok := args[1].(*core.File)
	if !ok {
		return nil, fmt.Errorf("json.dump: second argument must be a file object")
	}

	// Check for indent parameter
	indent := 0
	if len(args) >= 3 {
		if num, ok := args[2].(core.NumberValue); ok {
			indent = int(num)
		}
	}

	// Convert M28 value to Go value
	goValue, err := m28ToGo(args[0])
	if err != nil {
		return nil, fmt.Errorf("json.dump: %v", err)
	}

	// Marshal to JSON
	var jsonBytes []byte
	if indent > 0 {
		jsonBytes, err = json.MarshalIndent(goValue, "", strings.Repeat(" ", indent))
	} else {
		jsonBytes, err = json.Marshal(goValue)
	}
	if err != nil {
		return nil, fmt.Errorf("json.dump: %v", err)
	}

	// Write to file
	err = file.Write(string(jsonBytes))
	if err != nil {
		return nil, fmt.Errorf("json.dump: %v", err)
	}

	return core.NilValue{}, nil
}

// jsonLoad reads JSON from a file
func jsonLoad(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("json.load requires exactly 1 argument")
	}

	// Get the file object
	file, ok := args[0].(*core.File)
	if !ok {
		return nil, fmt.Errorf("json.load: argument must be a file object")
	}

	// Read all content
	content, err := file.Read(-1)
	if err != nil {
		return nil, fmt.Errorf("json.load: %v", err)
	}
	
	contentStr, ok := content.(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("json.load: unexpected content type")
	}

	// Parse JSON
	var goValue interface{}
	err = json.Unmarshal([]byte(contentStr), &goValue)
	if err != nil {
		return nil, fmt.Errorf("json.load: %v", err)
	}

	// Convert to M28 value
	return goToM28(goValue)
}

// m28ToGo converts an M28 value to a Go value for JSON encoding
func m28ToGo(v core.Value) (interface{}, error) {
	switch val := v.(type) {
	case core.NilValue:
		return nil, nil
	case core.BoolValue:
		return bool(val), nil
	case core.NumberValue:
		return float64(val), nil
	case core.StringValue:
		return string(val), nil
	case core.ListValue:
		result := make([]interface{}, len(val))
		for i, item := range val {
			goItem, err := m28ToGo(item)
			if err != nil {
				return nil, err
			}
			result[i] = goItem
		}
		return result, nil
	case *core.DictValue:
		result := make(map[string]interface{})
		keys := val.Keys()
		for _, k := range keys {
			v, _ := val.Get(k)
			// For JSON, we need string keys
			keyStr := k
			goValue, err := m28ToGo(v)
			if err != nil {
				return nil, err
			}
			result[keyStr] = goValue
		}
		return result, nil
	case core.TupleValue:
		// Convert tuples to lists for JSON
		result := make([]interface{}, len(val))
		for i, item := range val {
			goItem, err := m28ToGo(item)
			if err != nil {
				return nil, err
			}
			result[i] = goItem
		}
		return result, nil
	default:
		// For objects with to_dict method
		if obj, ok := v.(interface{ GetAttr(string) (core.Value, bool) }); ok {
			if method, found := obj.GetAttr("to_dict"); found {
				if callable, ok := method.(interface {
					Call([]core.Value, *core.Context) (core.Value, error)
				}); ok {
					// Call to_dict()
					dictValue, err := callable.Call([]core.Value{}, nil)
					if err != nil {
						return nil, err
					}
					return m28ToGo(dictValue)
				}
			}
		}
		return nil, fmt.Errorf("cannot convert %s to JSON", v.Type())
	}
}

// goToM28 converts a Go value from JSON decoding to an M28 value
func goToM28(v interface{}) (core.Value, error) {
	switch val := v.(type) {
	case nil:
		return core.NilValue{}, nil
	case bool:
		return core.BoolValue(val), nil
	case float64:
		return core.NumberValue(val), nil
	case string:
		return core.StringValue(val), nil
	case []interface{}:
		result := make(core.ListValue, len(val))
		for i, item := range val {
			m28Item, err := goToM28(item)
			if err != nil {
				return nil, err
			}
			result[i] = m28Item
		}
		return result, nil
	case map[string]interface{}:
		dict := core.NewDict()
		for key, value := range val {
			m28Value, err := goToM28(value)
			if err != nil {
				return nil, err
			}
			dict.SetWithKey(core.ValueToKey(core.StringValue(key)), core.StringValue(key), m28Value)
		}
		return dict, nil
	default:
		return nil, fmt.Errorf("unexpected JSON type: %T", v)
	}
}