package builtin

import (
	"bufio"
	"encoding/json"
	"fmt"
	"os"
	"strings"

	"github.com/mmichie/m28/common/builders"
	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// RegisterJSONL registers JSONL streaming functions
func RegisterJSONL(ctx *core.Context) {
	ctx.Define("parse-jsonl-line", core.NewNamedBuiltinFunction("parse-jsonl-line", ParseJSONLLineBuilder()))
	ctx.Define("format-jsonl-line", core.NewNamedBuiltinFunction("format-jsonl-line", FormatJSONLLineBuilder()))
	ctx.Define("read-jsonl", core.NewNamedBuiltinFunction("read-jsonl", ReadJSONLBuilder()))
	ctx.Define("write-jsonl", core.NewNamedBuiltinFunction("write-jsonl", WriteJSONLBuilder()))
	ctx.Define("append-jsonl-line", core.NewNamedBuiltinFunction("append-jsonl-line", AppendJSONLLineBuilder()))
}

// ParseJSONLLineBuilder implements parse-jsonl-line function
// (parse-jsonl-line line) - parse single JSONL line into object
func ParseJSONLLineBuilder() builders.BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("parse-jsonl-line", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		// Get line string
		lineArg := v.Get(0)
		line, ok := types.AsString(lineArg)
		if !ok {
			return nil, &core.TypeError{Message: fmt.Sprintf("parse-jsonl-line requires string argument, got %s", lineArg.Type())}
		}

		// Trim whitespace
		line = strings.TrimSpace(line)

		// Skip empty lines
		if line == "" {
			return core.Nil, nil
		}

		// Parse JSON
		var goValue interface{}
		if err := json.Unmarshal([]byte(line), &goValue); err != nil {
			return nil, &core.ValueError{Message: fmt.Sprintf("parse-jsonl-line: invalid JSON: %v", err)}
		}

		// Convert to M28 value
		result, err := goToM28Value(goValue)
		if err != nil {
			return nil, fmt.Errorf("parse-jsonl-line: %w", err)
		}

		return result, nil
	}
}

// FormatJSONLLineBuilder implements format-jsonl-line function
// (format-jsonl-line obj) - format object as JSONL line (with newline)
func FormatJSONLLineBuilder() builders.BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("format-jsonl-line", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		// Get object
		obj := v.Get(0)

		// Convert to Go value
		goValue, err := m28ValueToGo(obj)
		if err != nil {
			return nil, fmt.Errorf("format-jsonl-line: %w", err)
		}

		// Convert to JSON
		jsonBytes, err := json.Marshal(goValue)
		if err != nil {
			return nil, fmt.Errorf("format-jsonl-line: %w", err)
		}

		// Add newline
		return core.StringValue(string(jsonBytes) + "\n"), nil
	}
}

// ReadJSONLBuilder implements read-jsonl function
// (read-jsonl filename [skip-errors]) - read JSONL file, return list of objects
func ReadJSONLBuilder() builders.BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("read-jsonl", args)
		if err := v.Range(1, 2); err != nil {
			return nil, err
		}

		// Get filename
		filenameArg := v.Get(0)
		filename, ok := types.AsString(filenameArg)
		if !ok {
			return nil, &core.TypeError{Message: fmt.Sprintf("read-jsonl requires string filename, got %s", filenameArg.Type())}
		}

		// Check for skip-errors keyword argument
		skipErrors := false
		if v.Count() == 2 {
			kwArg := v.Get(1)
			// Check if it's a symbol starting with ":"
			if sym, ok := kwArg.(core.SymbolValue); ok {
				if string(sym) == ":skip-errors" || string(sym) == "skip-errors" {
					skipErrors = true
				}
			} else if b, ok := types.AsBool(kwArg); ok {
				skipErrors = b
			}
		}

		// Open file
		file, err := os.Open(filename)
		if err != nil {
			return nil, &core.OSError{Message: fmt.Sprintf("read-jsonl: failed to open file: %v", err)}
		}
		defer file.Close()

		// Read lines
		scanner := bufio.NewScanner(file)
		results := make([]core.Value, 0)
		lineNum := 0

		for scanner.Scan() {
			lineNum++
			line := strings.TrimSpace(scanner.Text())

			// Skip empty lines
			if line == "" {
				continue
			}

			// Parse JSON
			var goValue interface{}
			if err := json.Unmarshal([]byte(line), &goValue); err != nil {
				if skipErrors {
					// Log warning but continue
					fmt.Fprintf(os.Stderr, "Warning: Skipping invalid JSON on line %d: %v\n", lineNum, err)
					continue
				}
				return nil, &core.ValueError{Message: fmt.Sprintf("read-jsonl: invalid JSON on line %d: %v", lineNum, err)}
			}

			// Convert to M28 value
			obj, err := goToM28Value(goValue)
			if err != nil {
				if skipErrors {
					fmt.Fprintf(os.Stderr, "Warning: Skipping line %d: %v\n", lineNum, err)
					continue
				}
				return nil, fmt.Errorf("read-jsonl: line %d: %w", lineNum, err)
			}

			results = append(results, obj)
		}

		if err := scanner.Err(); err != nil {
			return nil, &core.OSError{Message: fmt.Sprintf("read-jsonl: error reading file: %v", err)}
		}

		return core.NewList(results...), nil
	}
}

// WriteJSONLBuilder implements write-jsonl function
// (write-jsonl filename data [mode]) - write list of objects to JSONL file
// mode can be "write" (default) or "append"
func WriteJSONLBuilder() builders.BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("write-jsonl", args)
		if err := v.Range(2, 3); err != nil {
			return nil, err
		}

		// Get filename
		filenameArg := v.Get(0)
		filename, ok := types.AsString(filenameArg)
		if !ok {
			return nil, &core.TypeError{Message: fmt.Sprintf("write-jsonl requires string filename, got %s", filenameArg.Type())}
		}

		// Get data (must be list)
		dataArg := v.Get(1)
		data, ok := types.AsList(dataArg)
		if !ok {
			return nil, &core.TypeError{Message: fmt.Sprintf("write-jsonl requires list as second argument, got %s", dataArg.Type())}
		}

		// Get mode (write or append)
		mode := "write"
		if v.Count() == 3 {
			modeArg := v.Get(2)
			if sym, ok := modeArg.(core.SymbolValue); ok {
				mode = strings.TrimPrefix(string(sym), ":")
			} else if s, ok := types.AsString(modeArg); ok {
				mode = s
			}
		}

		// Open file with appropriate flags
		var flags int
		if mode == "append" {
			flags = os.O_APPEND | os.O_CREATE | os.O_WRONLY
		} else {
			flags = os.O_CREATE | os.O_WRONLY | os.O_TRUNC
		}

		file, err := os.OpenFile(filename, flags, 0644)
		if err != nil {
			return nil, &core.OSError{Message: fmt.Sprintf("write-jsonl: failed to open file: %v", err)}
		}
		defer file.Close()

		// Write each object as JSONL
		writer := bufio.NewWriter(file)
		for i, obj := range data.Items() {
			// Convert to Go value
			goValue, err := m28ValueToGo(obj)
			if err != nil {
				return nil, fmt.Errorf("write-jsonl: failed to convert object at index %d: %w", i, err)
			}

			// Convert to JSON
			jsonBytes, err := json.Marshal(goValue)
			if err != nil {
				return nil, fmt.Errorf("write-jsonl: failed to format object at index %d: %w", i, err)
			}

			// Write with newline
			if _, err := writer.WriteString(string(jsonBytes) + "\n"); err != nil {
				return nil, &core.OSError{Message: fmt.Sprintf("write-jsonl: failed to write line %d: %v", i+1, err)}
			}
		}

		// Flush buffer
		if err := writer.Flush(); err != nil {
			return nil, &core.OSError{Message: fmt.Sprintf("write-jsonl: failed to flush buffer: %v", err)}
		}

		return core.Nil, nil
	}
}

// AppendJSONLLineBuilder implements append-jsonl-line function
// (append-jsonl-line filename obj) - append single object to JSONL file
func AppendJSONLLineBuilder() builders.BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("append-jsonl-line", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		// Get filename
		filenameArg := v.Get(0)
		filename, ok := types.AsString(filenameArg)
		if !ok {
			return nil, &core.TypeError{Message: fmt.Sprintf("append-jsonl-line requires string filename, got %s", filenameArg.Type())}
		}

		// Get object
		obj := v.Get(1)

		// Open file in append mode
		file, err := os.OpenFile(filename, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0644)
		if err != nil {
			return nil, &core.OSError{Message: fmt.Sprintf("append-jsonl-line: failed to open file: %v", err)}
		}
		defer file.Close()

		// Convert to Go value
		goValue, err := m28ValueToGo(obj)
		if err != nil {
			return nil, fmt.Errorf("append-jsonl-line: failed to convert object: %w", err)
		}

		// Convert to JSON
		jsonBytes, err := json.Marshal(goValue)
		if err != nil {
			return nil, fmt.Errorf("append-jsonl-line: failed to format object: %w", err)
		}

		// Write with newline
		if _, err := file.WriteString(string(jsonBytes) + "\n"); err != nil {
			return nil, &core.OSError{Message: fmt.Sprintf("append-jsonl-line: failed to write: %v", err)}
		}

		return core.Nil, nil
	}
}

// Helper functions for JSON conversion

// goToM28Value converts a Go value from JSON decoding to an M28 value
func goToM28Value(v interface{}) (core.Value, error) {
	switch val := v.(type) {
	case nil:
		return core.Nil, nil
	case bool:
		return core.BoolValue(val), nil
	case float64:
		return core.NumberValue(val), nil
	case string:
		return core.StringValue(val), nil
	case []interface{}:
		result := make([]core.Value, len(val))
		for i, item := range val {
			m28Item, err := goToM28Value(item)
			if err != nil {
				return nil, err
			}
			result[i] = m28Item
		}
		return core.NewList(result...), nil
	case map[string]interface{}:
		dict := core.NewDict()
		for key, value := range val {
			m28Value, err := goToM28Value(value)
			if err != nil {
				return nil, err
			}
			dict.SetWithKey(core.ValueToKey(core.StringValue(key)), core.StringValue(key), m28Value)
		}
		return dict, nil
	default:
		return nil, &core.TypeError{Message: fmt.Sprintf("unexpected JSON type: %T", v)}
	}
}

// m28ValueToGo converts an M28 value to a Go value for JSON encoding
func m28ValueToGo(v core.Value) (interface{}, error) {
	// Check nil first
	if types.IsNil(v) {
		return nil, nil
	}

	// Try basic types with helpers
	if b, ok := types.AsBool(v); ok {
		return b, nil
	}
	if n, ok := types.AsNumber(v); ok {
		return n, nil
	}
	if s, ok := types.AsString(v); ok {
		return s, nil
	}
	if list, ok := types.AsList(v); ok {
		result := make([]interface{}, list.Len())
		for i, item := range list.Items() {
			goItem, err := m28ValueToGo(item)
			if err != nil {
				return nil, err
			}
			result[i] = goItem
		}
		return result, nil
	}
	if dict, ok := types.AsDict(v); ok {
		result := make(map[string]interface{})
		keys := dict.Keys()
		for _, k := range keys {
			v, _ := dict.Get(k)
			// For JSON, we need string keys
			keyStr := strings.TrimPrefix(k, "s:") // Remove s: prefix if present
			goValue, err := m28ValueToGo(v)
			if err != nil {
				return nil, err
			}
			result[keyStr] = goValue
		}
		return result, nil
	}
	if tuple, ok := types.AsTuple(v); ok {
		// Convert tuples to lists for JSON
		result := make([]interface{}, len(tuple))
		for i, item := range tuple {
			goItem, err := m28ValueToGo(item)
			if err != nil {
				return nil, err
			}
			result[i] = goItem
		}
		return result, nil
	}

	return nil, &core.TypeError{Message: fmt.Sprintf("cannot convert %s to JSON", v.Type())}
}
