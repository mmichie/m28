package modules

import (
	"bytes"
	"encoding/gob"
	"fmt"

	"github.com/mmichie/m28/core"
)

// InitPickleModule creates the _pickle C extension module
// This provides basic pickle functionality using Go's gob encoding
func InitPickleModule() *core.DictValue {
	pickleModule := core.NewDict()

	// Protocol constants
	pickleModule.Set("HIGHEST_PROTOCOL", core.NumberValue(5))
	pickleModule.Set("DEFAULT_PROTOCOL", core.NumberValue(4))

	// PickleError base exception - create a simple class
	pickleError := createPickleException("PickleError", nil)
	pickleModule.Set("PickleError", pickleError)

	// PicklingError - error during pickling
	picklingError := createPickleException("PicklingError", pickleError)
	pickleModule.Set("PicklingError", picklingError)

	// UnpicklingError - error during unpickling
	unpicklingError := createPickleException("UnpicklingError", pickleError)
	pickleModule.Set("UnpicklingError", unpicklingError)

	// dumps(obj, protocol=None, *, fix_imports=True) - serialize object to bytes
	pickleModule.Set("dumps", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("dumps() requires at least 1 argument")
		}

		// For now, use a simple serialization approach
		// This is a stub - real pickle protocol is much more complex
		data, err := serializeValue(args[0])
		if err != nil {
			return nil, fmt.Errorf("can't pickle object: %v", err)
		}

		return core.BytesValue(data), nil
	}))

	// loads(data, *, fix_imports=True, encoding="ASCII", errors="strict") - deserialize bytes
	pickleModule.Set("loads", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("loads() requires at least 1 argument")
		}

		var data []byte
		switch v := args[0].(type) {
		case core.BytesValue:
			data = []byte(v)
		case core.StringValue:
			data = []byte(v)
		default:
			return nil, fmt.Errorf("loads() argument must be bytes-like object")
		}

		result, err := deserializeValue(data)
		if err != nil {
			return nil, fmt.Errorf("unpickling error: %v", err)
		}

		return result, nil
	}))

	// dump(obj, file, protocol=None) - serialize to file
	pickleModule.Set("dump", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 2 {
			return nil, fmt.Errorf("dump() requires at least 2 arguments")
		}

		data, err := serializeValue(args[0])
		if err != nil {
			return nil, fmt.Errorf("can't pickle object: %v", err)
		}

		// Write to file object
		file := args[1]
		if writer, ok := file.(interface {
			GetAttr(string) (core.Value, bool)
		}); ok {
			if writeMethod, found := writer.GetAttr("write"); found {
				if callable, ok := writeMethod.(interface {
					Call([]core.Value, *core.Context) (core.Value, error)
				}); ok {
					_, err := callable.Call([]core.Value{core.BytesValue(data)}, ctx)
					if err != nil {
						return nil, err
					}
					return core.None, nil
				}
			}
		}

		return nil, fmt.Errorf("file argument must have a write method")
	}))

	// load(file) - deserialize from file
	pickleModule.Set("load", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("load() requires at least 1 argument")
		}

		file := args[0]
		if reader, ok := file.(interface {
			GetAttr(string) (core.Value, bool)
		}); ok {
			if readMethod, found := reader.GetAttr("read"); found {
				if callable, ok := readMethod.(interface {
					Call([]core.Value, *core.Context) (core.Value, error)
				}); ok {
					result, err := callable.Call([]core.Value{}, ctx)
					if err != nil {
						return nil, err
					}

					var data []byte
					switch v := result.(type) {
					case core.BytesValue:
						data = []byte(v)
					case core.StringValue:
						data = []byte(v)
					default:
						return nil, fmt.Errorf("file.read() must return bytes")
					}

					return deserializeValue(data)
				}
			}
		}

		return nil, fmt.Errorf("file argument must have a read method")
	}))

	// Pickler class stub
	pickleModule.Set("Pickler", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("Pickler() requires a file argument")
		}
		return createPicklerInstance(args[0], ctx)
	}))

	// Unpickler class stub
	pickleModule.Set("Unpickler", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("Unpickler() requires a file argument")
		}
		return createUnpicklerInstance(args[0], ctx)
	}))

	// PickleBuffer class - wraps a buffer for zero-copy pickling
	pickleModule.Set("PickleBuffer", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("PickleBuffer() requires a buffer argument")
		}
		return createPickleBufferInstance(args[0])
	}))

	return pickleModule
}

// createPickleException creates a simple exception class for pickle errors
func createPickleException(name string, base core.Value) core.Value {
	// Create a simple callable that returns an error message
	return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		msg := ""
		if len(args) > 0 {
			if s, ok := args[0].(core.StringValue); ok {
				msg = string(s)
			}
		}
		return core.StringValue(fmt.Sprintf("%s: %s", name, msg)), nil
	})
}

// serializeValue converts an M28 value to bytes using a simple format
func serializeValue(v core.Value) ([]byte, error) {
	var buf bytes.Buffer

	// Use Go's gob encoding for simplicity
	// Real pickle would use Python's pickle protocol
	enc := gob.NewEncoder(&buf)

	// Convert to a serializable form
	serializable, err := toSerializable(v)
	if err != nil {
		return nil, err
	}

	if err := enc.Encode(serializable); err != nil {
		return nil, err
	}

	return buf.Bytes(), nil
}

// toSerializable converts M28 values to Go values that can be gob-encoded
func toSerializable(v core.Value) (interface{}, error) {
	switch val := v.(type) {
	case core.NumberValue:
		return float64(val), nil
	case core.StringValue:
		return string(val), nil
	case core.BoolValue:
		return bool(val), nil
	case core.NilValue:
		return nil, nil
	case core.BytesValue:
		return []byte(val), nil
	case *core.ListValue:
		items := val.Items()
		result := make([]interface{}, len(items))
		for i, item := range items {
			ser, err := toSerializable(item)
			if err != nil {
				return nil, err
			}
			result[i] = ser
		}
		return result, nil
	case *core.DictValue:
		result := make(map[string]interface{})
		for _, key := range val.OriginalKeys() {
			v, _ := val.GetValue(key)
			keyStr := fmt.Sprintf("%v", key)
			ser, _ := toSerializable(v)
			result[keyStr] = ser
		}
		return result, nil
	case core.TupleValue:
		result := make([]interface{}, len(val))
		for i, item := range val {
			ser, err := toSerializable(item)
			if err != nil {
				return nil, err
			}
			result[i] = ser
		}
		return result, nil
	default:
		return nil, fmt.Errorf("cannot serialize type %s", v.Type())
	}
}

// deserializeValue converts bytes back to an M28 value
func deserializeValue(data []byte) (core.Value, error) {
	if len(data) == 0 {
		return core.None, nil
	}

	buf := bytes.NewReader(data)
	dec := gob.NewDecoder(buf)

	var result interface{}
	if err := dec.Decode(&result); err != nil {
		return nil, err
	}

	return fromSerializable(result)
}

// fromSerializable converts Go values back to M28 values
func fromSerializable(v interface{}) (core.Value, error) {
	if v == nil {
		return core.None, nil
	}

	switch val := v.(type) {
	case float64:
		return core.NumberValue(val), nil
	case string:
		return core.StringValue(val), nil
	case bool:
		return core.BoolValue(val), nil
	case []byte:
		return core.BytesValue(val), nil
	case []interface{}:
		items := make([]core.Value, len(val))
		for i, item := range val {
			v, err := fromSerializable(item)
			if err != nil {
				return nil, err
			}
			items[i] = v
		}
		return core.NewList(items...), nil
	case map[string]interface{}:
		result := core.NewDict()
		for k, v := range val {
			val, err := fromSerializable(v)
			if err != nil {
				return nil, err
			}
			result.Set(k, val)
		}
		return result, nil
	default:
		return nil, fmt.Errorf("cannot deserialize type %T", v)
	}
}

// createPicklerInstance creates a Pickler object
func createPicklerInstance(file core.Value, ctx *core.Context) (core.Value, error) {
	pickler := core.NewDict()

	// Store the file
	pickler.Set("_file", file)

	// dump method
	pickler.Set("dump", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("dump() requires an object argument")
		}

		data, err := serializeValue(args[0])
		if err != nil {
			return nil, err
		}

		// Get the file from the pickler
		storedFile, _ := pickler.Get("_file")
		if writer, ok := storedFile.(interface {
			GetAttr(string) (core.Value, bool)
		}); ok {
			if writeMethod, found := writer.GetAttr("write"); found {
				if callable, ok := writeMethod.(interface {
					Call([]core.Value, *core.Context) (core.Value, error)
				}); ok {
					_, err := callable.Call([]core.Value{core.BytesValue(data)}, ctx)
					return core.None, err
				}
			}
		}

		return nil, fmt.Errorf("file has no write method")
	}))

	return pickler, nil
}

// createUnpicklerInstance creates an Unpickler object
func createUnpicklerInstance(file core.Value, ctx *core.Context) (core.Value, error) {
	unpickler := core.NewDict()

	// Store the file
	unpickler.Set("_file", file)

	// load method
	unpickler.Set("load", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Get the file from the unpickler
		storedFile, _ := unpickler.Get("_file")
		if reader, ok := storedFile.(interface {
			GetAttr(string) (core.Value, bool)
		}); ok {
			if readMethod, found := reader.GetAttr("read"); found {
				if callable, ok := readMethod.(interface {
					Call([]core.Value, *core.Context) (core.Value, error)
				}); ok {
					result, err := callable.Call([]core.Value{}, ctx)
					if err != nil {
						return nil, err
					}

					var data []byte
					switch v := result.(type) {
					case core.BytesValue:
						data = []byte(v)
					case core.StringValue:
						data = []byte(v)
					default:
						return nil, fmt.Errorf("file.read() must return bytes")
					}

					return deserializeValue(data)
				}
			}
		}

		return nil, fmt.Errorf("file has no read method")
	}))

	return unpickler, nil
}

// createPickleBufferInstance creates a PickleBuffer object for zero-copy pickling
func createPickleBufferInstance(buffer core.Value) (core.Value, error) {
	pickleBuffer := core.NewDict()

	// Store the underlying buffer
	var data []byte
	switch v := buffer.(type) {
	case core.BytesValue:
		data = []byte(v)
	case core.StringValue:
		data = []byte(v)
	case *core.ListValue:
		// Could be a memoryview or array-like
		items := v.Items()
		data = make([]byte, len(items))
		for i, item := range items {
			if n, ok := item.(core.NumberValue); ok {
				data[i] = byte(int(n) & 0xFF)
			}
		}
	default:
		// Try to get buffer interface
		if obj, ok := buffer.(interface {
			GetAttr(string) (core.Value, bool)
		}); ok {
			if tobytes, found := obj.GetAttr("tobytes"); found {
				if callable, ok := tobytes.(interface {
					Call([]core.Value, *core.Context) (core.Value, error)
				}); ok {
					result, err := callable.Call([]core.Value{}, nil)
					if err != nil {
						return nil, err
					}
					if b, ok := result.(core.BytesValue); ok {
						data = []byte(b)
					}
				}
			}
		}
		if data == nil {
			return nil, fmt.Errorf("PickleBuffer requires a buffer-like object")
		}
	}

	pickleBuffer.Set("_data", core.BytesValue(data))

	// raw() method - return the underlying buffer
	pickleBuffer.Set("raw", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		d, _ := pickleBuffer.Get("_data")
		return d, nil
	}))

	// release() method - release the buffer
	pickleBuffer.Set("release", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		pickleBuffer.Set("_data", core.None)
		return core.None, nil
	}))

	return pickleBuffer, nil
}
