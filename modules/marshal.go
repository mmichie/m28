package modules

import (
	"github.com/mmichie/m28/core"
)

// InitMarshalModule creates and returns the marshal module stub
// This is a minimal stub implementation of Python's marshal module for bytecode serialization
func InitMarshalModule() *core.DictValue {
	marshalModule := core.NewDict()

	// dump - serialize object to file
	marshalModule.Set("dump", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// No-op for now
		return core.NilValue{}, nil
	}))

	// load - deserialize object from file
	marshalModule.Set("load", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Return None for now
		return core.NilValue{}, nil
	}))

	// dumps - serialize object to bytes
	marshalModule.Set("dumps", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Return empty bytes for now
		return core.BytesValue([]byte{}), nil
	}))

	// loads - deserialize object from bytes
	marshalModule.Set("loads", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Return None for now
		return core.NilValue{}, nil
	}))

	// version - marshal format version
	marshalModule.Set("version", core.NumberValue(4))

	return marshalModule
}
