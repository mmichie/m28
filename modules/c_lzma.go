package modules

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// Init_LzmaModule creates a minimal stub for _lzma C extension module
// This module provides LZMA/XZ compression/decompression functionality
// Currently returns NotImplementedError for all functions to allow imports
func Init_LzmaModule() *core.DictValue {
	module := core.NewDict()

	// Module metadata
	module.Set("__name__", core.StringValue("_lzma"))
	module.Set("__doc__", core.StringValue("LZMA compression/decompression (stub)"))

	// LZMACompressor class stub
	module.Set("LZMACompressor", core.NewClassWithParents("LZMACompressor", []*core.Class{}))

	// LZMADecompressor class stub
	module.Set("LZMADecompressor", core.NewClassWithParents("LZMADecompressor", []*core.Class{}))

	// Constants
	module.Set("CHECK_NONE", core.NumberValue(0))
	module.Set("CHECK_CRC32", core.NumberValue(1))
	module.Set("CHECK_CRC64", core.NumberValue(4))
	module.Set("CHECK_SHA256", core.NumberValue(10))
	module.Set("FILTER_LZMA1", core.NumberValue(0x4000000000000001))
	module.Set("FILTER_LZMA2", core.NumberValue(0x21))
	module.Set("FORMAT_AUTO", core.NumberValue(0))
	module.Set("FORMAT_XZ", core.NumberValue(1))
	module.Set("FORMAT_ALONE", core.NumberValue(2))
	module.Set("FORMAT_RAW", core.NumberValue(3))
	module.Set("MF_HC3", core.NumberValue(0x03))
	module.Set("MF_HC4", core.NumberValue(0x04))
	module.Set("MF_BT2", core.NumberValue(0x12))
	module.Set("MF_BT3", core.NumberValue(0x13))
	module.Set("MF_BT4", core.NumberValue(0x14))
	module.Set("MODE_FAST", core.NumberValue(1))
	module.Set("MODE_NORMAL", core.NumberValue(2))
	module.Set("PRESET_DEFAULT", core.NumberValue(6))
	module.Set("PRESET_EXTREME", core.NumberValue(0x80000000))

	// compress function stub
	module.Set("compress", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("NotImplementedError: LZMA compression not yet implemented in M28")
	}))

	// decompress function stub
	module.Set("decompress", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("NotImplementedError: LZMA decompression not yet implemented in M28")
	}))

	// _encode_filter_properties - encode filter properties (stub)
	module.Set("_encode_filter_properties", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("NotImplementedError: LZMA filter encoding not yet implemented in M28")
	}))

	// _decode_filter_properties - decode filter properties (stub)
	module.Set("_decode_filter_properties", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("NotImplementedError: LZMA filter decoding not yet implemented in M28")
	}))

	// is_check_supported - check if a check type is supported
	module.Set("is_check_supported", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Return false for all checks since we don't implement LZMA
		return core.BoolValue(false), nil
	}))

	return module
}
