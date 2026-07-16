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
	module.SetStr("__name__", core.StringValue("_lzma"))
	module.SetStr("__doc__", core.StringValue("LZMA compression/decompression (stub)"))

	// LZMACompressor class stub
	module.SetStr("LZMACompressor", core.NewClassWithParents("LZMACompressor", []*core.Class{}))

	// LZMADecompressor class stub
	module.SetStr("LZMADecompressor", core.NewClassWithParents("LZMADecompressor", []*core.Class{}))

	// Constants
	module.SetStr("CHECK_NONE", core.NumberValue(0))
	module.SetStr("CHECK_CRC32", core.NumberValue(1))
	module.SetStr("CHECK_CRC64", core.NumberValue(4))
	module.SetStr("CHECK_SHA256", core.NumberValue(10))
	module.SetStr("FILTER_LZMA1", core.NumberValue(0x4000000000000001))
	module.SetStr("FILTER_LZMA2", core.NumberValue(0x21))
	module.SetStr("FORMAT_AUTO", core.NumberValue(0))
	module.SetStr("FORMAT_XZ", core.NumberValue(1))
	module.SetStr("FORMAT_ALONE", core.NumberValue(2))
	module.SetStr("FORMAT_RAW", core.NumberValue(3))
	module.SetStr("MF_HC3", core.NumberValue(0x03))
	module.SetStr("MF_HC4", core.NumberValue(0x04))
	module.SetStr("MF_BT2", core.NumberValue(0x12))
	module.SetStr("MF_BT3", core.NumberValue(0x13))
	module.SetStr("MF_BT4", core.NumberValue(0x14))
	module.SetStr("MODE_FAST", core.NumberValue(1))
	module.SetStr("MODE_NORMAL", core.NumberValue(2))
	module.SetStr("PRESET_DEFAULT", core.NumberValue(6))
	module.SetStr("PRESET_EXTREME", core.NumberValue(0x80000000))

	// compress function stub
	module.SetStr("compress", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("NotImplementedError: LZMA compression not yet implemented in M28")
	}))

	// decompress function stub
	module.SetStr("decompress", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("NotImplementedError: LZMA decompression not yet implemented in M28")
	}))

	// _encode_filter_properties - encode filter properties (stub)
	module.SetStr("_encode_filter_properties", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("NotImplementedError: LZMA filter encoding not yet implemented in M28")
	}))

	// _decode_filter_properties - decode filter properties (stub)
	module.SetStr("_decode_filter_properties", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("NotImplementedError: LZMA filter decoding not yet implemented in M28")
	}))

	// is_check_supported - check if a check type is supported
	module.SetStr("is_check_supported", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Return false for all checks since we don't implement LZMA
		return core.BoolValue(false), nil
	}))

	return module
}
