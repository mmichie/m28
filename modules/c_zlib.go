package modules

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// InitZlibModule creates a minimal stub for zlib C extension module
// This module provides gzip/zlib compression/decompression functionality
// Currently returns NotImplementedError for all functions to allow imports
func InitZlibModule() *core.DictValue {
	module := core.NewDict()

	// Module metadata
	module.SetStr("__name__", core.StringValue("zlib"))
	module.SetStr("__doc__", core.StringValue("zlib compression/decompression (stub)"))

	// Constants
	module.SetStr("Z_DEFAULT_COMPRESSION", core.NumberValue(-1))
	module.SetStr("MAX_WBITS", core.NumberValue(15))
	module.SetStr("DEFLATED", core.NumberValue(8))

	// compress function stub
	module.SetStr("compress", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("NotImplementedError: zlib compression not yet implemented in M28")
	}))

	// decompress function stub
	module.SetStr("decompress", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("NotImplementedError: zlib decompression not yet implemented in M28")
	}))

	// compressobj function stub
	module.SetStr("compressobj", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("NotImplementedError: zlib compressobj not yet implemented in M28")
	}))

	// decompressobj function stub
	module.SetStr("decompressobj", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("NotImplementedError: zlib decompressobj not yet implemented in M28")
	}))

	// crc32 function stub
	module.SetStr("crc32", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("NotImplementedError: zlib crc32 not yet implemented in M28")
	}))

	// adler32 function stub
	module.SetStr("adler32", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("NotImplementedError: zlib adler32 not yet implemented in M28")
	}))

	return module
}
