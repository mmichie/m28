package modules

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// Init_Bz2Module creates a minimal stub for _bz2 C extension module
// This module provides bzip2 compression/decompression functionality
// Currently returns NotImplementedError for all functions to allow imports
func Init_Bz2Module() *core.DictValue {
	module := core.NewDict()

	// Module metadata
	module.Set("__name__", core.StringValue("_bz2"))
	module.Set("__doc__", core.StringValue("bzip2 compression/decompression (stub)"))

	// BZ2Compressor class stub
	module.Set("BZ2Compressor", core.NewClassWithParents("BZ2Compressor", []*core.Class{}))

	// BZ2Decompressor class stub
	module.Set("BZ2Decompressor", core.NewClassWithParents("BZ2Decompressor", []*core.Class{}))

	// compress function stub
	module.Set("compress", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("NotImplementedError: bzip2 compression not yet implemented in M28")
	}))

	// decompress function stub
	module.Set("decompress", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("NotImplementedError: bzip2 decompression not yet implemented in M28")
	}))

	return module
}
