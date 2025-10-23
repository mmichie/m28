package modules

import (
	"fmt"
	"strings"

	"github.com/mmichie/m28/core"
)

// InitCodecsModule initializes the _codecs C extension stub module
// This provides minimal codec support for Python stdlib modules
func InitCodecsModule() *core.DictValue {
	module := core.NewDict()

	// codec_search_registry holds registered codec search functions
	codecSearchRegistry := core.NewList()

	// error_handler_registry holds registered error handlers
	errorHandlerRegistry := core.NewDict()

	// Register default error handlers
	errorHandlerRegistry.Set("strict", core.NewNamedBuiltinFunction("strict_errors", strictErrorHandler))
	errorHandlerRegistry.Set("ignore", core.NewNamedBuiltinFunction("ignore_errors", ignoreErrorHandler))
	errorHandlerRegistry.Set("replace", core.NewNamedBuiltinFunction("replace_errors", replaceErrorHandler))
	errorHandlerRegistry.Set("xmlcharrefreplace", core.NewNamedBuiltinFunction("xmlcharrefreplace_errors", replaceErrorHandler))
	errorHandlerRegistry.Set("backslashreplace", core.NewNamedBuiltinFunction("backslashreplace_errors", replaceErrorHandler))
	errorHandlerRegistry.Set("namereplace", core.NewNamedBuiltinFunction("namereplace_errors", replaceErrorHandler))
	errorHandlerRegistry.Set("surrogateescape", core.NewNamedBuiltinFunction("surrogateescape_errors", ignoreErrorHandler))
	errorHandlerRegistry.Set("surrogatepass", core.NewNamedBuiltinFunction("surrogatepass_errors", ignoreErrorHandler))

	// register(search_function) - register a codec search function
	module.Set("register", core.NewNamedBuiltinFunction("register", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, core.NewTypeError("register", nil, "register() takes exactly 1 argument")
		}
		// Add the search function to the registry
		codecSearchRegistry.Append(args[0])
		return core.None, nil
	}))

	// lookup(encoding) - lookup a codec by encoding name
	module.Set("lookup", core.NewNamedBuiltinFunction("lookup", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, core.NewTypeError("lookup", nil, "lookup() takes exactly 1 argument")
		}

		encodingVal, ok := args[0].(core.StringValue)
		if !ok {
			return nil, core.NewTypeError("lookup", args[0], "encoding name must be a string")
		}

		encoding := strings.ToLower(string(encodingVal))
		encoding = strings.ReplaceAll(encoding, "-", "")
		encoding = strings.ReplaceAll(encoding, "_", "")

		// For now, we only support UTF-8 (and aliases)
		if encoding == "utf8" || encoding == "utf_8" || encoding == "utf-8" || encoding == "u8" {
			// Return a CodecInfo tuple: (encode, decode, streamreader, streamwriter)
			// For simplicity, we return None for streamreader and streamwriter
			encodeFunc := core.NewNamedBuiltinFunction("utf8_encode", utf8Encode)
			decodeFunc := core.NewNamedBuiltinFunction("utf8_decode", utf8Decode)

			codecInfo := core.TupleValue{
				encodeFunc,
				decodeFunc,
				core.None,
				core.None,
			}
			return codecInfo, nil
		}

		// For ASCII
		if encoding == "ascii" {
			encodeFunc := core.NewNamedBuiltinFunction("ascii_encode", asciiEncode)
			decodeFunc := core.NewNamedBuiltinFunction("ascii_decode", asciiDecode)

			codecInfo := core.TupleValue{
				encodeFunc,
				decodeFunc,
				core.None,
				core.None,
			}
			return codecInfo, nil
		}

		// For Latin-1 (ISO-8859-1)
		if encoding == "latin1" || encoding == "iso88591" || encoding == "latin_1" {
			encodeFunc := core.NewNamedBuiltinFunction("latin1_encode", latin1Encode)
			decodeFunc := core.NewNamedBuiltinFunction("latin1_decode", latin1Decode)

			codecInfo := core.TupleValue{
				encodeFunc,
				decodeFunc,
				core.None,
				core.None,
			}
			return codecInfo, nil
		}

		// If we don't recognize the encoding, try calling registered search functions
		searchItems := codecSearchRegistry.Items()
		for i := 0; i < len(searchItems); i++ {
			searchFunc := searchItems[i]
			// Call the search function
			callable, ok := searchFunc.(interface {
				Call([]core.Value, *core.Context) (core.Value, error)
			})
			if !ok {
				continue
			}
			result, err := callable.Call([]core.Value{args[0]}, ctx)
			if err != nil {
				continue
			}
			// If the search function returns non-None, use that codec
			if _, isNone := result.(core.NilValue); !isNone {
				return result, nil
			}
		}

		return nil, fmt.Errorf("unknown encoding: %s", encodingVal)
	}))

	// register_error(name, handler) - register an error handler
	module.Set("register_error", core.NewNamedBuiltinFunction("register_error", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, core.NewTypeError("register_error", nil, "register_error() takes exactly 2 arguments")
		}

		name, ok := args[0].(core.StringValue)
		if !ok {
			return nil, core.NewTypeError("register_error", args[0], "error handler name must be a string")
		}

		errorHandlerRegistry.Set(string(name), args[1])
		return core.None, nil
	}))

	// lookup_error(name) - lookup an error handler
	module.Set("lookup_error", core.NewNamedBuiltinFunction("lookup_error", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, core.NewTypeError("lookup_error", nil, "lookup_error() takes exactly 1 argument")
		}

		name, ok := args[0].(core.StringValue)
		if !ok {
			return nil, core.NewTypeError("lookup_error", args[0], "error handler name must be a string")
		}

		handler, exists := errorHandlerRegistry.Get(string(name))
		if !exists {
			return nil, fmt.Errorf("unknown error handler name '%s'", name)
		}

		return handler, nil
	}))

	// encode(obj, encoding='utf-8', errors='strict') - encode an object
	module.Set("encode", core.NewNamedBuiltinFunction("encode", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 || len(args) > 3 {
			return nil, core.NewTypeError("encode", nil, "encode() takes 1 to 3 arguments")
		}

		obj := args[0]
		encoding := "utf-8"
		errors := "strict"

		if len(args) >= 2 {
			if encVal, ok := args[1].(core.StringValue); ok {
				encoding = string(encVal)
			}
		}

		if len(args) >= 3 {
			if errVal, ok := args[2].(core.StringValue); ok {
				errors = string(errVal)
			}
		}

		// Convert obj to string if needed
		str, ok := obj.(core.StringValue)
		if !ok {
			strVal := core.PrintValue(obj)
			str = core.StringValue(strVal)
		}

		// For now, just return the string as bytes (UTF-8 encoded)
		// In a real implementation, we'd handle different encodings properly
		_ = encoding
		_ = errors

		// Return a tuple (bytes, length)
		bytes := core.StringValue(str) // In M28, we don't have a separate bytes type yet
		length := core.NumberValue(len(str))
		return core.TupleValue{bytes, length}, nil
	}))

	// decode(obj, encoding='utf-8', errors='strict') - decode an object
	module.Set("decode", core.NewNamedBuiltinFunction("decode", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 || len(args) > 3 {
			return nil, core.NewTypeError("decode", nil, "decode() takes 1 to 3 arguments")
		}

		obj := args[0]
		encoding := "utf-8"
		errors := "strict"

		if len(args) >= 2 {
			if encVal, ok := args[1].(core.StringValue); ok {
				encoding = string(encVal)
			}
		}

		if len(args) >= 3 {
			if errVal, ok := args[2].(core.StringValue); ok {
				errors = string(errVal)
			}
		}

		// Convert obj to string
		str, ok := obj.(core.StringValue)
		if !ok {
			strVal := core.PrintValue(obj)
			str = core.StringValue(strVal)
		}

		// For now, just return the string as-is
		// In a real implementation, we'd handle different encodings properly
		_ = encoding
		_ = errors

		// Return a tuple (string, length)
		length := core.NumberValue(len(str))
		return core.TupleValue{str, length}, nil
	}))

	// escape_encode(data, errors='strict') - used for string_escape codec
	module.Set("escape_encode", core.NewNamedBuiltinFunction("escape_encode", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 || len(args) > 2 {
			return nil, core.NewTypeError("escape_encode", nil, "escape_encode() takes 1 to 2 arguments")
		}

		str, ok := args[0].(core.StringValue)
		if !ok {
			return nil, core.NewTypeError("escape_encode", args[0], "escape_encode() argument must be a string")
		}

		// For now, just return the string as-is
		// In a real implementation, we'd escape special characters
		length := core.NumberValue(len(str))
		return core.TupleValue{str, length}, nil
	}))

	// escape_decode(data, errors='strict') - used for string_escape codec
	module.Set("escape_decode", core.NewNamedBuiltinFunction("escape_decode", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 || len(args) > 2 {
			return nil, core.NewTypeError("escape_decode", nil, "escape_decode() takes 1 to 2 arguments")
		}

		str, ok := args[0].(core.StringValue)
		if !ok {
			return nil, core.NewTypeError("escape_decode", args[0], "escape_decode() argument must be a string")
		}

		// For now, just return the string as-is
		// In a real implementation, we'd unescape special characters
		length := core.NumberValue(len(str))
		return core.TupleValue{str, length}, nil
	}))

	return module
}

// Error handler functions

func strictErrorHandler(args []core.Value, ctx *core.Context) (core.Value, error) {
	// Strict mode: raise an error on encoding/decoding problems
	if len(args) > 0 {
		return nil, fmt.Errorf("codec encoding error: %v", args[0])
	}
	return nil, fmt.Errorf("codec encoding error")
}

func ignoreErrorHandler(args []core.Value, ctx *core.Context) (core.Value, error) {
	// Ignore mode: skip problematic characters
	return core.TupleValue{core.StringValue(""), core.NumberValue(1)}, nil
}

func replaceErrorHandler(args []core.Value, ctx *core.Context) (core.Value, error) {
	// Replace mode: replace problematic characters with '?'
	return core.TupleValue{core.StringValue("?"), core.NumberValue(1)}, nil
}

// Codec encode/decode functions

func utf8Encode(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 1 || len(args) > 2 {
		return nil, core.NewTypeError("utf8_encode", nil, "utf8_encode() takes 1 to 2 arguments")
	}

	str, ok := args[0].(core.StringValue)
	if !ok {
		strVal := core.PrintValue(args[0])
		str = core.StringValue(strVal)
	}

	// Return (bytes, length)
	length := core.NumberValue(len(str))
	return core.TupleValue{str, length}, nil
}

func utf8Decode(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 1 || len(args) > 2 {
		return nil, core.NewTypeError("utf8_decode", nil, "utf8_decode() takes 1 to 2 arguments")
	}

	str, ok := args[0].(core.StringValue)
	if !ok {
		strVal := core.PrintValue(args[0])
		str = core.StringValue(strVal)
	}

	// Return (string, length)
	length := core.NumberValue(len(str))
	return core.TupleValue{str, length}, nil
}

func asciiEncode(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 1 || len(args) > 2 {
		return nil, core.NewTypeError("ascii_encode", nil, "ascii_encode() takes 1 to 2 arguments")
	}

	str, ok := args[0].(core.StringValue)
	if !ok {
		strVal := core.PrintValue(args[0])
		str = core.StringValue(strVal)
	}

	// For simplicity, just return the string
	// A real implementation would check for non-ASCII characters
	length := core.NumberValue(len(str))
	return core.TupleValue{str, length}, nil
}

func asciiDecode(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 1 || len(args) > 2 {
		return nil, core.NewTypeError("ascii_decode", nil, "ascii_decode() takes 1 to 2 arguments")
	}

	str, ok := args[0].(core.StringValue)
	if !ok {
		strVal := core.PrintValue(args[0])
		str = core.StringValue(strVal)
	}

	// Return (string, length)
	length := core.NumberValue(len(str))
	return core.TupleValue{str, length}, nil
}

func latin1Encode(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 1 || len(args) > 2 {
		return nil, core.NewTypeError("latin1_encode", nil, "latin1_encode() takes 1 to 2 arguments")
	}

	str, ok := args[0].(core.StringValue)
	if !ok {
		strVal := core.PrintValue(args[0])
		str = core.StringValue(strVal)
	}

	// Return (bytes, length)
	length := core.NumberValue(len(str))
	return core.TupleValue{str, length}, nil
}

func latin1Decode(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 1 || len(args) > 2 {
		return nil, core.NewTypeError("latin1_decode", nil, "latin1_decode() takes 1 to 2 arguments")
	}

	str, ok := args[0].(core.StringValue)
	if !ok {
		strVal := core.PrintValue(args[0])
		str = core.StringValue(strVal)
	}

	// Return (string, length)
	length := core.NumberValue(len(str))
	return core.TupleValue{str, length}, nil
}
