package modules

import (
	"encoding/base64"
	"encoding/hex"
	"fmt"

	"github.com/mmichie/m28/core"
)

// InitBinasciiModule creates the binascii C extension module
// This provides binary to ASCII conversions
func InitBinasciiModule() *core.DictValue {
	binasciiModule := core.NewDict()

	// hexlify(data) / b2a_hex(data) - convert binary data to hex string
	hexlifyFunc := core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("hexlify() requires at least 1 argument")
		}

		data, err := getBytes(args[0])
		if err != nil {
			return nil, fmt.Errorf("hexlify() argument must be bytes-like: %v", err)
		}

		return core.BytesValue(hex.EncodeToString(data)), nil
	})
	binasciiModule.Set("hexlify", hexlifyFunc)
	binasciiModule.Set("b2a_hex", hexlifyFunc)

	// unhexlify(hexstr) / a2b_hex(hexstr) - convert hex string to binary
	unhexlifyFunc := core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("unhexlify() requires at least 1 argument")
		}

		var hexStr string
		switch v := args[0].(type) {
		case core.StringValue:
			hexStr = string(v)
		case core.BytesValue:
			hexStr = string(v)
		default:
			return nil, fmt.Errorf("unhexlify() argument must be str or bytes")
		}

		data, err := hex.DecodeString(hexStr)
		if err != nil {
			return nil, fmt.Errorf("binascii.Error: Non-hexadecimal digit found")
		}

		return core.BytesValue(data), nil
	})
	binasciiModule.Set("unhexlify", unhexlifyFunc)
	binasciiModule.Set("a2b_hex", unhexlifyFunc)

	// b2a_base64(data, newline=True) - convert binary to base64
	binasciiModule.Set("b2a_base64", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("b2a_base64() requires at least 1 argument")
		}

		data, err := getBytes(args[0])
		if err != nil {
			return nil, fmt.Errorf("b2a_base64() argument must be bytes-like: %v", err)
		}

		encoded := base64.StdEncoding.EncodeToString(data)

		// By default, add newline at end
		newline := true
		if len(args) > 1 {
			if b, ok := args[1].(core.BoolValue); ok {
				newline = bool(b)
			}
		}

		if newline {
			encoded += "\n"
		}

		return core.BytesValue(encoded), nil
	}))

	// a2b_base64(string) - convert base64 to binary
	binasciiModule.Set("a2b_base64", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("a2b_base64() requires at least 1 argument")
		}

		var b64Str string
		switch v := args[0].(type) {
		case core.StringValue:
			b64Str = string(v)
		case core.BytesValue:
			b64Str = string(v)
		default:
			return nil, fmt.Errorf("a2b_base64() argument must be str or bytes")
		}

		data, err := base64.StdEncoding.DecodeString(b64Str)
		if err != nil {
			return nil, fmt.Errorf("binascii.Error: Invalid base64-encoded string")
		}

		return core.BytesValue(data), nil
	}))

	// crc32(data, value=0) - compute CRC-32 checksum
	binasciiModule.Set("crc32", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("crc32() requires at least 1 argument")
		}

		data, err := getBytes(args[0])
		if err != nil {
			return nil, fmt.Errorf("crc32() argument must be bytes-like: %v", err)
		}

		// Initial CRC value
		var crc uint32 = 0
		if len(args) > 1 {
			if n, ok := args[1].(core.NumberValue); ok {
				crc = uint32(n)
			}
		}

		// CRC-32 polynomial
		crc = crc32Update(crc, data)

		return core.NumberValue(crc), nil
	}))

	// Error exception class
	binasciiModule.Set("Error", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		msg := ""
		if len(args) > 0 {
			if s, ok := args[0].(core.StringValue); ok {
				msg = string(s)
			}
		}
		return nil, fmt.Errorf("binascii.Error: %s", msg)
	}))

	// Incomplete exception class
	binasciiModule.Set("Incomplete", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		msg := ""
		if len(args) > 0 {
			if s, ok := args[0].(core.StringValue); ok {
				msg = string(s)
			}
		}
		return nil, fmt.Errorf("binascii.Incomplete: %s", msg)
	}))

	return binasciiModule
}

// getBytes converts a Value to a byte slice
func getBytes(v core.Value) ([]byte, error) {
	switch val := v.(type) {
	case core.BytesValue:
		return []byte(val), nil
	case core.StringValue:
		return []byte(val), nil
	default:
		// Try to get bytes from buffer-like object
		if obj, ok := v.(interface {
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
						return []byte(b), nil
					}
				}
			}
		}
		return nil, fmt.Errorf("expected bytes-like object")
	}
}

// CRC-32 lookup table (IEEE polynomial)
var crc32Table = makeCRC32Table()

func makeCRC32Table() [256]uint32 {
	var table [256]uint32
	for i := 0; i < 256; i++ {
		crc := uint32(i)
		for j := 0; j < 8; j++ {
			if crc&1 == 1 {
				crc = (crc >> 1) ^ 0xEDB88320
			} else {
				crc >>= 1
			}
		}
		table[i] = crc
	}
	return table
}

func crc32Update(crc uint32, data []byte) uint32 {
	crc = ^crc
	for _, b := range data {
		crc = crc32Table[byte(crc)^b] ^ (crc >> 8)
	}
	return ^crc
}
