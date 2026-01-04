package modules

import (
	"encoding/binary"
	"fmt"
	"math"
	"strings"

	"github.com/mmichie/m28/core"
)

// getFormatString extracts a format string from either str or bytes value
// Python's struct module accepts both str and bytes as format strings
func getFormatString(v core.Value) (string, error) {
	switch val := v.(type) {
	case core.StringValue:
		return string(val), nil
	case core.BytesValue:
		return string(val), nil
	default:
		return "", fmt.Errorf("format must be str or bytes, not %s", v.Type())
	}
}

// InitStructModule creates and returns the _struct module
// This implements Python's _struct C extension for binary data packing/unpacking
func InitStructModule() *core.DictValue {
	structModule := core.NewDict()

	// pack(format, v1, v2, ...) - pack values into bytes
	structModule.Set("pack", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, &core.TypeError{Message: "pack() missing required argument: 'format' (pos 1)"}
		}

		formatStr, err := getFormatString(args[0])
		if err != nil {
			return nil, &core.TypeError{Message: fmt.Sprintf("pack() argument 1 %v", err)}
		}

		values := args[1:]
		result, err := structPack(formatStr, values)
		if err != nil {
			return nil, err
		}

		return core.BytesValue(result), nil
	}))

	// unpack(format, buffer) - unpack bytes into tuple
	structModule.Set("unpack", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, &core.TypeError{Message: fmt.Sprintf("unpack() takes exactly 2 arguments (%d given)", len(args))}
		}

		formatStr, err := getFormatString(args[0])
		if err != nil {
			return nil, &core.TypeError{Message: fmt.Sprintf("unpack() argument 1 %v", err)}
		}

		buffer, ok := args[1].(core.BytesValue)
		if !ok {
			return nil, &core.TypeError{Message: fmt.Sprintf("unpack() argument 2 must be bytes, not %s", args[1].Type())}
		}

		result, err := structUnpack(formatStr, []byte(buffer))
		if err != nil {
			return nil, err
		}

		return core.TupleValue(result), nil
	}))

	// calcsize(format) - return size of struct
	structModule.Set("calcsize", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, &core.TypeError{Message: fmt.Sprintf("calcsize() takes exactly 1 argument (%d given)", len(args))}
		}

		formatStr, err := getFormatString(args[0])
		if err != nil {
			return nil, &core.TypeError{Message: fmt.Sprintf("calcsize() argument %v", err)}
		}

		size, err := structCalcsize(formatStr)
		if err != nil {
			return nil, err
		}

		return core.NumberValue(float64(size)), nil
	}))

	// error - exception class for struct errors
	structModule.Set("error", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		msg := "struct.error"
		if len(args) > 0 {
			if s, ok := args[0].(core.StringValue); ok {
				msg = string(s)
			}
		}
		return nil, &core.ValueError{Message: fmt.Sprintf("struct.error: %s", msg)}
	}))

	// _clearcache - clear format cache (no-op for now)
	structModule.Set("_clearcache", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// In CPython this clears the compiled format cache
		// We don't have one yet, so this is a no-op
		return core.NilValue{}, nil
	}))

	// pack_into - pack into existing buffer (stub for now)
	structModule.Set("pack_into", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, &core.ValueError{Message: "pack_into() not yet implemented"}
	}))

	// unpack_from - unpack from buffer at offset (stub for now)
	structModule.Set("unpack_from", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, &core.ValueError{Message: "unpack_from() not yet implemented"}
	}))

	// iter_unpack - iterator for repeated unpacking (stub for now)
	structModule.Set("iter_unpack", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, &core.ValueError{Message: "iter_unpack() not yet implemented"}
	}))

	// Struct class - compiled format strings
	structModule.Set("Struct", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, &core.TypeError{Message: "Struct() requires a format string"}
		}

		formatStr, err := getFormatString(args[0])
		if err != nil {
			return nil, &core.TypeError{Message: fmt.Sprintf("Struct() argument %v", err)}
		}

		// Pre-calculate size
		size, err := structCalcsize(formatStr)
		if err != nil {
			return nil, err
		}

		// Create struct object as a dict with methods
		structObj := core.NewDict()

		// format property
		structObj.Set("format", core.BytesValue(formatStr))

		// size property
		structObj.Set("size", core.NumberValue(float64(size)))

		// pack(*args) method
		structObj.Set("pack", core.NewBuiltinFunction(func(packArgs []core.Value, ctx *core.Context) (core.Value, error) {
			result, err := structPack(formatStr, packArgs)
			if err != nil {
				return nil, err
			}
			return core.BytesValue(result), nil
		}))

		// unpack(buffer) method
		structObj.Set("unpack", core.NewBuiltinFunction(func(unpackArgs []core.Value, ctx *core.Context) (core.Value, error) {
			if len(unpackArgs) != 1 {
				return nil, &core.TypeError{Message: "unpack() takes exactly 1 argument"}
			}

			buffer, ok := unpackArgs[0].(core.BytesValue)
			if !ok {
				return nil, &core.TypeError{Message: fmt.Sprintf("unpack() argument must be bytes, not %s", unpackArgs[0].Type())}
			}

			result, err := structUnpack(formatStr, []byte(buffer))
			if err != nil {
				return nil, err
			}
			return core.TupleValue(result), nil
		}))

		// pack_into(buffer, offset, *args) method (stub)
		structObj.Set("pack_into", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return nil, &core.ValueError{Message: "pack_into() not yet implemented"}
		}))

		// unpack_from(buffer, offset=0) method
		structObj.Set("unpack_from", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) < 1 {
				return nil, &core.TypeError{Message: "unpack_from() requires at least 1 argument"}
			}

			var buffer []byte
			switch v := args[0].(type) {
			case core.BytesValue:
				buffer = []byte(v)
			default:
				return nil, &core.TypeError{Message: fmt.Sprintf("unpack_from() argument must be bytes, not %s", args[0].Type())}
			}

			offset := 0
			if len(args) > 1 {
				if n, ok := args[1].(core.NumberValue); ok {
					offset = int(n)
				}
			}

			if offset < 0 || offset > len(buffer) {
				return nil, &core.ValueError{Message: "unpack_from() offset out of range"}
			}

			result, err := structUnpack(formatStr, buffer[offset:])
			if err != nil {
				return nil, err
			}
			return core.TupleValue(result), nil
		}))

		// iter_unpack(buffer) method (stub)
		structObj.Set("iter_unpack", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return nil, &core.ValueError{Message: "iter_unpack() not yet implemented"}
		}))

		return structObj, nil
	}))

	// __doc__ - module documentation
	structModule.Set("__doc__", core.StringValue("Functions to convert between Python values and C structs."))

	// __name__ - module name
	structModule.Set("__name__", core.StringValue("_struct"))

	return structModule
}

// structPack packs values into bytes according to format string
func structPack(format string, values []core.Value) ([]byte, error) {
	// Parse byte order
	var byteOrder binary.ByteOrder = binary.LittleEndian // default for '@' and '='
	idx := 0
	if len(format) > 0 {
		switch format[0] {
		case '<': // little-endian
			byteOrder = binary.LittleEndian
			idx = 1
		case '>': // big-endian
			byteOrder = binary.BigEndian
			idx = 1
		case '!': // network (big-endian)
			byteOrder = binary.BigEndian
			idx = 1
		case '@': // native
			byteOrder = binary.LittleEndian
			idx = 1
		case '=': // native, standard size
			byteOrder = binary.LittleEndian
			idx = 1
		}
	}

	formatChars := format[idx:]
	result := make([]byte, 0)
	valueIdx := 0

	for i := 0; i < len(formatChars); i++ {
		ch := formatChars[i]

		// Handle repeat count
		count := 1
		if i+1 < len(formatChars) && formatChars[i] >= '0' && formatChars[i] <= '9' {
			// Parse number
			countStr := ""
			for i < len(formatChars) && formatChars[i] >= '0' && formatChars[i] <= '9' {
				countStr += string(formatChars[i])
				i++
			}
			if i >= len(formatChars) {
				return nil, &core.ValueError{Message: "repeat count given without format specifier"}
			}
			fmt.Sscanf(countStr, "%d", &count)
			ch = formatChars[i]
		}

		for c := 0; c < count; c++ {
			switch ch {
			case 'x': // pad byte
				result = append(result, 0)

			case '?': // bool
				if valueIdx >= len(values) {
					return nil, &core.TypeError{Message: "pack() requires more arguments"}
				}
				val := values[valueIdx]
				valueIdx++

				boolVal := false
				switch v := val.(type) {
				case core.BoolValue:
					boolVal = bool(v)
				case core.NumberValue:
					boolVal = float64(v) != 0
				default:
					// Try to convert to bool
					boolVal = true // non-nil is true
				}

				if boolVal {
					result = append(result, 1)
				} else {
					result = append(result, 0)
				}

			case 'b': // signed char
				if valueIdx >= len(values) {
					return nil, &core.TypeError{Message: "pack() requires more arguments"}
				}
				val := values[valueIdx]
				valueIdx++
				num, err := toInt64(val)
				if err != nil {
					return nil, err
				}
				result = append(result, byte(int8(num)))

			case 'B': // unsigned char
				if valueIdx >= len(values) {
					return nil, &core.TypeError{Message: "pack() requires more arguments"}
				}
				val := values[valueIdx]
				valueIdx++
				num, err := toInt64(val)
				if err != nil {
					return nil, err
				}
				result = append(result, byte(uint8(num)))

			case 'h': // signed short
				if valueIdx >= len(values) {
					return nil, &core.TypeError{Message: "pack() requires more arguments"}
				}
				val := values[valueIdx]
				valueIdx++
				num, err := toInt64(val)
				if err != nil {
					return nil, err
				}
				buf := make([]byte, 2)
				byteOrder.PutUint16(buf, uint16(int16(num)))
				result = append(result, buf...)

			case 'H': // unsigned short
				if valueIdx >= len(values) {
					return nil, &core.TypeError{Message: "pack() requires more arguments"}
				}
				val := values[valueIdx]
				valueIdx++
				num, err := toInt64(val)
				if err != nil {
					return nil, err
				}
				buf := make([]byte, 2)
				byteOrder.PutUint16(buf, uint16(num))
				result = append(result, buf...)

			case 'i', 'l': // signed int/long
				if valueIdx >= len(values) {
					return nil, &core.TypeError{Message: "pack() requires more arguments"}
				}
				val := values[valueIdx]
				valueIdx++
				num, err := toInt64(val)
				if err != nil {
					return nil, err
				}
				buf := make([]byte, 4)
				byteOrder.PutUint32(buf, uint32(int32(num)))
				result = append(result, buf...)

			case 'I', 'L': // unsigned int/long
				if valueIdx >= len(values) {
					return nil, &core.TypeError{Message: "pack() requires more arguments"}
				}
				val := values[valueIdx]
				valueIdx++
				num, err := toInt64(val)
				if err != nil {
					return nil, err
				}
				buf := make([]byte, 4)
				byteOrder.PutUint32(buf, uint32(num))
				result = append(result, buf...)

			case 'q': // signed long long
				if valueIdx >= len(values) {
					return nil, &core.TypeError{Message: "pack() requires more arguments"}
				}
				val := values[valueIdx]
				valueIdx++
				num, err := toInt64(val)
				if err != nil {
					return nil, err
				}
				buf := make([]byte, 8)
				byteOrder.PutUint64(buf, uint64(num))
				result = append(result, buf...)

			case 'Q': // unsigned long long
				if valueIdx >= len(values) {
					return nil, &core.TypeError{Message: "pack() requires more arguments"}
				}
				val := values[valueIdx]
				valueIdx++
				num, err := toInt64(val)
				if err != nil {
					return nil, err
				}
				buf := make([]byte, 8)
				byteOrder.PutUint64(buf, uint64(num))
				result = append(result, buf...)

			case 'f': // float
				if valueIdx >= len(values) {
					return nil, &core.TypeError{Message: "pack() requires more arguments"}
				}
				val := values[valueIdx]
				valueIdx++
				num, err := toFloat64(val)
				if err != nil {
					return nil, err
				}
				buf := make([]byte, 4)
				byteOrder.PutUint32(buf, math.Float32bits(float32(num)))
				result = append(result, buf...)

			case 'd': // double
				if valueIdx >= len(values) {
					return nil, &core.TypeError{Message: "pack() requires more arguments"}
				}
				val := values[valueIdx]
				valueIdx++
				num, err := toFloat64(val)
				if err != nil {
					return nil, err
				}
				buf := make([]byte, 8)
				byteOrder.PutUint64(buf, math.Float64bits(num))
				result = append(result, buf...)

			case 's': // char[]
				if valueIdx >= len(values) {
					return nil, &core.TypeError{Message: "pack() requires more arguments"}
				}
				val := values[valueIdx]
				valueIdx++

				var bytes []byte
				switch v := val.(type) {
				case core.BytesValue:
					bytes = []byte(v)
				case core.StringValue:
					bytes = []byte(string(v))
				default:
					return nil, &core.TypeError{Message: fmt.Sprintf("argument for 's' must be bytes, not %s", val.Type())}
				}

				// Pad or truncate to count
				if len(bytes) > count {
					bytes = bytes[:count]
				} else if len(bytes) < count {
					padding := make([]byte, count-len(bytes))
					bytes = append(bytes, padding...)
				}
				result = append(result, bytes...)
				break // 's' consumes the count, so break the loop

			default:
				return nil, &core.ValueError{Message: fmt.Sprintf("bad char in struct format: %c", ch)}
			}
		}
	}

	if valueIdx < len(values) {
		return nil, &core.TypeError{Message: fmt.Sprintf("pack() expected %d items for packing (got %d)", valueIdx, len(values))}
	}

	return result, nil
}

// structUnpack unpacks bytes into values according to format string
func structUnpack(format string, buffer []byte) ([]core.Value, error) {
	// Parse byte order
	var byteOrder binary.ByteOrder = binary.LittleEndian // default
	idx := 0
	if len(format) > 0 {
		switch format[0] {
		case '<':
			byteOrder = binary.LittleEndian
			idx = 1
		case '>':
			byteOrder = binary.BigEndian
			idx = 1
		case '!':
			byteOrder = binary.BigEndian
			idx = 1
		case '@':
			byteOrder = binary.LittleEndian
			idx = 1
		case '=':
			byteOrder = binary.LittleEndian
			idx = 1
		}
	}

	formatChars := format[idx:]
	result := make([]core.Value, 0)
	bufIdx := 0

	for i := 0; i < len(formatChars); i++ {
		ch := formatChars[i]

		// Handle repeat count
		count := 1
		if i+1 < len(formatChars) && formatChars[i] >= '0' && formatChars[i] <= '9' {
			countStr := ""
			for i < len(formatChars) && formatChars[i] >= '0' && formatChars[i] <= '9' {
				countStr += string(formatChars[i])
				i++
			}
			if i >= len(formatChars) {
				return nil, &core.ValueError{Message: "repeat count given without format specifier"}
			}
			fmt.Sscanf(countStr, "%d", &count)
			ch = formatChars[i]
		}

		for c := 0; c < count; c++ {
			switch ch {
			case 'x': // pad byte
				if bufIdx >= len(buffer) {
					return nil, &core.ValueError{Message: fmt.Sprintf("unpack requires a buffer of %d bytes", bufIdx+1)}
				}
				bufIdx++

			case '?': // bool
				if bufIdx >= len(buffer) {
					return nil, &core.ValueError{Message: fmt.Sprintf("unpack requires a buffer of %d bytes", bufIdx+1)}
				}
				result = append(result, core.BoolValue(buffer[bufIdx] != 0))
				bufIdx++

			case 'b': // signed char
				if bufIdx >= len(buffer) {
					return nil, &core.ValueError{Message: fmt.Sprintf("unpack requires a buffer of %d bytes", bufIdx+1)}
				}
				result = append(result, core.NumberValue(float64(int8(buffer[bufIdx]))))
				bufIdx++

			case 'B': // unsigned char
				if bufIdx >= len(buffer) {
					return nil, &core.ValueError{Message: fmt.Sprintf("unpack requires a buffer of %d bytes", bufIdx+1)}
				}
				result = append(result, core.NumberValue(float64(buffer[bufIdx])))
				bufIdx++

			case 'h': // signed short
				if bufIdx+2 > len(buffer) {
					return nil, &core.ValueError{Message: fmt.Sprintf("unpack requires a buffer of %d bytes", bufIdx+2)}
				}
				val := int16(byteOrder.Uint16(buffer[bufIdx : bufIdx+2]))
				result = append(result, core.NumberValue(float64(val)))
				bufIdx += 2

			case 'H': // unsigned short
				if bufIdx+2 > len(buffer) {
					return nil, &core.ValueError{Message: fmt.Sprintf("unpack requires a buffer of %d bytes", bufIdx+2)}
				}
				val := byteOrder.Uint16(buffer[bufIdx : bufIdx+2])
				result = append(result, core.NumberValue(float64(val)))
				bufIdx += 2

			case 'i', 'l': // signed int/long
				if bufIdx+4 > len(buffer) {
					return nil, &core.ValueError{Message: fmt.Sprintf("unpack requires a buffer of %d bytes", bufIdx+4)}
				}
				val := int32(byteOrder.Uint32(buffer[bufIdx : bufIdx+4]))
				result = append(result, core.NumberValue(float64(val)))
				bufIdx += 4

			case 'I', 'L': // unsigned int/long
				if bufIdx+4 > len(buffer) {
					return nil, &core.ValueError{Message: fmt.Sprintf("unpack requires a buffer of %d bytes", bufIdx+4)}
				}
				val := byteOrder.Uint32(buffer[bufIdx : bufIdx+4])
				result = append(result, core.NumberValue(float64(val)))
				bufIdx += 4

			case 'q': // signed long long
				if bufIdx+8 > len(buffer) {
					return nil, &core.ValueError{Message: fmt.Sprintf("unpack requires a buffer of %d bytes", bufIdx+8)}
				}
				val := int64(byteOrder.Uint64(buffer[bufIdx : bufIdx+8]))
				result = append(result, core.NumberValue(float64(val)))
				bufIdx += 8

			case 'Q': // unsigned long long
				if bufIdx+8 > len(buffer) {
					return nil, &core.ValueError{Message: fmt.Sprintf("unpack requires a buffer of %d bytes", bufIdx+8)}
				}
				val := byteOrder.Uint64(buffer[bufIdx : bufIdx+8])
				result = append(result, core.NumberValue(float64(val)))
				bufIdx += 8

			case 'f': // float
				if bufIdx+4 > len(buffer) {
					return nil, &core.ValueError{Message: fmt.Sprintf("unpack requires a buffer of %d bytes", bufIdx+4)}
				}
				bits := byteOrder.Uint32(buffer[bufIdx : bufIdx+4])
				val := math.Float32frombits(bits)
				result = append(result, core.NumberValue(float64(val)))
				bufIdx += 4

			case 'd': // double
				if bufIdx+8 > len(buffer) {
					return nil, &core.ValueError{Message: fmt.Sprintf("unpack requires a buffer of %d bytes", bufIdx+8)}
				}
				bits := byteOrder.Uint64(buffer[bufIdx : bufIdx+8])
				val := math.Float64frombits(bits)
				result = append(result, core.NumberValue(val))
				bufIdx += 8

			case 's': // char[]
				if bufIdx+count > len(buffer) {
					return nil, &core.ValueError{Message: fmt.Sprintf("unpack requires a buffer of %d bytes", bufIdx+count)}
				}
				bytes := buffer[bufIdx : bufIdx+count]
				result = append(result, core.BytesValue(bytes))
				bufIdx += count
				break // 's' consumes the count

			default:
				return nil, &core.ValueError{Message: fmt.Sprintf("bad char in struct format: %c", ch)}
			}
		}
	}

	return result, nil
}

// structCalcsize calculates the size in bytes for a format string
func structCalcsize(format string) (int, error) {
	// Skip byte order prefix
	idx := 0
	if len(format) > 0 {
		switch format[0] {
		case '<', '>', '!', '@', '=':
			idx = 1
		}
	}

	formatChars := format[idx:]
	size := 0

	for i := 0; i < len(formatChars); i++ {
		ch := formatChars[i]

		// Handle repeat count
		count := 1
		if i+1 < len(formatChars) && formatChars[i] >= '0' && formatChars[i] <= '9' {
			countStr := ""
			for i < len(formatChars) && formatChars[i] >= '0' && formatChars[i] <= '9' {
				countStr += string(formatChars[i])
				i++
			}
			if i >= len(formatChars) {
				return 0, &core.ValueError{Message: "repeat count given without format specifier"}
			}
			fmt.Sscanf(countStr, "%d", &count)
			ch = formatChars[i]
		}

		switch ch {
		case 'x', '?', 'b', 'B':
			size += count
		case 'h', 'H':
			size += 2 * count
		case 'i', 'I', 'l', 'L', 'f':
			size += 4 * count
		case 'q', 'Q', 'd':
			size += 8 * count
		case 's', 'p':
			size += count
		default:
			return 0, &core.ValueError{Message: fmt.Sprintf("bad char in struct format: %c", ch)}
		}
	}

	return size, nil
}

// Helper functions

func toInt64(val core.Value) (int64, error) {
	switch v := val.(type) {
	case core.NumberValue:
		return int64(v), nil
	case core.BoolValue:
		if bool(v) {
			return 1, nil
		}
		return 0, nil
	default:
		// Try to call __int__ or convert
		if strings.Contains(val.String(), "int") {
			return 0, nil // fallback
		}
		return 0, &core.TypeError{Message: fmt.Sprintf("cannot convert %s to int", val.Type())}
	}
}

func toFloat64(val core.Value) (float64, error) {
	switch v := val.(type) {
	case core.NumberValue:
		return float64(v), nil
	case core.BoolValue:
		if bool(v) {
			return 1.0, nil
		}
		return 0.0, nil
	default:
		return 0, &core.TypeError{Message: fmt.Sprintf("cannot convert %s to float", val.Type())}
	}
}
