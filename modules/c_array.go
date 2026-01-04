package modules

import (
	"bytes"
	"encoding/binary"
	"fmt"
	"math"

	"github.com/mmichie/m28/core"
)

// ArrayValue represents a Python array.array object
type ArrayValue struct {
	core.BaseObject
	typecode byte        // The type code character
	itemsize int         // Size of each item in bytes
	data     []byte      // Raw byte storage
	class    *core.Class // The array class for type() support
}

// Type codes and their sizes
var typecodeSizes = map[byte]int{
	'b': 1, // signed char
	'B': 1, // unsigned char
	'u': 4, // Unicode character (deprecated, use 4 bytes)
	'h': 2, // signed short
	'H': 2, // unsigned short
	'i': 4, // signed int
	'I': 4, // unsigned int
	'l': 4, // signed long (4 bytes on most platforms)
	'L': 4, // unsigned long
	'q': 8, // signed long long
	'Q': 8, // unsigned long long
	'f': 4, // float
	'd': 8, // double
}

// NewArrayValue creates a new array with the given typecode
func NewArrayValue(typecode byte, class *core.Class) (*ArrayValue, error) {
	size, ok := typecodeSizes[typecode]
	if !ok {
		return nil, fmt.Errorf("bad typecode (must be b, B, u, h, H, i, I, l, L, q, Q, f or d)")
	}
	return &ArrayValue{
		BaseObject: *core.NewBaseObject(core.Type("array")),
		typecode:   typecode,
		itemsize:   size,
		data:       make([]byte, 0),
		class:      class,
	}, nil
}

func (a *ArrayValue) Type() core.Type {
	return core.Type("array.array")
}

func (a *ArrayValue) String() string {
	if len(a.data) == 0 {
		return fmt.Sprintf("array('%c')", a.typecode)
	}
	// Show first few items
	items := a.toList()
	if len(items) <= 5 {
		return fmt.Sprintf("array('%c', %s)", a.typecode, core.PrintValue(core.NewList(items...)))
	}
	first5 := items[:5]
	return fmt.Sprintf("array('%c', [%s, ...])", a.typecode, formatItems(first5))
}

func formatItems(items []core.Value) string {
	result := ""
	for i, item := range items {
		if i > 0 {
			result += ", "
		}
		result += core.PrintValue(item)
	}
	return result
}

// Len returns the number of items in the array
func (a *ArrayValue) Len() int {
	return len(a.data) / a.itemsize
}

// append adds an item to the array
func (a *ArrayValue) append(value core.Value) error {
	bytes, err := a.valueToBytes(value)
	if err != nil {
		return err
	}
	a.data = append(a.data, bytes...)
	return nil
}

// extend adds items from an iterable
func (a *ArrayValue) extend(values []core.Value) error {
	for _, v := range values {
		if err := a.append(v); err != nil {
			return err
		}
	}
	return nil
}

// getItem returns the item at index i
func (a *ArrayValue) getItem(i int) (core.Value, error) {
	length := a.Len()
	if i < 0 {
		i = length + i
	}
	if i < 0 || i >= length {
		return nil, fmt.Errorf("array index out of range")
	}
	start := i * a.itemsize
	end := start + a.itemsize
	return a.bytesToValue(a.data[start:end])
}

// setItem sets the item at index i
func (a *ArrayValue) setItem(i int, value core.Value) error {
	length := a.Len()
	if i < 0 {
		i = length + i
	}
	if i < 0 || i >= length {
		return fmt.Errorf("array assignment index out of range")
	}
	bytes, err := a.valueToBytes(value)
	if err != nil {
		return err
	}
	start := i * a.itemsize
	copy(a.data[start:], bytes)
	return nil
}

// pop removes and returns the item at index i
func (a *ArrayValue) pop(i int) (core.Value, error) {
	length := a.Len()
	if length == 0 {
		return nil, fmt.Errorf("pop from empty array")
	}
	if i < 0 {
		i = length + i
	}
	if i < 0 || i >= length {
		return nil, fmt.Errorf("pop index out of range")
	}
	value, err := a.getItem(i)
	if err != nil {
		return nil, err
	}
	// Remove the item
	start := i * a.itemsize
	end := start + a.itemsize
	a.data = append(a.data[:start], a.data[end:]...)
	return value, nil
}

// toList converts the array to a list of values
func (a *ArrayValue) toList() []core.Value {
	length := a.Len()
	result := make([]core.Value, length)
	for i := 0; i < length; i++ {
		v, _ := a.getItem(i)
		result[i] = v
	}
	return result
}

// tobytes returns the array data as bytes
func (a *ArrayValue) tobytes() core.BytesValue {
	return core.BytesValue(a.data)
}

// frombytes extends the array from bytes
func (a *ArrayValue) frombytes(b []byte) error {
	if len(b)%a.itemsize != 0 {
		return &core.ValueError{Message: "bytes length not a multiple of item size"}
	}
	a.data = append(a.data, b...)
	return nil
}

// valueToBytes converts a Python value to bytes for storage
func (a *ArrayValue) valueToBytes(value core.Value) ([]byte, error) {
	buf := new(bytes.Buffer)

	switch a.typecode {
	case 'b': // signed char
		n, err := arrayToInt64(value)
		if err != nil {
			return nil, err
		}
		if n < -128 || n > 127 {
			return nil, fmt.Errorf("signed char is greater than maximum")
		}
		binary.Write(buf, binary.LittleEndian, int8(n))
	case 'B': // unsigned char
		n, err := arrayToInt64(value)
		if err != nil {
			return nil, err
		}
		if n < 0 || n > 255 {
			return nil, fmt.Errorf("unsigned byte integer is greater than maximum")
		}
		binary.Write(buf, binary.LittleEndian, uint8(n))
	case 'h': // signed short
		n, err := arrayToInt64(value)
		if err != nil {
			return nil, err
		}
		if n < -32768 || n > 32767 {
			return nil, fmt.Errorf("signed short integer is greater than maximum")
		}
		binary.Write(buf, binary.LittleEndian, int16(n))
	case 'H': // unsigned short
		n, err := arrayToInt64(value)
		if err != nil {
			return nil, err
		}
		if n < 0 || n > 65535 {
			return nil, fmt.Errorf("unsigned short integer is greater than maximum")
		}
		binary.Write(buf, binary.LittleEndian, uint16(n))
	case 'i', 'l': // signed int/long
		n, err := arrayToInt64(value)
		if err != nil {
			return nil, err
		}
		if n < -2147483648 || n > 2147483647 {
			return nil, fmt.Errorf("signed integer is greater than maximum")
		}
		binary.Write(buf, binary.LittleEndian, int32(n))
	case 'I', 'L': // unsigned int/long
		n, err := arrayToInt64(value)
		if err != nil {
			return nil, err
		}
		if n < 0 || n > 4294967295 {
			return nil, fmt.Errorf("unsigned integer is greater than maximum")
		}
		binary.Write(buf, binary.LittleEndian, uint32(n))
	case 'q': // signed long long
		n, err := arrayToInt64(value)
		if err != nil {
			return nil, err
		}
		binary.Write(buf, binary.LittleEndian, n)
	case 'Q': // unsigned long long
		n, err := arrayToInt64(value)
		if err != nil {
			return nil, err
		}
		binary.Write(buf, binary.LittleEndian, uint64(n))
	case 'f': // float
		f, err := arrayToFloat64(value)
		if err != nil {
			return nil, err
		}
		binary.Write(buf, binary.LittleEndian, float32(f))
	case 'd': // double
		f, err := arrayToFloat64(value)
		if err != nil {
			return nil, err
		}
		binary.Write(buf, binary.LittleEndian, f)
	case 'u': // Unicode (deprecated)
		n, err := arrayToInt64(value)
		if err != nil {
			return nil, err
		}
		binary.Write(buf, binary.LittleEndian, int32(n))
	default:
		return nil, fmt.Errorf("unsupported typecode: %c", a.typecode)
	}

	return buf.Bytes(), nil
}

// bytesToValue converts stored bytes back to a Python value
func (a *ArrayValue) bytesToValue(data []byte) (core.Value, error) {
	buf := bytes.NewReader(data)

	switch a.typecode {
	case 'b':
		var v int8
		binary.Read(buf, binary.LittleEndian, &v)
		return core.NumberValue(v), nil
	case 'B':
		var v uint8
		binary.Read(buf, binary.LittleEndian, &v)
		return core.NumberValue(v), nil
	case 'h':
		var v int16
		binary.Read(buf, binary.LittleEndian, &v)
		return core.NumberValue(v), nil
	case 'H':
		var v uint16
		binary.Read(buf, binary.LittleEndian, &v)
		return core.NumberValue(v), nil
	case 'i', 'l':
		var v int32
		binary.Read(buf, binary.LittleEndian, &v)
		return core.NumberValue(v), nil
	case 'I', 'L':
		var v uint32
		binary.Read(buf, binary.LittleEndian, &v)
		return core.NumberValue(v), nil
	case 'q':
		var v int64
		binary.Read(buf, binary.LittleEndian, &v)
		return core.NumberValue(v), nil
	case 'Q':
		var v uint64
		binary.Read(buf, binary.LittleEndian, &v)
		return core.NumberValue(v), nil
	case 'f':
		var v float32
		binary.Read(buf, binary.LittleEndian, &v)
		return core.NumberValue(v), nil
	case 'd':
		var v float64
		binary.Read(buf, binary.LittleEndian, &v)
		return core.NumberValue(v), nil
	case 'u':
		var v int32
		binary.Read(buf, binary.LittleEndian, &v)
		return core.StringValue(string(rune(v))), nil
	default:
		return nil, fmt.Errorf("unsupported typecode: %c", a.typecode)
	}
}

func arrayToInt64(v core.Value) (int64, error) {
	switch n := v.(type) {
	case core.NumberValue:
		return int64(n), nil
	case core.BigIntValue:
		i, ok := n.ToInt64()
		if !ok {
			return 0, fmt.Errorf("integer too large for array")
		}
		return i, nil
	case core.BoolValue:
		if n {
			return 1, nil
		}
		return 0, nil
	default:
		return 0, fmt.Errorf("an integer is required (got type %s)", v.Type())
	}
}

func arrayToFloat64(v core.Value) (float64, error) {
	switch n := v.(type) {
	case core.NumberValue:
		return float64(n), nil
	case core.BigIntValue:
		return n.ToFloat64(), nil
	default:
		return 0, fmt.Errorf("a float is required (got type %s)", v.Type())
	}
}

// GetAttr implements attribute access for array objects
func (a *ArrayValue) GetAttr(name string) (core.Value, bool) {
	switch name {
	case "typecode":
		return core.StringValue(string(a.typecode)), true
	case "itemsize":
		return core.NumberValue(a.itemsize), true
	case "__len__":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.NumberValue(a.Len()), nil
		}), true
	case "__getitem__":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("__getitem__ requires 1 argument")
			}
			idx, err := arrayToInt64(args[0])
			if err != nil {
				return nil, err
			}
			return a.getItem(int(idx))
		}), true
	case "__setitem__":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) != 2 {
				return nil, fmt.Errorf("__setitem__ requires 2 arguments")
			}
			idx, err := arrayToInt64(args[0])
			if err != nil {
				return nil, err
			}
			return core.None, a.setItem(int(idx), args[1])
		}), true
	case "__iter__":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return &arrayIterator{array: a, index: 0}, nil
		}), true
	case "append":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("append() takes exactly one argument")
			}
			return core.None, a.append(args[0])
		}), true
	case "extend":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("extend() takes exactly one argument")
			}
			// Handle different iterable types
			switch v := args[0].(type) {
			case *ArrayValue:
				if v.typecode != a.typecode {
					return nil, fmt.Errorf("can only extend with array of same type")
				}
				a.data = append(a.data, v.data...)
			case *core.ListValue:
				return core.None, a.extend(v.Items())
			case core.TupleValue:
				return core.None, a.extend([]core.Value(v))
			default:
				return nil, fmt.Errorf("extend() argument must be iterable")
			}
			return core.None, nil
		}), true
	case "pop":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			i := -1
			if len(args) > 0 {
				idx, err := arrayToInt64(args[0])
				if err != nil {
					return nil, err
				}
				i = int(idx)
			}
			return a.pop(i)
		}), true
	case "insert":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) != 2 {
				return nil, fmt.Errorf("insert() takes exactly 2 arguments")
			}
			idx, err := arrayToInt64(args[0])
			if err != nil {
				return nil, err
			}
			i := int(idx)
			length := a.Len()
			if i < 0 {
				i = length + i
				if i < 0 {
					i = 0
				}
			}
			if i > length {
				i = length
			}
			// Convert value to bytes
			valBytes, err := a.valueToBytes(args[1])
			if err != nil {
				return nil, err
			}
			// Insert at position i
			pos := i * a.itemsize
			newData := make([]byte, len(a.data)+a.itemsize)
			copy(newData[:pos], a.data[:pos])
			copy(newData[pos:pos+a.itemsize], valBytes)
			copy(newData[pos+a.itemsize:], a.data[pos:])
			a.data = newData
			return core.None, nil
		}), true
	case "remove":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("remove() takes exactly 1 argument")
			}
			// Find and remove the first occurrence
			length := a.Len()
			for i := 0; i < length; i++ {
				item, _ := a.getItem(i)
				if core.EqualValues(item, args[0]) {
					a.pop(i)
					return core.None, nil
				}
			}
			return nil, fmt.Errorf("array.remove(x): x not in array")
		}), true
	case "reverse":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			length := a.Len()
			for i := 0; i < length/2; i++ {
				j := length - 1 - i
				// Swap items at i and j
				startI := i * a.itemsize
				startJ := j * a.itemsize
				for k := 0; k < a.itemsize; k++ {
					a.data[startI+k], a.data[startJ+k] = a.data[startJ+k], a.data[startI+k]
				}
			}
			return core.None, nil
		}), true
	case "count":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("count() takes exactly 1 argument")
			}
			count := 0
			length := a.Len()
			for i := 0; i < length; i++ {
				item, _ := a.getItem(i)
				if core.EqualValues(item, args[0]) {
					count++
				}
			}
			return core.NumberValue(count), nil
		}), true
	case "index":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) < 1 {
				return nil, fmt.Errorf("index() takes at least 1 argument")
			}
			start := 0
			stop := a.Len()
			if len(args) >= 2 {
				s, err := arrayToInt64(args[1])
				if err != nil {
					return nil, err
				}
				start = int(s)
			}
			if len(args) >= 3 {
				s, err := arrayToInt64(args[2])
				if err != nil {
					return nil, err
				}
				stop = int(s)
			}
			for i := start; i < stop; i++ {
				item, _ := a.getItem(i)
				if core.EqualValues(item, args[0]) {
					return core.NumberValue(i), nil
				}
			}
			return nil, fmt.Errorf("array.index(x): x not in array")
		}), true
	case "tobytes":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return a.tobytes(), nil
		}), true
	case "tolist":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.NewList(a.toList()...), nil
		}), true
	case "frombytes":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("frombytes() takes exactly 1 argument")
			}
			b, ok := args[0].(core.BytesValue)
			if !ok {
				return nil, fmt.Errorf("frombytes() argument must be bytes")
			}
			return core.None, a.frombytes([]byte(b))
		}), true
	case "fromlist":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("fromlist() takes exactly 1 argument")
			}
			list, ok := args[0].(*core.ListValue)
			if !ok {
				return nil, fmt.Errorf("fromlist() argument must be a list")
			}
			return core.None, a.extend(list.Items())
		}), true
	case "buffer_info":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			// Return (address, length) - address is simulated as 0
			return core.TupleValue{core.NumberValue(0), core.NumberValue(a.Len())}, nil
		}), true
	case "__class__":
		if a.class != nil {
			return a.class, true
		}
		return nil, false
	case "__eq__":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("__eq__ requires 1 argument")
			}
			other, ok := args[0].(*ArrayValue)
			if !ok {
				return core.False, nil
			}
			if a.typecode != other.typecode {
				return core.False, nil
			}
			if len(a.data) != len(other.data) {
				return core.False, nil
			}
			if !bytes.Equal(a.data, other.data) {
				return core.False, nil
			}
			return core.True, nil
		}), true
	case "__ne__":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("__ne__ requires 1 argument")
			}
			other, ok := args[0].(*ArrayValue)
			if !ok {
				return core.True, nil
			}
			if a.typecode != other.typecode {
				return core.True, nil
			}
			if len(a.data) != len(other.data) {
				return core.True, nil
			}
			if !bytes.Equal(a.data, other.data) {
				return core.True, nil
			}
			return core.False, nil
		}), true
	}
	return nil, false
}

// arrayIterator implements iteration over an array
type arrayIterator struct {
	core.BaseObject
	array *ArrayValue
	index int
}

func (it *arrayIterator) Type() core.Type {
	return core.Type("array_iterator")
}

func (it *arrayIterator) String() string {
	return "<array_iterator>"
}

func (it *arrayIterator) GetAttr(name string) (core.Value, bool) {
	if name == "__next__" {
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if it.index >= it.array.Len() {
				return nil, &core.StopIteration{}
			}
			val, err := it.array.getItem(it.index)
			it.index++
			return val, err
		}), true
	}
	if name == "__iter__" {
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return it, nil
		}), true
	}
	return nil, false
}

// Machine format codes for _array_reconstructor
const (
	UNSIGNED_INT8      = 0
	SIGNED_INT8        = 1
	UNSIGNED_INT16_LE  = 2
	UNSIGNED_INT16_BE  = 3
	SIGNED_INT16_LE    = 4
	SIGNED_INT16_BE    = 5
	UNSIGNED_INT32_LE  = 6
	UNSIGNED_INT32_BE  = 7
	SIGNED_INT32_LE    = 8
	SIGNED_INT32_BE    = 9
	UNSIGNED_INT64_LE  = 10
	UNSIGNED_INT64_BE  = 11
	SIGNED_INT64_LE    = 12
	SIGNED_INT64_BE    = 13
	IEEE_754_FLOAT_LE  = 14
	IEEE_754_FLOAT_BE  = 15
	IEEE_754_DOUBLE_LE = 16
	IEEE_754_DOUBLE_BE = 17
	UTF16_LE           = 18
	UTF16_BE           = 19
	UTF32_LE           = 20
	UTF32_BE           = 21
)

// decodeByMformatCode decodes bytes according to machine format code
func decodeByMformatCode(data []byte, mcode int) ([]core.Value, error) {
	var values []core.Value

	switch mcode {
	case UNSIGNED_INT8:
		for _, b := range data {
			values = append(values, core.NumberValue(b))
		}
	case SIGNED_INT8:
		for _, b := range data {
			values = append(values, core.NumberValue(int8(b)))
		}
	case UNSIGNED_INT16_LE:
		if len(data)%2 != 0 {
			return nil, &core.ValueError{Message: "bytes length not a multiple of item size"}
		}
		for i := 0; i < len(data); i += 2 {
			v := binary.LittleEndian.Uint16(data[i : i+2])
			values = append(values, core.NumberValue(v))
		}
	case UNSIGNED_INT16_BE:
		if len(data)%2 != 0 {
			return nil, &core.ValueError{Message: "bytes length not a multiple of item size"}
		}
		for i := 0; i < len(data); i += 2 {
			v := binary.BigEndian.Uint16(data[i : i+2])
			values = append(values, core.NumberValue(v))
		}
	case SIGNED_INT16_LE:
		if len(data)%2 != 0 {
			return nil, &core.ValueError{Message: "bytes length not a multiple of item size"}
		}
		for i := 0; i < len(data); i += 2 {
			v := int16(binary.LittleEndian.Uint16(data[i : i+2]))
			values = append(values, core.NumberValue(v))
		}
	case SIGNED_INT16_BE:
		if len(data)%2 != 0 {
			return nil, &core.ValueError{Message: "bytes length not a multiple of item size"}
		}
		for i := 0; i < len(data); i += 2 {
			v := int16(binary.BigEndian.Uint16(data[i : i+2]))
			values = append(values, core.NumberValue(v))
		}
	case UNSIGNED_INT32_LE:
		if len(data)%4 != 0 {
			return nil, &core.ValueError{Message: "bytes length not a multiple of item size"}
		}
		for i := 0; i < len(data); i += 4 {
			v := binary.LittleEndian.Uint32(data[i : i+4])
			values = append(values, core.NumberValue(v))
		}
	case UNSIGNED_INT32_BE:
		if len(data)%4 != 0 {
			return nil, &core.ValueError{Message: "bytes length not a multiple of item size"}
		}
		for i := 0; i < len(data); i += 4 {
			v := binary.BigEndian.Uint32(data[i : i+4])
			values = append(values, core.NumberValue(v))
		}
	case SIGNED_INT32_LE:
		if len(data)%4 != 0 {
			return nil, &core.ValueError{Message: "bytes length not a multiple of item size"}
		}
		for i := 0; i < len(data); i += 4 {
			v := int32(binary.LittleEndian.Uint32(data[i : i+4]))
			values = append(values, core.NumberValue(v))
		}
	case SIGNED_INT32_BE:
		if len(data)%4 != 0 {
			return nil, &core.ValueError{Message: "bytes length not a multiple of item size"}
		}
		for i := 0; i < len(data); i += 4 {
			v := int32(binary.BigEndian.Uint32(data[i : i+4]))
			values = append(values, core.NumberValue(v))
		}
	case UNSIGNED_INT64_LE:
		if len(data)%8 != 0 {
			return nil, &core.ValueError{Message: "bytes length not a multiple of item size"}
		}
		for i := 0; i < len(data); i += 8 {
			v := binary.LittleEndian.Uint64(data[i : i+8])
			values = append(values, core.NumberValue(v))
		}
	case UNSIGNED_INT64_BE:
		if len(data)%8 != 0 {
			return nil, &core.ValueError{Message: "bytes length not a multiple of item size"}
		}
		for i := 0; i < len(data); i += 8 {
			v := binary.BigEndian.Uint64(data[i : i+8])
			values = append(values, core.NumberValue(v))
		}
	case SIGNED_INT64_LE:
		if len(data)%8 != 0 {
			return nil, &core.ValueError{Message: "bytes length not a multiple of item size"}
		}
		for i := 0; i < len(data); i += 8 {
			v := int64(binary.LittleEndian.Uint64(data[i : i+8]))
			values = append(values, core.NumberValue(v))
		}
	case SIGNED_INT64_BE:
		if len(data)%8 != 0 {
			return nil, &core.ValueError{Message: "bytes length not a multiple of item size"}
		}
		for i := 0; i < len(data); i += 8 {
			v := int64(binary.BigEndian.Uint64(data[i : i+8]))
			values = append(values, core.NumberValue(v))
		}
	case IEEE_754_FLOAT_LE:
		if len(data)%4 != 0 {
			return nil, &core.ValueError{Message: "bytes length not a multiple of item size"}
		}
		for i := 0; i < len(data); i += 4 {
			bits := binary.LittleEndian.Uint32(data[i : i+4])
			v := math.Float32frombits(bits)
			values = append(values, core.NumberValue(v))
		}
	case IEEE_754_FLOAT_BE:
		if len(data)%4 != 0 {
			return nil, &core.ValueError{Message: "bytes length not a multiple of item size"}
		}
		for i := 0; i < len(data); i += 4 {
			bits := binary.BigEndian.Uint32(data[i : i+4])
			v := math.Float32frombits(bits)
			values = append(values, core.NumberValue(v))
		}
	case IEEE_754_DOUBLE_LE:
		if len(data)%8 != 0 {
			return nil, &core.ValueError{Message: "bytes length not a multiple of item size"}
		}
		for i := 0; i < len(data); i += 8 {
			bits := binary.LittleEndian.Uint64(data[i : i+8])
			v := math.Float64frombits(bits)
			values = append(values, core.NumberValue(v))
		}
	case IEEE_754_DOUBLE_BE:
		if len(data)%8 != 0 {
			return nil, &core.ValueError{Message: "bytes length not a multiple of item size"}
		}
		for i := 0; i < len(data); i += 8 {
			bits := binary.BigEndian.Uint64(data[i : i+8])
			v := math.Float64frombits(bits)
			values = append(values, core.NumberValue(v))
		}
	case UTF16_LE, UTF16_BE, UTF32_LE, UTF32_BE:
		// For unicode, just pass through as integers for now
		// Full unicode support would need proper conversion
		if mcode == UTF16_LE || mcode == UTF16_BE {
			if len(data)%2 != 0 {
				return nil, &core.ValueError{Message: "bytes length not a multiple of item size"}
			}
			for i := 0; i < len(data); i += 2 {
				var v uint16
				if mcode == UTF16_LE {
					v = binary.LittleEndian.Uint16(data[i : i+2])
				} else {
					v = binary.BigEndian.Uint16(data[i : i+2])
				}
				values = append(values, core.NumberValue(v))
			}
		} else {
			if len(data)%4 != 0 {
				return nil, &core.ValueError{Message: "bytes length not a multiple of item size"}
			}
			for i := 0; i < len(data); i += 4 {
				var v uint32
				if mcode == UTF32_LE {
					v = binary.LittleEndian.Uint32(data[i : i+4])
				} else {
					v = binary.BigEndian.Uint32(data[i : i+4])
				}
				values = append(values, core.NumberValue(v))
			}
		}
	default:
		return nil, fmt.Errorf("unknown machine format code: %d", mcode)
	}

	return values, nil
}

// InitArrayModule creates the array module
func InitArrayModule() *core.DictValue {
	arrayModule := core.NewDict()

	// Create the array class
	arrayClass := core.NewClass("array", nil)

	// array.array(typecode, [initializer]) constructor
	arrayModule.Set("array", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("array() missing required argument: 'typecode'")
		}

		// Get typecode
		typecodeStr, ok := args[0].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("array() argument 1 must be a unicode character, not %s", args[0].Type())
		}
		if len(typecodeStr) != 1 {
			return nil, fmt.Errorf("array() argument 1 must be a unicode character, not str")
		}
		typecode := byte(typecodeStr[0])

		arr, err := NewArrayValue(typecode, arrayClass)
		if err != nil {
			return nil, err
		}

		// Handle optional initializer
		if len(args) >= 2 {
			switch init := args[1].(type) {
			case *core.ListValue:
				if err := arr.extend(init.Items()); err != nil {
					return nil, err
				}
			case core.TupleValue:
				if err := arr.extend([]core.Value(init)); err != nil {
					return nil, err
				}
			case core.BytesValue:
				if err := arr.frombytes([]byte(init)); err != nil {
					return nil, err
				}
			case *ArrayValue:
				if init.typecode != typecode {
					return nil, fmt.Errorf("bad argument type for built-in operation")
				}
				arr.data = append(arr.data, init.data...)
			case core.StringValue:
				// For 'u' typecode, initialize from string
				if typecode == 'u' {
					for _, r := range string(init) {
						if err := arr.append(core.NumberValue(r)); err != nil {
							return nil, err
						}
					}
				} else {
					return nil, fmt.Errorf("cannot use a str to initialize an array with typecode '%c'", typecode)
				}
			default:
				// Try to iterate
				if iter, ok := args[1].(interface {
					GetAttr(string) (core.Value, bool)
				}); ok {
					if iterMethod, found := iter.GetAttr("__iter__"); found {
						if callable, ok := iterMethod.(core.Callable); ok {
							iterator, err := callable.Call(nil, ctx)
							if err != nil {
								return nil, err
							}
							if nextGetter, ok := iterator.(interface {
								GetAttr(string) (core.Value, bool)
							}); ok {
								if nextMethod, found := nextGetter.GetAttr("__next__"); found {
									if nextCallable, ok := nextMethod.(core.Callable); ok {
										for {
											item, err := nextCallable.Call(nil, ctx)
											if err != nil {
												if _, ok := err.(*core.StopIteration); ok {
													break
												}
												return nil, err
											}
											if err := arr.append(item); err != nil {
												return nil, err
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}

		return arr, nil
	}))

	// ArrayType for isinstance checks
	arrayModule.Set("ArrayType", arrayClass)

	// typecodes - string of all available type codes
	arrayModule.Set("typecodes", core.StringValue("bBuhHiIlLqQfd"))

	// _array_reconstructor(arraytype, typecode, mformat_code, items)
	// Internal function used for pickling support
	// Machine format codes:
	// 0=UNSIGNED_INT8, 1=SIGNED_INT8, 2=UNSIGNED_INT16_LE, 3=UNSIGNED_INT16_BE,
	// 4=SIGNED_INT16_LE, 5=SIGNED_INT16_BE, 6=UNSIGNED_INT32_LE, 7=UNSIGNED_INT32_BE,
	// 8=SIGNED_INT32_LE, 9=SIGNED_INT32_BE, 10=UNSIGNED_INT64_LE, 11=UNSIGNED_INT64_BE,
	// 12=SIGNED_INT64_LE, 13=SIGNED_INT64_BE, 14=IEEE_754_FLOAT_LE, 15=IEEE_754_FLOAT_BE,
	// 16=IEEE_754_DOUBLE_LE, 17=IEEE_754_DOUBLE_BE, 18=UTF16_LE, 19=UTF16_BE,
	// 20=UTF32_LE, 21=UTF32_BE
	arrayModule.Set("_array_reconstructor", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 4 {
			return nil, fmt.Errorf("_array_reconstructor() requires 4 arguments")
		}

		// args[0] is arraytype (the class) - must be array.array or subclass
		switch args[0].(type) {
		case *core.Class:
			// OK
		case core.StringValue:
			return nil, &core.TypeError{Message: "first argument must be a type object, not str"}
		default:
			if _, ok := args[0].(*core.BuiltinFunction); !ok {
				return nil, &core.TypeError{Message: fmt.Sprintf("first argument must be a type object, not %s", args[0].Type())}
			}
		}

		// args[1] is typecode
		typecodeVal, ok := args[1].(core.StringValue)
		if !ok {
			return nil, &core.TypeError{Message: fmt.Sprintf("array typecode must be str, not %s", args[1].Type())}
		}
		if len(typecodeVal) != 1 {
			return nil, &core.ValueError{Message: "bad typecode (must be b, B, u, h, H, i, I, l, L, q, Q, f or d)"}
		}
		typecode := byte(typecodeVal[0])
		if _, valid := typecodeSizes[typecode]; !valid {
			return nil, &core.ValueError{Message: "bad typecode (must be b, B, u, h, H, i, I, l, L, q, Q, f or d)"}
		}

		// args[2] is mformat_code
		mformatCode, ok := args[2].(core.NumberValue)
		if !ok {
			return nil, &core.TypeError{Message: fmt.Sprintf("third argument must be an integer, not %s", args[2].Type())}
		}
		mcode := int(mformatCode)
		if mcode < 0 || mcode > 21 {
			return nil, &core.ValueError{Message: "second argument not a valid machine format code."}
		}

		// args[3] is items (bytes)
		itemsBytes, ok := args[3].(core.BytesValue)
		if !ok {
			if _, isStr := args[3].(core.StringValue); isStr {
				return nil, &core.TypeError{Message: "fourth argument should be bytes, not str"}
			}
			return nil, &core.TypeError{Message: fmt.Sprintf("fourth argument should be bytes, not %s", args[3].Type())}
		}

		arr, err := NewArrayValue(typecode, arrayClass)
		if err != nil {
			return nil, err
		}

		// Decode bytes based on mformat_code and store as values
		data := []byte(itemsBytes)
		values, err := decodeByMformatCode(data, mcode)
		if err != nil {
			return nil, err
		}

		// Extend array with decoded values
		if err := arr.extend(values); err != nil {
			return nil, err
		}

		return arr, nil
	}))

	return arrayModule
}
