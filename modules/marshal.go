package modules

import (
	"encoding/binary"
	"fmt"

	"github.com/mmichie/m28/core"
)

// Marshal format type codes (matching CPython's marshal.c)
const (
	TYPE_NULL        = '0'
	TYPE_NONE        = 'N'
	TYPE_FALSE       = 'F'
	TYPE_TRUE        = 'T'
	TYPE_INT         = 'i'
	TYPE_FLOAT       = 'f'
	TYPE_STRING      = 's'
	TYPE_TUPLE       = '('
	TYPE_LIST        = '['
	TYPE_DICT        = '{'
	TYPE_CODE        = 'c'
	TYPE_UNICODE     = 'u'
	TYPE_BYTES       = 's' // Same as string in marshal
	TYPE_SHORT_ASCII = 'z'
	TYPE_SMALL_TUPLE = ')'
)

// InitMarshalModule creates and returns the marshal module
// Implements Python's marshal module for bytecode serialization
func InitMarshalModule() *core.DictValue {
	marshalModule := core.NewDict()

	// dumps - serialize object to bytes
	marshalModule.Set("dumps", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("dumps() takes exactly 1 argument (%d given)", len(args))
		}

		buf := make([]byte, 0)
		buf, err := marshalWrite(args[0], buf)
		if err != nil {
			return nil, err
		}

		return core.BytesValue(buf), nil
	}))

	// loads - deserialize object from bytes
	marshalModule.Set("loads", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("loads() takes exactly 1 argument (%d given)", len(args))
		}

		bytesVal, ok := args[0].(core.BytesValue)
		if !ok {
			return nil, fmt.Errorf("loads() argument must be bytes, not %s", args[0].Type())
		}

		val, _, err := marshalRead([]byte(bytesVal))
		if err != nil {
			return nil, err
		}

		return val, nil
	}))

	// dump - serialize object to file
	marshalModule.Set("dump", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, fmt.Errorf("dump() takes exactly 2 arguments (%d given)", len(args))
		}

		buf := make([]byte, 0)
		buf, err := marshalWrite(args[0], buf)
		if err != nil {
			return nil, err
		}

		// Write to file - not implemented yet
		_, ok := args[1].(*core.File)
		if !ok {
			return nil, fmt.Errorf("dump() argument 2 must be a file, not %s", args[1].Type())
		}

		// For now, skip file writing - focus on dumps/loads
		return core.NilValue{}, fmt.Errorf("dump() not fully implemented yet")
	}))

	// load - deserialize object from file
	marshalModule.Set("load", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("load() takes exactly 1 argument (%d given)", len(args))
		}

		// For now, skip file loading - focus on dumps/loads
		return core.NilValue{}, fmt.Errorf("load() not fully implemented yet")
	}))

	// version - marshal format version
	marshalModule.Set("version", core.NumberValue(4))

	return marshalModule
}

// marshalWrite serializes a value to bytes
func marshalWrite(val core.Value, buf []byte) ([]byte, error) {
	switch v := val.(type) {
	case core.NilValue:
		return append(buf, TYPE_NONE), nil

	case core.BoolValue:
		if bool(v) {
			return append(buf, TYPE_TRUE), nil
		}
		return append(buf, TYPE_FALSE), nil

	case core.NumberValue:
		// Check if it's an integer
		if float64(int64(v)) == float64(v) {
			buf = append(buf, TYPE_INT)
			intVal := int32(v)
			tmp := make([]byte, 4)
			binary.LittleEndian.PutUint32(tmp, uint32(intVal))
			return append(buf, tmp...), nil
		}
		// Float
		buf = append(buf, TYPE_FLOAT)
		tmp := make([]byte, 8)
		binary.LittleEndian.PutUint64(tmp, uint64(v))
		return append(buf, tmp...), nil

	case core.StringValue:
		buf = append(buf, TYPE_UNICODE)
		strBytes := []byte(string(v))
		lenBytes := make([]byte, 4)
		binary.LittleEndian.PutUint32(lenBytes, uint32(len(strBytes)))
		buf = append(buf, lenBytes...)
		return append(buf, strBytes...), nil

	case core.BytesValue:
		buf = append(buf, TYPE_STRING)
		lenBytes := make([]byte, 4)
		binary.LittleEndian.PutUint32(lenBytes, uint32(len(v)))
		buf = append(buf, lenBytes...)
		return append(buf, v...), nil

	case *core.ListValue:
		buf = append(buf, TYPE_LIST)
		items := v.Items() // Use Items() method to get the slice
		lenBytes := make([]byte, 4)
		binary.LittleEndian.PutUint32(lenBytes, uint32(len(items)))
		buf = append(buf, lenBytes...)
		for _, elem := range items {
			var err error
			buf, err = marshalWrite(elem, buf)
			if err != nil {
				return nil, err
			}
		}
		return buf, nil

	case core.TupleValue:
		if len(v) < 256 {
			buf = append(buf, TYPE_SMALL_TUPLE)
			buf = append(buf, byte(len(v)))
		} else {
			buf = append(buf, TYPE_TUPLE)
			lenBytes := make([]byte, 4)
			binary.LittleEndian.PutUint32(lenBytes, uint32(len(v)))
			buf = append(buf, lenBytes...)
		}
		for _, elem := range v {
			var err error
			buf, err = marshalWrite(elem, buf)
			if err != nil {
				return nil, err
			}
		}
		return buf, nil

	case *core.DictValue:
		buf = append(buf, TYPE_DICT)
		for _, key := range v.Keys() {
			var err error
			buf, err = marshalWrite(core.StringValue(key), buf)
			if err != nil {
				return nil, err
			}
			value, _ := v.Get(key)
			buf, err = marshalWrite(value, buf)
			if err != nil {
				return nil, err
			}
		}
		buf = append(buf, TYPE_NULL) // Dict terminator
		return buf, nil

	default:
		return nil, fmt.Errorf("marshal: cannot serialize type %s", val.Type())
	}
}

// marshalRead deserializes bytes to a value
func marshalRead(buf []byte) (core.Value, int, error) {
	if len(buf) == 0 {
		return nil, 0, fmt.Errorf("marshal: EOF while reading type byte")
	}

	typeCode := buf[0]
	pos := 1

	switch typeCode {
	case TYPE_NONE:
		return core.NilValue{}, pos, nil

	case TYPE_TRUE:
		return core.BoolValue(true), pos, nil

	case TYPE_FALSE:
		return core.BoolValue(false), pos, nil

	case TYPE_INT:
		if len(buf) < 5 {
			return nil, 0, fmt.Errorf("marshal: EOF while reading int")
		}
		intVal := int32(binary.LittleEndian.Uint32(buf[1:5]))
		return core.NumberValue(float64(intVal)), 5, nil

	case TYPE_FLOAT:
		if len(buf) < 9 {
			return nil, 0, fmt.Errorf("marshal: EOF while reading float")
		}
		floatBits := binary.LittleEndian.Uint64(buf[1:9])
		return core.NumberValue(float64(floatBits)), 9, nil

	case TYPE_UNICODE, TYPE_STRING:
		if len(buf) < 5 {
			return nil, 0, fmt.Errorf("marshal: EOF while reading string length")
		}
		strlen := binary.LittleEndian.Uint32(buf[1:5])
		pos = 5
		if len(buf) < pos+int(strlen) {
			return nil, 0, fmt.Errorf("marshal: EOF while reading string data")
		}
		strData := buf[pos : pos+int(strlen)]
		pos += int(strlen)
		if typeCode == TYPE_UNICODE {
			return core.StringValue(string(strData)), pos, nil
		}
		return core.BytesValue(strData), pos, nil

	case TYPE_LIST:
		if len(buf) < 5 {
			return nil, 0, fmt.Errorf("marshal: EOF while reading list length")
		}
		listLen := binary.LittleEndian.Uint32(buf[1:5])
		pos = 5
		elements := make([]core.Value, 0, listLen)
		for i := 0; i < int(listLen); i++ {
			elem, n, err := marshalRead(buf[pos:])
			if err != nil {
				return nil, 0, err
			}
			elements = append(elements, elem)
			pos += n
		}
		return core.NewList(elements...), pos, nil

	case TYPE_SMALL_TUPLE:
		if len(buf) < 2 {
			return nil, 0, fmt.Errorf("marshal: EOF while reading tuple length")
		}
		tupleLen := int(buf[1])
		pos = 2
		elements := make([]core.Value, 0, tupleLen)
		for i := 0; i < tupleLen; i++ {
			elem, n, err := marshalRead(buf[pos:])
			if err != nil {
				return nil, 0, err
			}
			elements = append(elements, elem)
			pos += n
		}
		return core.TupleValue(elements), pos, nil

	case TYPE_TUPLE:
		if len(buf) < 5 {
			return nil, 0, fmt.Errorf("marshal: EOF while reading tuple length")
		}
		tupleLen := binary.LittleEndian.Uint32(buf[1:5])
		pos = 5
		elements := make([]core.Value, 0, tupleLen)
		for i := 0; i < int(tupleLen); i++ {
			elem, n, err := marshalRead(buf[pos:])
			if err != nil {
				return nil, 0, err
			}
			elements = append(elements, elem)
			pos += n
		}
		return core.TupleValue(elements), pos, nil

	case TYPE_DICT:
		dict := core.NewDict()
		for {
			if pos >= len(buf) {
				return nil, 0, fmt.Errorf("marshal: EOF while reading dict")
			}
			if buf[pos] == TYPE_NULL {
				pos++
				break
			}
			key, n, err := marshalRead(buf[pos:])
			if err != nil {
				return nil, 0, err
			}
			pos += n

			keyStr, ok := key.(core.StringValue)
			if !ok {
				return nil, 0, fmt.Errorf("marshal: dict key must be string")
			}

			value, n, err := marshalRead(buf[pos:])
			if err != nil {
				return nil, 0, err
			}
			pos += n

			dict.Set(string(keyStr), value)
		}
		return dict, pos, nil

	default:
		return nil, 0, fmt.Errorf("marshal: unknown type code %c (0x%02x)", typeCode, typeCode)
	}
}
