// Package modules provides the io module for M28
package modules

import (
	"bytes"
	"strings"

	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// InitIOModule registers the io module
func InitIOModule() *core.DictValue {
	ioModule := core.NewDict()

	// Register StringIO class
	ioModule.SetWithKey("StringIO", core.StringValue("StringIO"), core.NewBuiltinFunction(newStringIO))
	// Register BytesIO class
	ioModule.SetWithKey("BytesIO", core.StringValue("BytesIO"), core.NewBuiltinFunction(newBytesIO))

	return ioModule
}

// StringIO is an in-memory text stream
type StringIO struct {
	buffer strings.Builder
	pos    int
}

func newStringIO(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("StringIO", args)

	s := &StringIO{pos: 0}

	// Optional initial value
	if v.Count() > 0 {
		if initialStr, err := v.GetString(0); err == nil {
			s.buffer.WriteString(initialStr)
		}
	}

	// Create dict with methods
	obj := core.NewDict()
	obj.SetWithKey("write", core.StringValue("write"), core.NewBuiltinFunction(s.write))
	obj.SetWithKey("read", core.StringValue("read"), core.NewBuiltinFunction(s.read))
	obj.SetWithKey("getvalue", core.StringValue("getvalue"), core.NewBuiltinFunction(s.getvalue))
	obj.SetWithKey("close", core.StringValue("close"), core.NewBuiltinFunction(s.close))

	return obj, nil
}

func (s *StringIO) write(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("write", args)
	if err := v.Exact(1); err != nil {
		return nil, err
	}

	str, err := v.GetString(0)
	if err != nil {
		return nil, err
	}

	n, _ := s.buffer.WriteString(str)
	return core.NumberValue(n), nil
}

func (s *StringIO) read(args []core.Value, ctx *core.Context) (core.Value, error) {
	return core.StringValue(s.buffer.String()), nil
}

func (s *StringIO) getvalue(args []core.Value, ctx *core.Context) (core.Value, error) {
	return core.StringValue(s.buffer.String()), nil
}

func (s *StringIO) close(args []core.Value, ctx *core.Context) (core.Value, error) {
	return core.NilValue{}, nil
}

// BytesIO is an in-memory bytes stream
type BytesIO struct {
	buffer bytes.Buffer
	pos    int
}

func newBytesIO(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("BytesIO", args)

	b := &BytesIO{pos: 0}

	// Optional initial value
	if v.Count() > 0 {
		if initialBytes, ok := args[0].(core.BytesValue); ok {
			b.buffer.Write(initialBytes)
		} else if initialStr, err := v.GetString(0); err == nil {
			b.buffer.WriteString(initialStr)
		}
	}

	// Create dict with methods
	obj := core.NewDict()
	obj.SetWithKey("write", core.StringValue("write"), core.NewBuiltinFunction(b.write))
	obj.SetWithKey("read", core.StringValue("read"), core.NewBuiltinFunction(b.read))
	obj.SetWithKey("getvalue", core.StringValue("getvalue"), core.NewBuiltinFunction(b.getvalue))
	obj.SetWithKey("close", core.StringValue("close"), core.NewBuiltinFunction(b.close))

	return obj, nil
}

func (b *BytesIO) write(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("write", args)
	if err := v.Exact(1); err != nil {
		return nil, err
	}

	if bytesVal, ok := args[0].(core.BytesValue); ok {
		n, _ := b.buffer.Write(bytesVal)
		return core.NumberValue(n), nil
	}

	str, err := v.GetString(0)
	if err != nil {
		return nil, err
	}

	n, _ := b.buffer.WriteString(str)
	return core.NumberValue(n), nil
}

func (b *BytesIO) read(args []core.Value, ctx *core.Context) (core.Value, error) {
	return core.BytesValue(b.buffer.Bytes()), nil
}

func (b *BytesIO) getvalue(args []core.Value, ctx *core.Context) (core.Value, error) {
	return core.BytesValue(b.buffer.Bytes()), nil
}

func (b *BytesIO) close(args []core.Value, ctx *core.Context) (core.Value, error) {
	return core.NilValue{}, nil
}
