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

	// Constants
	ioModule.Set("DEFAULT_BUFFER_SIZE", core.NumberValue(8192))

	// Base I/O classes - these are abstract base classes in Python
	// They define the interface but don't provide implementations
	ioBaseClass := core.NewClass("IOBase", nil)
	ioModule.Set("IOBase", ioBaseClass)

	rawIOBaseClass := core.NewClass("RawIOBase", ioBaseClass)
	ioModule.Set("RawIOBase", rawIOBaseClass)

	bufferedIOBaseClass := core.NewClass("BufferedIOBase", ioBaseClass)
	ioModule.Set("BufferedIOBase", bufferedIOBaseClass)

	textIOBaseClass := core.NewClass("TextIOBase", ioBaseClass)
	ioModule.Set("TextIOBase", textIOBaseClass)

	// Register StringIO class
	ioModule.SetWithKey("StringIO", core.StringValue("StringIO"), core.NewBuiltinFunction(newStringIO))
	// Register BytesIO class
	ioModule.SetWithKey("BytesIO", core.StringValue("BytesIO"), core.NewBuiltinFunction(newBytesIO))
	// Register TextIOWrapper class
	ioModule.SetWithKey("TextIOWrapper", core.StringValue("TextIOWrapper"), core.NewBuiltinFunction(newTextIOWrapper))

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

// TextIOWrapper wraps a binary stream and provides text I/O
type TextIOWrapper struct {
	buffer    strings.Builder
	pos       int
	buffer_io core.Value // Underlying binary stream
}

func newTextIOWrapper(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("TextIOWrapper", args)

	// TextIOWrapper(buffer, encoding=None, errors=None, newline=None, line_buffering=False)
	// For now, we only care about the buffer argument
	if err := v.Range(1, 5); err != nil {
		return nil, err
	}

	t := &TextIOWrapper{
		pos:       0,
		buffer_io: args[0],
	}

	// Create dict with methods
	obj := core.NewDict()
	obj.SetWithKey("write", core.StringValue("write"), core.NewBuiltinFunction(t.write))
	obj.SetWithKey("read", core.StringValue("read"), core.NewBuiltinFunction(t.read))
	obj.SetWithKey("readline", core.StringValue("readline"), core.NewBuiltinFunction(t.readline))
	obj.SetWithKey("close", core.StringValue("close"), core.NewBuiltinFunction(t.close))
	obj.SetWithKey("flush", core.StringValue("flush"), core.NewBuiltinFunction(t.flush))
	obj.SetWithKey("__enter__", core.StringValue("__enter__"), core.NewBuiltinFunction(t.enter))
	obj.SetWithKey("__exit__", core.StringValue("__exit__"), core.NewBuiltinFunction(t.exit))

	return obj, nil
}

func (t *TextIOWrapper) write(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("write", args)
	if err := v.Exact(1); err != nil {
		return nil, err
	}

	str, err := v.GetString(0)
	if err != nil {
		return nil, err
	}

	n, _ := t.buffer.WriteString(str)
	return core.NumberValue(n), nil
}

func (t *TextIOWrapper) read(args []core.Value, ctx *core.Context) (core.Value, error) {
	return core.StringValue(t.buffer.String()), nil
}

func (t *TextIOWrapper) readline(args []core.Value, ctx *core.Context) (core.Value, error) {
	// For now, just return empty string
	// In a real implementation, this would read one line from the buffer
	return core.StringValue(""), nil
}

func (t *TextIOWrapper) close(args []core.Value, ctx *core.Context) (core.Value, error) {
	return core.None, nil
}

func (t *TextIOWrapper) flush(args []core.Value, ctx *core.Context) (core.Value, error) {
	return core.None, nil
}

func (t *TextIOWrapper) enter(args []core.Value, ctx *core.Context) (core.Value, error) {
	// Return self for context manager protocol
	// We need to return the wrapper object itself
	// For now, return None as we don't have easy access to self
	return core.None, nil
}

func (t *TextIOWrapper) exit(args []core.Value, ctx *core.Context) (core.Value, error) {
	// Close on exit
	return t.close(args, ctx)
}
