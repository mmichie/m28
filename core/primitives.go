package core

import (
	"fmt"
	"strconv"

	"github.com/shopspring/decimal"
)

// NumberValue represents a numeric value
type NumberValue float64

// Type implements Value.Type
func (n NumberValue) Type() Type {
	return NumberType
}

// String implements Value.String
func (n NumberValue) String() string {
	// Format the number to avoid unnecessary decimal places
	s := strconv.FormatFloat(float64(n), 'f', -1, 64)
	return s
}

// GetAttr implements basic number methods using TypeDescriptor
func (n NumberValue) GetAttr(name string) (Value, bool) {
	desc := GetTypeDescriptor(NumberType)
	if desc != nil {
		val, err := desc.GetAttribute(n, name)
		if err == nil {
			return val, true
		}
	}
	return nil, false
}

// ComplexValue represents a complex number value
type ComplexValue complex128

// Type implements Value.Type
func (c ComplexValue) Type() Type {
	return ComplexType
}

// String implements Value.String
func (c ComplexValue) String() string {
	real := real(complex128(c))
	imag := imag(complex128(c))

	// Format like Python: (1+2j), (1-2j), 2j, (1+0j)
	if real == 0 {
		// Pure imaginary: 2j
		return fmt.Sprintf("%gj", imag)
	}

	// Complex with real part
	if imag >= 0 {
		return fmt.Sprintf("(%g+%gj)", real, imag)
	}
	return fmt.Sprintf("(%g%gj)", real, imag)
}

// GetAttr implements basic complex methods using TypeDescriptor
func (c ComplexValue) GetAttr(name string) (Value, bool) {
	desc := GetTypeDescriptor(ComplexType)
	if desc != nil {
		val, err := desc.GetAttribute(c, name)
		if err == nil {
			return val, true
		}
	}
	return nil, false
}

// StringValue represents a string value
type StringValue string

// Type implements Value.Type
func (s StringValue) Type() Type {
	return StringType
}

// String implements Value.String
func (s StringValue) String() string {
	return fmt.Sprintf("%q", string(s))
}

// GetAttr implements basic string methods using TypeDescriptor
func (s StringValue) GetAttr(name string) (Value, bool) {
	desc := GetTypeDescriptor(StringType)
	if desc != nil {
		val, err := desc.GetAttribute(s, name)
		if err == nil {
			return val, true
		}
	}
	return nil, false
}

// BoolValue represents a boolean value
type BoolValue bool

// Type implements Value.Type
func (b BoolValue) Type() Type {
	return BoolType
}

// String implements Value.String
func (b BoolValue) String() string {
	if b {
		return "True"
	}
	return "False"
}

// Pre-defined boolean constants
var (
	True  = BoolValue(true)
	False = BoolValue(false)
)

// SymbolValue represents a symbol identifier
type SymbolValue string

// Type implements Value.Type
func (s SymbolValue) Type() Type {
	return SymbolType
}

// String implements Value.String
func (s SymbolValue) String() string {
	return string(s)
}

// NilValue represents a nil/null/None value
type NilValue struct{}

// Type implements Value.Type
func (n NilValue) Type() Type {
	return NilType
}

// String implements Value.String
func (n NilValue) String() string {
	return "None"
}

// Predefined nil value
var (
	Nil  = NilValue{}
	None = NilValue{} // Python-style alias
)

// BuiltinFunction represents a Go function that can be called from M28
type BuiltinFunction struct {
	BaseObject
	fn       func(args []Value, ctx *Context) (Value, error)
	name     string
	registry *MethodRegistry
}

// NewBuiltinFunction creates a new builtin function
func NewBuiltinFunction(fn func(args []Value, ctx *Context) (Value, error)) *BuiltinFunction {
	f := &BuiltinFunction{
		BaseObject: *NewBaseObject(FunctionType),
		fn:         fn,
		name:       "",
	}

	// Initialize the method registry
	f.registry = f.createRegistry()

	return f
}

// NewNamedBuiltinFunction creates a new builtin function with a name
func NewNamedBuiltinFunction(name string, fn func(args []Value, ctx *Context) (Value, error)) *BuiltinFunction {
	f := &BuiltinFunction{
		BaseObject: *NewBaseObject(FunctionType),
		fn:         fn,
		name:       name,
	}

	// Initialize the method registry
	f.registry = f.createRegistry()

	return f
}

// Call implements Callable.Call
func (f *BuiltinFunction) Call(args []Value, ctx *Context) (Value, error) {
	return f.fn(args, ctx)
}

// String implements Value.String
func (f *BuiltinFunction) String() string {
	if f.name != "" {
		return fmt.Sprintf("<builtin function %s>", f.name)
	}
	return "<builtin function>"
}

// createRegistry sets up all properties for BuiltinFunction
func (f *BuiltinFunction) createRegistry() *MethodRegistry {
	registry := NewMethodRegistry()

	// Register properties
	registry.RegisterProperties(
		MakeProperty("__name__", "Function name", func(receiver Value) (Value, error) {
			fn := receiver.(*BuiltinFunction)
			if fn.name != "" {
				return StringValue(fn.name), nil
			}
			return StringValue("<anonymous>"), nil
		}),
	)

	return registry
}

// GetRegistry implements AttributeProvider
func (f *BuiltinFunction) GetRegistry() *MethodRegistry {
	return f.registry
}

// GetBaseObject implements AttributeProvider
func (f *BuiltinFunction) GetBaseObject() *BaseObject {
	return &f.BaseObject
}

// GetAttr implements the new simplified GetAttr pattern
func (f *BuiltinFunction) GetAttr(name string) (Value, bool) {
	return GetAttrWithRegistry(f, name)
}

// BuiltinMethod represents a method that's implemented in Go
type BuiltinMethod struct {
	BaseObject
	fn       func(receiver Value, args []Value, ctx *Context) (Value, error)
	receiver Value
}

// Type implements Value.Type
func (m *BuiltinMethod) Type() Type {
	return MethodType
}

// Bind implements Method.Bind
func (m *BuiltinMethod) Bind(receiver Value) Value {
	return &BuiltinMethod{
		BaseObject: *NewBaseObject(MethodType),
		fn:         m.fn,
		receiver:   receiver,
	}
}

// Call implements Callable.Call
func (m *BuiltinMethod) Call(args []Value, ctx *Context) (Value, error) {
	return m.fn(m.receiver, args, ctx)
}

// String implements Value.String
func (m *BuiltinMethod) String() string {
	return "<builtin method>"
}

// BytesValue represents an immutable sequence of bytes
type BytesValue []byte

// Type implements Value.Type
func (b BytesValue) Type() Type {
	return BytesType
}

// String implements Value.String
func (b BytesValue) String() string {
	// Format bytes as Python does: b'...'
	// Escape non-printable characters
	result := "b'"
	for _, ch := range b {
		switch ch {
		case '\n':
			result += "\\n"
		case '\r':
			result += "\\r"
		case '\t':
			result += "\\t"
		case '\\':
			result += "\\\\"
		case '\'':
			result += "\\'"
		default:
			if ch >= 32 && ch < 127 {
				result += string(ch)
			} else {
				result += fmt.Sprintf("\\x%02x", ch)
			}
		}
	}
	result += "'"
	return result
}

// GetAttr implements basic bytes methods using TypeDescriptor
func (b BytesValue) GetAttr(name string) (Value, bool) {
	desc := GetTypeDescriptor(BytesType)
	if desc != nil {
		val, err := desc.GetAttribute(b, name)
		if err == nil {
			return val, true
		}
	}
	return nil, false
}

// GetItem gets a byte by index
func (b BytesValue) GetItem(index int) (Value, error) {
	if index < 0 {
		index = len(b) + index
	}
	if index < 0 || index >= len(b) {
		return nil, &IndexError{Index: index, Length: len(b)}
	}
	return NumberValue(b[index]), nil
}

// Iterator implements Iterable
func (b BytesValue) Iterator() Iterator {
	return &bytesIterator{
		bytes: b,
		index: 0,
	}
}

type bytesIterator struct {
	bytes BytesValue
	index int
}

func (it *bytesIterator) Next() (Value, bool) {
	if it.index >= len(it.bytes) {
		return nil, false
	}
	val := NumberValue(it.bytes[it.index])
	it.index++
	return val, true
}

func (it *bytesIterator) Reset() {
	it.index = 0
}

// ByteArrayValue represents a mutable sequence of bytes
type ByteArrayValue struct {
	data []byte
}

// NewByteArray creates a new bytearray
func NewByteArray(data []byte) *ByteArrayValue {
	return &ByteArrayValue{data: data}
}

// Type implements Value.Type
func (b *ByteArrayValue) Type() Type {
	return ByteArrayType
}

// String implements Value.String
func (b *ByteArrayValue) String() string {
	// Format bytearray as Python does: bytearray(b'...')
	result := "bytearray(b'"
	for _, ch := range b.data {
		switch ch {
		case '\n':
			result += "\\n"
		case '\r':
			result += "\\r"
		case '\t':
			result += "\\t"
		case '\\':
			result += "\\\\"
		case '\'':
			result += "\\'"
		default:
			if ch >= 32 && ch < 127 {
				result += string(ch)
			} else {
				result += fmt.Sprintf("\\x%02x", ch)
			}
		}
	}
	result += "')"
	return result
}

// GetAttr implements basic bytearray methods using TypeDescriptor
func (b *ByteArrayValue) GetAttr(name string) (Value, bool) {
	desc := GetTypeDescriptor(ByteArrayType)
	if desc != nil {
		val, err := desc.GetAttribute(b, name)
		if err == nil {
			return val, true
		}
	}
	return nil, false
}

// GetItem gets a byte by index
func (b *ByteArrayValue) GetItem(index int) (Value, error) {
	if index < 0 {
		index = len(b.data) + index
	}
	if index < 0 || index >= len(b.data) {
		return nil, &IndexError{Index: index, Length: len(b.data)}
	}
	return NumberValue(b.data[index]), nil
}

// SetItem sets a byte by index
func (b *ByteArrayValue) SetItem(index int, value Value) error {
	if index < 0 {
		index = len(b.data) + index
	}
	if index < 0 || index >= len(b.data) {
		return &IndexError{Index: index, Length: len(b.data)}
	}

	// Value must be an integer between 0 and 255
	num, ok := value.(NumberValue)
	if !ok {
		return fmt.Errorf("an integer is required")
	}

	intVal := int(num)
	if intVal < 0 || intVal > 255 {
		return fmt.Errorf("byte must be in range(0, 256)")
	}

	b.data[index] = byte(intVal)
	return nil
}

// Iterator implements Iterable
func (b *ByteArrayValue) Iterator() Iterator {
	return &bytearrayIterator{
		bytearray: b,
		index:     0,
	}
}

// GetData returns the internal byte data
func (b *ByteArrayValue) GetData() []byte {
	return b.data
}

type bytearrayIterator struct {
	bytearray *ByteArrayValue
	index     int
}

func (it *bytearrayIterator) Next() (Value, bool) {
	if it.index >= len(it.bytearray.data) {
		return nil, false
	}
	val := NumberValue(it.bytearray.data[it.index])
	it.index++
	return val, true
}

func (it *bytearrayIterator) Reset() {
	it.index = 0
}

// DecimalValue represents a high-precision decimal number
type DecimalValue struct {
	Value decimal.Decimal
}

// NewDecimal creates a new decimal value
func NewDecimal(d decimal.Decimal) *DecimalValue {
	return &DecimalValue{Value: d}
}

// NewDecimalFromString creates a decimal from a string
func NewDecimalFromString(s string) (*DecimalValue, error) {
	d, err := decimal.NewFromString(s)
	if err != nil {
		return nil, err
	}
	return &DecimalValue{Value: d}, nil
}

// NewDecimalFromFloat creates a decimal from a float64
func NewDecimalFromFloat(f float64) *DecimalValue {
	return &DecimalValue{Value: decimal.NewFromFloat(f)}
}

// NewDecimalFromInt creates a decimal from an int64
func NewDecimalFromInt(i int64) *DecimalValue {
	return &DecimalValue{Value: decimal.NewFromInt(i)}
}

// Type implements Value.Type
func (d *DecimalValue) Type() Type {
	return DecimalType
}

// String implements Value.String
func (d *DecimalValue) String() string {
	return d.Value.String()
}

// GetAttr implements basic decimal methods using TypeDescriptor
func (d *DecimalValue) GetAttr(name string) (Value, bool) {
	desc := GetTypeDescriptor(DecimalType)
	if desc != nil {
		val, err := desc.GetAttribute(d, name)
		if err == nil {
			return val, true
		}
	}
	return nil, false
}

// GetDecimal returns the underlying decimal.Decimal
func (d *DecimalValue) GetDecimal() decimal.Decimal {
	return d.Value
}
