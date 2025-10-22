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

// GetAttr implements basic bool methods using TypeDescriptor
func (b BoolValue) GetAttr(name string) (Value, bool) {
	desc := GetTypeDescriptor(BoolType)
	if desc != nil {
		val, err := desc.GetAttribute(b, name)
		if err == nil {
			return val, true
		}
	}
	return nil, false
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

// EllipsisValue represents Python's Ellipsis literal (...)
type EllipsisValue struct{}

// Type implements Value.Type
func (e EllipsisValue) Type() Type {
	return "ellipsis"
}

// String implements Value.String
func (e EllipsisValue) String() string {
	return "Ellipsis"
}

// Predefined Ellipsis value
var Ellipsis = EllipsisValue{}

// NotImplementedValue represents Python's NotImplemented singleton
// Used to indicate that a comparison operation is not implemented
type NotImplementedValue struct{}

// Type implements Value.Type
func (n NotImplementedValue) Type() Type {
	return "NotImplementedType"
}

// String implements Value.String
func (n NotImplementedValue) String() string {
	return "NotImplemented"
}

// Predefined NotImplemented value
var NotImplemented = NotImplementedValue{}

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

// CallWithKeywords implements keyword argument support for BuiltinFunction
// By default, it rejects keyword arguments, but can be overridden
func (f *BuiltinFunction) CallWithKeywords(args []Value, kwargs map[string]Value, ctx *Context) (Value, error) {
	// Default behavior: reject keyword arguments
	if len(kwargs) > 0 {
		funcName := "unknown"
		if f.name != "" {
			funcName = f.name
		}
		return nil, fmt.Errorf("builtin function '%s' does not support keyword arguments", funcName)
	}
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
		MakeProperty("__qualname__", "Qualified name", func(receiver Value) (Value, error) {
			fn := receiver.(*BuiltinFunction)
			// Check if __qualname__ was set in BaseObject attrs
			if val, ok := fn.BaseObject.GetAttr("__qualname__"); ok {
				return val, nil
			}
			// Default to same as __name__
			if fn.name != "" {
				return StringValue(fn.name), nil
			}
			return StringValue("<anonymous>"), nil
		}),
		MakeProperty("__module__", "Module name", func(receiver Value) (Value, error) {
			fn := receiver.(*BuiltinFunction)
			// Check if __module__ was set in BaseObject attrs
			if val, ok := fn.BaseObject.GetAttr("__module__"); ok {
				return val, nil
			}
			// Default to "builtins" for builtin functions
			return StringValue("builtins"), nil
		}),
		MakeProperty("__doc__", "Docstring", func(receiver Value) (Value, error) {
			fn := receiver.(*BuiltinFunction)
			// Check if __doc__ was set in BaseObject attrs
			if val, ok := fn.BaseObject.GetAttr("__doc__"); ok {
				return val, nil
			}
			// Default to None for no docstring
			return None, nil
		}),
		MakeProperty("__dict__", "Function attributes", func(receiver Value) (Value, error) {
			// Return an empty dict for builtin functions
			return NewDict(), nil
		}),
		MakeProperty("__code__", "Function code object", func(receiver Value) (Value, error) {
			// Return a simple code object
			codeObj := NewDict()
			codeObj.Set("co_flags", NumberValue(0))
			codeObj.Set("co_argcount", NumberValue(0))
			return codeObj, nil
		}),
		MakeProperty("__closure__", "Function closure", func(receiver Value) (Value, error) {
			// Return None for builtin functions (no closure)
			// Actually, Python returns None for functions without closure
			return None, nil
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
	// Special handling for __getitem__ to support generic type syntax
	// This allows list[int], dict[str, int], etc. for type hints
	if name == "__getitem__" {
		// Only provide __getitem__ for collection types
		if f.name == "list" || f.name == "tuple" || f.name == "dict" ||
			f.name == "set" || f.name == "frozenset" {
			// Return a function that creates generic alias objects
			return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
				// Create a generic alias with this function as the origin
				// For now, we'll create a pseudo-class to wrap the builtin
				class := NewClass(f.name, nil)
				return NewGenericAlias(class, args), nil
			}), true
		}
	}

	// Special handling for __or__ to support union type syntax (int | str, etc.)
	if name == "__or__" {
		// Type constructor functions support union types
		if f.name == "int" || f.name == "float" || f.name == "str" || f.name == "bool" ||
			f.name == "list" || f.name == "tuple" || f.name == "dict" || f.name == "set" ||
			f.name == "bytes" || f.name == "bytearray" {
			// Return a function that creates union type objects
			return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("__or__ takes exactly 1 argument")
				}
				return NewUnionType([]Value{f, args[0]}), nil
			}), true
		}
	}

	// Special handling for __repr__
	// This allows pprint and other modules to use function.__repr__ as a type marker
	if name == "__repr__" {
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("__repr__ requires exactly 1 argument")
			}
			funcName := f.name
			if funcName == "" {
				funcName = "builtin_function"
			}
			return StringValue(fmt.Sprintf("<%s object>", funcName)), nil
		}), true
	}

	// Try registry and base object
	if val, ok := GetAttrWithRegistry(f, name); ok {
		return val, true
	}

	// Provide default values for standard function attributes
	switch name {
	case "__name__":
		if f.name != "" {
			return StringValue(f.name), true
		}
		return StringValue("<builtin_function>"), true
	case "__qualname__":
		if f.name != "" {
			return StringValue(f.name), true
		}
		return StringValue("<builtin_function>"), true
	case "__module__":
		return StringValue("builtins"), true
	case "__doc__":
		return None, true
	case "__annotations__":
		return NewDict(), true
	case "__type_params__":
		return TupleValue{}, true
	case "__dict__":
		return NewDict(), true
	case "__hash__":
		// Return a hash function for the function
		// Builtin functions are hashable by identity (pointer address)
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			// Use the function's memory address as hash
			hashStr := fmt.Sprintf("%p", f)
			// Convert hex string to number (simple hash)
			var hashNum int64
			fmt.Sscanf(hashStr, "%x", &hashNum)
			return NumberValue(float64(hashNum)), nil
		}), true
	}

	return nil, false
}

// SetAttr implements attribute setting for builtin functions
func (f *BuiltinFunction) SetAttr(name string, value Value) error {
	// Allow setting special attributes like __name__, __doc__, __qualname__
	// Delegate to BaseObject which stores them in the attrs map
	return f.BaseObject.SetAttr(name, value)
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
