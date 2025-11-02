package core

import (
	"fmt"
	"math"
	"math/big"
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

// BigIntValue represents an arbitrary precision integer (Python's int type)
// This is used when integers exceed float64's safe integer range (2^53)
type BigIntValue struct {
	value *big.Int
}

// NewBigInt creates a BigIntValue from a big.Int (makes a copy)
func NewBigInt(i *big.Int) BigIntValue {
	return BigIntValue{value: new(big.Int).Set(i)}
}

// NewBigIntFromInt64 creates a BigIntValue from an int64
func NewBigIntFromInt64(i int64) BigIntValue {
	return BigIntValue{value: big.NewInt(i)}
}

// NewBigIntFromString creates a BigIntValue from a string in the given base
func NewBigIntFromString(s string, base int) (BigIntValue, error) {
	bi := new(big.Int)
	_, ok := bi.SetString(s, base)
	if !ok {
		return BigIntValue{}, fmt.Errorf("invalid integer: %s", s)
	}
	return BigIntValue{value: bi}, nil
}

// Type implements Value.Type
func (b BigIntValue) Type() Type {
	return BigIntType
}

// String implements Value.String - returns the decimal representation
func (b BigIntValue) String() string {
	if b.value == nil {
		return "0"
	}
	return b.value.String()
}

// ToFloat64 converts to float64 (may lose precision for large values)
func (b BigIntValue) ToFloat64() float64 {
	if b.value == nil {
		return 0
	}
	f, _ := new(big.Float).SetInt(b.value).Float64()
	return f
}

// ToInt64 converts to int64 if it fits, otherwise returns error
func (b BigIntValue) ToInt64() (int64, bool) {
	if b.value == nil {
		return 0, true
	}
	if !b.value.IsInt64() {
		return 0, false
	}
	return b.value.Int64(), true
}

// FitsInFloat64 checks if the value can be safely represented as float64
// without precision loss (i.e., fits in range -2^53 to 2^53)
func (b BigIntValue) FitsInFloat64() bool {
	if b.value == nil {
		return true
	}
	// Check if -2^53 <= value <= 2^53
	maxSafeInt := new(big.Int).Lsh(big.NewInt(1), 53) // 2^53
	minSafeInt := new(big.Int).Neg(maxSafeInt)
	return b.value.Cmp(minSafeInt) >= 0 && b.value.Cmp(maxSafeInt) <= 0
}

// GetBigInt returns the underlying big.Int (for operators)
func (b BigIntValue) GetBigInt() *big.Int {
	if b.value == nil {
		return big.NewInt(0)
	}
	return b.value
}

// Sign returns -1, 0, or 1 depending on whether the value is negative, zero, or positive
func (b BigIntValue) Sign() int {
	if b.value == nil {
		return 0
	}
	return b.value.Sign()
}

// GetAttr implements basic int methods using TypeDescriptor
func (b BigIntValue) GetAttr(name string) (Value, bool) {
	desc := GetTypeDescriptor(BigIntType)
	if desc != nil {
		val, err := desc.GetAttribute(b, name)
		if err == nil {
			return val, true
		}
	}
	return nil, false
}

// Promotion constants for NumberValue -> BigIntValue conversion
const (
	MaxSafeInt float64 = 9007199254740992  // 2^53
	MinSafeInt float64 = -9007199254740992 // -2^53
)

// IsInteger checks if a float64 represents an integer value
func IsInteger(f float64) bool {
	return f == math.Floor(f) && !math.IsInf(f, 0) && !math.IsNaN(f)
}

// IsInSafeRange checks if an integer value fits precisely in float64
func IsInSafeRange(f float64) bool {
	return f >= MinSafeInt && f <= MaxSafeInt
}

// ShouldPromoteToBigInt determines if a NumberValue should be promoted to BigIntValue
func ShouldPromoteToBigInt(f float64) bool {
	return IsInteger(f) && !IsInSafeRange(f)
}

// PromoteToBigInt converts a NumberValue to BigIntValue
// For large floats, uses string conversion to preserve precision
func PromoteToBigInt(n NumberValue) BigIntValue {
	f := float64(n)
	if ShouldPromoteToBigInt(f) {
		// Use string conversion for large numbers to preserve all digits
		s := fmt.Sprintf("%.0f", f)
		bi, _ := NewBigIntFromString(s, 10)
		return bi
	}
	// Safe to convert via int64
	return NewBigIntFromInt64(int64(f))
}

// DemoteToNumber converts BigIntValue to NumberValue if it fits safely
// Returns (NumberValue, true) if demotion is safe, (0, false) otherwise
func DemoteToNumber(b BigIntValue) (NumberValue, bool) {
	if b.FitsInFloat64() {
		return NumberValue(b.ToFloat64()), true
	}
	return 0, false
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

// GetAttr implements attribute access for None
// In Python, None has __class__ and other dunder attributes
func (n NilValue) GetAttr(name string) (Value, bool) {
	// First check type descriptor (if registered)
	desc := GetTypeDescriptor(NilType)
	if desc != nil {
		val, err := desc.GetAttribute(n, name)
		if err == nil {
			return val, true
		}
	}

	// Fallback: handle basic attributes directly
	switch name {
	case "__bool__":
		// None is falsy
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			return BoolValue(false), nil
		}), true
	}
	return nil, false
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
		fmt.Printf("[DEBUG BuiltinFunction] Function name: %s\n", funcName)
		fmt.Printf("[DEBUG BuiltinFunction] Function: %v\n", f)
		fmt.Printf("[DEBUG BuiltinFunction] kwargs: %v\n", kwargs)
		return nil, fmt.Errorf("builtin function '%s' does not support keyword arguments", funcName)
	}
	return f.fn(args, ctx)
}

// BuiltinFunctionWithKwargs represents a builtin function that supports keyword arguments
type BuiltinFunctionWithKwargs struct {
	BaseObject
	Name string
	Fn   func(args []Value, kwargs map[string]Value, ctx *Context) (Value, error)
}

// Call implements Callable.Call
func (f *BuiltinFunctionWithKwargs) Call(args []Value, ctx *Context) (Value, error) {
	return f.Fn(args, nil, ctx)
}

// CallWithKeywords implements keyword argument support
func (f *BuiltinFunctionWithKwargs) CallWithKeywords(args []Value, kwargs map[string]Value, ctx *Context) (Value, error) {
	return f.Fn(args, kwargs, ctx)
}

// String implements Value.String
func (f *BuiltinFunctionWithKwargs) String() string {
	if f.Name != "" {
		return fmt.Sprintf("<builtin function %s>", f.Name)
	}
	return "<builtin function>"
}

// Type implements Value.Type
func (f *BuiltinFunctionWithKwargs) Type() Type {
	return FunctionType
}

// GetAttr implements attribute access for BuiltinFunctionWithKwargs
func (f *BuiltinFunctionWithKwargs) GetAttr(name string) (Value, bool) {
	switch name {
	case "__name__":
		if f.Name != "" {
			return StringValue(f.Name), true
		}
		return StringValue("<anonymous>"), true
	case "__qualname__":
		if f.Name != "" {
			return StringValue(f.Name), true
		}
		return StringValue("<anonymous>"), true
	case "__module__":
		return StringValue("builtins"), true
	}
	return f.BaseObject.GetAttr(name)
}

// SetAttr implements attribute setting for BuiltinFunctionWithKwargs
func (f *BuiltinFunctionWithKwargs) SetAttr(name string, value Value) {
	f.BaseObject.SetAttr(name, value)
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
			fn := receiver.(*BuiltinFunction)

			// Check if a custom __code__ was set via SetAttr first
			if customCode, ok := fn.BaseObject.GetAttr("__code__"); ok {
				return customCode, nil
			}

			// Return a proper code object with required attributes for inspect.signature()
			codeObj := NewCodeObject(receiver)

			// Set code object attributes that inspect.py expects
			// For builtin functions, we use minimal/empty values
			codeObj.SetAttr("co_argcount", NumberValue(0))           // Number of positional args
			codeObj.SetAttr("co_posonlyargcount", NumberValue(0))    // Number of positional-only args (Python 3.8+)
			codeObj.SetAttr("co_kwonlyargcount", NumberValue(0))     // Number of keyword-only args
			codeObj.SetAttr("co_nlocals", NumberValue(0))            // Number of local variables
			codeObj.SetAttr("co_stacksize", NumberValue(0))          // Required stack size
			codeObj.SetAttr("co_flags", NumberValue(0))              // CO_* flags
			codeObj.SetAttr("co_code", StringValue(""))              // Bytecode (empty for builtins)
			codeObj.SetAttr("co_consts", TupleValue{})               // Constants used
			codeObj.SetAttr("co_names", TupleValue{})                // Names used
			codeObj.SetAttr("co_varnames", TupleValue{})             // Local variable names (empty for builtins)
			codeObj.SetAttr("co_freevars", TupleValue{})             // Free variables
			codeObj.SetAttr("co_cellvars", TupleValue{})             // Cell variables
			codeObj.SetAttr("co_filename", StringValue("<builtin>")) // Filename
			codeObj.SetAttr("co_name", StringValue(""))              // Function name
			codeObj.SetAttr("co_firstlineno", NumberValue(0))        // First line number
			codeObj.SetAttr("co_lnotab", StringValue(""))            // Line number table

			return codeObj, nil
		}),
		MakeProperty("__closure__", "Function closure", func(receiver Value) (Value, error) {
			// Return None for builtin functions (no closure)
			// Actually, Python returns None for functions without closure
			return None, nil
		}),
		MakeProperty("__defaults__", "Default argument values", func(receiver Value) (Value, error) {
			// Return None for builtin functions (no defaults)
			// When functions have defaults, this should be a tuple
			return None, nil
		}),
		MakeProperty("__kwdefaults__", "Keyword-only default argument values", func(receiver Value) (Value, error) {
			// Return None for builtin functions (no kw defaults)
			// When functions have kw defaults, this should be a dict
			return None, nil
		}),
		MakeProperty("__annotations__", "Type annotations", func(receiver Value) (Value, error) {
			// Return an empty dict for builtin functions (no annotations)
			return NewDict(), nil
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

// IteratorValue returns the iterator as a Value
func (b *ByteArrayValue) IteratorValue() Value {
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

// Type implements Value.Type for bytearrayIterator
func (it *bytearrayIterator) Type() Type {
	return "bytearray_iterator"
}

// String implements Value.String for bytearrayIterator
func (it *bytearrayIterator) String() string {
	return "<bytearray_iterator>"
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
