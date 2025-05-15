package core

// TypeConstants contains the built-in type constants for the M28 language
// These are used with isinstance() and issubclass() functions

// We need to create distinct type identifiers since strings might conflict
// with actual functions like "list"
type TypeIdentifier string

// Type constants that will be accessible from M28 code
var TypeConstants = map[string]LispValue{
	"int":      TypeIdentifier("int"),
	"float":    TypeIdentifier("float"),
	"str":      TypeIdentifier("str"),
	"list":     TypeIdentifier("list"),
	"tuple":    TypeIdentifier("tuple"),
	"dict":     TypeIdentifier("dict"),
	"bool":     TypeIdentifier("bool"),
	"function": TypeIdentifier("function"),
	"none":     TypeIdentifier("none"),
	"set":      TypeIdentifier("set"),
}

// String representation of TypeIdentifier
func (t TypeIdentifier) String() string {
	return string(t)
}

// RegisterTypeConstants registers all the type constants in the given environment
func RegisterTypeConstants(env Environment) {
	for name, value := range TypeConstants {
		env.Define(LispSymbol(name), value)
	}
}
