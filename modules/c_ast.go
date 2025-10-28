package modules

import (
	"github.com/mmichie/m28/core"
)

// Init_ASTModule creates and returns the _ast module stub
// This is a minimal stub implementation of Python's _ast C extension module
// It provides empty classes for AST node types to allow ast.py to import
func Init_ASTModule() *core.DictValue {
	astModule := core.NewDict()

	// Create stub classes for the most common AST node types
	// These are empty classes that can be instantiated but don't do anything
	nodeTypes := []string{
		// Module types
		"Module", "Interactive", "Expression", "FunctionType",

		// Statement types
		"FunctionDef", "AsyncFunctionDef", "ClassDef", "Return",
		"Delete", "Assign", "AugAssign", "AnnAssign",
		"For", "AsyncFor", "While", "If", "With", "AsyncWith",
		"Match", "Raise", "Try", "TryStar", "Assert",
		"Import", "ImportFrom", "Global", "Nonlocal",
		"Expr", "Pass", "Break", "Continue",

		// Expression types
		"BoolOp", "NamedExpr", "BinOp", "UnaryOp", "Lambda",
		"IfExp", "Dict", "Set", "ListComp", "SetComp",
		"DictComp", "GeneratorExp", "Await", "Yield", "YieldFrom",
		"Compare", "Call", "FormattedValue", "JoinedStr",
		"Constant", "Attribute", "Subscript", "Starred",
		"Name", "List", "Tuple", "Slice",

		// Expression context types
		"Load", "Store", "Del",

		// Boolean operators
		"And", "Or",

		// Binary operators
		"Add", "Sub", "Mult", "MatMult", "Div", "Mod",
		"Pow", "LShift", "RShift", "BitOr", "BitXor",
		"BitAnd", "FloorDiv",

		// Unary operators
		"Invert", "Not", "UAdd", "USub",

		// Comparison operators
		"Eq", "NotEq", "Lt", "LtE", "Gt", "GtE",
		"Is", "IsNot", "In", "NotIn",

		// Other node types
		"comprehension", "ExceptHandler", "arguments", "arg",
		"keyword", "alias", "withitem", "match_case",
		"MatchValue", "MatchSingleton", "MatchSequence",
		"MatchMapping", "MatchClass", "MatchStar", "MatchAs", "MatchOr",
		"TypeIgnore", "TypeVar", "ParamSpec", "TypeVarTuple", "TypeAlias",

		// Base types
		"AST", "mod", "stmt", "expr", "expr_context",
		"boolop", "operator", "unaryop", "cmpop",
		"excepthandler", "pattern", "type_ignore", "type_param",
	}

	// Create a simple stub class for each node type
	for _, nodeType := range nodeTypes {
		class := core.NewClass(nodeType, nil)
		astModule.Set(nodeType, class)
	}

	// Add PyCF_ constants used for compilation flags
	astModule.Set("PyCF_ONLY_AST", core.NumberValue(1024))
	astModule.Set("PyCF_TYPE_COMMENTS", core.NumberValue(4096))
	astModule.Set("PyCF_ALLOW_TOP_LEVEL_AWAIT", core.NumberValue(8192))

	return astModule
}
