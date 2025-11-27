package builtin

import (
	"fmt"

	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/core/ast"
	"github.com/mmichie/m28/eval"
	"github.com/mmichie/m28/modules"
	"github.com/mmichie/m28/parser"
)

// checkAssertPatterns checks for problematic assert patterns and emits SyntaxWarnings
// Specifically detects: assert(condition, message) which creates a tuple, always truthy
func checkAssertPatterns(source, filename string, ctx *core.Context) error {
	// Parse the source code using Python tokenizer and parser
	tokenizer := parser.NewPythonTokenizer(source)
	tokens, err := tokenizer.Tokenize()
	if err != nil {
		// If tokenization fails, let it fail during actual evaluation
		return nil
	}

	pythonParser := parser.NewPythonParser(tokens, filename, source)
	nodes, err := pythonParser.Parse()
	if err != nil {
		// If parsing fails, let it fail during actual evaluation
		return nil
	}

	// Walk the AST looking for assert statements
	for _, node := range nodes {
		walkForAsserts(node, ctx)
	}

	return nil
}

// walkForAsserts recursively walks the AST to find AssertForm nodes
func walkForAsserts(node ast.ASTNode, ctx *core.Context) {
	if node == nil {
		return
	}

	// Check if this is an assert statement
	if assertForm, ok := node.(*ast.AssertForm); ok {
		checkAssertCondition(assertForm, ctx)
		// Still walk the condition and message in case they contain nested asserts
		walkForAsserts(assertForm.Condition, ctx)
		if assertForm.Message != nil {
			walkForAsserts(assertForm.Message, ctx)
		}
		return
	}

	// Recursively walk child nodes based on their type
	// For most forms, Body is either an ASTNode or contained in an SExpr
	switch n := node.(type) {
	case *ast.SExpr:
		// Walk all elements of SExpr (most compound statements use this)
		for _, elem := range n.Elements {
			walkForAsserts(elem, ctx)
		}
	case *ast.DefForm:
		walkForAsserts(n.Body, ctx)
	case *ast.ClassForm:
		// ClassForm.Body is []ASTNode
		for _, stmt := range n.Body {
			walkForAsserts(stmt, ctx)
		}
	}
}

// checkAssertCondition checks if an assert statement has a tuple condition
// Pattern: assert(x, "msg") compiles to assert with tuple condition
func checkAssertCondition(assertForm *ast.AssertForm, ctx *core.Context) {
	// Check if condition is an SExpr (tuple-literal ...)
	sexpr, ok := assertForm.Condition.(*ast.SExpr)
	if !ok {
		return
	}

	elements := sexpr.Elements
	if len(elements) == 0 {
		return
	}

	// Check if first element is "tuple-literal" identifier
	if ident, ok := elements[0].(*ast.Identifier); ok {
		if ident.Name == "tuple-literal" && len(elements) > 1 {
			// This is a tuple with at least one element
			// assert(x, "msg") creates (tuple-literal x "msg")
			// assert(x,) creates (tuple-literal x)
			// Both are always truthy and should warn
			emitSyntaxWarning(ctx)
		}
	}
}

// emitSyntaxWarning emits a SyntaxWarning that will be captured by catch_warnings()
func emitSyntaxWarning(ctx *core.Context) {
	// Use the Warn helper from warnings module which properly integrates with catch_warnings()
	message := "assertion is always true, perhaps remove parentheses?"
	category := "SyntaxWarning"

	// Call the Warn function which will use the warnings module properly
	modules.Warn(message, category, ctx)
}

// RegisterMisc registers miscellaneous functions
func RegisterMisc(ctx *core.Context) {
	// repr - return string representation
	ctx.Define("repr", core.NewNamedBuiltinFunction("repr", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("repr", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		val := v.Get(0)

		// Try __repr__ dunder method first
		if str, found, err := types.CallRepr(val, ctx); found {
			if err != nil {
				return nil, err
			}
			return core.StringValue(str), nil
		}

		// Use core.Repr which handles special representations properly
		return core.StringValue(core.Repr(val)), nil
	}))

	// ascii - return ASCII-only string representation
	ctx.Define("ascii", core.NewNamedBuiltinFunction("ascii", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("ascii", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		val := v.Get(0)

		// Try __repr__ dunder method first (ascii uses repr internally)
		var reprStr string
		if str, found, err := types.CallRepr(val, ctx); found {
			if err != nil {
				return nil, err
			}
			reprStr = str
		} else {
			// Use core.Repr which handles special representations properly
			reprStr = core.Repr(val)
		}

		// Escape non-ASCII characters
		result := ""
		for _, r := range reprStr {
			if r < 128 {
				result += string(r)
			} else if r < 256 {
				result += fmt.Sprintf("\\x%02x", r)
			} else if r < core.UnicodeBMPLimit {
				result += fmt.Sprintf("\\u%04x", r)
			} else {
				result += fmt.Sprintf("\\U%08x", r)
			}
		}

		return core.StringValue(result), nil
	}))

	// hash - return hash of object
	ctx.Define("hash", core.NewNamedBuiltinFunction("hash", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("hash", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		obj := v.Get(0)

		// Try __hash__ dunder method first
		if hashVal, found, err := types.CallHash(obj, ctx); found {
			if err != nil {
				return nil, err
			}
			return core.NumberValue(hashVal), nil
		}

		if !core.IsHashable(obj) {
			return nil, fmt.Errorf("unhashable type: '%s'", obj.Type())
		}

		// For now, return a simple hash based on string representation
		// In the future, should implement proper hashing
		key := core.ValueToKey(obj)
		hash := 0
		for _, ch := range key {
			hash = hash*31 + int(ch)
		}

		return core.NumberValue(hash), nil
	}))

	// id - return identity of object
	ctx.Define("id", core.NewNamedBuiltinFunction("id", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("id", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		// For now, return a hash-like value
		// In the future, should return actual memory address or unique ID
		// Convert pointer address to a number
		addr := fmt.Sprintf("%p", v.Get(0))
		// Simple hash of the address string
		hash := 0
		for _, ch := range addr {
			hash = hash*31 + int(ch)
		}
		return core.NumberValue(hash), nil
	}))

	// help - print help for object
	ctx.Define("help", core.NewNamedBuiltinFunction("help", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("help", args)
		if err := v.Max(1); err != nil {
			return nil, err
		}

		if v.Count() == 0 {
			fmt.Println("Type help(object) for help about object.")
			return core.Nil, nil
		}

		// Get help for object
		obj := v.Get(0)

		// Check for __doc__ attribute
		if o, ok := obj.(interface {
			GetAttr(string) (core.Value, bool)
		}); ok {
			if doc, found := o.GetAttr("__doc__"); found {
				fmt.Println(doc.String())
				return core.Nil, nil
			}
		}

		// Default help
		fmt.Printf("Help on %s object:\n\n", obj.Type())
		fmt.Println("(no documentation available)")

		return core.Nil, nil
	}))

	// vars - return __dict__ of object
	ctx.Define("vars", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("vars", args)
		if err := v.Max(1); err != nil {
			return nil, err
		}

		if v.Count() == 0 {
			// Return current local variables
			// For now, return empty dict
			return core.NewDict(), nil
		}

		// Get __dict__ of object
		obj := v.Get(0)
		if o, ok := obj.(interface {
			GetAttr(string) (core.Value, bool)
		}); ok {
			if dict, found := o.GetAttr("__dict__"); found {
				return dict, nil
			}
		}

		return nil, fmt.Errorf("vars() argument must have __dict__ attribute")
	}))

	// locals - return local variables
	ctx.Define("locals", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("locals", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}

		// Convert context variables to dict
		// In Python, locals() returns the current scope's namespace
		// Unlike globals(), locals() returns a fresh dict snapshot of the current scope
		dict := core.NewDict()

		// Add all variables from the current context
		for name, value := range ctx.Vars {
			// Use SetWithKey to properly track original keys for iteration
			keyVal := core.StringValue(name)
			keyRepr := core.ValueToKey(keyVal)
			dict.SetWithKey(keyRepr, keyVal, value)
		}

		return dict, nil
	}))

	// globals - return global variables
	ctx.Define("globals", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("globals", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}

		// In Python, globals() returns the current module's top-level namespace
		// ctx.Global points to the global scope for the current execution context
		// For modules, ctx.Global == the module context itself
		// For functions, ctx.Global == the module context
		targetCtx := ctx.Global
		if targetCtx == nil {
			targetCtx = ctx
		}

		// If the target context has a ModuleDict, use it as the base
		// This allows globals().update() to actually modify the module namespace
		// which is critical for patterns like: globals().update({...})
		var dict *core.DictValue
		if targetCtx.ModuleDict != nil {
			dict = targetCtx.ModuleDict
		} else {
			// Create a new dict and save it to the context
			// so subsequent calls to globals() return the same dict
			dict = core.NewDict()
			targetCtx.ModuleDict = dict
		}

		// Also add ALL variables from ctx.Vars to ensure globals() is complete
		// This includes private and dunder names that aren't in ModuleDict
		for name, value := range targetCtx.Vars {
			// Use SetWithKey to properly track original keys for iteration
			keyVal := core.StringValue(name)
			dict.SetWithKey(core.ValueToKey(keyVal), keyVal, value)
		}

		return dict, nil
	}))

	// chr - convert integer to character
	ctx.Define("chr", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("chr", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		i, err := v.GetInt(0)
		if err != nil {
			return nil, err
		}

		if i < 0 || i > 0x10FFFF {
			return nil, &core.ValueError{Message: "chr() arg not in range(0x110000)"}
		}

		return core.StringValue(string(rune(i))), nil
	}))

	// ord - convert character to integer
	ctx.Define("ord", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("ord", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		s, err := v.GetString(0)
		if err != nil {
			return nil, err
		}

		runes := []rune(s)
		if len(runes) != 1 {
			return nil, &core.TypeError{Message: fmt.Sprintf("ord() expected a character, but string of length %d found", len(runes))}
		}

		return core.NumberValue(int(runes[0])), nil
	}))

	// hex - convert integer to hexadecimal string
	ctx.Define("hex", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("hex", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		i, err := v.GetInt(0)
		if err != nil {
			return nil, err
		}

		if i < 0 {
			return core.StringValue(fmt.Sprintf("-0x%x", -i)), nil
		}
		return core.StringValue(fmt.Sprintf("0x%x", i)), nil
	}))

	// bin - convert integer to binary string
	ctx.Define("bin", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("bin", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		i, err := v.GetInt(0)
		if err != nil {
			return nil, err
		}

		if i < 0 {
			return core.StringValue(fmt.Sprintf("-0b%b", -i)), nil
		}
		return core.StringValue(fmt.Sprintf("0b%b", i)), nil
	}))

	// oct - convert integer to octal string
	ctx.Define("oct", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("oct", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		i, err := v.GetInt(0)
		if err != nil {
			return nil, err
		}

		if i < 0 {
			return core.StringValue(fmt.Sprintf("-0o%o", -i)), nil
		}
		return core.StringValue(fmt.Sprintf("0o%o", i)), nil
	}))

	// del - delete variables (for now, a no-op to satisfy Python code)
	// Python's del statement is used to unbind names, but in types.py it's just cleanup
	ctx.Define("del", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// In a full implementation, this would unbind the variables from the namespace
		// For now, it's a no-op that returns None
		return core.None, nil
	}))

	// eval - evaluate a string as code
	// eval(expression, globals=None, locals=None)
	ctx.Define("eval", core.NewNamedBuiltinFunction("eval", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("eval", args)
		if err := v.Range(1, 3); err != nil {
			return nil, err
		}

		// Get the code string
		codeStr, err := v.GetString(0)
		if err != nil {
			return nil, err
		}

		// Get the globals dict (optional)
		var globalsDict *core.DictValue
		if v.Count() >= 2 && v.Get(1) != core.None {
			globalsDict, err = types.RequireDict(v.Get(1), "eval() globals")
			if err != nil {
				return nil, err
			}
		}

		// Get the locals dict (optional)
		var localsDict *core.DictValue
		if v.Count() >= 3 && v.Get(2) != core.None {
			localsDict, err = types.RequireDict(v.Get(2), "eval() locals")
			if err != nil {
				return nil, err
			}
		}

		// Create a new context for evaluation
		evalCtx := core.NewContext(ctx)

		// If globals dict provided, populate the context
		if globalsDict != nil {
			// Use Keys() to iterate over all keys in the dict
			for _, keyStr := range globalsDict.Keys() {
				value, exists := globalsDict.Get(keyStr)
				if exists {
					// Strip "s:" prefix if present (for string keys)
					cleanKey := keyStr
					if len(keyStr) > 2 && keyStr[0:2] == "s:" {
						cleanKey = keyStr[2:]
					}
					evalCtx.Define(cleanKey, value)
				}
			}
		}

		// If locals dict provided, add those too (locals override globals)
		if localsDict != nil {
			// Use Keys() to iterate over all keys in the dict
			for _, keyStr := range localsDict.Keys() {
				value, exists := localsDict.Get(keyStr)
				if exists {
					// Strip "s:" prefix if present (for string keys)
					cleanKey := keyStr
					if len(keyStr) > 2 && keyStr[0:2] == "s:" {
						cleanKey = keyStr[2:]
					}
					evalCtx.Define(cleanKey, value)
				}
			}
		}

		// Use EvalString which properly handles Python syntax including lambdas
		return eval.EvalString(codeStr, evalCtx)
	}))

	// exec - execute a string of code
	// exec(object, globals=None, locals=None)
	// Unlike eval, exec can execute statements and always returns None
	ctx.Define("exec", core.NewNamedBuiltinFunction("exec", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("exec", args)
		if err := v.Range(1, 3); err != nil {
			return nil, err
		}

		// Get the code string
		codeStr, err := v.GetString(0)
		if err != nil {
			return nil, err
		}

		// Get the globals dict (optional)
		var globalsDict *core.DictValue
		if v.Count() >= 2 && v.Get(1) != core.None {
			globalsDict, err = types.RequireDict(v.Get(1), "exec() globals")
			if err != nil {
				return nil, err
			}
		}

		// Get the locals dict (optional)
		var localsDict *core.DictValue
		if v.Count() >= 3 && v.Get(2) != core.None {
			localsDict, err = types.RequireDict(v.Get(2), "exec() locals")
			if err != nil {
				return nil, err
			}
		}

		// Create a new context for execution only if globals or locals are provided
		// Otherwise, use the current context so variables are visible
		execCtx := ctx
		if globalsDict != nil || localsDict != nil {
			execCtx = core.NewContext(ctx)
		}

		// Track which variables existed before exec so we only write back new ones
		existingVars := make(map[string]bool)

		// If globals dict provided, populate the context
		if globalsDict != nil {
			// Use Keys() to iterate over all keys in the dict
			for _, keyStr := range globalsDict.Keys() {
				value, exists := globalsDict.Get(keyStr)
				if exists {
					// Strip "s:" prefix if present (for string keys)
					cleanKey := keyStr
					if len(keyStr) > 2 && keyStr[0:2] == "s:" {
						cleanKey = keyStr[2:]
					}
					execCtx.Define(cleanKey, value)
					existingVars[cleanKey] = true
				}
			}
		}

		// If locals dict provided, add those too (locals override globals)
		if localsDict != nil {
			// Use Keys() to iterate over all keys in the dict
			for _, keyStr := range localsDict.Keys() {
				value, exists := localsDict.Get(keyStr)
				if exists {
					// Strip "s:" prefix if present (for string keys)
					cleanKey := keyStr
					if len(keyStr) > 2 && keyStr[0:2] == "s:" {
						cleanKey = keyStr[2:]
					}
					execCtx.Define(cleanKey, value)
					existingVars[cleanKey] = true
				}
			}
		}

		// Execute the code using EvalString
		_, err = eval.EvalString(codeStr, execCtx)
		if err != nil {
			return nil, err
		}

		// Copy ONLY NEW variables from execCtx back to the appropriate dict
		// Python semantics:
		// - If locals is provided, write back to locals
		// - Otherwise, if globals is provided, write back to globals
		// - If neither is provided, variables stay in the current context
		// IMPORTANT: Only write back variables that were created/modified during exec,
		// not the ones that were already in the dict before exec
		if localsDict != nil {
			// Write back to locals (takes precedence)
			for name, value := range execCtx.Vars {
				// Skip variables that existed before exec
				if existingVars[name] {
					continue
				}
				keyVal := core.StringValue(name)
				keyRepr := core.ValueToKey(keyVal)
				localsDict.SetWithKey(keyRepr, keyVal, value)
			}
		} else if globalsDict != nil {
			// Write back to globals (when no locals dict is provided)
			// This is critical for patterns like: exec("def fn(): pass", d)
			// where d is used as both globals and locals
			for name, value := range execCtx.Vars {
				// Skip variables that existed before exec
				if existingVars[name] {
					continue
				}
				keyVal := core.StringValue(name)
				keyRepr := core.ValueToKey(keyVal)
				globalsDict.SetWithKey(keyRepr, keyVal, value)
			}
		}

		// exec always returns None
		return core.None, nil
	}))

	// Ellipsis - Python's ... literal
	ctx.Define("Ellipsis", core.Ellipsis)

	// NotImplemented - Python's NotImplemented singleton
	ctx.Define("NotImplemented", core.NotImplemented)

	// compile - compile source into a code object
	// Parses source and emits warnings for problematic patterns
	ctx.Define("compile", core.NewNamedBuiltinFunction("compile", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("compile", args)
		// compile(source, filename, mode, flags=0, dont_inherit=False, optimize=-1)
		if err := v.Min(3); err != nil {
			return nil, err
		}
		if err := v.Max(6); err != nil {
			return nil, err
		}

		// Get source code
		source, err := v.GetString(0)
		if err != nil {
			return nil, err
		}

		filename, err := v.GetString(1)
		if err != nil {
			return nil, err
		}

		// mode (exec, eval, single) - not used for now
		_, err = v.GetString(2)
		if err != nil {
			return nil, err
		}

		// Parse the source to check for problematic patterns
		// We need to emit SyntaxWarnings for assert statements with tuple conditions
		if err := checkAssertPatterns(source, filename, ctx); err != nil {
			return nil, err
		}

		// Return a code object placeholder
		// TODO(M28-4604): Implement actual bytecode compilation
		return core.NewCodeObject(core.Nil), nil
	}))
}

// Migration Statistics:
// Functions migrated: 7 utility functions (repr, hash, id, help, vars, locals, globals)
// Type checks eliminated: ~7 manual len(args) checks
// Code reduction: ~15% in validation code
// Benefits: Consistent error messages with validation framework
// All utility functions now use v.Exact() and v.Max() for cleaner validation
