package eval

import (
	"fmt"
	"strings"

	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/core/protocols"
)

// ArgumentElement represents a single positional argument element (regular or
// *iterable unpacking)
type ArgumentElement struct {
	IsUnpack bool       // true if this is *expr
	Expr     core.Value // The expression (without *)
}

// KeywordElement is one element of the keyword segment of a call: either a
// named keyword argument (name=expr) or a **mapping unpacking. Source order is
// preserved across both kinds so the resulting kwargs observe PEP 468.
type KeywordElement struct {
	Name     string     // keyword name; "" for **unpack
	Expr     core.Value // the value (or mapping) expression
	IsUnpack bool       // true if this is **expr
}

// ArgumentInfo holds information about parsed arguments including unpacking
type ArgumentInfo struct {
	Elements []ArgumentElement // Positional elements in order
	Keywords []KeywordElement  // Named keywords and **unpackings in source order
}

// parseArgumentsWithUnpacking extracts all argument information including unpacking
func parseArgumentsWithUnpacking(items []core.Value) (*ArgumentInfo, error) {
	// Both fields are lazily allocated: the common call has only positional
	// args (Keywords stays nil) and both slices grow via append from nil.
	info := &ArgumentInfo{}

	// addKeyword records a named keyword arg. Duplicate detection is a linear
	// scan: keyword lists are small, and order must be preserved.
	addKeyword := func(name string, value core.Value) error {
		for i := range info.Keywords {
			if !info.Keywords[i].IsUnpack && info.Keywords[i].Name == name {
				return fmt.Errorf("duplicate keyword argument: %s", name)
			}
		}
		info.Keywords = append(info.Keywords, KeywordElement{Name: name, Expr: value})
		return nil
	}

	i := 0
	seenKeyword := false

	for i < len(items) {
		// Check for unpacking operators (unwrap LocatedValues first)
		arg := items[i]
		if located, ok := arg.(core.LocatedValue); ok {
			arg = located.Unwrap()
		}

		if sym, ok := arg.(core.SymbolValue); ok {
			symStr := string(sym)

			// Check for **unpack marker (parser creates: ["**unpack", <expr>])
			if symStr == "**unpack" {
				if i+1 >= len(items) {
					return nil, fmt.Errorf("** unpacking requires an expression")
				}
				info.Keywords = append(info.Keywords, KeywordElement{
					IsUnpack: true,
					Expr:     items[i+1],
				})
				i += 2 // Skip marker and expression
				continue
			}

			// Check for *unpack marker (parser creates: ["*unpack", <expr>])
			if symStr == "*unpack" {
				if i+1 >= len(items) {
					return nil, fmt.Errorf("* unpacking requires an expression")
				}
				if seenKeyword {
					return nil, fmt.Errorf("* unpacking cannot follow keyword arguments")
				}
				info.Elements = append(info.Elements, ArgumentElement{
					IsUnpack: true,
					Expr:     items[i+1],
				})
				i += 2 // Skip marker and expression
				continue
			}

			// Legacy support: Check for **kwargs unpacking (old style where expr is in the symbol)
			// Also handles parser-generated **kwargs markers followed by dict-literal
			if strings.HasPrefix(symStr, "**") && symStr != "**unpack" {
				exprStr := strings.TrimPrefix(symStr, "**")
				if exprStr == "" {
					return nil, fmt.Errorf("** unpacking requires an expression")
				}

				// Check if next argument is the expression to unpack (parser style)
				// Parser generates: ["**kwargs", (dict-literal ...)]
				if i+1 < len(items) {
					// Use the next argument as the dict expression
					info.Keywords = append(info.Keywords, KeywordElement{
						IsUnpack: true,
						Expr:     items[i+1],
					})
					i += 2 // Skip marker and dict expression
					continue
				}

				// Old legacy style where variable name is embedded
				info.Keywords = append(info.Keywords, KeywordElement{
					IsUnpack: true,
					Expr:     core.SymbolValue(exprStr),
				})
				i++
				continue
			}

			// Legacy support: Check for *args unpacking (old style)
			if strings.HasPrefix(symStr, "*") && !strings.HasPrefix(symStr, "**") && symStr != "*unpack" {
				exprStr := strings.TrimPrefix(symStr, "*")
				if exprStr == "" {
					return nil, fmt.Errorf("* unpacking requires an expression")
				}
				if seenKeyword {
					return nil, fmt.Errorf("* unpacking cannot follow keyword arguments")
				}
				info.Elements = append(info.Elements, ArgumentElement{
					IsUnpack: true,
					Expr:     core.SymbolValue(exprStr),
				})
				i++
				continue
			}
		}

		// Check if this is a keyword argument pattern: symbol = value
		// (arg is items[i] with any LocatedValue wrapper removed; the "="
		// separator must be unwrapped too, since the Pythonic parser attaches
		// source locations to the identifiers it emits)
		if i+2 < len(items) {
			eqItem := items[i+1]
			if located, ok := eqItem.(core.LocatedValue); ok {
				eqItem = located.Unwrap()
			}
			if sym, ok := arg.(core.SymbolValue); ok {
				if eq, ok := eqItem.(core.SymbolValue); ok && string(eq) == "=" {
					// This is a keyword argument
					paramName := string(sym)
					paramValue := items[i+2]
					seenKeyword = true

					if err := addKeyword(paramName, paramValue); err != nil {
						return nil, err
					}
					i += 3 // Skip symbol, =, and value
					continue
				}
			}
		}

		// This is a positional argument
		// But we can't have positional args after keyword args
		if seenKeyword {
			return nil, fmt.Errorf("positional argument follows keyword argument")
		}

		info.Elements = append(info.Elements, ArgumentElement{
			IsUnpack: false,
			Expr:     items[i],
		})
		i++
	}

	return info, nil
}

// evalFunctionCallWithKeywords evaluates a function call with keyword argument support
func evalFunctionCallWithKeywords(expr *core.ListValue, ctx *core.Context) (core.Value, error) {
	// items aliases the call expression's storage (read-only here); ItemsRef
	// avoids the defensive slice copy Items() would make on this very hot path.
	items := expr.ItemsRef()

	// Check if the function is referenced by a symbol (for better error messages)
	var symbolName string
	first := items[0]
	// Unwrap LocatedValue if needed
	if located, ok := first.(core.LocatedValue); ok {
		first = located.Value
	}
	if sym, ok := first.(core.SymbolValue); ok {
		symbolName = string(sym)
	}
	core.DebugLog("[KWCALL] evalFunctionCallWithKeywords: %s, %d args\n", symbolName, len(items)-1)

	// Evaluate the function
	core.DebugLog("[KWCALL] Evaluating function expression\n")
	fn, err := Eval(items[0], ctx)
	if err != nil {
		return nil, err
	}
	core.DebugLog("[KWCALL] Function evaluated: %T\n", fn)

	// Parse arguments including unpacking. Pass the args sub-slice directly so
	// no intermediate list is allocated.
	core.DebugLog("[KWCALL] Parsing arguments with unpacking\n")
	argInfo, err := parseArgumentsWithUnpacking(items[1:])
	if err != nil {
		return nil, err
	}
	core.DebugLog("[KWCALL] Arguments parsed, processing positional args\n")

	// Process all positional elements in order
	positionalArgs := make([]core.Value, 0, len(argInfo.Elements))
	for _, elem := range argInfo.Elements {
		if elem.IsUnpack {
			// Evaluate the expression to unpack
			val, err := Eval(elem.Expr, ctx)
			if err != nil {
				return nil, err
			}

			// Unpack based on type
			switch v := val.(type) {
			case *core.ListValue:
				positionalArgs = append(positionalArgs, v.Items()...)
			case core.TupleValue:
				positionalArgs = append(positionalArgs, []core.Value(v)...)
			case *core.SetValue:
				// Sets can be unpacked in Python 3.5+
				// Note: order is not guaranteed for sets
				iter := v.Iterator()
				for {
					item, ok := iter.Next()
					if !ok {
						break
					}
					positionalArgs = append(positionalArgs, item)
				}
			case core.StringValue:
				// String unpacking: each character becomes an argument
				for _, ch := range string(v) {
					positionalArgs = append(positionalArgs, core.StringValue(string(ch)))
				}
			default:
				// Try to iterate
				if iterable, ok := val.(core.Iterable); ok {
					iter := iterable.Iterator()
					for {
						item, ok := iter.Next()
						if !ok {
							break
						}
						positionalArgs = append(positionalArgs, item)
					}
				} else {
					return nil, fmt.Errorf("* unpacking requires an iterable, got %s", val.Type())
				}
			}
		} else {
			// Regular positional argument
			evalArg, err := Eval(elem.Expr, ctx)
			if err != nil {
				return nil, err
			}
			positionalArgs = append(positionalArgs, evalArg)
		}
	}

	// Evaluate the keyword segment — named keywords and ** unpackings — in
	// source order, so the resulting kwargs observe call-site keyword order
	// (PEP 468) and side effects run left to right. The common positional call
	// leaves keywordArgs nil, which the call paths treat as "no kwargs".
	var keywordArgs *core.Kwargs
	if len(argInfo.Keywords) > 0 {
		keywordArgs = core.NewKwargs(len(argInfo.Keywords))
	}
	for _, kw := range argInfo.Keywords {
		if !kw.IsUnpack {
			evalArg, err := Eval(kw.Expr, ctx)
			if err != nil {
				return nil, err
			}
			// Parse already rejects duplicate named keywords; a duplicate here
			// means the name also arrived via an earlier ** unpacking.
			if keywordArgs.Has(kw.Name) {
				return nil, fmt.Errorf("duplicate keyword argument: %s", kw.Name)
			}
			keywordArgs.Set(kw.Name, evalArg)
			continue
		}

		// ** unpacking: evaluate the mapping expression
		val, err := Eval(kw.Expr, ctx)
		if err != nil {
			return nil, err
		}

		// Accept a dict or a dict-subclass Instance (via BackingDict).
		var dict *core.DictValue
		switch x := val.(type) {
		case *core.DictValue:
			dict = x
		case *core.Instance:
			if x.BackingDict != nil {
				dict = x.BackingDict
			}
		}
		// Otherwise accept any mapping: an object exposing keys() and
		// __getitem__ (CPython's ** protocol). Covers collections.OrderedDict,
		// defaultdict, and user mappings.
		if dict == nil {
			if built, ok, err := dictFromMapping(val, ctx); err != nil {
				return nil, err
			} else if ok {
				dict = built
			}
		}
		if dict == nil {
			return nil, &core.TypeError{Message: fmt.Sprintf("argument after ** must be a mapping, not %s", val.Type())}
		}

		// Unpack the dict into keyword arguments (keys must be strings)
		var unpackErr error
		dict.ForEach(func(k, val core.Value) bool {
			keyStr, ok := k.(core.StringValue)
			if !ok {
				unpackErr = &core.TypeError{Message: fmt.Sprintf("keywords must be strings (got %s)", k.Type())}
				return false
			}
			paramName := string(keyStr)
			if keywordArgs.Has(paramName) {
				unpackErr = fmt.Errorf("duplicate keyword argument: %s", paramName)
				return false
			}
			keywordArgs.Set(paramName, val)
			return true
		})
		if unpackErr != nil {
			return nil, unpackErr
		}
	}

	// Check if it's a function that supports keyword arguments
	// This includes UserFunction (via CallWithKeywords) and KwargsBuiltinFunction
	if kwargsFunc, ok := fn.(interface {
		CallWithKeywords([]core.Value, *core.Kwargs, *core.Context) (core.Value, error)
	}); ok {
		// Get source location and push to call stack before calling
		file := ""
		line := 0
		col := 0
		if loc := ctx.CurrentLocation(); loc != nil {
			file = loc.File
			line = loc.Line
			col = loc.Column
		}
		funcName := symbolName
		if funcName == "" {
			funcName = "<anonymous>"
		}
		ctx.PushStack(funcName, file, line, col)
		defer ctx.PopStack()

		result, err := kwargsFunc.CallWithKeywords(positionalArgs, keywordArgs, ctx)
		if err != nil {
			// Preserve PythonError for proper try/except matching
			if _, ok := err.(*core.PythonError); ok {
				return nil, err
			}
			// Preserve Exception for proper try/except matching
			if _, ok := err.(*Exception); ok {
				return nil, err
			}
			// Preserve StopIteration so callers can catch it
			if _, ok := err.(*protocols.StopIteration); ok {
				return nil, err
			}
			if _, ok := err.(*core.StopIteration); ok {
				return nil, err
			}
			// Wrap error with call stack
			return nil, core.WrapEvalError(err, fmt.Sprintf("error in %s: %v", funcName, err), ctx)
		}
		return result, nil
	}

	// For functions that don't support keyword arguments,
	// only allow calls with no keyword arguments
	if keywordArgs.Len() > 0 {
		return nil, fmt.Errorf("function (type %T) does not support keyword arguments", fn)
	}

	// Call as normal with just positional arguments
	callable, ok := fn.(core.Callable)
	if !ok {
		// Check if object has __call__ method
		if result, found, err := types.CallCall(fn, positionalArgs, ctx); found {
			return result, err
		}

		return nil, &core.TypeError{Message: fmt.Sprintf("'%s' object is not callable", core.GetPythonTypeName(fn))}
	}

	// Add function name to call stack if available
	var funcName string

	// Prefer the symbol name if we have it (for better error messages)
	if symbolName != "" {
		funcName = symbolName
	} else {
		// Fall back to introspecting the function object
		switch f := fn.(type) {
		case core.SymbolValue:
			funcName = string(f)
		case *core.BuiltinFunction:
			if nameVal, ok := f.GetAttr("__name__"); ok {
				if nameStr, ok := nameVal.(core.StringValue); ok {
					funcName = string(nameStr)
				} else {
					funcName = "<builtin>"
				}
			} else {
				funcName = "<builtin>"
			}
		case *core.BoundMethod:
			funcName = fmt.Sprintf("%s.%s", f.TypeDesc.PythonName, f.Method.Name)
		case *UserFunction:
			if f.name != "" {
				funcName = f.name
			} else {
				funcName = "<anonymous>"
			}
		default:
			funcName = "<anonymous>"
		}
	}

	// Get source location from context for better error messages
	file := ""
	line := 0
	col := 0
	if loc := ctx.CurrentLocation(); loc != nil {
		file = loc.File
		line = loc.Line
		col = loc.Column
	}
	ctx.PushStack(funcName, file, line, col)
	defer ctx.PopStack()

	result, err := callable.Call(positionalArgs, ctx)
	if err != nil {
		// Preserve PythonError for proper try/except matching
		if _, ok := err.(*core.PythonError); ok {
			return nil, err
		}

		// Preserve Exception for proper try/except matching
		if _, ok := err.(*Exception); ok {
			return nil, err
		}

		// Check if this is already a well-formatted error (e.g., from assert)
		// Don't wrap errors that are already EvalError or have specific messages
		if _, isEvalError := err.(*core.EvalError); isEvalError {
			return nil, err
		}

		// Special handling for assert - preserve its custom error messages
		if funcName == "assert" {
			return nil, core.WrapEvalError(err, err.Error(), ctx)
		}

		// Include the underlying error message for better debugging
		return nil, core.WrapEvalError(err, fmt.Sprintf("error in %s: %v", funcName, err), ctx)
	}

	return result, nil
}

// dictFromMapping builds a DictValue from any object implementing the mapping
// protocol: a keys() method plus __getitem__. This is how CPython's ** unpacking
// accepts arbitrary mappings (e.g. collections.OrderedDict, which is not a plain
// dict in M28). Returns ok=false if v is not a mapping.
func dictFromMapping(v core.Value, ctx *core.Context) (*core.DictValue, bool, error) {
	obj, ok := v.(interface {
		GetAttr(string) (core.Value, bool)
	})
	if !ok {
		return nil, false, nil
	}
	keysAttr, ok := obj.GetAttr("keys")
	if !ok {
		return nil, false, nil
	}
	getitemAttr, ok := obj.GetAttr("__getitem__")
	if !ok {
		return nil, false, nil
	}
	keysCallable, ok := keysAttr.(interface {
		Call([]core.Value, *core.Context) (core.Value, error)
	})
	if !ok {
		return nil, false, nil
	}
	getitem, ok := getitemAttr.(interface {
		Call([]core.Value, *core.Context) (core.Value, error)
	})
	if !ok {
		return nil, false, nil
	}

	keysResult, err := keysCallable.Call(nil, ctx)
	if err != nil {
		return nil, false, err
	}

	// Collect the keys from the common iterable return types.
	var keys []core.Value
	switch k := keysResult.(type) {
	case *core.ListValue:
		keys = k.Items()
	case core.TupleValue:
		keys = []core.Value(k)
	default:
		// Not an iterable shape we can unpack; let the caller report TypeError.
		return nil, false, nil
	}

	built := core.NewDict()
	for _, key := range keys {
		val, err := getitem.Call([]core.Value{key}, ctx)
		if err != nil {
			return nil, false, err
		}
		if err := built.SetValue(key, val); err != nil {
			return nil, false, err
		}
	}
	return built, true, nil
}
