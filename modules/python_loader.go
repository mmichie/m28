package modules

import (
	"fmt"
	"os"
	"time"

	"github.com/mmichie/m28/core"
)

func init() {
	// Register Python module loader with core
	core.SetPythonLoader(LoadPythonModule)
}

// C extension modules that cannot be transpiled
// These are either .so/.pyd files or built into the Python interpreter
var cExtensionModules = map[string]bool{
	"_io":              true,
	"_thread":          true,
	"_socket":          true,
	"_ssl":             true,
	"_hashlib":         true,
	"_json":            true,
	"_pickle":          true,
	"_datetime":        true,
	"_sqlite3":         true,
	"_csv":             true,
	"_struct":          true,
	"_random":          true,
	"_posixsubprocess": true,
	// "_weakref" removed - provided as M28 stub module
	// "_abc" removed - let it fail naturally to trigger fallback to _py_abc
	"builtins": true, // Built-in functions
}

// LoadPythonModule attempts to load a Python module by name
// Returns (*DictValue, error) to match ModuleLoader interface
func LoadPythonModule(name string, ctx *core.Context, evalFunc func(core.Value, *core.Context) (core.Value, error)) (*core.DictValue, error) {
	fmt.Fprintf(os.Stderr, "[PROFILE] LoadPythonModule called for '%s'\n", name)

	// Check if this is a known C extension
	if cExtensionModules[name] {
		return nil, fmt.Errorf(
			"module '%s' is a C extension and cannot be auto-imported.\n"+
				"Consider implementing a native M28 version in builtin/ or modules/",
			name,
		)
	}

	// Find the Python file
	finder, err := GetPythonFinder()
	if err != nil {
		return nil, fmt.Errorf("Python finder initialization failed: %w", err)
	}

	fmt.Fprintf(os.Stderr, "[PROFILE] Finding '%s'...\n", name)
	pyPath, isPackage, err := finder.Find(name)
	if err != nil {
		fmt.Fprintf(os.Stderr, "[PROFILE] Find failed for '%s': %v\n", name, err)
		return nil, err // Module not found
	}
	fmt.Fprintf(os.Stderr, "[PROFILE] Found '%s' at %s\n", name, pyPath)

	startLoad := time.Now()

	// Transpile the Python file
	transpiler := GetPythonTranspiler()
	astNode, err := transpiler.Transpile(pyPath)
	if err != nil {
		return nil, fmt.Errorf("failed to transpile Python module '%s': %w", name, err)
	}

	// Create module context as its own global scope
	// Pass nil to make it a top-level context
	moduleCtx := core.NewContext(nil)
	// Keep a reference to the parent global so builtins are accessible
	moduleCtx.Outer = ctx.Global
	moduleCtx.Define("__name__", core.StringValue(name))
	moduleCtx.Define("__file__", core.StringValue(pyPath))

	// Add __package__ for packages
	if isPackage {
		moduleCtx.Define("__package__", core.StringValue(name))
	} else {
		// For modules, __package__ is the parent package
		// e.g., for "os.path", __package__ is "os"
		// For "os", __package__ is ""
		lastDot := -1
		for i := len(name) - 1; i >= 0; i-- {
			if name[i] == '.' {
				lastDot = i
				break
			}
		}
		if lastDot > 0 {
			moduleCtx.Define("__package__", core.StringValue(name[:lastDot]))
		} else {
			moduleCtx.Define("__package__", core.StringValue(""))
		}
	}

	// Convert AST to IR
	startToIR := time.Now()
	ir := astNode.ToIR()
	toIRTime := time.Since(startToIR)

	// Evaluate the IR
	startEval := time.Now()
	_, err = evalFunc(ir, moduleCtx)
	if err != nil {
		return nil, fmt.Errorf("error in Python module '%s' (transpiled from %s):\n  %w", name, pyPath, err)
	}
	evalTime := time.Since(startEval)

	totalLoad := time.Since(startLoad)
	fmt.Fprintf(os.Stderr, "[PROFILE] LoadModule %s: total=%v toIR=%v eval=%v\n",
		name, totalLoad, toIRTime, evalTime)

	// Create module dictionary with exports
	moduleDict := core.NewDict()

	// Check for __all__ to determine what to export
	if allVal, err := moduleCtx.Lookup("__all__"); err == nil {
		// __all__ is defined, only export listed names
		if allList, ok := allVal.(core.ListValue); ok {
			for _, item := range allList {
				var exportName string
				switch v := item.(type) {
				case core.StringValue:
					exportName = string(v)
				case core.SymbolValue:
					exportName = string(v)
				default:
					continue
				}

				if val, err := moduleCtx.Lookup(exportName); err == nil {
					moduleDict.Set(exportName, val)
				}
			}
		}
	} else {
		// No __all__, export all non-private, non-dunder variables
		for varName, value := range moduleCtx.Vars {
			// Skip dunder variables (__name__, __file__, etc.)
			if len(varName) >= 2 && varName[:2] == "__" && varName[len(varName)-2:] == "__" {
				continue
			}
			// Skip private variables (starting with _)
			if len(varName) > 0 && varName[0] == '_' {
				continue
			}
			moduleDict.Set(varName, value)
		}
	}

	return moduleDict, nil
}
