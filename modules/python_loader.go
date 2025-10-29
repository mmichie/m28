package modules

import (
	"fmt"
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
	"_bz2":             true,
	"_lzma":            true,
	"zlib":             true,
	// Frozen modules (compiled into CPython interpreter)
	"_frozen_importlib":          true,
	"_frozen_importlib_external": true,
	// "_weakref" removed - provided as M28 stub module
	// "_abc" removed - let it fail naturally to trigger fallback to _py_abc
	// "builtins" removed - handled specially in LoadPythonModule
}

// LoadPythonModule attempts to load a Python module by name
// Returns (*DictValue, error) to match ModuleLoader interface
// If partialModule is provided, it will be populated during evaluation for circular import support
func LoadPythonModule(name string, ctx *core.Context, evalFunc func(core.Value, *core.Context) (core.Value, error), partialModule *core.DictValue) (*core.DictValue, error) {
	core.DebugLog("[PROFILE] LoadPythonModule called for '%s'\n", name)

	// Create partial module if not provided
	if partialModule == nil {
		partialModule = core.NewDict()
	}

	// Note: Circular import handling is done by ModuleLoaderEnhanced
	// This function loads content and populates partialModule during evaluation

	// Special case: builtins module - create a dict with builtin functions
	if name == "builtins" {
		moduleDict := core.NewDict()
		// Expose the most commonly used builtin functions
		// These are already defined in the global context
		builtinNames := []string{
			"repr", "str", "int", "float", "bool", "list", "dict", "tuple", "set",
			"len", "range", "enumerate", "zip", "map", "filter", "sum", "min", "max",
			"abs", "all", "any", "sorted", "reversed", "print", "input", "open",
			"type", "isinstance", "issubclass", "hasattr", "getattr", "setattr", "delattr",
			"callable", "dir", "vars", "id", "hash", "hex", "oct", "bin", "chr", "ord",
			"bytes", "bytearray", "memoryview", "frozenset",
			"property", "staticmethod", "classmethod",
			"object", "super", "slice",
			"eval", "exec", "compile",
			"__import__", "__build_class__",
		}

		for _, fnName := range builtinNames {
			val, err := ctx.Lookup(fnName)
			if err == nil {
				moduleDict.Set(fnName, val)
			}
		}

		return moduleDict, nil
	}

	// Special case: os.path should import posixpath (on Unix) or ntpath (on Windows)
	// For now, we always use posixpath
	if name == "os.path" {
		core.DebugLog("[DEBUG] Redirecting os.path import to posixpath\n")
		return LoadPythonModule("posixpath", ctx, evalFunc, partialModule)
	}

	// Check if this is a known C extension
	if cExtensionModules[name] {
		// Return ImportError so Python code can catch it
		return nil, &core.ImportError{
			ModuleName: name,
			Message: fmt.Sprintf(
				"module '%s' is a C extension and cannot be auto-imported.\n"+
					"Consider implementing a native M28 version in builtin/ or modules/",
				name,
			),
		}
	}

	// Find the Python file
	finder, err := GetPythonFinder()
	if err != nil {
		return nil, fmt.Errorf("Python finder initialization failed: %w", err)
	}

	core.DebugLog("[PROFILE] Finding '%s'...\n", name)
	pyPath, isPackage, err := finder.Find(name)
	if err != nil {
		core.DebugLog("[PROFILE] Find failed for '%s': %v\n", name, err)
		return nil, err // Module not found
	}
	core.DebugLog("[PROFILE] Found '%s' at %s\n", name, pyPath)

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

	// Link to partial module for real-time syncing during evaluation
	// This enables circular imports to see partially-populated modules
	moduleCtx.ModuleDict = partialModule

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

	// Evaluate the IR with timeout
	startEval := time.Now()
	evalTimeout := 5 * time.Second // 5 second timeout for module evaluation

	type evalResult struct {
		val core.Value
		err error
	}
	resultChan := make(chan evalResult, 1)

	go func() {
		val, err := evalFunc(ir, moduleCtx)
		resultChan <- evalResult{val, err}
	}()

	var evalErr error
	select {
	case result := <-resultChan:
		evalErr = result.err
	case <-time.After(evalTimeout):
		return nil, fmt.Errorf(
			"timeout evaluating Python module '%s' (transpiled from %s):\n"+
				"  Module evaluation exceeded %v timeout.\n"+
				"  This often indicates:\n"+
				"  - Missing C extension dependency\n"+
				"  - Infinite loop in module initialization\n"+
				"  - Complex import chain requiring unavailable modules",
			name, pyPath, evalTimeout)
	}

	if evalErr != nil {
		// If it's an ImportError, return it directly so it can be caught by try/except
		// This allows modules like shutil to check for optional dependencies
		if _, ok := evalErr.(*core.ImportError); ok {
			return nil, evalErr
		}
		// For other errors, wrap with context
		return nil, fmt.Errorf("error in Python module '%s' (transpiled from %s):\n  %w", name, pyPath, evalErr)
	}
	evalTime := time.Since(startEval)

	totalLoad := time.Since(startLoad)
	core.DebugLog("[PROFILE] LoadModule %s: total=%v toIR=%v eval=%v\n",
		name, totalLoad, toIRTime, evalTime)

	// Note: Module dict was already populated during evaluation via Context.ModuleDict
	// Context.Define() automatically syncs public names to partialModule
	// We need to also add private names (starting with _) that Python stdlib uses

	// Add underscore-prefixed names that are used internally by Python stdlib
	for varName, value := range moduleCtx.Vars {
		// Skip dunder variables (__name__, __file__, etc.) except special ones
		// __all__ defines the public API and must be exported
		// __import__ and __build_class__ are special functions from importlib._bootstrap
		isDunder := len(varName) >= 2 && varName[:2] == "__" && varName[len(varName)-2:] == "__"
		if isDunder && varName != "__all__" && varName != "__import__" && varName != "__build_class__" {
			continue
		}
		// Only process private names (starting with _) - public names already synced
		if len(varName) > 0 && varName[0] == '_' {
			key := core.ValueToKey(core.StringValue(varName))
			partialModule.SetWithKey(key, core.StringValue(varName), value)
		}
	}

	// Special post-load fixes for specific modules
	// Debug: always print which module is being loaded
	fmt.Printf("[DEBUG] Post-processing module: %s\n", name)
	if name == "re" {
		fmt.Printf("[DEBUG] Applying post-load fix for 're' module\n")
		core.DebugLog("[PYTHON_LOADER] Applying post-load fix for 're' module\n")
		// The @enum.global_enum decorator in re/__init__.py doesn't work in M28
		// It's supposed to export enum members to module level, but fails
		// Manually add the missing T and DEBUG flags from _compiler
		compilerKey := core.ValueToKey(core.StringValue("_compiler"))
		fmt.Printf("[DEBUG] Looking for _compiler with key: %s\n", compilerKey)
		if compilerVal, ok := partialModule.Get(compilerKey); ok {
			fmt.Printf("[DEBUG] Found _compiler in re module, type: %T\n", compilerVal)

			// _compiler can be either a *core.Module or *core.DictValue
			var getCompilerAttr func(string) (core.Value, bool)

			if compilerModule, ok := compilerVal.(*core.Module); ok {
				fmt.Printf("[DEBUG] _compiler is a Module, checking exports\n")
				getCompilerAttr = func(name string) (core.Value, bool) {
					return compilerModule.GetExport(name)
				}
			} else if compilerDict, ok := compilerVal.(*core.DictValue); ok {
				fmt.Printf("[DEBUG] _compiler is a DictValue\n")
				getCompilerAttr = func(name string) (core.Value, bool) {
					key := core.ValueToKey(core.StringValue(name))
					return compilerDict.Get(key)
				}
			} else {
				fmt.Printf("[DEBUG] _compiler is neither Module nor DictValue\n")
				getCompilerAttr = nil
			}

			if getCompilerAttr != nil {
				// The @enum.global_enum decorator doesn't work in M28, so we manually
				// export all the regex flags from _compiler to the re module level
				flagMappings := []struct {
					srcName  string   // name in _compiler
					dstNames []string // names to export to re module
				}{
					{"SRE_FLAG_TEMPLATE", []string{"T", "TEMPLATE"}},
					{"SRE_FLAG_DEBUG", []string{"DEBUG"}},
					{"SRE_FLAG_ASCII", []string{"A", "ASCII"}},
					{"SRE_FLAG_IGNORECASE", []string{"I", "IGNORECASE"}},
					{"SRE_FLAG_LOCALE", []string{"L", "LOCALE"}},
					{"SRE_FLAG_MULTILINE", []string{"M", "MULTILINE"}},
					{"SRE_FLAG_DOTALL", []string{"S", "DOTALL"}},
					{"SRE_FLAG_UNICODE", []string{"U", "UNICODE"}},
					{"SRE_FLAG_VERBOSE", []string{"X", "VERBOSE"}},
				}

				for _, mapping := range flagMappings {
					if flagVal, ok := getCompilerAttr(mapping.srcName); ok {
						fmt.Printf("[DEBUG] Found %s: %v, adding %v\n", mapping.srcName, flagVal, mapping.dstNames)
						for _, dstName := range mapping.dstNames {
							partialModule.Set(dstName, flagVal)
							if moduleCtx != nil {
								moduleCtx.Define(dstName, flagVal)
							}
						}
					} else {
						fmt.Printf("[DEBUG] %s not found in _compiler\n", mapping.srcName)
					}
				}
			}
		} else {
			fmt.Printf("[DEBUG] WARNING: _compiler not found in re module\n")
		}

		// Add NOFLAG = 0 (defined in re/__init__.py but not exported by @enum.global_enum)
		fmt.Printf("[DEBUG] Adding NOFLAG = 0\n")
		partialModule.Set("NOFLAG", core.NumberValue(0))
		if moduleCtx != nil {
			moduleCtx.Define("NOFLAG", core.NumberValue(0))
		}
	}

	// Register the module in sys.modules so Python code can find it
	// This is essential for importlib and other modules that check sys.modules
	registerModuleInSysModules(name, partialModule)

	return partialModule, nil
}

// registerModuleInSysModules registers a loaded module in sys.modules
func registerModuleInSysModules(name string, moduleDict *core.DictValue) {
	fmt.Printf("[REGISTER] Attempting to register module '%s'\n", name)

	// Get sys module from builtin registry
	sysModule, ok := GetBuiltinModule("sys")
	if !ok {
		fmt.Printf("[REGISTER] sys module not available\n")
		return // sys not available yet
	}

	// Get sys.modules dict
	sysModulesVal, ok := sysModule.Get("modules")
	if !ok {
		fmt.Printf("[REGISTER] sys.modules not found (tried key 'modules')\n")
		return // sys.modules not available
	}

	sysModulesDict, ok := sysModulesVal.(*core.DictValue)
	if !ok {
		fmt.Printf("[REGISTER] sys.modules is not a dict (type: %T)\n", sysModulesVal)
		return // sys.modules is not a dict
	}

	// Register the module
	// Use ValueToKey to generate the proper key format (e.g., "s:importlib._bootstrap")
	key := core.ValueToKey(core.StringValue(name))
	sysModulesDict.SetWithKey(key, core.StringValue(name), moduleDict)
	fmt.Printf("[REGISTER] Successfully registered module '%s' in sys.modules (key=%s)\n", name, key)
}
