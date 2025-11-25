package modules

import (
	"fmt"
	"os"
	"time"

	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/core/ast"
)

func init() {
	// Register Python module loader with core
	core.SetPythonLoader(LoadPythonModule)
}

// C extension modules that cannot be transpiled
// These are either .so/.pyd files or built into the Python interpreter
var cExtensionModules = map[string]bool{
	"_io":     true,
	"_thread": true,
	// "_socket" removed - provided as M28 stub module
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
	core.Log.Info(core.SubsystemImport, "Python loader invoked", "module", name)

	// Create partial module if not provided
	if partialModule == nil {
		partialModule = core.NewDict()
		core.Log.Debug(core.SubsystemImport, "Created new partial module", "module", name)
	}

	// Note: Circular import handling is done by ModuleLoaderEnhanced
	// This function loads content and populates partialModule during evaluation

	// Special case: builtins module - create a dict with builtin functions
	if name == "builtins" {
		core.Log.Debug(core.SubsystemImport, "Loading special builtins module", "module", name, "source_type", "builtin_special")
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
			// Exception types - all must be available as builtins
			"BaseException", "Exception",
			"KeyboardInterrupt", "SystemExit",
			"TypeError", "ValueError", "NameError", "AttributeError",
			"SyntaxError", "IndentationError", "TabError",
			"LookupError", "KeyError", "IndexError",
			"ArithmeticError", "ZeroDivisionError", "OverflowError", "FloatingPointError",
			"RuntimeError", "RecursionError", "NotImplementedError",
			"SystemError", "ImportError", "ModuleNotFoundError",
			"OSError", "IOError", "FileNotFoundError", "PermissionError",
			"IsADirectoryError", "NotADirectoryError", "FileExistsError",
			"EOFError", "StopIteration", "AssertionError",
			"UnboundLocalError",
			"TimeoutError", "InterruptedError",
			"ConnectionError", "BrokenPipeError", "ConnectionAbortedError",
			"ConnectionRefusedError", "ConnectionResetError",
			"UnicodeError", "UnicodeDecodeError", "UnicodeEncodeError", "UnicodeTranslateError",
			"Warning", "DeprecationWarning", "UserWarning", "PendingDeprecationWarning",
			"SyntaxWarning", "RuntimeWarning", "FutureWarning", "ImportWarning",
			"UnicodeWarning", "BytesWarning", "ResourceWarning",
		}

		for _, fnName := range builtinNames {
			val, err := ctx.Lookup(fnName)
			if err == nil {
				moduleDict.Set(fnName, val)
			}
		}

		// Register in sys.modules so pickle can find builtins.bool etc
		registerModuleInSysModules(name, moduleDict)

		return moduleDict, nil
	}

	// Special case: os.path should import posixpath (on Unix) or ntpath (on Windows)
	// For now, we always use posixpath
	if name == "os.path" {
		core.Log.Debug(core.SubsystemImport, "Redirecting os.path to posixpath", "module", name, "target", "posixpath")
		return LoadPythonModule("posixpath", ctx, evalFunc, partialModule)
	}

	// Special case: importlib requires _thread, _warnings, and _weakref to be pre-loaded
	// The _bootstrap._setup() function expects to find these in sys.modules
	if name == "importlib" {
		loader := core.GetModuleLoader()
		if loader != nil {
			// Pre-load the required builtin modules so they're in sys.modules
			// _setup() will find them there and won't try to use BuiltinImporter
			for _, modName := range []string{"_thread", "_warnings", "_weakref"} {
				if _, err := loader.LoadModule(modName, ctx); err != nil {
					// Log but continue - _setup() will handle the error
					core.DebugLog("[DEBUG] Failed to pre-load %s for importlib: %v\n", modName, err)
				}
			}
		}
	}

	// Check if this is a known C extension
	if cExtensionModules[name] {
		core.Log.Warn(core.SubsystemImport, "C extension module cannot be loaded", "module", name, "source_type", "c_extension")
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

	// Get sys.path for runtime search paths
	var sysPathDirs []string
	if sysModule, ok := GetBuiltinModule("sys"); ok {
		if sysPathVal, ok := sysModule.Get("path"); ok {
			if sysPathList, ok := sysPathVal.(*core.ListValue); ok {
				for _, item := range sysPathList.Items() {
					if strVal, ok := item.(core.StringValue); ok {
						path := string(strVal)
						// Skip empty paths
						if path == "" {
							continue
						}
						// Expand "." to current working directory
						if path == "." {
							if cwd, err := os.Getwd(); err == nil {
								path = cwd
							} else {
								continue
							}
						}
						sysPathDirs = append(sysPathDirs, path)
					}
				}
				if len(sysPathDirs) > 0 {
					core.Log.Debug(core.SubsystemImport, "Extracted sys.path directories", "module", name, "path_count", len(sysPathDirs))
				}
			}
		}
	}

	// Find the Python file
	core.Log.Debug(core.SubsystemImport, "Searching for Python file", "module", name)
	finder, err := GetPythonFinder()
	if err != nil {
		core.Log.Error(core.SubsystemImport, "Python finder initialization failed", "module", name, "error", err)
		return nil, fmt.Errorf("Python finder initialization failed: %w", err)
	}

	core.Log.Debug(core.SubsystemImport, "Invoking Python finder with paths", "module", name, "extra_paths", len(sysPathDirs))
	pyPath, isPackage, err := finder.FindWithExtraPaths(name, sysPathDirs)
	if err != nil {
		core.Log.Info(core.SubsystemImport, "Python module not found", "module", name, "error", err.Error())
		return nil, err // Module not found
	}
	core.Log.Info(core.SubsystemImport, "Python file located", "module", name, "path", pyPath, "is_package", isPackage)

	// Special case: if pyPath is a directory (e.g., for "." import), create an empty namespace package
	// This supports PEP 420 namespace packages and allows unittest to import "." for test discovery
	if info, err := os.Stat(pyPath); err == nil && info.IsDir() {
		core.Log.Debug(core.SubsystemImport, "Path is a directory, creating namespace package", "module", name, "path", pyPath)
		// Create an empty module dict for the namespace package
		// Set __path__ to allow submodule imports
		partialModule.Set("__name__", core.StringValue(name))
		partialModule.Set("__path__", core.NewList(core.StringValue(pyPath)))
		partialModule.Set("__file__", core.NilValue{})
		// For "." import, __package__ should be None, not "." to avoid trying to import ""
		if name == "." {
			partialModule.Set("__package__", core.NilValue{})
		} else {
			partialModule.Set("__package__", core.StringValue(name))
		}
		// Register in sys.modules
		registerModuleInSysModules(name, partialModule)
		core.Log.Info(core.SubsystemImport, "Namespace package created successfully", "module", name, "source_type", "namespace_package")
		return partialModule, nil
	}

	startLoad := time.Now()

	// Transpile the Python file
	core.Log.Debug(core.SubsystemImport, "Transpiling Python file", "module", name, "path", pyPath)
	transpiler := GetPythonTranspiler()
	astNode, err := transpiler.Transpile(pyPath)
	if err != nil {
		core.Log.Error(core.SubsystemImport, "Transpilation failed", "module", name, "error", err)
		return nil, fmt.Errorf("failed to transpile Python module '%s': %w", name, err)
	}
	core.Log.Debug(core.SubsystemImport, "Transpilation completed", "module", name)

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
	// Initialize empty __annotations__ dict for PEP 526 support
	// Python automatically creates this dict for modules that use annotations
	moduleCtx.Define("__annotations__", core.NewDict())

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

	// Convert AST to IR - for better error reporting, evaluate statements one by one
	// Check if astNode is a BlockForm (multiple statements)
	var stmts []ast.ASTNode
	if blockForm, ok := astNode.(*ast.BlockForm); ok {
		stmts = blockForm.Statements
	} else {
		// Single statement
		stmts = []ast.ASTNode{astNode}
	}

	// Evaluate statements one by one for better error reporting
	startEval := time.Now()
	evalTimeout := 120 * time.Second // 120 second timeout for module evaluation (increased for test.support)
	core.Log.Debug(core.SubsystemImport, "Beginning module evaluation", "module", name, "statements", len(stmts))

	type evalResult struct {
		val        core.Value
		err        error
		stmtIndex  int
		failedStmt ast.ASTNode
	}
	resultChan := make(chan evalResult, 1)

	go func() {
		var lastVal core.Value
		for i, stmt := range stmts {
			ir := stmt.ToIR()
			val, err := evalFunc(ir, moduleCtx)
			if err != nil {
				resultChan <- evalResult{nil, err, i + 1, stmt}
				return
			}
			lastVal = val
		}
		resultChan <- evalResult{lastVal, nil, 0, nil}
	}()

	var evalErr error
	var failedStmtNum int
	var failedStmt ast.ASTNode
	select {
	case result := <-resultChan:
		evalErr = result.err
		failedStmtNum = result.stmtIndex
		failedStmt = result.failedStmt
	case <-time.After(evalTimeout):
		// Provide diagnostic info about where evaluation stopped
		evalCount := moduleCtx.EvalCount
		return nil, fmt.Errorf(
			"timeout evaluating Python module '%s' (transpiled from %s):\n"+
				"  Module evaluation exceeded %v timeout.\n"+
				"  Evaluation count: %d operations\n"+
				"  This often indicates:\n"+
				"  - Missing C extension dependency\n"+
				"  - Infinite loop in module initialization\n"+
				"  - Complex import chain requiring unavailable modules",
			name, pyPath, evalTimeout, evalCount)
	}

	if evalErr != nil {
		core.Log.Error(core.SubsystemImport, "Module evaluation failed", "module", name, "statement", failedStmtNum, "error", evalErr)

		// Build detailed error message with source location
		var errMsg string
		if failedStmt != nil {
			loc := failedStmt.Location()
			stmtStr := failedStmt.String()
			if loc != nil {
				errMsg = fmt.Sprintf("  File \"%s\", line %d\n    %s\n -> %s",
					pyPath, loc.Line, stmtStr, evalErr.Error())
			} else {
				errMsg = fmt.Sprintf("  Statement: %s\n -> %s", stmtStr, evalErr.Error())
			}
		} else {
			errMsg = evalErr.Error()
		}

		// If it's an ImportError, return it directly so it can be caught by try/except
		// This allows modules like shutil to check for optional dependencies
		if _, ok := evalErr.(*core.ImportError); ok {
			return nil, evalErr
		}

		// Return formatted error with location context
		return nil, fmt.Errorf("%s", errMsg)
	}
	evalTime := time.Since(startEval)

	totalLoad := time.Since(startLoad)
	core.Log.Info(core.SubsystemImport, "Python module loaded successfully", "module", name, "total_time", totalLoad, "eval_time", evalTime)

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
	if name == "inspect" {
		// Set object.__signature__ to an empty Signature for inspect.signature() compatibility
		// This allows inspect.signature(SomeClass) to work for classes with no custom __init__
		core.DebugLog("[PYTHON_LOADER] Applying post-load fix for 'inspect' module\n")

		// Get Signature class from the inspect module
		sigKey := core.ValueToKey(core.StringValue("Signature"))
		if sigClass, ok := partialModule.Get(sigKey); ok {
			// Create an empty Signature instance
			if callable, ok := sigClass.(interface {
				Call([]core.Value, *core.Context) (core.Value, error)
			}); ok {
				if emptySig, err := callable.Call([]core.Value{}, ctx); err == nil {
					// Get object class from builtins
					objectClass, err := ctx.Lookup("object")
					if err == nil {
						if obj, ok := objectClass.(core.Object); ok {
							obj.SetAttr("__signature__", emptySig)
							core.DebugLog("[PYTHON_LOADER] Set object.__signature__ to empty Signature\n")
						}
					}
				}
			}
		}
	} else if name == "re" {
		// 		fmt.Printf("[DEBUG] Applying post-load fix for 're' module\n")
		core.DebugLog("[PYTHON_LOADER] Applying post-load fix for 're' module\n")
		// The @enum.global_enum decorator in re/__init__.py doesn't work in M28
		// It's supposed to export enum members to module level, but fails
		// Manually add the missing T and DEBUG flags from _compiler
		compilerKey := core.ValueToKey(core.StringValue("_compiler"))
		// 		fmt.Printf("[DEBUG] Looking for _compiler with key: %s\n", compilerKey)
		if compilerVal, ok := partialModule.Get(compilerKey); ok {
			// 			fmt.Printf("[DEBUG] Found _compiler in re module, type: %T\n", compilerVal)

			// _compiler can be either a *core.Module or *core.DictValue
			var getCompilerAttr func(string) (core.Value, bool)

			if compilerModule, ok := compilerVal.(*core.Module); ok {
				// 				fmt.Printf("[DEBUG] _compiler is a Module, checking exports\n")
				getCompilerAttr = func(name string) (core.Value, bool) {
					return compilerModule.GetExport(name)
				}
			} else if compilerDict, ok := compilerVal.(*core.DictValue); ok {
				// 				fmt.Printf("[DEBUG] _compiler is a DictValue\n")
				getCompilerAttr = func(name string) (core.Value, bool) {
					key := core.ValueToKey(core.StringValue(name))
					return compilerDict.Get(key)
				}
			} else {
				// 				fmt.Printf("[DEBUG] _compiler is neither Module nor DictValue\n")
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
						// 						fmt.Printf("[DEBUG] Found %s: %v, adding %v\n", mapping.srcName, flagVal, mapping.dstNames)
						for _, dstName := range mapping.dstNames {
							partialModule.Set(dstName, flagVal)
							if moduleCtx != nil {
								moduleCtx.Define(dstName, flagVal)
							}
						}
					} else {
						// 						fmt.Printf("[DEBUG] %s not found in _compiler\n", mapping.srcName)
					}
				}
			}
		} else {
			// 			fmt.Printf("[DEBUG] WARNING: _compiler not found in re module\n")
		}

		// Add NOFLAG = 0 (defined in re/__init__.py but not exported by @enum.global_enum)
		// 		fmt.Printf("[DEBUG] Adding NOFLAG = 0\n")
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
	// Get sys module from builtin registry
	sysModule, ok := GetBuiltinModule("sys")
	if !ok {
		return // sys not available yet
	}

	// Get sys.modules dict
	sysModulesVal, ok := sysModule.Get("modules")
	if !ok {
		return // sys.modules not available
	}

	sysModulesDict, ok := sysModulesVal.(*core.DictValue)
	if !ok {
		return // sys.modules is not a dict
	}

	// Register the module
	// Use ValueToKey to generate the proper key format (e.g., "s:importlib._bootstrap")
	key := core.ValueToKey(core.StringValue(name))
	sysModulesDict.SetWithKey(key, core.StringValue(name), moduleDict)
	core.DebugLog("[DEBUG] Registered module '%s' in sys.modules\n", name)

	// For dotted module names (e.g., "test.test_bool"), set the submodule
	// as an attribute on the parent package module
	// This allows sys.modules['test'].test_bool to work
	lastDot := -1
	for i := len(name) - 1; i >= 0; i-- {
		if name[i] == '.' {
			lastDot = i
			break
		}
	}

	if lastDot > 0 {
		parentName := name[:lastDot]
		childName := name[lastDot+1:]

		// Try to get existing parent module
		parentKey := core.ValueToKey(core.StringValue(parentName))
		parentModule, parentExists := sysModulesDict.Get(parentKey)

		// Only set child on parent if parent already exists as a real module
		// Don't create placeholder parent modules - they block the real module from loading
		if parentExists {
			if parentDict, ok := parentModule.(*core.DictValue); ok {
				// Only set if the child doesn't already exist OR if it's also a dict (module)
				// This prevents overwriting attributes like 'collections.namedtuple' (a function)
				// with partial module dicts when 'collections.namedtuple' fails to load as a module
				existingChild, childExists := parentDict.Get(childName)
				if !childExists || isModuleLike(existingChild) {
					parentDict.Set(childName, moduleDict)
					core.DebugLog("[DEBUG] Set '%s' as attribute on parent module '%s'\n", childName, parentName)
				} else {
					core.DebugLog("[DEBUG] Skipped setting '%s' on parent '%s' (already exists as non-module: %T)\n", childName, parentName, existingChild)
				}
			}
		} else {
			core.DebugLog("[DEBUG] Parent module '%s' not loaded yet, will set '%s' attribute when parent loads\n", parentName, childName)
		}
	}
}

// isModuleLike checks if a value looks like a module (dict or Module object)
func isModuleLike(val core.Value) bool {
	switch val.(type) {
	case *core.DictValue, *core.Module:
		return true
	default:
		return false
	}
}
