package special_forms

import (
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"

	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/env"
	"github.com/mmichie/m28/parser"
)

// ModuleLoaderImpl implements the core.ModuleLoader interface
type ModuleLoaderImpl struct {
	evaluator core.Evaluator
}

// NewModuleLoader creates a new ModuleLoader
func NewModuleLoader() *ModuleLoaderImpl {
	return &ModuleLoaderImpl{}
}

// SetEvaluator sets the evaluator for the module loader
func (m *ModuleLoaderImpl) SetEvaluator(e core.Evaluator) {
	m.evaluator = e
}

// GetEvaluator returns the evaluator for the module loader
func (m *ModuleLoaderImpl) GetEvaluator() core.Evaluator {
	return m.evaluator
}

// LoadModule loads a module and returns its contents
func (m *ModuleLoaderImpl) LoadModule(name string, e core.Evaluator) (*core.PythonicDict, error) {
	registry := core.GetModuleRegistry()

	// Check if the module is already loaded
	if module, ok := registry.GetModule(name); ok {
		return module, nil
	}

	// Resolve the module file path
	modulePath, err := registry.ResolveModulePath(name)
	if err != nil {
		// Create a detailed error message with search paths and suggestions
		searchPaths := registry.GetSearchPaths()
		errMsg := fmt.Sprintf("module not found: %s\n\nSearched in the following locations:\n", name)
		for _, path := range searchPaths {
			errMsg += fmt.Sprintf("  - %s\n", path)
		}
		errMsg += "\nPossible reasons for this error:\n"
		errMsg += "  - The module file does not exist\n"
		errMsg += "  - The module name is misspelled\n"
		errMsg += "  - The module is in a different directory not in the search path\n"
		errMsg += "\nTry one of the following solutions:\n"
		errMsg += "  - Check for typos in the module name\n"
		errMsg += "  - Use an absolute path to the module file\n"
		errMsg += "  - Create the module file in one of the search paths\n"

		// Return a formatted error as a module exception
		return nil, core.NewException("ModuleNotFoundError", errMsg)
	}

	// Track dependencies for this module
	dependencies := []string{}

	// Read the module file
	content, err := os.ReadFile(modulePath)
	if err != nil {
		// Provide detailed information about file access errors
		errMsg := fmt.Sprintf("Failed to read module file: %s\n\nError details: %v\n\n", modulePath, err)
		errMsg += "Possible reasons for this error:\n"
		errMsg += "  - Permission denied: You do not have read access to the file\n"
		errMsg += "  - File is in use by another process\n"
		errMsg += "  - File is corrupted or not a valid text file\n"

		// Return a formatted error as a file I/O exception
		return nil, core.NewException("IOError", errMsg)
	}

	// Register source code for better error reporting
	core.RegisterSourceCode(modulePath, string(content))

	// Parse the module content
	p := parser.NewParser()
	p.SetFilename(modulePath) // Set filename for better error reporting
	parsed, err := p.Parse(string(content))
	if err != nil {
		// Provide detailed syntax error information
		errMsg := fmt.Sprintf("Syntax error in module %s:\n\nError details: %v\n\n", name, err)
		errMsg += "This is likely caused by invalid syntax in the module file.\n"
		errMsg += "Check for issues like:\n"
		errMsg += "  - Mismatched parentheses\n"
		errMsg += "  - Invalid characters or expressions\n"
		errMsg += "  - Incomplete expressions\n"

		// Try to extract line and column information from the error message
		// Typical format: "parse error at filename.m28:12:3 (token): message"
		errorMsg := err.Error()
		lineColRegex := regexp.MustCompile(`at\s+[^:]+:(\d+):(\d+)`)
		if matches := lineColRegex.FindStringSubmatch(errorMsg); len(matches) >= 3 {
			lineStr, colStr := matches[1], matches[2]
			if lineNum, err := strconv.Atoi(lineStr); err == nil {
				colNum, _ := strconv.Atoi(colStr)

				// Get the corresponding line from the file content
				lines := strings.Split(string(content), "\n")
				if lineNum > 0 && lineNum <= len(lines) {
					lineContent := lines[lineNum-1]
					errMsg += fmt.Sprintf("\nError at line %d:\n%s\n", lineNum, lineContent)

					// Add a pointer to the error position if column info is available
					if colNum > 0 {
						pointer := strings.Repeat(" ", colNum-1) + "^"
						errMsg += pointer + "\n"
					}
				}
			}
		}

		// Return a formatted error as a syntax exception
		return nil, core.NewException("SyntaxError", errMsg)
	}

	// Create a new environment for the module
	moduleEnv := env.NewEnvironment(nil)
	moduleEnv.SetupBuiltins() // Set up builtin functions in the environment

	// Register special forms in the module environment
	RegisterSpecialForms(moduleEnv)

	module := core.NewPythonicDict()

	// Store module metadata in the module itself
	module.Set("__name__", name)
	module.Set("__file__", modulePath)

	// Execute the module code
	parsedList, ok := parsed.(core.LispList)
	if !ok {
		errMsg := fmt.Sprintf("Invalid module content in '%s'\n\n", name)
		errMsg += "The parsed content from the module file is not valid Lisp code.\n"
		errMsg += "This is likely caused by a corruption in the file or an issue with the parser.\n\n"
		errMsg += "Please check that the file contains valid M28 Lisp code and is not corrupted."
		return nil, core.NewException("ModuleError", errMsg)
	}

	// First pass: execute all expressions in the module file
	for i, expr := range parsedList {
		_, err := e.Eval(expr, moduleEnv)
		if err != nil {
			// Check if it's already an Exception with traceback
			if ex, ok := err.(*core.Exception); ok {
				// Add module context to the error message
				moduleMeta := fmt.Sprintf("\nError while loading module '%s' from '%s'", name, modulePath)
				ex.Message += moduleMeta
				return nil, ex
			}

			// Create a new detailed error message
			errMsg := fmt.Sprintf("Runtime error in module %s (expression #%d):\n\nError details: %v\n\n",
				name, i+1, err)
			errMsg += "This error occurred during the execution of the module code.\n"
			errMsg += "Possible causes:\n"
			errMsg += "  - Reference to undefined symbols or variables\n"
			errMsg += "  - Invalid operations or type errors\n"
			errMsg += "  - Missing or circular dependencies\n"

			// Create an appropriate exception type based on the error
			var exType string
			errorMsg := err.Error()
			switch {
			case strings.Contains(errorMsg, "undefined symbol"):
				exType = "NameError"
			case strings.Contains(errorMsg, "type error"):
				exType = "TypeError"
			case strings.Contains(errorMsg, "index out of range"):
				exType = "IndexError"
			default:
				exType = "RuntimeError"
			}

			// Create the exception with the detailed message
			exception := core.NewException(exType, errMsg)

			// Return the exception
			return nil, exception
		}
	}

	// Second pass: collect all defined symbols from the module's environment
	moduleEnv.ForEachSymbol(func(symbol core.LispSymbol, value core.LispValue) {
		// Only add non-builtin symbols to the module dictionary
		if !isBuiltinSymbol(symbol) {
			// Add to module dictionary - only user-defined symbols
			module.Set(symbol, value)
		}
	})

	// Check if the module defined an __exports__ list
	exportsVal, hasExports := module.Get("__exports__")
	if hasExports {
		// If __exports__ is defined, only export the symbols listed in it
		if exportsList, ok := exportsVal.(core.LispList); ok {
			// First, build a list of keys to keep
			keysToKeep := make(map[string]bool)

			// Always keep metadata
			keysToKeep["__name__"] = true
			keysToKeep["__file__"] = true
			keysToKeep["__exports__"] = true

			// Keep the dot notation special forms
			keysToKeep["."] = true
			keysToKeep["dot"] = true

			// Keep all exported symbols
			for _, symbolVal := range exportsList {
				var symbolName string
				switch sym := symbolVal.(type) {
				case core.LispSymbol:
					symbolName = string(sym)
				case string:
					symbolName = sym
				default:
					continue // Skip non-symbol/string items
				}

				if _, ok := module.Get(symbolName); ok {
					keysToKeep[symbolName] = true
				}
			}

			// Create a list of user-defined keys to remove
			var keysToRemove []string
			keysList, err := module.CallMethod("keys", []core.LispValue{})
			if err == nil {
				if keyList, ok := keysList.(core.LispList); ok {
					for _, keyVal := range keyList {
						if keyStr, ok := keyVal.(string); ok {
							// If it's not in the keys to keep and isn't a builtin or special form
							// (don't filter out the language constructs)
							if !keysToKeep[keyStr] &&
								!isBuiltinSymbol(core.LispSymbol(keyStr)) &&
								!strings.HasPrefix(keyStr, "__") {
								keysToRemove = append(keysToRemove, keyStr)
							}
						}
					}
				}
			}

			// Remove the user-defined keys that aren't in the export list
			for _, key := range keysToRemove {
				_, _ = module.CallMethod("delete", []core.LispValue{key})
			}
		}
	}

	// Store the loaded module with its metadata
	registry.StoreModule(name, module, modulePath, dependencies)

	return module, nil
}

// LoadModule is a backward-compatible wrapper for the module loader
func LoadModule(name string, e core.Evaluator) (*core.PythonicDict, error) {
	// Get the global module loader, or create one if it doesn't exist
	moduleLoader := core.GetModuleLoader()
	if moduleLoader == nil {
		return nil, fmt.Errorf("no module loader registered")
	}

	module, err := moduleLoader.LoadModule(name, e)
	if err != nil {
		return nil, err
	}

	return module, nil
}

// createImportError creates a detailed error message for import failures
func createImportError(moduleName string, err error) error {
	// Check if it's already a detailed exception
	if ex, ok := err.(*core.Exception); ok {
		// Add context about this specific import attempt
		contextMsg := fmt.Sprintf("\nError occurred during import of module: %s", moduleName)
		ex.Message += contextMsg
		return ex
	}

	// Create a detailed import error message
	errMsg := fmt.Sprintf("Failed to import module '%s':\n\n%v\n\n", moduleName, err)
	errMsg += "Possible reasons for this error:\n"
	errMsg += "  - The module file does not exist\n"
	errMsg += "  - The module has syntax errors\n"
	errMsg += "  - The module has runtime errors during initialization\n"
	errMsg += "  - The module has circular dependencies\n"

	return core.NewException("ImportError", errMsg)
}

// Helper function to create a module dot handler
func createModuleDotHandler(moduleName string, moduleBaseName string, module *core.PythonicDict) core.BuiltinFunc {
	return core.BuiltinFunc(func(args []core.LispValue, env core.Environment) (core.LispValue, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf(core.ErrDotMissingArgs)
		}

		// Get the property name
		var propName string
		switch prop := args[0].(type) {
		case string:
			propName = prop
		case core.LispSymbol:
			propName = string(prop)
		default:
			return nil, fmt.Errorf(core.ErrDotPropertyType, args[0])
		}

		// Get the property from the module
		value, ok := module.Get(propName)
		if !ok {
			return nil, core.ErrDotModulePropertyf(moduleBaseName, propName)
		}

		// Check if additional arguments were provided (method call)
		if len(args) > 1 {
			// Extract the method arguments (skip the property name)
			methodArgs := args[1:]

			// Handle different callable types
			switch fn := value.(type) {
			case core.BuiltinFunc:
				// Call the builtin function directly
				return fn(methodArgs, env)
			case *core.Lambda:
				// Use evaluator to apply the lambda
				// We need the evaluator to apply the lambda
				if moduleLoader := core.GetModuleLoader(); moduleLoader != nil {
					evaluator := moduleLoader.GetEvaluator()
					return evaluator.Apply(fn, methodArgs, env)
				}
				return nil, fmt.Errorf(core.ErrDotEvaluatorMissing, propName)
			case core.DotAccessible:
				// Check if the object has a __call__ method
				if fn.HasMethod("__call__") {
					return fn.CallMethod("__call__", methodArgs)
				}
				return nil, core.ErrDotNoMethodf(propName)
			default:
				return nil, core.ErrDotNotCallablef(propName, value)
			}
		}

		return value, nil
	})
}

// Helper function to register a module with a specific alias name
func registerModuleWithAlias(moduleName string, aliasName string, module *core.PythonicDict, e core.Evaluator, env core.Environment) error {
	// Define the module in the environment with the alias name
	env.Define(core.LispSymbol(aliasName), module)

	// Create a dot handler for the module to allow attribute access
	env.Define(core.LispSymbol(aliasName+".__dot__"), createModuleDotHandler(moduleName, aliasName, module))

	return nil
}

// Helper function to register a module in the environment
func registerModule(moduleName string, module *core.PythonicDict, e core.Evaluator, env core.Environment) error {
	// Determine module name for environment binding
	// Use the last part of the path for module name if there's a path separator
	moduleNameParts := strings.Split(moduleName, "/")
	moduleBaseName := moduleNameParts[len(moduleNameParts)-1]

	// Remove .m28 extension if present
	moduleBaseName = strings.TrimSuffix(moduleBaseName, ".m28")

	// Define the module in the environment
	env.Define(core.LispSymbol(moduleBaseName), module)

	// Create a dot handler for the module to allow attribute access
	env.Define(core.LispSymbol(moduleBaseName+".__dot__"), createModuleDotHandler(moduleName, moduleBaseName, module))

	return nil
}

func EvalImport(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 1 {
		// Return a proper error with usage information
		errMsg := "import requires at least one argument\n\n"
		errMsg += "Correct usage examples:\n"
		errMsg += "  (import module_name)          # Import a module by name\n"
		errMsg += "  (import \"path/to/module\")     # Import a module by path\n"
		errMsg += "  (import module1 module2)      # Import multiple modules\n"
		errMsg += "  (import (from module import symbol1 symbol2))  # Import specific symbols\n"
		errMsg += "  (import (module as alias))    # Import a module with an alias\n"
		errMsg += "  (import (from module import (symbol as alias))) # Import a symbol with an alias\n"

		return nil, core.NewException("ImportError", errMsg)
	}

	// Get the module loader
	moduleLoader := core.GetModuleLoader()
	if moduleLoader == nil {
		errMsg := "no module loader registered in the interpreter\n\n"
		errMsg += "This is an internal system error that should not occur during normal operation.\n"
		errMsg += "The module system has not been properly initialized.\n"

		return nil, core.NewException("SystemError", errMsg)
	}

	for _, arg := range args {
		switch importSpec := arg.(type) {
		case core.LispSymbol:
			// Simple import: import module
			moduleName := string(importSpec)
			module, err := moduleLoader.LoadModule(moduleName, e)
			if err != nil {
				return nil, createImportError(moduleName, err)
			}

			// Register the module in the environment
			if err := registerModule(moduleName, module, e, env); err != nil {
				return nil, err
			}

		case string:
			// String literal module path: import "path/to/module"
			modulePath := importSpec
			module, err := moduleLoader.LoadModule(modulePath, e)
			if err != nil {
				return nil, createImportError(modulePath, err)
			}

			// Register the module in the environment
			if err := registerModule(modulePath, module, e, env); err != nil {
				return nil, err
			}

		case core.LispList:
			// Check for aliased import syntax: (import (module as alias))
			if len(importSpec) == 3 && importSpec[1] == core.LispSymbol("as") {
				// Parse the module name and alias
				var moduleName string
				var aliasName string

				// Get the module name
				switch mn := importSpec[0].(type) {
				case core.LispSymbol:
					moduleName = string(mn)
				case string:
					moduleName = mn
				default:
					errMsg := "Module name must be a symbol or string in aliased import\n\n"
					errMsg += "For aliased imports, use the format:\n"
					errMsg += "  (import (module_name as alias_name))\n\n"
					errMsg += "Examples:\n"
					errMsg += "  (import (math as m))\n"
					errMsg += "  (import (\"path/to/module\" as custom_name))"
					return nil, core.NewException("ImportError", errMsg)
				}

				// Get the alias name
				switch an := importSpec[2].(type) {
				case core.LispSymbol:
					aliasName = string(an)
				default:
					errMsg := "Alias must be a symbol in aliased import\n\n"
					errMsg += "The alias should be a valid identifier:\n"
					errMsg += "  (import (module_name as alias_name))"
					return nil, core.NewException("ImportError", errMsg)
				}

				// Load the module
				module, err := moduleLoader.LoadModule(moduleName, e)
				if err != nil {
					return nil, createImportError(moduleName, err)
				}

				// Register the module with the custom alias name
				if err := registerModuleWithAlias(moduleName, aliasName, module, e, env); err != nil {
					return nil, err
				}

				continue
			}

			// Complex import: from module import symbol1, symbol2
			if len(importSpec) < 4 || importSpec[0] != core.LispSymbol("from") || importSpec[2] != core.LispSymbol("import") {
				errMsg := fmt.Sprintf("Invalid import specification: %v\n\n", importSpec)
				errMsg += "For 'from-import' statements, use the format:\n"
				errMsg += "  (import (from module import symbol1 symbol2 ...))\n\n"
				errMsg += "For aliased imports, use the format:\n"
				errMsg += "  (import (module as alias))\n\n"
				errMsg += "Examples:\n"
				errMsg += "  (import (from math import sin cos tan))\n"
				errMsg += "  (import (from \"path/to/module\" import function1 function2))\n"
				errMsg += "  (import (math as m))"
				return nil, core.NewException("ImportError", errMsg)
			}

			var moduleName string
			switch mn := importSpec[1].(type) {
			case core.LispSymbol:
				moduleName = string(mn)
			case string:
				moduleName = mn
			default:
				errMsg := "Module name must be a symbol or string\n\n"
				errMsg += "In a from-import statement:\n"
				errMsg += "  (import (from module_name import symbols))\n"
				errMsg += "  (import (from \"module/path\" import symbols))\n\n"
				errMsg += "The 'module_name' must be either a symbol or a string literal."
				return nil, core.NewException("ImportError", errMsg)
			}

			module, err := moduleLoader.LoadModule(moduleName, e)
			if err != nil {
				return nil, createImportError(moduleName, err)
			}

			// Process all symbols after "import"
			for i := 3; i < len(importSpec); i++ {
				// Check for individual symbol to import
				var symName core.LispSymbol
				var targetSymName core.LispSymbol
				symbol := importSpec[i]

				// Get the symbol name
				switch s := symbol.(type) {
				case core.LispSymbol:
					symName = s
					targetSymName = s
				case string:
					symName = core.LispSymbol(s)
					targetSymName = symName
				case core.LispList:
					// Check for symbol aliasing within the import: (from module import (symbol as alias))
					if len(s) == 3 && s[1] == core.LispSymbol("as") {
						// Get the original symbol name
						switch origSym := s[0].(type) {
						case core.LispSymbol:
							symName = origSym
						case string:
							symName = core.LispSymbol(origSym)
						default:
							errMsg := "Symbol name must be a symbol or string in aliased import\n\n"
							errMsg += "For aliased symbols in from-import, use the format:\n"
							errMsg += "  (import (from module import (symbol as alias) ...))"
							return nil, core.NewException("ImportError", errMsg)
						}

						// Get the alias name
						switch aliasSym := s[2].(type) {
						case core.LispSymbol:
							targetSymName = aliasSym
						default:
							errMsg := "Alias must be a symbol in aliased import\n\n"
							errMsg += "The alias should be a valid identifier"
							return nil, core.NewException("ImportError", errMsg)
						}
					} else {
						errMsg := "Invalid symbol alias specification in from-import statement\n\n"
						errMsg += "For aliased symbols, use the format:\n"
						errMsg += "  (import (from module import (symbol as alias) ...))"
						return nil, core.NewException("ImportError", errMsg)
					}
				default:
					errMsg := "Imported symbol must be a symbol, string, or alias specification\n\n"
					errMsg += "In a from-import statement:\n"
					errMsg += "  (import (from module import symbol1 symbol2 ...))\n"
					errMsg += "  (import (from module import (symbol1 as alias1) symbol2 ...))\n\n"
					errMsg += "Each symbol after 'import' must be either a symbol, string, or alias specification."
					return nil, core.NewException("ImportError", errMsg)
				}

				// Look up the value in the module
				value, ok := module.Get(symName)
				if !ok {
					errMsg := fmt.Sprintf("Symbol '%s' not found in module '%s'\n\n", symName, moduleName)
					errMsg += "The symbol you are trying to import does not exist in the module.\n"
					errMsg += "Possible reasons:\n"
					errMsg += "  - The symbol name is misspelled\n"
					errMsg += "  - The symbol is not defined in the module\n"
					errMsg += "  - The symbol is defined but not exported (if the module uses __exports__)\n\n"
					errMsg += "Available symbols in this module might include functions, variables, or classes\n"
					errMsg += "defined in the module. Check the module source code or documentation."
					return nil, core.NewException("ImportError", errMsg)
				}

				// Define the symbol in the environment with its original or aliased name
				env.Define(targetSymName, value)
			}

		default:
			errMsg := fmt.Sprintf("Invalid import specification: %v\n\n", arg)
			errMsg += "Import arguments must be one of:\n"
			errMsg += "  - A symbol (import module_name)\n"
			errMsg += "  - A string (import \"path/to/module\")\n"
			errMsg += "  - A from-import expression (import (from module import symbol1 symbol2))"
			return nil, core.NewException("ImportError", errMsg)
		}
	}

	return core.PythonicNone{}, nil
}

// isBuiltinSymbol checks if a symbol is a builtin function or constant
func isBuiltinSymbol(symbol core.LispSymbol) bool {
	_, isBuiltin := core.BuiltinFuncs[symbol]
	return isBuiltin || symbol == "None" || symbol == "True" || symbol == "False"
}
