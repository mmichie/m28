package special_forms

import (
	"fmt"
	"os"
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
		return nil, fmt.Errorf("module not found: %s (searched in: %s)", 
			name, strings.Join(registry.GetSearchPaths(), ", "))
	}

	// Track dependencies for this module
	dependencies := []string{}

	// Read the module file
	content, err := os.ReadFile(modulePath)
	if err != nil {
		return nil, fmt.Errorf("failed to read module file %s: %v", modulePath, err)
	}

	// Parse the module content
	p := parser.NewParser()
	parsed, err := p.Parse(string(content))
	if err != nil {
		return nil, fmt.Errorf("syntax error in module %s: %v", name, err)
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
		return nil, fmt.Errorf("parsed content is not valid code in module %s", name)
	}

	// First pass: execute all expressions in the module file
	for _, expr := range parsedList {
		_, err := e.Eval(expr, moduleEnv)
		if err != nil {
			return nil, fmt.Errorf("runtime error in module %s: %v", name, err)
		}
	}

	// Second pass: collect all defined symbols from the module's environment
	// Debug - print all symbols in the module's environment
	fmt.Println("DEBUG: Module environment symbols:")
	moduleEnv.ForEachSymbol(func(symbol core.LispSymbol, value core.LispValue) {
		// Only add non-builtin symbols to the module dictionary
		if !isBuiltinSymbol(symbol) {
			fmt.Printf("DEBUG: Adding symbol '%s' to module, type: %T\n", symbol, value)
			// Add to module dictionary - only user-defined symbols
			module.Set(symbol, value)
		}
	})

	// Check if the module defined an __exports__ list
	fmt.Printf("DEBUG: Checking for __exports__ in module\n")
	exportsVal, hasExports := module.Get("__exports__")
	if hasExports {
		fmt.Printf("DEBUG: Module has __exports__: %v (type: %T)\n", exportsVal, exportsVal)
		// If __exports__ is defined, only export the symbols listed in it
		if exportsList, ok := exportsVal.(core.LispList); ok {
			fmt.Printf("DEBUG: Exports list is valid with %d items\n", len(exportsList))
			
			// CRITICAL BUGFIX: We need to operate directly in the original module
			// rather than creating a new filtered module, to ensure all properties
			// are correctly preserved for dot notation and dictionary operations
			
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
					fmt.Printf("DEBUG: Skipping non-symbol export: %T %v\n", symbolVal, symbolVal)
					continue // Skip non-symbol/string items
				}
				
				if _, ok := module.Get(symbolName); ok {
					fmt.Printf("DEBUG: Adding '%s' to keys to keep\n", symbolName)
					keysToKeep[symbolName] = true
				} else {
					fmt.Printf("DEBUG: Symbol '%s' in exports but not found in module\n", symbolName)
				}
			}
			
			// DEBUG: Show all keys in the original module
			items, _ := module.CallMethod("items", []core.LispValue{})
			if itemsList, ok := items.(core.LispList); ok {
				fmt.Printf("DEBUG: Original module has %d keys\n", len(itemsList))
			}
			
			// Create a list of user-defined keys to remove
			var keysToRemove []string
			keysList, err := module.CallMethod("keys", []core.LispValue{})
			if err != nil {
				fmt.Printf("DEBUG: Error getting keys: %v\n", err)
			} else {
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
				fmt.Printf("DEBUG: Removing unexported key '%s' from module\n", key)
				_, err := module.CallMethod("delete", []core.LispValue{key})
				if err != nil {
					fmt.Printf("DEBUG: Error removing key '%s': %v\n", key, err)
				}
			}
		}
	}

	// Debug - print the final module content
	fmt.Println("DEBUG: Final module content:")
	// Get the keys from the dictionary
	items, _ := module.CallMethod("items", []core.LispValue{})
	if itemsList, ok := items.(core.LispList); ok {
		for _, item := range itemsList {
			if pair, ok := item.(core.LispList); ok && len(pair) == 2 {
				key := pair[0]
				value := pair[1]
				fmt.Printf("DEBUG: Module key '%v', value type: %T\n", key, value)
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

	// Debug - print the wrapper module content
	fmt.Println("DEBUG: Module after LoadModule wrapper:")
	// Get the keys from the dictionary
	items, _ := module.CallMethod("items", []core.LispValue{})
	if itemsList, ok := items.(core.LispList); ok {
		for _, item := range itemsList {
			if pair, ok := item.(core.LispList); ok && len(pair) == 2 {
				key := pair[0]
				value := pair[1]
				fmt.Printf("DEBUG: Module wrapper key '%v', value type: %T\n", key, value)
			}
		}
	}

	return module, nil
}

func EvalImport(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("import requires at least one argument")
	}

	// Get the module loader
	moduleLoader := core.GetModuleLoader()
	if moduleLoader == nil {
		return nil, fmt.Errorf("no module loader registered")
	}

	for _, arg := range args {
		switch importSpec := arg.(type) {
		case core.LispSymbol:
			// Simple import: import module
			moduleName := string(importSpec)
			module, err := moduleLoader.LoadModule(moduleName, e)
			if err != nil {
				return nil, fmt.Errorf("failed to import module %s: %v", moduleName, err)
			}

			// Determine module name for environment binding
			// Use the last part of the path for module name if there's a path separator
			moduleNameParts := strings.Split(moduleName, "/")
			moduleBaseName := moduleNameParts[len(moduleNameParts)-1]

			// Remove .m28 extension if present
			moduleBaseName = strings.TrimSuffix(moduleBaseName, ".m28")

			// Debug - print info before defining
			fmt.Printf("DEBUG: Defining symbol module '%s' in environment\n", moduleBaseName)
			
			// Define the module in the environment
			env.Define(core.LispSymbol(moduleBaseName), module)
			
			// Create a dot handler for the module to allow attribute access
			fmt.Printf("DEBUG: Creating dot handler for module '%s'\n", moduleBaseName)
			env.Define(core.LispSymbol(moduleBaseName+".__dot__"), core.BuiltinFunc(func(args []core.LispValue, env core.Environment) (core.LispValue, error) {
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
				
				fmt.Printf("DEBUG: Dot handler for %s accessing property: %s\n", moduleBaseName, propName)
				
				// Get the property from the module
				value, ok := module.Get(propName)
				if !ok {
					return nil, core.ErrDotModulePropertyf(moduleBaseName, propName)
				}
				
				fmt.Printf("DEBUG: Found property %s in module %s, type: %T\n", propName, moduleBaseName, value)
				
				// Check if additional arguments were provided (method call)
				if len(args) > 1 {
					fmt.Printf("DEBUG: Method call detected with %d additional arguments\n", len(args)-1)
					
					// Extract the method arguments (skip the property name)
					methodArgs := args[1:]
					
					// Handle different callable types
					switch fn := value.(type) {
					case core.BuiltinFunc:
						// Call the builtin function directly
						fmt.Printf("DEBUG: Calling builtin function %s\n", propName)
						return fn(methodArgs, env)
					case *core.Lambda:
						// Use evaluator to apply the lambda
						fmt.Printf("DEBUG: Calling lambda function %s\n", propName)
						// We need the evaluator to apply the lambda
						if moduleLoader := core.GetModuleLoader(); moduleLoader != nil {
							evaluator := moduleLoader.GetEvaluator()
							return evaluator.Apply(fn, methodArgs, env)
						}
						return nil, fmt.Errorf(core.ErrDotEvaluatorMissing, propName)
					case core.DotAccessible:
						// Check if the object has a __call__ method
						if fn.HasMethod("__call__") {
							fmt.Printf("DEBUG: Calling __call__ method on %s\n", propName)
							return fn.CallMethod("__call__", methodArgs)
						}
						return nil, core.ErrDotNoMethodf(propName)
					default:
						return nil, core.ErrDotNotCallablef(propName, value)
					}
				}
				
				return value, nil
			}))
			
			// Debug - verify it was added
			if newValue, found := env.Get(core.LispSymbol(moduleBaseName)); found {
				fmt.Printf("DEBUG: Symbol module '%s' defined as type: %T\n", moduleBaseName, newValue)
				
				// Check module content
				if dict, ok := newValue.(*core.PythonicDict); ok {
					items, _ := dict.CallMethod("items", []core.LispValue{})
					if itemsList, ok := items.(core.LispList); ok {
						fmt.Printf("DEBUG: Symbol module '%s' has %d items\n", moduleBaseName, len(itemsList))
					}
				}
				
				// Verify dot handler was registered
				if handler, ok := env.Get(core.LispSymbol(moduleBaseName + ".__dot__")); ok {
					fmt.Printf("DEBUG: Dot handler for '%s' registered successfully: %T\n", moduleBaseName, handler)
				} else {
					fmt.Printf("DEBUG: Failed to register dot handler for '%s'\n", moduleBaseName)
				}
			} else {
				fmt.Printf("DEBUG: Failed to define symbol module '%s' in environment\n", moduleBaseName)
			}

		case string:
			// String literal module path: import "path/to/module"
			modulePath := importSpec
			module, err := moduleLoader.LoadModule(modulePath, e)
			if err != nil {
				return nil, fmt.Errorf("failed to import module %s: %v", modulePath, err)
			}

			// Determine module name for environment binding
			// Use the last part of the path for module name if there's a path separator
			moduleNameParts := strings.Split(modulePath, "/")
			moduleBaseName := moduleNameParts[len(moduleNameParts)-1]

			// Remove .m28 extension if present
			moduleBaseName = strings.TrimSuffix(moduleBaseName, ".m28")

			// Debug - print info before defining
			fmt.Printf("DEBUG: Defining string module '%s' in environment\n", moduleBaseName)
			
			// Define the module in the environment
			env.Define(core.LispSymbol(moduleBaseName), module)
			
			// Create a dot handler for the module to allow attribute access
			fmt.Printf("DEBUG: Creating dot handler for module '%s'\n", moduleBaseName)
			env.Define(core.LispSymbol(moduleBaseName+".__dot__"), core.BuiltinFunc(func(args []core.LispValue, env core.Environment) (core.LispValue, error) {
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
				
				fmt.Printf("DEBUG: Dot handler for %s accessing property: %s\n", moduleBaseName, propName)
				
				// Get the property from the module
				value, ok := module.Get(propName)
				if !ok {
					return nil, core.ErrDotModulePropertyf(moduleBaseName, propName)
				}
				
				fmt.Printf("DEBUG: Found property %s in module %s, type: %T\n", propName, moduleBaseName, value)
				
				// Check if additional arguments were provided (method call)
				if len(args) > 1 {
					fmt.Printf("DEBUG: Method call detected with %d additional arguments\n", len(args)-1)
					
					// Extract the method arguments (skip the property name)
					methodArgs := args[1:]
					
					// Handle different callable types
					switch fn := value.(type) {
					case core.BuiltinFunc:
						// Call the builtin function directly
						fmt.Printf("DEBUG: Calling builtin function %s\n", propName)
						return fn(methodArgs, env)
					case *core.Lambda:
						// Use evaluator to apply the lambda
						fmt.Printf("DEBUG: Calling lambda function %s\n", propName)
						// We need the evaluator to apply the lambda
						if moduleLoader := core.GetModuleLoader(); moduleLoader != nil {
							evaluator := moduleLoader.GetEvaluator()
							return evaluator.Apply(fn, methodArgs, env)
						}
						return nil, fmt.Errorf(core.ErrDotEvaluatorMissing, propName)
					case core.DotAccessible:
						// Check if the object has a __call__ method
						if fn.HasMethod("__call__") {
							fmt.Printf("DEBUG: Calling __call__ method on %s\n", propName)
							return fn.CallMethod("__call__", methodArgs)
						}
						return nil, core.ErrDotNoMethodf(propName)
					default:
						return nil, core.ErrDotNotCallablef(propName, value)
					}
				}
				
				return value, nil
			}))
			
			// Debug - verify it was added
			if newValue, found := env.Get(core.LispSymbol(moduleBaseName)); found {
				fmt.Printf("DEBUG: String module '%s' defined as type: %T\n", moduleBaseName, newValue)
				
				// Check module content
				if dict, ok := newValue.(*core.PythonicDict); ok {
					items, _ := dict.CallMethod("items", []core.LispValue{})
					if itemsList, ok := items.(core.LispList); ok {
						fmt.Printf("DEBUG: String module '%s' has %d items\n", moduleBaseName, len(itemsList))
					}
				}
				
				// Verify dot handler was registered
				if handler, ok := env.Get(core.LispSymbol(moduleBaseName + ".__dot__")); ok {
					fmt.Printf("DEBUG: Dot handler for '%s' registered successfully: %T\n", moduleBaseName, handler)
				} else {
					fmt.Printf("DEBUG: Failed to register dot handler for '%s'\n", moduleBaseName)
				}
			} else {
				fmt.Printf("DEBUG: Failed to define string module '%s' in environment\n", moduleBaseName)
			}

		case core.LispList:
			// Complex import: from module import symbol1, symbol2
			if len(importSpec) < 4 || importSpec[0] != core.LispSymbol("from") || importSpec[2] != core.LispSymbol("import") {
				return nil, fmt.Errorf("invalid import specification: %v", importSpec)
			}

			var moduleName string
			switch mn := importSpec[1].(type) {
			case core.LispSymbol:
				moduleName = string(mn)
			case string:
				moduleName = mn
			default:
				return nil, fmt.Errorf("module name must be a symbol or string")
			}

			module, err := moduleLoader.LoadModule(moduleName, e)
			if err != nil {
				return nil, fmt.Errorf("failed to import module %s: %v", moduleName, err)
			}

			for _, symbol := range importSpec[3:] {
				var symName core.LispSymbol
				switch s := symbol.(type) {
				case core.LispSymbol:
					symName = s
				case string:
					symName = core.LispSymbol(s)
				default:
					return nil, fmt.Errorf("imported symbol must be a symbol or string")
				}

				value, ok := module.Get(symName)
				if !ok {
					return nil, fmt.Errorf("symbol %s not found in module %s", symName, moduleName)
				}

				env.Define(symName, value)
			}

		default:
			return nil, fmt.Errorf("invalid import specification: %v", arg)
		}
	}

	return core.PythonicNone{}, nil
}

// isBuiltinSymbol checks if a symbol is a builtin function or constant
func isBuiltinSymbol(symbol core.LispSymbol) bool {
	_, isBuiltin := core.BuiltinFuncs[symbol]
	return isBuiltin || symbol == "None" || symbol == "True" || symbol == "False"
}