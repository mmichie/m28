package special_forms

import (
	"fmt"
	"io/ioutil"
	"path/filepath"
	"strings"

	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/env"
	"github.com/mmichie/m28/parser"
)

// ModuleRegistry keeps track of loaded modules
type ModuleRegistry struct {
	modules map[string]*core.PythonicDict
}

// NewModuleRegistry creates a new ModuleRegistry
func NewModuleRegistry() *ModuleRegistry {
	return &ModuleRegistry{
		modules: make(map[string]*core.PythonicDict),
	}
}

// Global module registry
var globalRegistry = NewModuleRegistry()

// LoadModule loads a module and returns its environment
func (r *ModuleRegistry) LoadModule(name string, e core.Evaluator) (*core.PythonicDict, error) {
	// Check if the module is already loaded
	if module, ok := r.modules[name]; ok {
		return module, nil
	}

	// Resolve the module file path
	modulePath, err := resolveModulePath(name)
	if err != nil {
		return nil, fmt.Errorf("failed to resolve module path: %v", err)
	}

	// Read the module file
	content, err := ioutil.ReadFile(modulePath)
	if err != nil {
		return nil, fmt.Errorf("failed to read module file: %v", err)
	}

	// Parse the module content
	p := parser.NewParser()
	parsed, err := p.Parse(string(content))
	if err != nil {
		return nil, fmt.Errorf("failed to parse module content: %v", err)
	}

	// Create a new environment for the module
	moduleEnv := env.NewEnvironment(nil)
	moduleEnv.SetupBuiltins() // Set up builtin functions in the environment

	// Register special forms in the module environment
	RegisterSpecialForms(moduleEnv)

	module := core.NewPythonicDict()

	// Execute the module code
	parsedList, ok := parsed.(core.LispList)
	if !ok {
		return nil, fmt.Errorf("parsed content is not a LispList")
	}

	// First pass: execute all expressions in the module file
	for _, expr := range parsedList {
		_, err := e.Eval(expr, moduleEnv)
		if err != nil {
			return nil, fmt.Errorf("error executing module code: %v", err)
		}
	}

	// Second pass: collect all defined symbols from the module's environment
	// This will capture both def and = assignments
	// Use moduleEnv directly since it's already an *env.Environment
	moduleEnv.ForEachSymbol(func(symbol core.LispSymbol, value core.LispValue) {
		// Only add non-builtin symbols to the module dictionary
		// This ensures we don't pollute the module with all the builtins
		if !isBuiltinSymbol(symbol) {
			// Add to module dictionary - only user-defined symbols
			module.Set(symbol, value)
		}
	})

	// Store the loaded module
	r.modules[name] = module

	return module, nil
}

func EvalImport(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("import requires at least one argument")
	}

	for _, arg := range args {
		switch importSpec := arg.(type) {
		case core.LispSymbol:
			// Simple import: import module
			moduleName := string(importSpec)
			module, err := globalRegistry.LoadModule(moduleName, e)
			if err != nil {
				return nil, fmt.Errorf("failed to import module %s: %v", moduleName, err)
			}

			// Determine module name for environment binding
			// Use the last part of the path for module name if there's a path separator
			moduleNameParts := strings.Split(moduleName, "/")
			moduleBaseName := moduleNameParts[len(moduleNameParts)-1]

			// Remove .m28 extension if present
			moduleBaseName = strings.TrimSuffix(moduleBaseName, ".m28")

			// Define the module in the environment
			env.Define(core.LispSymbol(moduleBaseName), module)

		case string:
			// String literal module path: import "path/to/module"
			modulePath := importSpec
			module, err := globalRegistry.LoadModule(modulePath, e)
			if err != nil {
				return nil, fmt.Errorf("failed to import module %s: %v", modulePath, err)
			}

			// Determine module name for environment binding
			// Use the last part of the path for module name if there's a path separator
			moduleNameParts := strings.Split(modulePath, "/")
			moduleBaseName := moduleNameParts[len(moduleNameParts)-1]

			// Remove .m28 extension if present
			moduleBaseName = strings.TrimSuffix(moduleBaseName, ".m28")

			// Define the module in the environment
			env.Define(core.LispSymbol(moduleBaseName), module)

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

			module, err := globalRegistry.LoadModule(moduleName, e)
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

// Helper function to resolve module paths
// isBuiltinSymbol checks if a symbol is a builtin function or constant
func isBuiltinSymbol(symbol core.LispSymbol) bool {
	_, isBuiltin := core.BuiltinFuncs[symbol]
	return isBuiltin || symbol == "None" || symbol == "True" || symbol == "False"
}

func resolveModulePath(name string) (string, error) {
	// Check if the name is a path itself (contains / or .)
	if filepath.Ext(name) == ".m28" {
		if _, err := ioutil.ReadFile(name); err == nil {
			return name, nil
		}
	}

	// Look for .m28 files in the current directory and predefined module paths
	paths := []string{
		".",
		"./tests",
		"./modules",
		"./examples",
		"/usr/local/lib/m28/modules",
		"/usr/lib/m28/modules",
	}

	for _, path := range paths {
		fullPath := filepath.Join(path, name+".m28")
		if _, err := ioutil.ReadFile(fullPath); err == nil {
			return fullPath, nil
		}
	}

	return "", fmt.Errorf("module %s not found", name)
}
