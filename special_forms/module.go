package special_forms

import (
	"fmt"
	"io/ioutil"
	"path/filepath"

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
	module := core.NewPythonicDict()

	// Execute the module code
	parsedList, ok := parsed.(core.LispList)
	if !ok {
		return nil, fmt.Errorf("parsed content is not a LispList")
	}
	for _, expr := range parsedList {
		result, err := e.Eval(expr, moduleEnv)
		if err != nil {
			return nil, fmt.Errorf("error executing module code: %v", err)
		}

		// If the result is a definition, add it to the module dictionary
		if def, ok := expr.(core.LispList); ok && len(def) > 2 {
			if defSymbol, ok := def[0].(core.LispSymbol); ok && defSymbol == "def" {
				if defName, ok := def[1].(core.LispSymbol); ok {
					module.Set(defName, result)
				}
			}
		}
	}

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
			env.Define(core.LispSymbol(moduleName), module)

		case core.LispList:
			// Complex import: from module import symbol1, symbol2
			if len(importSpec) < 4 || importSpec[0] != core.LispSymbol("from") || importSpec[2] != core.LispSymbol("import") {
				return nil, fmt.Errorf("invalid import specification: %v", importSpec)
			}

			moduleName, ok := importSpec[1].(core.LispSymbol)
			if !ok {
				return nil, fmt.Errorf("module name must be a symbol")
			}

			module, err := globalRegistry.LoadModule(string(moduleName), e)
			if err != nil {
				return nil, fmt.Errorf("failed to import module %s: %v", moduleName, err)
			}

			for _, symbol := range importSpec[3:] {
				symName, ok := symbol.(core.LispSymbol)
				if !ok {
					return nil, fmt.Errorf("imported symbol must be a symbol")
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

	return nil, nil
}

// Helper function to resolve module paths
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
