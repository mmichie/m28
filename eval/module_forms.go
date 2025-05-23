package eval

import (
	"fmt"
	"path/filepath"

	"github.com/mmichie/m28/core"
)

// enhancedImportForm implements advanced import forms:
// (import module-name)                    - imports module as module-name
// (import module-name :as alias)          - imports module as alias
// (import module-name :from [name1 name2]) - imports specific names
// (import module-name :from *)            - imports all exported names
func enhancedImportForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("import requires at least 1 argument")
	}

	// Get the module name
	var moduleName string
	switch name := args[0].(type) {
	case core.StringValue:
		moduleName = string(name)
	case core.SymbolValue:
		moduleName = string(name)
	default:
		return nil, fmt.Errorf("module name must be a string or symbol")
	}

	// Parse import options
	var alias string
	var importNames []string
	var importAll bool

	// Default alias is the module name
	alias = filepath.Base(moduleName)
	if ext := filepath.Ext(alias); ext == ".m28" {
		alias = alias[:len(alias)-len(ext)]
	}

	// Parse options
	i := 1
	for i < len(args) {
		if sym, ok := args[i].(core.SymbolValue); ok && string(sym) == ":as" {
			// :as alias
			if i+1 >= len(args) {
				return nil, fmt.Errorf("import :as requires an alias")
			}
			if aliasSym, ok := args[i+1].(core.SymbolValue); ok {
				alias = string(aliasSym)
			} else {
				return nil, fmt.Errorf("import alias must be a symbol")
			}
			i += 2
		} else if sym, ok := args[i].(core.SymbolValue); ok && string(sym) == ":from" {
			// :from [names] or :from *
			if i+1 >= len(args) {
				return nil, fmt.Errorf("import :from requires a list of names or *")
			}
			
			if sym, ok := args[i+1].(core.SymbolValue); ok && string(sym) == "*" {
				importAll = true
			} else if list, ok := args[i+1].(core.ListValue); ok {
				for _, item := range list {
					if nameSym, ok := item.(core.SymbolValue); ok {
						importNames = append(importNames, string(nameSym))
					} else {
						return nil, fmt.Errorf("import names must be symbols")
					}
				}
			} else {
				return nil, fmt.Errorf("import :from requires a list of names or *")
			}
			i += 2
		} else {
			return nil, fmt.Errorf("unexpected import option: %v", args[i])
		}
	}

	// Load the module
	module, err := loadModule(moduleName, ctx)
	if err != nil {
		return nil, err
	}

	// Handle different import forms
	if len(importNames) > 0 {
		// Import specific names
		for _, name := range importNames {
			if val, ok := module.GetExport(name); ok {
				ctx.Define(name, val)
			} else {
				return nil, fmt.Errorf("module '%s' has no export '%s'", moduleName, name)
			}
		}
	} else if importAll {
		// Import all exported names
		for _, name := range module.GetAllExports() {
			if val, ok := module.GetExport(name); ok {
				ctx.Define(name, val)
			}
		}
	} else {
		// Import module as namespace
		ctx.Define(alias, module)
	}

	return module, nil
}

// exportForm implements the export special form:
// (export name)           - exports a single name
// (export [name1 name2])  - exports multiple names
// (export :all [names])   - sets __all__ for * imports
func exportForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("export requires at least 1 argument")
	}

	// Get the current module from context
	moduleVal, err := ctx.Lookup("__module__")
	if err != nil {
		return nil, fmt.Errorf("export can only be used within a module")
	}
	
	module, ok := moduleVal.(*core.Module)
	if !ok {
		return nil, fmt.Errorf("__module__ is not a module object")
	}

	// Handle :all directive
	if sym, ok := args[0].(core.SymbolValue); ok && string(sym) == ":all" {
		if len(args) != 2 {
			return nil, fmt.Errorf("export :all requires a list of names")
		}
		if list, ok := args[1].(core.ListValue); ok {
			allNames := []string{}
			for _, item := range list {
				if nameSym, ok := item.(core.SymbolValue); ok {
					allNames = append(allNames, string(nameSym))
				} else {
					return nil, fmt.Errorf("export names must be symbols")
				}
			}
			module.SetAll(allNames)
			return core.Nil, nil
		}
		return nil, fmt.Errorf("export :all requires a list")
	}

	// Handle single name or list of names
	var names []string
	
	if sym, ok := args[0].(core.SymbolValue); ok {
		// Single name
		names = []string{string(sym)}
	} else if list, ok := args[0].(core.ListValue); ok {
		// List of names
		for _, item := range list {
			if nameSym, ok := item.(core.SymbolValue); ok {
				names = append(names, string(nameSym))
			} else {
				return nil, fmt.Errorf("export names must be symbols")
			}
		}
	} else {
		return nil, fmt.Errorf("export requires a symbol or list of symbols")
	}

	// Export each name
	for _, name := range names {
		val, err := ctx.Lookup(name)
		if err != nil {
			return nil, fmt.Errorf("cannot export '%s': %v", name, err)
		}
		module.Export(name, val)
	}

	return core.Nil, nil
}

// loadModule loads a module and returns a Module object
func loadModule(moduleName string, ctx *core.Context) (*core.Module, error) {
	// Get the module registry
	registry := core.GetModuleRegistry()
	
	// Check if module is already loaded
	if moduleDict, found := registry.GetModule(moduleName); found {
		// Convert old dict-based module to new Module type
		// For now, create a simple module wrapper
		module := core.NewModule(moduleName, "")
		
		// Add all dict entries as exports
		for _, key := range moduleDict.Keys() {
			if val, ok := moduleDict.Get(key); ok {
				module.Export(key, val)
			}
		}
		
		return module, nil
	}

	// Resolve the module path
	path, err := registry.ResolveModulePath(moduleName)
	if err != nil {
		return nil, fmt.Errorf("failed to resolve module path: %v", err)
	}

	// Create a new module
	module := core.NewModule(moduleName, path)
	
	// Create module context
	moduleCtx := core.NewContext(ctx.Global)
	moduleCtx.Define("__name__", core.StringValue(moduleName))
	moduleCtx.Define("__file__", core.StringValue(path))
	moduleCtx.Define("__module__", module)
	
	// Parse the module content
	// This requires access to the parser, which we'll need to inject
	loader := core.GetModuleLoader()
	if loader == nil {
		return nil, fmt.Errorf("no module loader registered")
	}

	// Use the existing loader to parse and evaluate
	// For now, we'll use the old mechanism and convert
	dictModule, err := loader.LoadModule(moduleName, ctx)
	if err != nil {
		return nil, err
	}

	// Convert dict module to new Module type
	for _, key := range dictModule.Keys() {
		if val, ok := dictModule.Get(key); ok {
			module.Export(key, val)
		}
	}

	return module, nil
}

// RegisterModuleForms registers the enhanced import/export forms
func RegisterModuleForms() {
	// Override the basic import with our enhanced version
	specialForms["import"] = enhancedImportForm
	RegisterSpecialForm("export", exportForm)
}