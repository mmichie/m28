package eval

import (
	"fmt"
	"path/filepath"
	"strings"

	"github.com/mmichie/m28/core"
)

// importNameSpec represents a name to import with optional alias
type importNameSpec struct {
	name  string
	alias string // empty if no alias
}

// resolveRelativeImport resolves a relative import to an absolute module name
// level: number of dots in the import (1 for ".", 2 for "..", etc.)
// moduleName: the module name after the dots (empty for "from . import")
// ctx: context to get __package__ from
func resolveRelativeImport(moduleName string, level int, ctx *core.Context) (string, error) {
	// Get the current package from context
	packageVal, err := ctx.Lookup("__package__")
	if err != nil {
		return "", fmt.Errorf("relative import attempted but __package__ not defined in context")
	}

	var currentPackage string
	if pkgStr, ok := packageVal.(core.StringValue); ok {
		currentPackage = string(pkgStr)
	} else {
		return "", fmt.Errorf("__package__ must be a string")
	}

	// If __package__ is empty, we're at the top level and can't do relative imports
	if currentPackage == "" && level > 0 {
		return "", fmt.Errorf("attempted relative import beyond top-level package")
	}

	// Split the current package by "."
	var packageParts []string
	if currentPackage != "" {
		packageParts = strings.Split(currentPackage, ".")
	}

	// Go up (level - 1) levels
	// Level 1 means current package, level 2 means parent, etc.
	levelsUp := level - 1
	if levelsUp > len(packageParts) {
		return "", fmt.Errorf("attempted relative import beyond top-level package")
	}

	// Remove the last 'levelsUp' parts
	if levelsUp > 0 {
		packageParts = packageParts[:len(packageParts)-levelsUp]
	}

	// Build the resolved name
	var resolved string
	if len(packageParts) > 0 {
		resolved = strings.Join(packageParts, ".")
		if moduleName != "" {
			resolved += "." + moduleName
		}
	} else {
		resolved = moduleName
	}

	return resolved, nil
}

// enhancedImportForm implements advanced import forms:
// (import module-name)                    - imports module as module-name
// (import module-name as alias)           - imports module as alias
// (import module-name from [name1 name2]) - imports specific names
// (import module-name from [name as alias]) - imports specific name with alias
// (import module-name from *)             - imports all exported names
// Note: :as and :from syntax also supported but may cause issues with parser
func enhancedImportForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() < 1 {
		return nil, fmt.Errorf("import requires at least 1 argument")
	}

	// Get the module name
	var moduleName string
	switch name := args.Items()[0].(type) {
	case core.StringValue:
		moduleName = string(name)
	case core.SymbolValue:
		moduleName = string(name)
	default:
		return nil, fmt.Errorf("module name must be a string or symbol")
	}

	// Parse import options
	var alias string
	var importNames []importNameSpec
	var importAll bool

	// Default alias is the module name
	alias = filepath.Base(moduleName)
	if ext := filepath.Ext(alias); ext == ".m28" {
		alias = alias[:len(alias)-len(ext)]
	}

	// Parse options
	i := 1
	for i < args.Len() {
		if sym, ok := args.Items()[i].(core.SymbolValue); ok && (string(sym) == ":as" || string(sym) == "as") {
			// :as alias
			if i+1 >= args.Len() {
				return nil, fmt.Errorf("import :as requires an alias")
			}
			if aliasSym, ok := args.Items()[i+1].(core.SymbolValue); ok {
				alias = string(aliasSym)
			} else {
				return nil, fmt.Errorf("import alias must be a symbol")
			}
			i += 2
		} else if sym, ok := args.Items()[i].(core.SymbolValue); ok && (string(sym) == ":from" || string(sym) == "from") {
			// :from [names] or :from *
			if i+1 >= args.Len() {
				return nil, fmt.Errorf("import :from requires a list of names or *")
			}

			if sym, ok := args.Items()[i+1].(core.SymbolValue); ok && string(sym) == "*" {
				importAll = true
			} else if list, ok := args.Items()[i+1].(*core.ListValue); ok {
				// Check if this is a list-literal form
				if list.Len() > 0 {
					if sym, ok := list.Items()[0].(core.SymbolValue); ok && string(sym) == "list-literal" {
						// It's [name1 name2] syntax, extract the symbols
						// Each element can be either:
						//   - A symbol (name without alias)
						//   - A list-literal with [name, alias]
						for j := 1; j < list.Len(); j++ {
							if nameSym, ok := list.Items()[j].(core.SymbolValue); ok {
								// Plain name without alias
								importNames = append(importNames, importNameSpec{
									name:  string(nameSym),
									alias: "",
								})
							} else if pairList, ok := list.Items()[j].(*core.ListValue); ok {
								// Check if it's a [name, alias] pair
								if pairList.Len() == 3 {
									if listLitSym, ok := pairList.Items()[0].(core.SymbolValue); ok && string(listLitSym) == "list-literal" {
										if nameSym, ok := pairList.Items()[1].(core.SymbolValue); ok {
											if aliasSym, ok := pairList.Items()[2].(core.SymbolValue); ok {
												// [name, alias] pair
												importNames = append(importNames, importNameSpec{
													name:  string(nameSym),
													alias: string(aliasSym),
												})
											} else {
												return nil, fmt.Errorf("import alias must be a symbol")
											}
										} else {
											return nil, fmt.Errorf("import name must be a symbol")
										}
									} else {
										return nil, fmt.Errorf("import names must be symbols or [name, alias] pairs")
									}
								} else {
									return nil, fmt.Errorf("import names must be symbols or [name, alias] pairs")
								}
							} else {
								return nil, fmt.Errorf("import names must be symbols or [name, alias] pairs")
							}
						}
					} else {
						// It's a regular list, process normally
						for _, item := range list.Items() {
							if nameSym, ok := item.(core.SymbolValue); ok {
								importNames = append(importNames, importNameSpec{
									name:  string(nameSym),
									alias: "",
								})
							} else {
								return nil, fmt.Errorf("import names must be symbols")
							}
						}
					}
				}
			} else {
				return nil, fmt.Errorf("import :from requires a list of names or *")
			}
			i += 2
		} else if sym, ok := args.Items()[i].(core.SymbolValue); ok && (string(sym) == ":level" || string(sym) == "level") {
			// :level N (for relative imports)
			if i+1 >= args.Len() {
				return nil, fmt.Errorf("import :level requires a number")
			}
			if levelNum, ok := args.Items()[i+1].(core.NumberValue); !ok {
				return nil, fmt.Errorf("import :level must be a number")
			} else {
				// Resolve relative import
				level := int(levelNum)
				resolved, err := resolveRelativeImport(moduleName, level, ctx)
				if err != nil {
					return nil, err
				}
				moduleName = resolved
			}
			i += 2
		} else {
			return nil, fmt.Errorf("unexpected import option: %v", args.Items()[i])
		}
	}

	// Load the module using the loader directly to get a dict
	loader := core.GetModuleLoader()
	if loader == nil {
		return nil, fmt.Errorf("no module loader registered")
	}

	// Handle different import forms
	if len(importNames) > 0 {
		// Import specific names (with optional aliases)
		// Try two strategies:
		// 1. Load as submodule (for "from . import result" -> load "unittest.result")
		// 2. Load parent and extract attribute (for "from unittest.result import TestResult")
		for _, spec := range importNames {
			// Build the full submodule name
			var submoduleName string
			if moduleName != "" {
				submoduleName = moduleName + "." + spec.name
			} else {
				submoduleName = spec.name
			}

			// Try to load as a submodule first
			submodule, err := loader.LoadModule(submoduleName, ctx)
			if err == nil {
				// Successfully loaded as submodule
				targetName := spec.name
				if spec.alias != "" {
					targetName = spec.alias
				}
				ctx.Define(targetName, submodule)
				continue
			}

			// Not a submodule, try loading parent module and extracting the name
			if moduleName == "" {
				return nil, fmt.Errorf("cannot import name '%s': %w", spec.name, err)
			}

			parentModule, parentErr := loader.LoadModule(moduleName, ctx)
			if parentErr != nil {
				// If it's an ImportError, preserve it so try/except can catch it
				if _, ok := parentErr.(*core.ImportError); ok {
					return nil, parentErr
				}
				return nil, fmt.Errorf("cannot import name '%s' from '%s': %w", spec.name, moduleName, parentErr)
			}

			// Extract the name from the parent module
			val, ok := parentModule.Get(spec.name)
			if !ok {
				// Also try with "s:" prefix (symbol key)
				val, ok = parentModule.Get("s:" + spec.name)
			}
			if !ok {
				// Return an ImportError so it can be caught by try/except
				return nil, &core.ImportError{
					ModuleName: moduleName,
					Message:    fmt.Sprintf("cannot import name '%s' from module '%s'", spec.name, moduleName),
				}
			}

			// Define in context
			targetName := spec.name
			if spec.alias != "" {
				targetName = spec.alias
			}
			ctx.Define(targetName, val)
		}
		return core.NilValue{}, nil
	} else if importAll {
		// For import *, we need to load the module and import all its exports
		dictModule, err := loader.LoadModule(moduleName, ctx)
		if err != nil {
			return nil, err
		}
		// Import all exported names
		for _, key := range dictModule.Keys() {
			if val, ok := dictModule.Get(key); ok {
				// Extract the actual key name (remove prefix if any)
				keyName := key
				if strings.HasPrefix(key, "s:") {
					keyName = key[2:]
				}
				// Skip private names
				if !strings.HasPrefix(keyName, "_") {
					ctx.Define(keyName, val)
				}
			}
		}
		return dictModule, nil
	} else {
		// Import module as namespace
		dictModule, err := loader.LoadModule(moduleName, ctx)
		if err != nil {
			return nil, err
		}
		ctx.Define(alias, dictModule)
		return dictModule, nil
	}
}

// exportForm implements the export special form:
// (export name)           - exports a single name
// (export [name1 name2])  - exports multiple names
// (export :all [names])   - sets __all__ for * imports
func exportForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() < 1 {
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
	if sym, ok := args.Items()[0].(core.SymbolValue); ok && string(sym) == ":all" {
		if args.Len() != 2 {
			return nil, fmt.Errorf("export :all requires a list of names")
		}
		if list, ok := args.Items()[1].(*core.ListValue); ok {
			allNames := []string{}
			// Check if this is a list-literal form
			if list.Len() > 0 {
				if sym, ok := list.Items()[0].(core.SymbolValue); ok && string(sym) == "list-literal" {
					// It's [name1 name2] syntax, extract the symbols
					for j := 1; j < list.Len(); j++ {
						if nameSym, ok := list.Items()[j].(core.SymbolValue); ok {
							allNames = append(allNames, string(nameSym))
						} else {
							return nil, fmt.Errorf("export names must be symbols")
						}
					}
				} else {
					// It's a regular list, process normally
					for _, item := range list.Items() {
						if nameSym, ok := item.(core.SymbolValue); ok {
							allNames = append(allNames, string(nameSym))
						} else {
							return nil, fmt.Errorf("export names must be symbols")
						}
					}
				}
			}
			module.SetAll(allNames)
			return core.Nil, nil
		}
		return nil, fmt.Errorf("export :all requires a list")
	}

	// Handle single name or list of names
	var names []string

	if sym, ok := args.Items()[0].(core.SymbolValue); ok {
		// Single name
		names = []string{string(sym)}
	} else if list, ok := args.Items()[0].(*core.ListValue); ok {
		// Check if this is a list-literal form
		if list.Len() > 0 {
			if sym, ok := list.Items()[0].(core.SymbolValue); ok && string(sym) == "list-literal" {
				// It's [name1 name2] syntax, extract the symbols
				for j := 1; j < list.Len(); j++ {
					if nameSym, ok := list.Items()[j].(core.SymbolValue); ok {
						names = append(names, string(nameSym))
					} else {
						return nil, fmt.Errorf("export names must be symbols")
					}
				}
			} else {
				// It's a regular list, process normally
				for _, item := range list.Items() {
					if nameSym, ok := item.(core.SymbolValue); ok {
						names = append(names, string(nameSym))
					} else {
						return nil, fmt.Errorf("export names must be symbols")
					}
				}
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
	// Get the module loader
	loader := core.GetModuleLoader()
	if loader == nil {
		return nil, fmt.Errorf("no module loader registered")
	}

	// Use the loader to load the module
	dictModule, err := loader.LoadModule(moduleName, ctx)
	if err != nil {
		return nil, err
	}

	// Convert dict-based module to new Module type
	module := core.NewModule(moduleName, "")

	// Add all dict entries as exports
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
