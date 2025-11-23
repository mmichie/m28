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
	nameVal := unwrapLocated(args.Items()[0])
	switch name := nameVal.(type) {
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
		argI := unwrapLocated(args.Items()[i])
		if sym, ok := argI.(core.SymbolValue); ok && (string(sym) == ":as" || string(sym) == "as") {
			// :as alias
			if i+1 >= args.Len() {
				return nil, fmt.Errorf("import :as requires an alias")
			}
			argI1 := unwrapLocated(args.Items()[i+1])
			if aliasSym, ok := argI1.(core.SymbolValue); ok {
				alias = string(aliasSym)
			} else {
				return nil, fmt.Errorf("import alias must be a symbol")
			}
			i += 2
		} else if sym, ok := argI.(core.SymbolValue); ok && (string(sym) == ":from" || string(sym) == "from") {
			// :from [names] or :from *
			if i+1 >= args.Len() {
				return nil, fmt.Errorf("import :from requires a list of names or *")
			}

			argI1 := unwrapLocated(args.Items()[i+1])
			if sym, ok := argI1.(core.SymbolValue); ok && string(sym) == "*" {
				importAll = true
			} else if list, ok := argI1.(*core.ListValue); ok {
				// Check if this is a list-literal form
				if list.Len() > 0 {
					firstListElem := unwrapLocated(list.Items()[0])
					if sym, ok := firstListElem.(core.SymbolValue); ok && string(sym) == "list-literal" {
						// It's [name1 name2] syntax, extract the symbols
						// Each element can be either:
						//   - A symbol (name without alias)
						//   - A list-literal with [name, alias]
						for j := 1; j < list.Len(); j++ {
							listItemJ := unwrapLocated(list.Items()[j])
							if nameSym, ok := listItemJ.(core.SymbolValue); ok {
								// Plain name without alias
								importNames = append(importNames, importNameSpec{
									name:  string(nameSym),
									alias: "",
								})
							} else if pairList, ok := listItemJ.(*core.ListValue); ok {
								// Check if it's a [name, alias] pair
								if pairList.Len() == 3 {
									pairItem0 := unwrapLocated(pairList.Items()[0])
									if listLitSym, ok := pairItem0.(core.SymbolValue); ok && string(listLitSym) == "list-literal" {
										pairItem1 := unwrapLocated(pairList.Items()[1])
										if nameSym, ok := pairItem1.(core.SymbolValue); ok {
											pairItem2 := unwrapLocated(pairList.Items()[2])
											if aliasSym, ok := pairItem2.(core.SymbolValue); ok {
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
		// Python's import semantics for "from X import Y":
		// 1. First check if Y is an attribute of module X
		// 2. Only if not found, try to load Y as a submodule X.Y

		// First, load the parent module (if specified)
		var parentModule *core.DictValue
		var parentErr error
		if moduleName != "" {
			parentModule, parentErr = loader.LoadModule(moduleName, ctx)
			if parentErr != nil {
				// If it's already an ImportError or ModuleNotFoundError, preserve it so try/except can catch it
				if _, ok := parentErr.(*core.ImportError); ok {
					return nil, parentErr
				}
				if _, ok := parentErr.(*core.ModuleNotFoundError); ok {
					return nil, parentErr
				}
				// Wrap other errors as ImportError so they can be caught by except ImportError
				return nil, &core.ImportError{
					ModuleName: moduleName,
					Message:    fmt.Sprintf("cannot import from '%s': %v", moduleName, parentErr),
				}
			}
		}

		for _, spec := range importNames {
			// Strategy 1: Try to get as an attribute from parent module (most common case)
			if parentModule != nil {
				// Try to extract the name from the parent module
				val, ok := parentModule.Get(spec.name)
				if !ok {
					// Also try with "s:" prefix (symbol key format)
					val, ok = parentModule.Get("s:" + spec.name)
				}
				if ok {
					// Found as attribute - define in context
					targetName := spec.name
					if spec.alias != "" {
						targetName = spec.alias
					}
					ctx.Define(targetName, val)
					continue
				}
			}

			// Strategy 2: Try to load as a submodule (for package imports)
			// Example: "from . import result" -> load "unittest.result" as a file
			var submoduleName string
			if moduleName != "" {
				submoduleName = moduleName + "." + spec.name
			} else {
				submoduleName = spec.name
			}

			submodule, err := loader.LoadModule(submoduleName, ctx)
			if err == nil {
				// Successfully loaded as submodule
				targetName := spec.name
				if spec.alias != "" {
					targetName = spec.alias
				}
				// Wrap as Module object for attribute access
				moduleObj := wrapDictAsModule(submoduleName, submodule)
				ctx.Define(targetName, moduleObj)
				continue
			}

			// Both strategies failed - return error
			if moduleName == "" {
				// Module not found - use ModuleNotFoundError (Python 3 subclass of ImportError)
				return nil, core.NewModuleNotFoundError(spec.name)
			}
			// Module found but specific name cannot be imported - use ImportError
			return nil, &core.ImportError{
				ModuleName: moduleName,
				Message:    fmt.Sprintf("cannot import name '%s' from '%s'", spec.name, moduleName),
			}
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

		// Python behavior: for "from .module import *", the module object is also made available
		// For example, "from .events import *" makes both the exported names AND the events module available
		// Extract the base module name (last component after the last dot)
		baseModuleName := moduleName
		if lastDot := strings.LastIndex(moduleName, "."); lastDot != -1 {
			baseModuleName = moduleName[lastDot+1:]
		}
		// Make the module object available under its base name
		moduleObj := wrapDictAsModule(moduleName, dictModule)
		ctx.Define(baseModuleName, moduleObj)

		return dictModule, nil
	} else {
		// Import module as namespace
		dictModule, err := loader.LoadModule(moduleName, ctx)
		if err != nil {
			return nil, err
		}

		// Wrap the dict as a Module object to support attribute access
		moduleObj := wrapDictAsModule(moduleName, dictModule)

		// Handle dotted imports: for "import a.b.c", we need to:
		// 1. Define 'a' in the current namespace (or create a stub if it doesn't exist)
		// 2. Ensure a.b and a.b.c are accessible as attributes
		// Exception: if an explicit alias is provided (import a.b.c as x), use the alias instead
		if strings.Contains(moduleName, ".") {
			// Check if user provided an explicit alias
			hasExplicitAlias := (alias != filepath.Base(moduleName))

			if hasExplicitAlias {
				// User provided explicit alias like "import a.b.c as x"
				// Just define the alias and return
				ctx.Define(alias, moduleObj)
				return dictModule, nil
			}

			// No explicit alias, use Python's dotted import behavior
			parts := strings.Split(moduleName, ".")
			topLevelName := parts[0]

			// Check if top-level module is already imported
			topLevelVal, err := ctx.Lookup(topLevelName)
			var topLevelModule *core.Module

			if err != nil {
				// Top-level not imported yet
				// Try to load it, but if it fails, create a stub package module
				topLevelDict, err := loader.LoadModule(topLevelName, ctx)
				if err != nil {
					// Failed to load top-level module, create a stub package
					core.DebugLog("[DEBUG] Failed to load top-level module '%s', creating stub: %v\n", topLevelName, err)
					topLevelModule = core.NewModule(topLevelName, "")
					ctx.Define(topLevelName, topLevelModule)
				} else {
					topLevelModule = wrapDictAsModule(topLevelName, topLevelDict)
					ctx.Define(topLevelName, topLevelModule)
				}
			} else {
				// Top-level already imported, verify it's a module
				var ok bool
				topLevelModule, ok = topLevelVal.(*core.Module)
				if !ok {
					return nil, fmt.Errorf("name '%s' is already defined as non-module", topLevelName)
				}
			}

			// Build the chain: for "a.b.c", ensure a.b exists and has c as attribute
			currentModule := topLevelModule
			for i := 1; i < len(parts); i++ {
				partialName := strings.Join(parts[:i+1], ".")
				part := parts[i]

				// Check if this submodule is already an attribute
				if subVal, exists := currentModule.GetAttr(part); exists {
					// Already exists, use it
					if subModule, ok := subVal.(*core.Module); ok {
						currentModule = subModule
						continue
					}
				}

				// Load the submodule
				subDict, err := loader.LoadModule(partialName, ctx)
				if err != nil {
					return nil, fmt.Errorf("failed to load submodule '%s': %w", partialName, err)
				}
				subModule := wrapDictAsModule(partialName, subDict)

				// Set it as an attribute of the parent
				currentModule.SetAttr(part, subModule)
				currentModule = subModule
			}

			return dictModule, nil
		}

		// Non-dotted import: just define the module with the alias
		ctx.Define(alias, moduleObj)
		return dictModule, nil
	}
}

// wrapDictAsModule wraps a DictValue as a Module object
// This allows attribute access like `module.function` to work properly
func wrapDictAsModule(name string, dict *core.DictValue) *core.Module {
	module := core.NewModule(name, "")

	// Keep a reference to the dict for dynamic lookups
	// This allows Python code to use setattr() on the module and have
	// those updates be visible when accessing module attributes
	module.Dict = dict

	// Copy all items from dict to module exports
	for _, key := range dict.Keys() {
		if val, ok := dict.Get(key); ok {
			// Remove prefix if present
			cleanKey := key
			if strings.HasPrefix(key, "s:") {
				cleanKey = key[2:]
			}
			module.Export(cleanKey, val)
		}
	}

	return module
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
