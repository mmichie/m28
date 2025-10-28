// Package modules provides Go-implemented standard library modules for M28.
package modules

import (
	"sync"

	"github.com/mmichie/m28/core"
)

// ModuleInitializer is a function that creates and returns a module dictionary
type ModuleInitializer func() *core.DictValue

// builtinModules maps module names to their initializers
// Following CPython philosophy: ONLY stub C extension modules (starting with _ or built-in)
// Pure Python stdlib modules (.py files) should run directly without stubs
var builtinModules = map[string]ModuleInitializer{
	// Built-in C extension modules
	"posix":        InitPosixModule,       // C extension for Unix - used by Python's os.py
	"sys":          InitSysModule,         // Built-in module
	"io":           InitIOModule,          // Built-in module
	"time":         InitTimeModule,        // Built-in C module
	"math":         InitMathModule,        // Built-in C module
	"errno":        InitErrnoModule,       // Built-in C module
	"itertools":    InitItertoolsModule,   // Built-in C module
	"_collections": InitCollectionsModule, // C extension module for collections.py
	"_weakref":     InitWeakrefModule,     // C extension module for weakref.py
	"_thread":      InitThreadModule,      // C extension module for threading.py
	"_functools":   Init_FunctoolsModule,  // C extension module for functools.py
	"_signal":      Init_SignalModule,     // C extension module for signal.py
	"_string":      Init_StringModule,     // C extension module for string.py
	"_codecs":      InitCodecsModule,      // C extension module for codecs.py
	"enum":         InitEnumModule,        // Stub for enum.py (provides basic enum support)
	"_sre":         Init_SREModule,        // C extension module for re (regex engine)
	"re._parser":   Init_REParserModule,   // C extension module for re._parser
	"_tokenize":    Init_TokenizeModule,   // C extension module for tokenize.py
}

// moduleCache stores initialized modules to avoid re-initialization
var moduleCache = make(map[string]*core.DictValue)
var moduleCacheMutex sync.RWMutex

// GetBuiltinModule returns a builtin module by name if it exists
func GetBuiltinModule(name string) (*core.DictValue, bool) {
	// Check cache first
	moduleCacheMutex.RLock()
	if module, cached := moduleCache[name]; cached {
		moduleCacheMutex.RUnlock()
		return module, true
	}
	moduleCacheMutex.RUnlock()

	// Not in cache, check if it's a builtin module
	initializer, exists := builtinModules[name]
	if !exists {
		return nil, false
	}

	// Initialize the module (with write lock to prevent races)
	moduleCacheMutex.Lock()
	defer moduleCacheMutex.Unlock()

	// Double-check in case another goroutine initialized it
	if module, cached := moduleCache[name]; cached {
		return module, true
	}

	// Lazy initialization - create the module only when requested
	module := initializer()
	moduleCache[name] = module
	return module, true
}

// IsBuiltinModule checks if a name corresponds to a builtin module
func IsBuiltinModule(name string) bool {
	_, exists := builtinModules[name]
	return exists
}

// ListBuiltinModules returns a list of all available builtin module names
func ListBuiltinModules() []string {
	names := make([]string, 0, len(builtinModules))
	for name := range builtinModules {
		names = append(names, name)
	}
	return names
}
