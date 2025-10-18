// Package modules provides Go-implemented standard library modules for M28.
package modules

import (
	"sync"

	"github.com/mmichie/m28/core"
)

// ModuleInitializer is a function that creates and returns a module dictionary
type ModuleInitializer func() *core.DictValue

// builtinModules maps module names to their initializers
var builtinModules = map[string]ModuleInitializer{
	"os":            InitOSModule,
	"sys":           InitSysModule,
	"io":            InitIOModule,
	"json":          InitJSONModule,
	"time":          InitTimeModule,
	"datetime":      InitDatetimeModule,
	"pathlib":       InitPathlibModule,
	"random":        InitRandomModule,
	"shutil":        InitShutilModule,
	"math":          InitMathModule,
	"collections":   InitCollectionsModule,
	"itertools":     InitItertoolsModule,
	"functools":     InitFunctoolsModule,
	"operator":      InitOperatorModule,
	"copy":          InitCopyModule,
	"heapq":         InitHeapqModule,
	"traceback":     InitTracebackModule,
	"unittest.util": InitUnittestUtilModule,
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
