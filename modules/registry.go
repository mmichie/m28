// Package modules provides Go-implemented standard library modules for M28.
package modules

import (
	"github.com/mmichie/m28/core"
)

// ModuleInitializer is a function that creates and returns a module dictionary
type ModuleInitializer func() *core.DictValue

// builtinModules maps module names to their initializers
var builtinModules = map[string]ModuleInitializer{
	"os":       InitOSModule,
	"json":     InitJSONModule,
	"time":     InitTimeModule,
	"datetime": InitDatetimeModule,
	"pathlib":  InitPathlibModule,
	"random":   InitRandomModule,
	"shutil":   InitShutilModule,
	"math":     InitMathModule,
}

// GetBuiltinModule returns a builtin module by name if it exists
func GetBuiltinModule(name string) (*core.DictValue, bool) {
	if initializer, exists := builtinModules[name]; exists {
		// Lazy initialization - create the module only when requested
		module := initializer()
		return module, true
	}
	return nil, false
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
