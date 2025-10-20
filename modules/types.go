package modules

import (
	"github.com/mmichie/m28/core"
)

// InitTypesModule creates and returns the types module
func InitTypesModule() *core.DictValue {
	typesModule := core.NewDict()

	// DynamicClassAttribute - similar to property but with special class/instance behavior
	// For now, create a simple class that will be used as a base for enum.property
	typesModule.Set("DynamicClassAttribute", core.NewClass("DynamicClassAttribute", nil))

	// MappingProxyType - read-only proxy for mappings
	// For now, return a simple class
	typesModule.Set("MappingProxyType", core.NewClass("MappingProxyType", nil))

	// GenericAlias - used for generic type hints like list[int]
	// For now, return a simple class
	typesModule.Set("GenericAlias", core.NewClass("GenericAlias", nil))

	return typesModule
}
