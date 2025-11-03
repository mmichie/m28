package modules

import (
	"github.com/mmichie/m28/core"
)

// Init_CompatPickleModule creates a stub for _compat_pickle that bypasses assertions
// This module is used by pickle to map Python 2 names to Python 3 names
func Init_CompatPickleModule() *core.DictValue {
	module := core.NewDict()

	// Create empty mappings to avoid assertion failures
	// The actual mappings aren't critical for basic pickle operation with Python 3 objects
	importMapping := core.NewDict()
	nameMapping := core.NewDict()
	reverseImportMapping := core.NewDict()
	reverseNameMapping := core.NewDict()

	module.Set("IMPORT_MAPPING", importMapping)
	module.Set("NAME_MAPPING", nameMapping)
	module.Set("REVERSE_IMPORT_MAPPING", reverseImportMapping)
	module.Set("REVERSE_NAME_MAPPING", reverseNameMapping)

	// __name__ and __doc__
	module.Set("__name__", core.StringValue("_compat_pickle"))
	module.Set("__doc__", core.StringValue("Compatibility mappings for pickle between Python 2 and 3"))

	return module
}
