package special_forms

import (
	"github.com/mmichie/m28/concurrency"
	"github.com/mmichie/m28/core"
)

// RegisterConcurrencyForms registers all concurrency-related special forms
func RegisterConcurrencyForms(specialForms map[core.LispSymbol]SpecialFormFunc) {
	// Register the go special form
	specialForms[core.LispSymbol("go")] = concurrency.EvalGo

	// More concurrency forms will be registered here
}
