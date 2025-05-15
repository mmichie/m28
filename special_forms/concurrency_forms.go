package special_forms

import (
	"github.com/mmichie/m28/concurrency"
	"github.com/mmichie/m28/core"
)

// RegisterConcurrencyForms registers all concurrency-related special forms
func RegisterConcurrencyForms(specialForms map[core.LispSymbol]SpecialFormFunc) {
	// Register the go special form
	specialForms[core.LispSymbol("go")] = concurrency.EvalGo

	// Register the select forms
	specialForms[core.LispSymbol("select")] = concurrency.EvalSelect
	specialForms[core.LispSymbol("select-timeout")] = concurrency.EvalSelectTimeout

	// Register the mutex forms
	specialForms[core.LispSymbol("with-mutex")] = concurrency.EvalWithMutex
	specialForms[core.LispSymbol("with-rlock")] = concurrency.EvalWithRLock
}
