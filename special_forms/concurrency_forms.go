package special_forms

import (
	"github.com/mmichie/m28/core"
)

// These variables will be set by the concurrency package to break the import cycle
var (
	EvalGoForm            SpecialFormFunc
	EvalSelectForm        SpecialFormFunc
	EvalSelectTimeoutForm SpecialFormFunc
	EvalWithMutexForm     SpecialFormFunc
	EvalWithRLockForm     SpecialFormFunc
)

// RegisterConcurrencyFormHandlers registers handlers for concurrency special forms
func RegisterConcurrencyFormHandlers(
	goForm SpecialFormFunc,
	selectForm SpecialFormFunc,
	selectTimeoutForm SpecialFormFunc,
	withMutexForm SpecialFormFunc,
	withRLockForm SpecialFormFunc) {

	// Store the form handlers
	EvalGoForm = goForm
	EvalSelectForm = selectForm
	EvalSelectTimeoutForm = selectTimeoutForm
	EvalWithMutexForm = withMutexForm
	EvalWithRLockForm = withRLockForm
}

// RegisterConcurrencyForms registers all concurrency-related special forms
func RegisterConcurrencyForms() {
	// Make sure the form handlers have been set
	if EvalGoForm == nil {
		// Don't print a warning here, as this is expected during initialization
		return
	}

	// Register the forms
	specialForms[core.LispSymbol("go")] = EvalGoForm
	specialForms[core.LispSymbol("select")] = EvalSelectForm
	specialForms[core.LispSymbol("select-timeout")] = EvalSelectTimeoutForm
	specialForms[core.LispSymbol("with-mutex")] = EvalWithMutexForm
	specialForms[core.LispSymbol("with-rlock")] = EvalWithRLockForm
}
