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
	SpecialForms[core.LispSymbol("go")] = EvalGoForm
	SpecialForms[core.LispSymbol("select")] = EvalSelectForm
	SpecialForms[core.LispSymbol("select-timeout")] = EvalSelectTimeoutForm
	SpecialForms[core.LispSymbol("with-mutex")] = EvalWithMutexForm
	SpecialForms[core.LispSymbol("with-rlock")] = EvalWithRLockForm
}
