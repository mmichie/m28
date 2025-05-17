package initialize

import (
	"github.com/mmichie/m28/concurrency"
	"github.com/mmichie/m28/special_forms"
)

// RegisterConcurrencyFormHandlers registers all concurrency special forms
func RegisterConcurrencyFormHandlers() {
	// Register the handlers with the special_forms package
	special_forms.RegisterConcurrencyFormHandlers(
		concurrency.EvalGo,
		concurrency.EvalSelect,
		concurrency.EvalSelectTimeout,
		concurrency.WithMutex,
		concurrency.WithRLock,
	)

	// Register the forms to make them available
	special_forms.RegisterConcurrencyForms()
}
