package special_forms

// InitializeAllForms ensures all special forms are properly registered
// This includes both standard forms and concurrency forms
func InitializeAllForms() {
	// Make sure concurrency forms are registered if the handlers exist
	if EvalGoForm != nil {
		RegisterConcurrencyForms()
	}
}
