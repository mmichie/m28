// Package special_forms provides special form implementations for the M28 language.
package special_forms

import (
	"m28/core"
)

// Special form handler function type
type SpecialFormHandler func([]core.Value, *core.Context) (core.Value, error)

// Map of special form names to handlers
var specialForms = make(map[string]SpecialFormHandler)

// registerSpecialForm registers a special form handler
func registerSpecialForm(name string, handler SpecialFormHandler) {
	specialForms[name] = handler
}

// GetSpecialForm returns a special form handler by name
func GetSpecialForm(name string) (SpecialFormHandler, bool) {
	handler, ok := specialForms[name]
	return handler, ok
}

// InitializeSpecialForms registers all special forms
func InitializeSpecialForms() {
	// Register basic special forms
	registerSpecialForm("if", IfForm)
	registerSpecialForm("def", DefForm)
	registerSpecialForm("=", AssignForm)
	registerSpecialForm("quote", QuoteForm)
	registerSpecialForm("do", DoForm)
	registerSpecialForm("lambda", LambdaForm)
	
	// Register loop forms
	RegisterLoopForms()
	
	// Register exception forms
	RegisterExceptionForms()
	
	// Register module forms
	RegisterModuleForms()
}

// IfForm implements the 'if' special form
func IfForm(args []core.Value, ctx *core.Context) (core.Value, error) {
	// Implementation from eval/evaluator.go
	return eval.EvalIf(args, ctx)
}

// DefForm implements the 'def' special form for function/variable definition
func DefForm(args []core.Value, ctx *core.Context) (core.Value, error) {
	// Implementation from eval/evaluator.go
	return eval.EvalDef(args, ctx)
}

// AssignForm implements the '=' special form for assignment
func AssignForm(args []core.Value, ctx *core.Context) (core.Value, error) {
	// Implementation from eval/evaluator.go
	return eval.EvalAssign(args, ctx)
}

// QuoteForm implements the 'quote' special form
func QuoteForm(args []core.Value, ctx *core.Context) (core.Value, error) {
	// Implementation from eval/evaluator.go
	return eval.EvalQuote(args, ctx)
}

// DoForm implements the 'do' special form for grouping expressions
func DoForm(args []core.Value, ctx *core.Context) (core.Value, error) {
	// Implementation from eval/evaluator.go
	return eval.EvalDo(args, ctx)
}

// LambdaForm implements the 'lambda' special form for anonymous functions
func LambdaForm(args []core.Value, ctx *core.Context) (core.Value, error) {
	// Implementation from eval/evaluator.go
	return eval.EvalLambda(args, ctx)
}

// RegisterModuleForms registers module-related special forms
func RegisterModuleForms() {
	// Register module forms (placeholder for when we implement modules)
}