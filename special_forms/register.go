package special_forms

import (
	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/eval"
)

// RegisterAllForms registers all special forms with the evaluator
func RegisterAllForms() {
	// Register basic control flow forms
	registerControlFlowForms()

	// Register definition forms
	registerDefinitionForms()

	// Register module forms
	registerModuleForms()
}

// registerControlFlowForms registers special forms for control flow
func registerControlFlowForms() {
	// if: Conditional execution
	eval.RegisterSpecialForm("if", ifForm)

	// do: Sequential execution
	eval.RegisterSpecialForm("do", doForm)

	// begin: Same as do
	eval.RegisterSpecialForm("begin", doForm)

	// return: Return from a function
	eval.RegisterSpecialForm("return", returnForm)

	// Loop forms
	eval.RegisterSpecialForm("while", whileForm)
	eval.RegisterSpecialForm("for", forForm)
	eval.RegisterSpecialForm("break", breakForm)
	eval.RegisterSpecialForm("continue", continueForm)
}

// registerDefinitionForms registers special forms for definitions
func registerDefinitionForms() {
	// def: Define variables and functions
	eval.RegisterSpecialForm("def", defForm)

	// =: Assignment
	eval.RegisterSpecialForm("=", assignForm)

	// :=: Walrus operator (assignment expression)
	eval.RegisterSpecialForm(":=", walrusForm)

	// global: Mark variables as global
	eval.RegisterSpecialForm("global", globalForm)

	// nonlocal: Mark variables as nonlocal
	eval.RegisterSpecialForm("nonlocal", nonlocalForm)

	// quote: Quote an expression
	eval.RegisterSpecialForm("quote", quoteForm)

	// .: Dot notation for property access
	eval.RegisterSpecialForm(".", dotForm)

	// dict-literal: Dictionary literal construction
	eval.RegisterSpecialForm("dict-literal", dictLiteralForm)

	// list-comp: List comprehension
	eval.RegisterSpecialForm("list-comp", listCompForm)
}

// registerModuleForms registers special forms for module management
func registerModuleForms() {
	// import: Import modules
	// NOTE: Commented out because enhanced import is registered in eval/module_forms.go
	// eval.RegisterSpecialForm("import", importForm)
}

// ifForm implements the 'if' special form
func ifForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	return eval.IfForm(args, ctx)
}

// doForm implements the 'do' and 'begin' special forms
func doForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	return eval.DoForm(args, ctx)
}

// defForm implements the 'def' special form
func defForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	return eval.DefForm(args, ctx)
}

// assignForm implements the '=' special form
func assignForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	return eval.AssignForm(args, ctx)
}

// walrusForm implements the ':=' special form (walrus operator)
func walrusForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	return eval.WalrusForm(args, ctx)
}

// quoteForm implements the 'quote' special form
func quoteForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	return eval.QuoteForm(args, ctx)
}

// importForm implements the 'import' special form
func importForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	return eval.ImportForm(args, ctx)
}

// returnForm implements the 'return' special form
func returnForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	return eval.ReturnForm(args, ctx)
}

// whileForm implements the 'while' special form
func whileForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	return eval.WhileFormHandler(args, ctx)
}

// forForm implements the 'for' special form
func forForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	return eval.ForFormHandler(args, ctx)
}

// breakForm implements the 'break' special form
func breakForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	return eval.BreakFormHandler(args, ctx)
}

// continueForm implements the 'continue' special form
func continueForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	return eval.ContinueFormHandler(args, ctx)
}

// dotForm implements the '.' special form for property access
func dotForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	return eval.DotForm(args, ctx)
}

// dictLiteralForm implements the 'dict-literal' special form for dictionary construction
func dictLiteralForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	return eval.DictLiteralForm(args, ctx)
}

// listCompForm implements the 'list-comp' special form for list comprehensions
func listCompForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	return eval.ListCompForm(args, ctx)
}

// globalForm implements the 'global' special form
func globalForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	return eval.GlobalForm(args, ctx)
}

// nonlocalForm implements the 'nonlocal' special form
func nonlocalForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	return eval.NonlocalForm(args, ctx)
}
