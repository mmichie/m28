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
}

// registerDefinitionForms registers special forms for definitions
func registerDefinitionForms() {
	// def: Define variables and functions
	eval.RegisterSpecialForm("def", defForm)
	
	// =: Assignment
	eval.RegisterSpecialForm("=", assignForm)
	
	// quote: Quote an expression
	eval.RegisterSpecialForm("quote", quoteForm)
}

// registerModuleForms registers special forms for module management
func registerModuleForms() {
	// import: Import modules
	eval.RegisterSpecialForm("import", importForm)
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