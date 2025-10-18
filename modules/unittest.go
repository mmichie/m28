package modules

import (
	_ "embed"

	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/eval"
	"github.com/mmichie/m28/parser"
)

//go:embed unittest.m28
var unittestSource string

// InitUnittestModule creates and returns the unittest module
func InitUnittestModule() *core.DictValue {
	// Get the global context to inherit builtins
	loader := core.GetModuleLoader()
	if loader == nil {
		panic("Module loader not initialized")
	}
	globalCtx := loader.GetContext().Global

	// Create a module context with global scope
	moduleCtx := core.NewContext(globalCtx)

	// Parse the unittest.m28 source
	p := parser.NewParser()
	expr, err := p.Parse(unittestSource)
	if err != nil {
		panic("Failed to parse unittest module: " + err.Error())
	}

	// Evaluate the module
	_, err = eval.Eval(expr, moduleCtx)
	if err != nil {
		panic("Failed to evaluate unittest module: " + err.Error())
	}

	// Create the module dictionary
	moduleDict := core.NewDict()

	// Export TestCase class
	if testCase, err := moduleCtx.Lookup("TestCase"); err == nil {
		moduleDict.Set("TestCase", testCase)
	}

	// Export main function
	if mainFunc, err := moduleCtx.Lookup("main"); err == nil {
		moduleDict.Set("main", mainFunc)
	}

	return moduleDict
}
