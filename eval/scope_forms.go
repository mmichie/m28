package eval

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// GlobalForm implements the 'global' special form
// Syntax: (global name1 name2 ...)
// Marks the given names as global in the current scope
func GlobalForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("global requires at least one variable name")
	}

	// Mark each name as global
	for _, arg := range args {
		sym, ok := arg.(core.SymbolValue)
		if !ok {
			return nil, fmt.Errorf("global requires symbol arguments, got %s", arg.Type())
		}

		// Mark this variable as global in the current scope
		ctx.DeclareGlobal(string(sym))
	}

	return core.Nil, nil
}

// NonlocalForm implements the 'nonlocal' special form
// Syntax: (nonlocal name1 name2 ...)
// Marks the given names as nonlocal in the current scope
func NonlocalForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("nonlocal requires at least one variable name")
	}

	// Mark each name as nonlocal
	for _, arg := range args {
		sym, ok := arg.(core.SymbolValue)
		if !ok {
			return nil, fmt.Errorf("nonlocal requires symbol arguments, got %s", arg.Type())
		}

		// Mark this variable as nonlocal in the current scope
		// This will error if the variable doesn't exist in an enclosing scope
		if err := ctx.DeclareNonlocal(string(sym)); err != nil {
			return nil, err
		}
	}

	return core.Nil, nil
}
