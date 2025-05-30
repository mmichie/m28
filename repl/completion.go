package repl

import (
	"sort"
	"strings"

	"github.com/mmichie/m28/core"
)

// Completer provides tab completion for the REPL
type Completer struct {
	ctx *core.Context
}

// NewCompleter creates a new completer
func NewCompleter(ctx *core.Context) *Completer {
	return &Completer{ctx: ctx}
}

// Complete returns completions for the given prefix
func (c *Completer) Complete(prefix string) []string {
	if prefix == "" {
		return nil
	}

	var completions []string

	// Get all defined symbols from context
	symbols := c.ctx.GetAllSymbols()

	// Add special forms
	specialForms := []string{
		"def", "lambda", "if", "do", "let", "quote",
		"set!", "begin", "cond", "case", "and", "or",
		"for", "while", "break", "continue", "return",
		"try", "except", "finally", "raise", "assert",
		"class", "super", "isinstance", "issubclass",
		"import", "from", "export", "with", "as",
		"yield", "async", "await", "channel", "select",
		"send!", "recv!", "go",
	}

	// Add builtins
	builtins := []string{
		"print", "str", "len", "type", "repr",
		"list", "dict", "tuple", "set",
		"range", "enumerate", "zip", "map", "filter", "reduce",
		"all", "any", "sum", "min", "max",
		"abs", "round", "pow", "sqrt",
		"open", "read", "write", "close",
		"true", "false", "nil",
	}

	// Collect all possible completions
	allCompletions := make(map[string]bool)

	for _, sym := range symbols {
		allCompletions[sym] = true
	}

	for _, sf := range specialForms {
		allCompletions[sf] = true
	}

	for _, b := range builtins {
		allCompletions[b] = true
	}

	// Filter by prefix
	for comp := range allCompletions {
		if strings.HasPrefix(comp, prefix) {
			completions = append(completions, comp)
		}
	}

	// Sort completions
	sort.Strings(completions)

	return completions
}

// CompleteFilename returns filename completions for the given prefix
func CompleteFilename(prefix string) []string {
	// This is a placeholder - real implementation would use filepath.Glob
	// and handle proper path completion
	return nil
}
