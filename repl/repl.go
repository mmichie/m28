package repl

import (
	"fmt"
	"io"
	"strings"

	"github.com/chzyer/readline"
	"github.com/mmichie/m28/builtin"
	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/env"
	"github.com/mmichie/m28/eval"
	"github.com/mmichie/m28/parser"
	"github.com/mmichie/m28/special_forms"
)

type REPL struct {
	env       core.Environment
	evaluator core.Evaluator
	parser    *parser.Parser
	rl        *readline.Instance
	viMode    bool
}

func NewREPL() *REPL {
	// Initialize the environment
	environment := env.NewEnvironment(nil)

	// Set up builtins in the environment
	environment.SetupBuiltins()

	// Create the evaluator
	evaluator := eval.NewEvaluator()

	// Set up the evaluator for builtins
	builtin.SetEvaluator(evaluator)

	// Register special forms in the environment
	special_forms.RegisterSpecialForms(environment)

	// Set up readline for REPL
	rl, err := readline.NewEx(&readline.Config{
		Prompt:          "m28> ",
		HistoryFile:     "/tmp/m28_history",
		InterruptPrompt: "^C",
		EOFPrompt:       "exit",
	})
	if err != nil {
		panic(err)
	}

	return &REPL{
		env:       environment,
		evaluator: evaluator,
		parser:    parser.NewParser(),
		rl:        rl,
		viMode:    false,
	}
}

func (r *REPL) Run() {
	fmt.Println("Welcome to the M28 Lisp REPL!")
	fmt.Println("Type 'exit' or 'quit' to exit the REPL.")
	fmt.Println("Use Ctrl+C to interrupt.")
	fmt.Println("Type ':toggle-keybindings' to switch between Emacs and VI keybindings.")

	for {
		input, err := r.rl.Readline()
		if err != nil {
			if err == readline.ErrInterrupt {
				continue
			} else if err == io.EOF {
				fmt.Println("\nGoodbye!")
				return
			}
			fmt.Println("Error reading input:", err)
			continue
		}

		input = strings.TrimSpace(input)
		if input == "exit" || input == "quit" {
			fmt.Println("Goodbye!")
			return
		}

		if input == ":toggle-keybindings" {
			r.toggleKeybindings()
			continue
		}

		if input == "" {
			continue
		}

		result, err := r.EvaluateString(input)
		if err != nil {
			fmt.Println("Error:", err)
		} else {
			fmt.Println("=>", core.PrintValue(result))
		}
	}
}

func (r *REPL) EvaluateString(input string) (core.LispValue, error) {
	expr, err := r.parser.Parse(input)
	if err != nil {
		// If it's a parse error, enrich it
		return nil, fmt.Errorf("parse error: %v", err)
	}

	// Check if we got multiple expressions as a list
	if exprList, ok := expr.(core.LispList); ok && len(exprList) > 0 {
		// Execute each expression in the list
		var result core.LispValue = core.PythonicNone{}

		for _, subExpr := range exprList {
			result, err = r.evaluator.Eval(subExpr, r.env)
			if err != nil {
				return nil, err
			}
		}

		// Return the last result
		return result, nil
	}

	// Single expression or empty list case
	result, err := r.evaluator.Eval(expr, r.env)

	// Return the result and error directly - don't rewrap errors
	// This preserves exceptions, tracebacks, and control flow signals
	return result, err
}

func (r *REPL) toggleKeybindings() {
	r.viMode = !r.viMode
	if r.viMode {
		r.rl.SetVimMode(true)
		fmt.Println("Switched to VI keybindings")
	} else {
		r.rl.SetVimMode(false)
		fmt.Println("Switched to Emacs keybindings")
	}
}

func (r *REPL) Close() {
	r.rl.Close()
}
