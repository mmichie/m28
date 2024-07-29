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
)

type REPL struct {
	env       core.Environment
	evaluator core.Evaluator
	parser    *parser.Parser
	rl        *readline.Instance
	viMode    bool
}

func NewREPL() *REPL {
	environment := env.NewEnvironment(nil)
	environment.SetupBuiltins()
	evaluator := eval.NewEvaluator()
	builtin.SetEvaluator(evaluator)

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
		return nil, err
	}

	result, err := r.evaluator.Eval(expr, r.env)

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
