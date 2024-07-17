package repl

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strings"

	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/env"
	"github.com/mmichie/m28/eval"
	"github.com/mmichie/m28/parser"
)

type REPL struct {
	env       core.Environment
	evaluator *eval.Evaluator
	parser    *parser.Parser
}

func NewREPL() *REPL {
	environment := env.NewEnvironment(nil)
	environment.SetupBuiltins()
	return &REPL{
		env:       environment,
		evaluator: eval.NewEvaluator(),
		parser:    parser.NewParser(),
	}
}

func (r *REPL) Run() {
	fmt.Println("Welcome to the M28 Lisp REPL!")
	fmt.Println("Type 'exit' or 'quit' to exit the REPL.")

	reader := bufio.NewReader(os.Stdin)

	for {
		fmt.Print("m28> ")
		input, err := reader.ReadString('\n')
		if err != nil {
			if err == io.EOF {
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

	return r.evaluator.Eval(expr, r.env)
}
