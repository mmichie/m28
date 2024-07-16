package m28

import (
	"fmt"
	"io"
	"strings"

	"github.com/chzyer/readline"
)

// REPL starts a Read-Eval-Print Loop for the M28 Lisp interpreter
func (i *Interpreter) REPL() {
	rl, err := readline.New("m28> ")
	if err != nil {
		panic(err)
	}
	defer rl.Close()

	fmt.Println("M28 Lisp REPL")
	fmt.Println("Type 'exit', 'quit', or use Ctrl-D to exit the REPL")
	fmt.Println("Use Ctrl-C to interrupt the current evaluation")

	for {
		input, err := rl.Readline()
		if err != nil {
			if err == readline.ErrInterrupt {
				// Ctrl-C was pressed, just continue to the next prompt
				continue
			} else if err == io.EOF {
				// Ctrl-D was pressed
				fmt.Println("\nExiting M28 Lisp REPL")
				break
			}
			fmt.Println("Error reading input:", err)
			continue
		}

		input = strings.TrimSpace(input)
		if input == "exit" || input == "quit" {
			fmt.Println("Exiting M28 Lisp REPL")
			break
		}

		if input == "" {
			continue
		}

		result, err := i.Execute(input)
		if err != nil {
			fmt.Println("Error:", err)
		} else {
			fmt.Println("=>", result)
		}

		rl.SaveHistory(input)
	}
}

// RunREPL creates a new interpreter and starts the REPL
func RunREPL() {
	interpreter := New()
	interpreter.REPL()
}
