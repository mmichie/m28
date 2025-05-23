// Package repl provides the read-eval-print loop for interactive M28 sessions.
package repl

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strings"

	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/eval"
	"github.com/mmichie/m28/parser"
)

// REPL represents a read-eval-print loop
type REPL struct {
	ctx    *core.Context
	reader *bufio.Reader
	writer io.Writer
}

// NewREPL creates a new REPL with the given context
func NewREPL(globalCtx *core.Context) *REPL {
	return &REPL{
		ctx:    globalCtx,
		reader: bufio.NewReader(os.Stdin),
		writer: os.Stdout,
	}
}

// SetInput sets the input reader for the REPL
func (r *REPL) SetInput(reader io.Reader) {
	r.reader = bufio.NewReader(reader)
}

// SetOutput sets the output writer for the REPL
func (r *REPL) SetOutput(writer io.Writer) {
	r.writer = writer
}

// Start starts the REPL
func (r *REPL) Start() {
	fmt.Fprintln(r.writer, "M28 REPL (New Implementation)")
	fmt.Fprintln(r.writer, "Type 'exit' to quit, 'help' for more information")

	for {
		fmt.Fprint(r.writer, "> ")

		line, err := r.reader.ReadString('\n')
		if err != nil {
			if err == io.EOF {
				fmt.Fprintln(r.writer, "\nExiting...")
				return
			}
			fmt.Fprintf(r.writer, "Error reading input: %s\n", err)
			continue
		}

		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}

		// Handle special commands
		switch line {
		case "exit", "quit":
			fmt.Fprintln(r.writer, "Exiting...")
			return
		case "help":
			printHelp(r.writer)
			continue
		}

		// Parse the input
		p := parser.NewParser()
		expr, err := p.Parse(line)
		if err != nil {
			fmt.Fprintf(r.writer, "Parse error: %s\n", err)
			continue
		}

		// Evaluate the expression
		result, err := eval.Eval(expr, r.ctx)
		if err != nil {
			fmt.Fprintf(r.writer, "Evaluation error: %s\n", err)
			continue
		}

		// Print the result
		fmt.Fprintln(r.writer, result.String())
	}
}

// printHelp displays help information
func printHelp(w io.Writer) {
	fmt.Fprintln(w, "M28 REPL Help:")
	fmt.Fprintln(w, "  exit, quit - Exit the REPL")
	fmt.Fprintln(w, "  help       - Display this help message")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Basic Syntax:")
	fmt.Fprintln(w, "  Numbers:   123, 45.67")
	fmt.Fprintln(w, "  Strings:   \"hello\"")
	fmt.Fprintln(w, "  Lists:     [1, 2, 3]")
	fmt.Fprintln(w, "  Function:  (def add (a b) (+ a b))")
	fmt.Fprintln(w, "  Call:      (add 1 2)")
	fmt.Fprintln(w)
}
