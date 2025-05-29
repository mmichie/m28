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
	ctx               *core.Context
	reader            *bufio.Reader
	writer            io.Writer
	outputTracker     *OutputTracker
	history           *History
	completer         *Completer
	helpSystem        *HelpSystem
	executionState    *ExecutionState
	errorReporter     *ErrorReporter
	commandHandler    *CommandHandler
	indentTracker     *IndentationTracker
	colorManager      *ColorManager
}

// NewREPL creates a new REPL with the given context
func NewREPL(globalCtx *core.Context) *REPL {
	outputTracker := NewOutputTracker(os.Stdout)
	colorManager := NewColorManager(true) // Colors enabled by default
	
	r := &REPL{
		ctx:            globalCtx,
		reader:         bufio.NewReader(os.Stdin),
		writer:         outputTracker,
		outputTracker:  outputTracker,
		history:        NewHistory(1000),
		completer:      NewCompleter(globalCtx),
		helpSystem:     NewHelpSystem(globalCtx),
		executionState: NewExecutionState(globalCtx),
		errorReporter:  NewErrorReporter(colorManager),
		indentTracker:  NewIndentationTracker(),
		colorManager:   colorManager,
	}
	r.commandHandler = NewCommandHandler(r)
	return r
}

// SetInput sets the input reader for the REPL
func (r *REPL) SetInput(reader io.Reader) {
	r.reader = bufio.NewReader(reader)
}

// SetOutput sets the output writer for the REPL
func (r *REPL) SetOutput(writer io.Writer) {
	r.outputTracker = NewOutputTracker(writer)
	r.writer = r.outputTracker
}

// Start starts the REPL
func (r *REPL) Start() {
	fmt.Fprintln(r.writer, "M28 REPL")
	fmt.Fprintln(r.writer, "Type 'exit' to quit, 'help' for more information")
	fmt.Fprintln(r.writer)

	for {
		// Display the input prompt with execution number
		prompt := r.executionState.FormatInputPrompt()
		fmt.Fprint(r.writer, r.colorManager.ColorizePrompt(prompt))

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

		// Check if this is a REPL command
		if r.commandHandler.IsCommand(line) {
			handled, err := r.commandHandler.HandleCommand(line)
			if err != nil {
				fmt.Fprintf(r.writer, "Command error: %s\n", err)
				continue
			}
			if handled {
				continue
			}
			// If not handled (e.g., ! command that returns code to execute),
			// get the command to execute
			if cmdToExec := r.commandHandler.GetCommandToExecute(); cmdToExec != "" {
				line = cmdToExec
				fmt.Fprintf(r.writer, "%s%s\n", r.executionState.FormatInputPrompt(), line)
			}
		}
		
		// Add to history (after command processing)
		r.history.Add(line)

		// Handle built-in commands
		switch {
		case line == "exit" || line == "quit":
			fmt.Fprintln(r.writer, "Exiting...")
			return
		case line == "help":
			r.helpSystem.ShowHelp("", r.writer)
			continue
		case strings.HasPrefix(line, "help "):
			topic := strings.TrimSpace(line[5:])
			r.helpSystem.ShowHelp(topic, r.writer)
			continue
		}

		// Check for incomplete input (multi-line)
		fullInput := line
		if isIncomplete(line) {
			multiLine, err := r.readMultilineInput(line)
			if err != nil {
				fmt.Fprintf(r.writer, "Error: %s\n", err)
				continue
			}
			fullInput = multiLine
		}

		// Get the execution number for this command
		execNum := r.executionState.NextExecutionNumber()

		// Parse the input
		p := parser.NewParser()
		expr, err := p.Parse(fullInput)
		if err != nil {
			fmt.Fprintf(r.writer, "Parse error: %s\n", err)
			continue
		}

		// Start tracking output for duplicate detection
		r.outputTracker.StartTracking()

		// Evaluate the expression
		result, err := eval.Eval(expr, r.ctx)
		
		// Stop tracking and get the printed output
		printedOutput := r.outputTracker.StopTracking()

		if err != nil {
			r.errorReporter.ReportError(err, r.ctx, r.writer)
			continue
		}

		// Store the result in output history
		if result != core.Nil {
			r.executionState.StoreOutput(execNum, result)
		}

		// Only print the result if it's not nil and wasn't already printed
		if result != core.Nil {
			resultStr := result.String()
			// Check if the result was already printed to avoid duplication
			if printedOutput == "" || printedOutput != resultStr {
				outputPrompt := r.executionState.FormatOutputPrompt(execNum)
				fmt.Fprintf(r.writer, "%s%s\n", 
					r.colorManager.ColorizeOutputPrompt(outputPrompt), 
					resultStr)
			}
		}
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
