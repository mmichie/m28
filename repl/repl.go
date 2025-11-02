// Package repl provides the read-eval-print loop for interactive M28 sessions.
package repl

import (
	"bufio"
	"errors"
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
	ctx            *core.Context
	reader         *bufio.Reader
	writer         io.Writer
	outputTracker  *OutputTracker
	history        *History
	completer      *Completer
	helpSystem     *HelpSystem
	executionState *ExecutionState
	errorReporter  *ErrorReporter
	commandHandler *CommandHandler
	indentTracker  *IndentationTracker
	colorManager   *ColorManager
	readlineInput  *ReadlineInput // New field for readline support
	useReadline    bool           // Flag to control readline usage
	pythonMode     bool           // Flag to enable Python syntax parsing
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
		useReadline:    true, // Default to using readline
	}
	r.commandHandler = NewCommandHandler(r)

	// Check if readline should be disabled
	if os.Getenv("M28_NO_READLINE") == "1" {
		r.useReadline = false
		fmt.Fprintln(os.Stderr, "Readline disabled by M28_NO_READLINE=1")
	}

	// Try to initialize readline
	if r.useReadline {
		readlineInput, err := NewReadlineInput(r.completer, r.history, r.colorManager, r.indentTracker)
		if err != nil {
			// Fall back to basic input if readline fails
			fmt.Fprintf(os.Stderr, "Warning: Could not initialize readline: %v\n", err)
			fmt.Fprintf(os.Stderr, "Falling back to basic input mode. Set M28_NO_READLINE=1 to disable this warning.\n")
			r.useReadline = false
		} else {
			r.readlineInput = readlineInput
		}
	}

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

// SetKeyBindingMode sets the key binding mode (vi or emacs)
func (r *REPL) SetKeyBindingMode(mode string) error {
	if r.readlineInput == nil {
		return fmt.Errorf("readline not available")
	}

	switch mode {
	case "vi":
		return r.readlineInput.SetViMode(true)
	case "emacs":
		return r.readlineInput.SetViMode(false)
	default:
		return fmt.Errorf("unknown key binding mode: %s (use 'vi' or 'emacs')", mode)
	}
}

// IsReadlineEnabled returns whether readline is enabled
func (r *REPL) IsReadlineEnabled() bool {
	return r.useReadline && r.readlineInput != nil
}

// SetPythonMode enables or disables Python syntax parsing mode
func (r *REPL) SetPythonMode(enabled bool) {
	r.pythonMode = enabled
	// Also set it on readline input if available
	if r.readlineInput != nil {
		r.readlineInput.SetPythonMode(enabled)
	}
}

// Start starts the REPL
func (r *REPL) Start() {
	if r.pythonMode {
		fmt.Fprintln(r.writer, "M28 REPL (Python Mode)")
	} else {
		fmt.Fprintln(r.writer, "M28 REPL")
	}
	fmt.Fprintln(r.writer, "Type 'exit' to quit, 'help' for more information")
	fmt.Fprintln(r.writer)

	// Clean up readline on exit if enabled
	if r.readlineInput != nil {
		defer r.readlineInput.Close()
	}

	for {
		// Display the input prompt with execution number
		prompt := r.executionState.FormatInputPrompt()

		var line string
		var err error

		if r.useReadline && r.readlineInput != nil {
			// Use readline for input
			line, err = r.readlineInput.ReadLine(prompt)
		} else {
			// Fall back to basic input
			fmt.Fprint(r.writer, r.colorManager.ColorizePrompt(prompt))
			line, err = r.reader.ReadString('\n')
			if err == nil {
				line = strings.TrimRight(line, "\n\r")
			}
		}

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
		if !r.useReadline || r.readlineInput == nil {
			// Only add to history if not using readline (readline handles it)
			r.history.Add(line)
		}

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
		needsMoreInput := false
		if r.pythonMode {
			needsMoreInput = isPythonIncomplete(line)
		} else {
			needsMoreInput = isIncomplete(line)
		}

		if needsMoreInput {
			var multiLine string
			var err error

			if r.useReadline && r.readlineInput != nil {
				// Use readline for multiline input
				multiLine, err = r.readlineInput.ReadMultiLine(line)
			} else {
				// Fall back to basic multiline input
				multiLine, err = r.readMultilineInput(line)
			}

			if err != nil {
				fmt.Fprintf(r.writer, "Error: %s\n", err)
				continue
			}
			fullInput = multiLine
		}

		// Get the execution number for this command
		execNum := r.executionState.NextExecutionNumber()

		// Parse the input based on mode
		var expr core.Value

		if r.pythonMode {
			// Use Python parser
			tokenizer := parser.NewPythonTokenizer(fullInput)
			tokens, tokErr := tokenizer.Tokenize()
			if tokErr != nil {
				fmt.Fprintf(r.writer, "Tokenization error: %s\n", tokErr)
				continue
			}

			pythonParser := parser.NewPythonParser(tokens)
			nodes, parseErr := pythonParser.Parse()
			if parseErr != nil {
				fmt.Fprintf(r.writer, "Parse error: %s\n", parseErr)
				continue
			}

			// Convert all Python AST nodes to IR and evaluate
			// For REPL, we want to evaluate each statement and return the last value
			var lastResult core.Value = core.NilValue{}
			var evalErr error
			for _, node := range nodes {
				ir := node.ToIR()
				lastResult, evalErr = eval.Eval(ir, r.ctx)
				if evalErr != nil {
					// Check if it's SystemExit - if so, exit the REPL
					var sysExit *core.SystemExit
					if errors.As(evalErr, &sysExit) {
						fmt.Fprintln(r.writer, "Exiting...")
						os.Exit(sysExit.Code)
					}
					r.errorReporter.ReportError(evalErr, r.ctx, r.writer)
					break
				}
			}

			if evalErr != nil {
				continue
			}

			// For REPL, we'll handle the result directly here when in Python mode
			if lastResult != core.Nil {
				r.executionState.StoreOutput(execNum, lastResult)
				resultStr := core.Repr(lastResult)
				outputPrompt := r.executionState.FormatOutputPrompt(execNum)
				fmt.Fprintf(r.writer, "%s%s\n",
					r.colorManager.ColorizeOutputPrompt(outputPrompt),
					resultStr)
			}
			continue
		} else {
			// Use M28 parser
			p := parser.NewParser()
			var parseErr error
			expr, parseErr = p.Parse(fullInput)
			if parseErr != nil {
				fmt.Fprintf(r.writer, "Parse error: %s\n", parseErr)
				continue
			}
		}

		// Start tracking output for duplicate detection
		r.outputTracker.StartTracking()

		// Evaluate the expression
		result, err := eval.Eval(expr, r.ctx)

		// Stop tracking and get the printed output
		printedOutput := r.outputTracker.StopTracking()

		if err != nil {
			// Check if it's SystemExit - if so, exit the REPL
			var sysExit *core.SystemExit
			if errors.As(err, &sysExit) {
				fmt.Fprintln(r.writer, "Exiting...")
				os.Exit(sysExit.Code)
			}
			r.errorReporter.ReportError(err, r.ctx, r.writer)
			continue
		}

		// Store the result in output history
		if result != core.Nil {
			r.executionState.StoreOutput(execNum, result)
		}

		// Only print the result if it's not nil and wasn't already printed
		if result != core.Nil {
			// Use Repr for developer-friendly output in REPL
			resultStr := core.Repr(result)
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
