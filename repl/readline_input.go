package repl

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"

	"github.com/chzyer/readline"
)

// ReadlineInput provides readline-based input handling for the REPL
type ReadlineInput struct {
	rl            *readline.Instance
	completer     *Completer
	history       *History
	colorManager  *ColorManager
	indentTracker *IndentationTracker
	viMode        bool
}

// NewReadlineInput creates a new readline input handler
func NewReadlineInput(completer *Completer, history *History, colorManager *ColorManager, indentTracker *IndentationTracker) (*ReadlineInput, error) {
	// Check terminal type
	termType := os.Getenv("TERM")
	if termType == "" {
		// Set a reasonable default
		os.Setenv("TERM", "xterm-256color")
	}

	ri := &ReadlineInput{
		completer:     completer,
		history:       history,
		colorManager:  colorManager,
		indentTracker: indentTracker,
		viMode:        false, // Default to emacs mode
	}

	// Create readline config
	config := &readline.Config{
		Prompt:          "In [1]: ",
		HistoryFile:     filepath.Join(os.Getenv("HOME"), ".m28_history"),
		InterruptPrompt: "^C",
		EOFPrompt:       "exit",
		AutoComplete:    ri.createCompleter(),
		VimMode:         ri.viMode,

		// Enable history features
		HistorySearchFold:      true,
		DisableAutoSaveHistory: false,

		// Disable masking - this might be causing the asterisk issue
		EnableMask: false,

		// Explicitly set stdin/stdout/stderr
		Stdin:  os.Stdin,
		Stdout: os.Stdout,
		Stderr: os.Stderr,

		// Don't use custom filter that might interfere
		// FuncFilterInputRune: ri.filterInputRune,
	}

	rl, err := readline.NewEx(config)
	if err != nil {
		return nil, err
	}

	ri.rl = rl

	// Load history from our history manager
	if err := ri.loadHistory(); err != nil {
		// Non-fatal, just log it
		fmt.Fprintf(os.Stderr, "Warning: could not load history: %v\n", err)
	}

	return ri, nil
}

// SetViMode switches between vi and emacs modes
func (ri *ReadlineInput) SetViMode(enabled bool) error {
	ri.viMode = enabled
	if ri.rl != nil {
		ri.rl.SetVimMode(enabled)
	}
	return nil
}

// createCompleter creates a readline completer from our Completer
func (ri *ReadlineInput) createCompleter() readline.AutoCompleter {
	// Create a custom auto completer
	return &customCompleter{completer: ri.completer}
}

// customCompleter implements readline.AutoCompleter
type customCompleter struct {
	completer *Completer
}

func (cc *customCompleter) Do(line []rune, pos int) (newLine [][]rune, length int) {
	// Convert to string for our completer
	input := string(line[:pos])

	// Get completions from our completer
	completions := cc.completer.Complete(input)

	// Convert back to rune slices for readline
	result := make([][]rune, len(completions))
	for i, comp := range completions {
		result[i] = []rune(comp)
	}

	// Find the start of the word being completed
	wordStart := strings.LastIndexAny(input, " \t\n()[]{}\"'") + 1

	return result, pos - wordStart
}

// filterInputRune allows custom key handling
func (ri *ReadlineInput) filterInputRune(r rune) (rune, bool) {
	// Allow all normal input
	return r, true
}

// loadHistory loads history from our history manager into readline
func (ri *ReadlineInput) loadHistory() error {
	// Get the last N commands from history
	items := ri.history.GetLastN(1000)
	for _, item := range items {
		ri.rl.SaveHistory(item)
	}
	return nil
}

// ReadLine reads a line of input with all readline features
func (ri *ReadlineInput) ReadLine(prompt string) (string, error) {
	// Set colored prompt
	coloredPrompt := ri.colorManager.ColorizePrompt(prompt)
	ri.rl.SetPrompt(coloredPrompt)

	// Read the line
	line, err := ri.rl.Readline()
	if err != nil {
		return "", err
	}

	// Add to our history manager
	if line != "" {
		ri.history.Add(line)
	}

	return line, nil
}

// ReadMultiLine reads multiple lines for incomplete expressions
func (ri *ReadlineInput) ReadMultiLine(firstLine string) (string, error) {
	lines := []string{firstLine}

	// Reset indent tracker for new multiline input
	ri.indentTracker.Reset()
	ri.indentTracker.UpdateLevel(firstLine)

	for {
		// Calculate continuation prompt with proper indentation
		lastLine := lines[len(lines)-1]
		ri.indentTracker.UpdateLevel(lastLine)
		indent := ri.indentTracker.GetIndentation()
		contPrompt := fmt.Sprintf("... %s", indent)
		coloredPrompt := ri.colorManager.ColorizePrompt(contPrompt)
		ri.rl.SetPrompt(coloredPrompt)

		line, err := ri.rl.Readline()
		if err != nil {
			if err == io.EOF || err == readline.ErrInterrupt {
				// User wants to cancel multi-line input
				return "", fmt.Errorf("multi-line input cancelled")
			}
			return "", err
		}

		lines = append(lines, line)

		// Check if we have a complete expression
		fullInput := strings.Join(lines, "\n")
		if !isIncomplete(fullInput) {
			return fullInput, nil
		}
	}
}

// Close closes the readline instance
func (ri *ReadlineInput) Close() error {
	if ri.rl != nil {
		return ri.rl.Close()
	}
	return nil
}

// Terminal returns the underlying terminal
func (ri *ReadlineInput) Terminal() *readline.Terminal {
	if ri.rl != nil {
		return ri.rl.Terminal
	}
	return nil
}

// SetPrompt updates the prompt
func (ri *ReadlineInput) SetPrompt(prompt string) {
	if ri.rl != nil {
		coloredPrompt := ri.colorManager.ColorizePrompt(prompt)
		ri.rl.SetPrompt(coloredPrompt)
	}
}
