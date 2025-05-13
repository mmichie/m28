package repl

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strconv"
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
	env          core.Environment
	evaluator    core.Evaluator
	parser       *parser.Parser
	rl           *readline.Instance
	viMode       bool
	history      []string
	historyFile  string
	historyLimit int
}

func NewREPL(flags *CommandFlags) *REPL {
	// Initialize the environment
	environment := env.NewEnvironment(nil)

	// Set up builtins in the environment
	environment.SetupBuiltins()

	// Create the evaluator
	evaluator := eval.NewEvaluator()

	// Set up the evaluator for builtins
	builtin.SetEvaluator(evaluator)

	// Create and register the module loader
	moduleLoader := special_forms.NewModuleLoader()
	moduleLoader.SetEvaluator(evaluator)
	core.SetModuleLoader(moduleLoader)

	// Register special forms in the environment
	special_forms.RegisterSpecialForms(environment)

	// Use command flags for history configuration or defaults if flags is nil
	historySize := 1000
	noHistory := false
	if flags != nil {
		historySize = flags.HistorySize
		noHistory = flags.NoHistoryFile
	}

	// Get the history file path
	historyFilePath := getHistoryFilePath(noHistory)

	// Create a new REPL instance to use for tab completion
	replInstance := &REPL{
		env:          environment,
		evaluator:    evaluator,
		parser:       parser.NewParser(),
		viMode:       false,
		history:      []string{},
		historyFile:  historyFilePath,
		historyLimit: historySize,
	}

	// Load history from file if it exists
	replInstance.loadHistory()

	// Set up readline for REPL with improved history file location and tab completion
	rl, err := readline.NewEx(&readline.Config{
		Prompt:          "m28> ",
		HistoryFile:     historyFilePath,
		InterruptPrompt: "^C",
		EOFPrompt:       "exit",
		HistoryLimit:    historySize,
		AutoComplete:    newCompleter(replInstance),
	})
	if err != nil {
		panic(err)
	}

	// Return the REPL with all settings
	return &REPL{
		env:          environment,
		evaluator:    evaluator,
		parser:       parser.NewParser(),
		rl:           rl,
		viMode:       false,
		history:      replInstance.history,
		historyFile:  historyFilePath,
		historyLimit: historySize,
	}
}

func (r *REPL) Run() {
	fmt.Println("Welcome to the M28 Lisp REPL!")
	fmt.Println("Type 'exit' or 'quit' to exit the REPL.")
	fmt.Println("Use Ctrl+C to interrupt.")
	fmt.Println("Type ':help' for a list of commands.")
	fmt.Println("History navigation: Up/Down arrows, Ctrl+R for reverse search.")
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

		// Handle REPL commands
		if strings.HasPrefix(input, ":") {
			handled := r.handleCommand(input)
			if handled {
				continue
			}
		}

		if input == "" {
			continue
		}

		// Add the command to our history
		r.addToHistory(input)

		result, err := r.EvaluateString(input)
		if err != nil {
			// Print the detailed error message
			if ex, ok := err.(*core.Exception); ok {
				// Use the formatted exception output
				fmt.Println(ex.String())
			} else {
				// Fall back to basic error display
				fmt.Println("Error:", err)
			}
		} else {
			fmt.Println("=>", core.PrintValue(result))
		}
	}
}

// handleCommand processes REPL commands that start with ':'
func (r *REPL) handleCommand(cmd string) bool {
	// Handle commands with arguments
	if strings.HasPrefix(cmd, ":history") {
		return r.handleHistoryCommand(cmd)
	}

	// Handle :! command to execute history by index
	if strings.HasPrefix(cmd, ":!") {
		return r.handleHistoryExecCommand(cmd)
	}

	// Handle :doc command for documentation lookup
	if strings.HasPrefix(cmd, ":doc") {
		return r.handleDocCommand(cmd)
	}

	switch cmd {
	case ":toggle-keybindings":
		r.toggleKeybindings()
		return true

	case ":toggle-colors":
		if core.ColoredErrors {
			core.DisableColors()
			fmt.Println("Colored error output disabled.")
		} else {
			core.EnableColors()
			fmt.Println("Colored error output enabled.")
		}
		return true

	case ":help":
		fmt.Println("Available commands:")
		fmt.Println("  :help                  - Show this help message")
		fmt.Println("  :toggle-keybindings    - Switch between Emacs and VI keybindings")
		fmt.Println("  :toggle-colors         - Enable/disable colored error output")
		fmt.Println("  :history               - Show command history (last 10 commands)")
		fmt.Println("  :history N             - Show last N history commands")
		fmt.Println("  :history clear         - Clear command history")
		fmt.Println("  :history search term   - Search history for term")
		fmt.Println("  :!N                    - Execute history entry N")
		fmt.Println("  :doc symbol            - Show documentation for a symbol")
		fmt.Println("  :doc-builtins          - List all built-in functions")
		fmt.Println("  :doc-special-forms     - List all special forms")
		fmt.Println("  :doc-modules           - List all available modules")
		fmt.Println("  exit, quit             - Exit the REPL")
		fmt.Println("")
		fmt.Println("Key bindings for history navigation (Emacs mode):")
		fmt.Println("  Up/Down arrows         - Navigate through history")
		fmt.Println("  Ctrl+R                 - Reverse search through history")
		fmt.Println("  Ctrl+S                 - Forward search through history")
		fmt.Println("  Alt+>                  - Go to the end of history")
		fmt.Println("  Alt+<                  - Go to the beginning of history")
		fmt.Println("")
		fmt.Println("Key bindings for history navigation (VI mode):")
		fmt.Println("  j, k                   - Navigate through history (in command mode)")
		fmt.Println("  /                      - Search forward (in command mode)")
		fmt.Println("  ?                      - Search backward (in command mode)")
		fmt.Println("  G                      - Go to the end of history (in command mode)")
		fmt.Println("  gg                     - Go to the beginning of history (in command mode)")
		return true
	}

	// Command not recognized
	return false
}

// handleHistoryCommand processes the :history command with its various options
func (r *REPL) handleHistoryCommand(cmd string) bool {
	parts := strings.Fields(cmd)

	// Default to showing last 10 entries
	if len(parts) == 1 {
		r.showHistory(10)
		return true
	}

	if len(parts) >= 2 {
		arg := parts[1]

		// Check if the argument is "clear"
		if arg == "clear" {
			r.clearHistory()
			return true
		}

		// Check if the argument is "search"
		if arg == "search" && len(parts) >= 3 {
			// Join the remaining parts as the search term
			searchTerm := strings.Join(parts[2:], " ")
			r.searchHistory(searchTerm)
			return true
		}

		// Check if the argument is a number
		if n, err := strconv.Atoi(arg); err == nil && n > 0 {
			r.showHistory(n)
			return true
		}
	}

	// Invalid argument
	fmt.Println("Invalid :history command. Usage: :history [N|clear|search term]")
	return true
}

// showHistory displays the last n commands in the history
func (r *REPL) showHistory(n int) {
	// Calculate how many entries to show
	historyLen := len(r.history)
	start := 0
	if historyLen > n {
		start = historyLen - n
	}

	// Display the history entries
	fmt.Printf("Command history (last %d of %d):\n", historyLen-start, historyLen)
	for i := start; i < historyLen; i++ {
		entry := r.history[i]
		if strings.TrimSpace(entry) != "" {
			fmt.Printf("%3d: %s\n", i+1, entry)
		}
	}
}

// clearHistory clears the command history
func (r *REPL) clearHistory() {
	// Clear our history array
	r.history = []string{}

	// We don't need to clear readline's history since it's managed with a file
	// and we'll update it the next time we run

	// Save the empty history to file
	r.saveHistory()

	fmt.Println("Command history cleared.")
}

// handleHistoryExecCommand executes a command from history by index
func (r *REPL) handleHistoryExecCommand(cmd string) bool {
	// Extract the index
	parts := strings.Fields(cmd)
	if len(parts) != 1 {
		fmt.Println("Invalid history execution command. Usage: :!N")
		return true
	}

	indexStr := strings.TrimPrefix(parts[0], ":!")
	index, err := strconv.Atoi(indexStr)
	if err != nil || index < 1 {
		fmt.Println("Invalid history index. Please specify a positive number.")
		return true
	}

	// Check if the index is valid
	if index > len(r.history) {
		fmt.Printf("History index %d out of range (max: %d)\n", index, len(r.history))
		return true
	}

	// Get the command from history (adjusting for 0-based indexing)
	historyCmdLine := r.history[index-1]

	// Skip if it's a REPL command
	if strings.HasPrefix(historyCmdLine, ":") {
		fmt.Printf("Cannot execute REPL command from history: %s\n", historyCmdLine)
		return true
	}

	// Execute the command
	fmt.Printf("Executing: %s\n", historyCmdLine)
	result, err := r.EvaluateString(historyCmdLine)
	if err != nil {
		// Print the detailed error message
		if ex, ok := err.(*core.Exception); ok {
			// Use the formatted exception output
			fmt.Println(ex.String())
		} else {
			// Fall back to basic error display
			fmt.Println("Error:", err)
		}
	} else {
		fmt.Println("=>", core.PrintValue(result))
	}

	return true
}

// handleDocCommand processes documentation lookup commands
func (r *REPL) handleDocCommand(cmd string) bool {
	parts := strings.Fields(cmd)

	// Check if it's one of the special doc commands
	if len(parts) == 1 {
		switch parts[0] {
		case ":doc-builtins":
			return r.handleDocBuiltinsCommand()
		case ":doc-special-forms":
			return r.handleDocSpecialFormsCommand()
		case ":doc-modules":
			return r.handleDocModulesCommand()
		case ":doc":
			// Show help for doc command when used without arguments
			fmt.Println("Usage: :doc <symbol>")
			fmt.Println("  Look up documentation for a specific symbol")
			fmt.Println("\nSee also:")
			fmt.Println("  :doc-builtins       - List all built-in functions")
			fmt.Println("  :doc-special-forms  - List all special forms")
			fmt.Println("  :doc-modules        - List all available modules")
			return true
		}
	}

	// Handle :doc <symbol> command
	if len(parts) >= 2 && parts[0] == ":doc" {
		symbol := parts[1]
		return r.showDocumentation(symbol)
	}

	// Command not recognized
	return false
}

// handleDocBuiltinsCommand handles the :doc-builtins command
func (r *REPL) handleDocBuiltinsCommand() bool {
	entries := core.ListByType("builtin-function")
	if len(entries) == 0 {
		fmt.Println("No documentation available for built-in functions.")
		return true
	}

	fmt.Println(core.FormatDocList(entries, "Built-in Functions"))
	return true
}

// handleDocSpecialFormsCommand handles the :doc-special-forms command
func (r *REPL) handleDocSpecialFormsCommand() bool {
	entries := core.ListByType("special-form")
	if len(entries) == 0 {
		fmt.Println("No documentation available for special forms.")
		return true
	}

	fmt.Println(core.FormatDocList(entries, "Special Forms"))
	return true
}

// handleDocModulesCommand handles the :doc-modules command
func (r *REPL) handleDocModulesCommand() bool {
	modules := core.ListModules()
	if len(modules) == 0 {
		fmt.Println("No modules documented.")
		return true
	}

	fmt.Println("=== Available Modules ===\n")
	for _, module := range modules {
		fmt.Println(module)
	}

	return true
}

// showDocumentation displays documentation for a symbol
func (r *REPL) showDocumentation(symbol string) bool {
	entry, found := core.GetDoc(symbol)
	if !found {
		fmt.Printf("No documentation available for '%s'.\n", symbol)
		return true
	}

	fmt.Println(core.FormatDocEntry(entry))
	return true
}

// searchHistory searches for a term in the command history
func (r *REPL) searchHistory(term string) {
	// Search for the term in history
	term = strings.ToLower(term)
	count := 0
	fmt.Printf("Searching history for '%s':\n", term)

	for i, entry := range r.history {
		entryStr := strings.ToLower(entry)
		if strings.Contains(entryStr, term) {
			fmt.Printf("%3d: %s\n", i+1, entry)
			count++
		}
	}

	if count == 0 {
		fmt.Printf("No history entries found containing '%s'\n", term)
	} else {
		fmt.Printf("\nFound %d matches\n", count)
		fmt.Println("\nTip: Use Ctrl+R for interactive history search")
	}
}

func (r *REPL) EvaluateString(input string) (core.LispValue, error) {
	// Register the REPL input for better error reporting
	replInputFile := "<repl>"
	core.RegisterSourceCode(replInputFile, input)

	// Set filename to special REPL indicator
	r.parser.SetFilename(replInputFile)

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
	// Save history before closing
	r.saveHistory()
	r.rl.Close()
}

// loadHistory loads the history from the history file
func (r *REPL) loadHistory() {
	if r.historyFile == "" {
		return // History is disabled
	}

	// Try to read the history file
	data, err := os.ReadFile(r.historyFile)
	if err != nil {
		// File may not exist yet, that's okay
		return
	}

	// Parse the lines
	lines := strings.Split(string(data), "\n")
	for _, line := range lines {
		if line = strings.TrimSpace(line); line != "" {
			r.history = append(r.history, line)
		}
	}

	// Trim history if needed
	if len(r.history) > r.historyLimit {
		r.history = r.history[len(r.history)-r.historyLimit:]
	}
}

// saveHistory saves the history to the history file
func (r *REPL) saveHistory() {
	if r.historyFile == "" {
		return // History is disabled
	}

	// Create the content
	var content strings.Builder
	for _, line := range r.history {
		content.WriteString(line)
		content.WriteString("\n")
	}

	// Write to the file
	err := os.WriteFile(r.historyFile, []byte(content.String()), 0644)
	if err != nil {
		fmt.Printf("Error saving history: %v\n", err)
	}
}

// addToHistory adds a command to the history
func (r *REPL) addToHistory(cmd string) {
	// Skip empty commands
	if strings.TrimSpace(cmd) == "" {
		return
	}

	// Add to our history
	r.history = append(r.history, cmd)

	// Trim if needed
	if len(r.history) > r.historyLimit {
		r.history = r.history[len(r.history)-r.historyLimit:]
	}
}

// GetEvaluator returns the REPL's evaluator
func (r *REPL) GetEvaluator() *eval.Evaluator {
	if e, ok := r.evaluator.(*eval.Evaluator); ok {
		return e
	}
	return nil
}

// StoreEvaluator stores the evaluator in the environment for object method calls
func (r *REPL) StoreEvaluator() {
	// Store the evaluator in the environment for class methods to access
	r.env.Define("EVALUATOR", r.evaluator)
}

// getAllSymbols returns all available symbols in the current environment
func (r *REPL) getAllSymbols() []string {
	symbols := []string{}

	// Get symbols from current environment
	if env, ok := r.env.(core.MappableEnvironment); ok {
		currentEnvSymbols := env.GetSymbolMap()
		for symbol := range currentEnvSymbols {
			symbols = append(symbols, string(symbol))
		}
	}

	// Add commonly used special forms
	specialForms := []string{
		"def", "fn", "if", "do", "let", "quote", "for", "while",
		"try", "except", "with", "class", "import", "from", "break", "continue",
		"return", "print", "list", "dict", "set", "get", "in", "not", "and", "or",
	}

	symbols = append(symbols, specialForms...)

	return symbols
}

// symbolCompleter implements the readline.AutoCompleter interface
type symbolCompleter struct {
	repl *REPL
}

// newCompleter creates a new completer for the REPL
func newCompleter(repl *REPL) *symbolCompleter {
	return &symbolCompleter{repl: repl}
}

// Do implements the readline.AutoCompleter interface
func (sc *symbolCompleter) Do(line []rune, pos int) (newLine [][]rune, length int) {
	// Get all symbols from the environment
	symbols := sc.repl.getAllSymbols()

	// Get the word we're completing
	lineStr := string(line[:pos])
	words := strings.Fields(lineStr)

	// If there are no words, we're completing at the beginning of the line
	if len(words) == 0 {
		return sc.matchSymbols("", symbols), 0
	}

	// Get the last word, which is what we're completing
	lastWord := words[len(words)-1]

	// Check for special cases like dot notation (obj.method)
	if strings.Contains(lastWord, ".") {
		parts := strings.Split(lastWord, ".")
		if len(parts) == 2 {
			// Get the object name and current attribute prefix
			objName := parts[0]
			attrPrefix := parts[1]

			// Try to find attributes for this object
			attrs := sc.getAttributesForObject(objName, attrPrefix)
			if len(attrs) > 0 {
				completions := make([][]rune, 0, len(attrs))
				for _, attr := range attrs {
					// Create the full completion with the object name
					fullCompletion := objName + "." + attr
					completions = append(completions, []rune(fullCompletion))
				}
				return completions, len(lastWord)
			}
		}
	}

	// Normal symbol completion
	return sc.matchSymbols(lastWord, symbols), len(lastWord)
}

// matchSymbols finds all symbols that match the given prefix
func (sc *symbolCompleter) matchSymbols(prefix string, symbols []string) [][]rune {
	var matches [][]rune
	for _, sym := range symbols {
		if strings.HasPrefix(sym, prefix) {
			matches = append(matches, []rune(sym))
		}
	}
	return matches
}

// getAttributesForObject tries to find attributes for a given object
func (sc *symbolCompleter) getAttributesForObject(objName, attrPrefix string) []string {
	// Currently, we don't have a good way to introspect object attributes
	// This is a placeholder for future implementation
	// For now, return an empty list
	return []string{}
}

// getHistoryFilePath returns the path to the history file, creating the directory if needed
// If noHistory is true, it returns an empty string to disable history
func getHistoryFilePath(noHistory bool) string {
	if noHistory {
		return "" // Empty string disables history
	}

	// Try to get the user's home directory
	homeDir, err := os.UserHomeDir()
	if err != nil {
		// Fallback to temporary directory if home directory is not available
		return "/tmp/m28_history"
	}

	// Create the .m28 directory in the user's home if it doesn't exist
	m28Dir := filepath.Join(homeDir, ".m28")
	err = os.MkdirAll(m28Dir, 0755)
	if err != nil {
		// Fallback to home directory directly if we can't create .m28 dir
		return filepath.Join(homeDir, ".m28_history")
	}

	// Return the path to the history file
	return filepath.Join(m28Dir, "history")
}
