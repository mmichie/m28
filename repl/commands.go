package repl

import (
	"fmt"
	"io"
	"strconv"
	"strings"
)

// CommandHandler handles REPL-specific commands
type CommandHandler struct {
	repl            *REPL
	settings        *REPLSettings
	commandToExecute string // Command to execute after handling
}

// REPLSettings stores configurable REPL settings
type REPLSettings struct {
	KeybindingMode string // "emacs" or "vi"
	ColorsEnabled  bool
	HistorySize    int
}

// NewCommandHandler creates a new command handler
func NewCommandHandler(repl *REPL) *CommandHandler {
	return &CommandHandler{
		repl: repl,
		settings: &REPLSettings{
			KeybindingMode: "emacs",
			ColorsEnabled:  true,
			HistorySize:    1000,
		},
	}
}

// IsCommand checks if the input is a REPL command
func (ch *CommandHandler) IsCommand(input string) bool {
	return strings.HasPrefix(input, ":") || strings.HasPrefix(input, "!")
}

// HandleCommand processes a REPL command and returns whether it was handled
func (ch *CommandHandler) HandleCommand(input string) (bool, error) {
	input = strings.TrimSpace(input)
	ch.commandToExecute = "" // Reset
	
	// Handle : commands
	if strings.HasPrefix(input, ":") {
		return ch.handleColonCommand(input[1:])
	}
	
	// Handle ! commands (history execution)
	if strings.HasPrefix(input, "!") {
		return ch.handleBangCommand(input[1:])
	}
	
	return false, nil
}

// GetCommandToExecute returns the command to execute after handling
func (ch *CommandHandler) GetCommandToExecute() string {
	return ch.commandToExecute
}

// handleColonCommand handles commands that start with :
func (ch *CommandHandler) handleColonCommand(cmd string) (bool, error) {
	parts := strings.Fields(cmd)
	if len(parts) == 0 {
		return false, nil
	}
	
	command := parts[0]
	args := parts[1:]
	
	switch command {
	case "help":
		ch.showCommandHelp(ch.repl.writer)
		return true, nil
		
	case "history":
		return ch.handleHistoryCommand(args)
		
	case "toggle-keybindings":
		return ch.toggleKeybindings()
		
	case "toggle-colors":
		return ch.toggleColors()
		
	case "settings":
		ch.showSettings(ch.repl.writer)
		return true, nil
		
	case "clear":
		// Clear screen (ANSI escape sequence)
		fmt.Fprint(ch.repl.writer, "\033[2J\033[H")
		return true, nil
		
	case "reset":
		// Reset execution counter
		ch.repl.executionState.executionCount = 0
		fmt.Fprintln(ch.repl.writer, "Execution counter reset.")
		return true, nil
		
	default:
		fmt.Fprintf(ch.repl.writer, "Unknown command: :%s\n", command)
		fmt.Fprintln(ch.repl.writer, "Type :help for available commands.")
		return true, nil
	}
}

// handleBangCommand handles ! history commands
func (ch *CommandHandler) handleBangCommand(cmd string) (bool, error) {
	if cmd == "" {
		return false, nil
	}
	
	// Handle !! (repeat last command)
	if cmd == "!" {
		if lastCmd, ok := ch.repl.history.Previous(); ok {
			ch.repl.history.Next() // Reset position
			ch.commandToExecute = lastCmd
			return false, nil // Return false to let the main loop evaluate it
		}
		fmt.Fprintln(ch.repl.writer, "No commands in history.")
		return true, nil
	}
	
	// Handle !n (execute command n from history)
	if num, err := strconv.Atoi(cmd); err == nil {
		if histCmd := ch.repl.history.GetCommand(num); histCmd != "" {
			ch.commandToExecute = histCmd
			return false, nil // Return false to let the main loop evaluate it
		}
		fmt.Fprintf(ch.repl.writer, "No command at position %d in history.\n", num)
		return true, nil
	}
	
	// Handle !prefix (execute most recent command starting with prefix)
	if histCmd := ch.repl.history.FindByPrefix(cmd); histCmd != "" {
		ch.commandToExecute = histCmd
		return false, nil // Return false to let the main loop evaluate it
	}
	
	fmt.Fprintf(ch.repl.writer, "No command starting with '%s' found in history.\n", cmd)
	return true, nil
}

// handleHistoryCommand handles the :history command
func (ch *CommandHandler) handleHistoryCommand(args []string) (bool, error) {
	limit := 10 // Default to showing last 10 commands
	
	if len(args) > 0 {
		if num, err := strconv.Atoi(args[0]); err == nil {
			limit = num
		}
	}
	
	commands := ch.repl.history.GetLastN(limit)
	if len(commands) == 0 {
		fmt.Fprintln(ch.repl.writer, "No commands in history.")
		return true, nil
	}
	
	fmt.Fprintln(ch.repl.writer, "Command History:")
	startNum := ch.repl.history.GetTotalCount() - len(commands) + 1
	for i, cmd := range commands {
		fmt.Fprintf(ch.repl.writer, "%4d  %s\n", startNum+i, cmd)
	}
	
	return true, nil
}

// toggleKeybindings toggles between emacs and vi keybinding modes
func (ch *CommandHandler) toggleKeybindings() (bool, error) {
	if ch.settings.KeybindingMode == "emacs" {
		ch.settings.KeybindingMode = "vi"
	} else {
		ch.settings.KeybindingMode = "emacs"
	}
	
	fmt.Fprintf(ch.repl.writer, "Keybinding mode set to: %s\n", ch.settings.KeybindingMode)
	fmt.Fprintln(ch.repl.writer, "Note: Keybinding support requires readline integration (not yet implemented).")
	return true, nil
}

// toggleColors toggles colored output
func (ch *CommandHandler) toggleColors() (bool, error) {
	ch.settings.ColorsEnabled = !ch.settings.ColorsEnabled
	ch.repl.colorManager.SetEnabled(ch.settings.ColorsEnabled)
	
	status := "disabled"
	if ch.settings.ColorsEnabled {
		status = "enabled"
	}
	
	fmt.Fprintf(ch.repl.writer, "Colored output %s.\n", status)
	return true, nil
}

// showSettings displays current REPL settings
func (ch *CommandHandler) showSettings(w io.Writer) {
	fmt.Fprintln(w, "Current REPL Settings:")
	fmt.Fprintf(w, "  Keybinding mode: %s\n", ch.settings.KeybindingMode)
	fmt.Fprintf(w, "  Colors enabled: %v\n", ch.settings.ColorsEnabled)
	fmt.Fprintf(w, "  History size: %d\n", ch.settings.HistorySize)
	fmt.Fprintf(w, "  Execution count: %d\n", ch.repl.executionState.GetExecutionNumber())
}

// showCommandHelp displays help for REPL commands
func (ch *CommandHandler) showCommandHelp(w io.Writer) {
	fmt.Fprintln(w, "REPL Commands:")
	fmt.Fprintln(w, "  :help                  - Show this help")
	fmt.Fprintln(w, "  :history [n]           - Show last n commands (default: 10)")
	fmt.Fprintln(w, "  :toggle-keybindings    - Toggle between emacs and vi modes")
	fmt.Fprintln(w, "  :toggle-colors         - Toggle colored output")
	fmt.Fprintln(w, "  :settings              - Show current REPL settings")
	fmt.Fprintln(w, "  :clear                 - Clear the screen")
	fmt.Fprintln(w, "  :reset                 - Reset execution counter")
	fmt.Fprintln(w, "")
	fmt.Fprintln(w, "History Commands:")
	fmt.Fprintln(w, "  !!                     - Execute the last command")
	fmt.Fprintln(w, "  !n                     - Execute command number n")
	fmt.Fprintln(w, "  !prefix                - Execute most recent command starting with prefix")
	fmt.Fprintln(w, "")
	fmt.Fprintln(w, "Type 'help' for language help.")
}