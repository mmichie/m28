package repl

import (
	"bufio"
	"os"
	"path/filepath"
	"strings"
)

// History manages REPL command history
type History struct {
	commands   []string
	maxSize    int
	position   int
	file       string
	totalCount int // Track total commands ever entered
}

// NewHistory creates a new history manager
func NewHistory(maxSize int) *History {
	homeDir, _ := os.UserHomeDir()
	histFile := filepath.Join(homeDir, ".m28_history")

	h := &History{
		commands: make([]string, 0, maxSize),
		maxSize:  maxSize,
		position: 0,
		file:     histFile,
	}

	h.load()
	return h
}

// Add adds a command to history
func (h *History) Add(cmd string) {
	cmd = strings.TrimSpace(cmd)
	if cmd == "" {
		return
	}

	// Don't add duplicates of the last command
	if len(h.commands) > 0 && h.commands[len(h.commands)-1] == cmd {
		return
	}

	h.commands = append(h.commands, cmd)
	h.totalCount++

	// Trim history if it exceeds max size
	if len(h.commands) > h.maxSize {
		h.commands = h.commands[len(h.commands)-h.maxSize:]
	}

	h.position = len(h.commands)
	h.save()
}

// Previous returns the previous command in history
func (h *History) Previous() (string, bool) {
	if h.position > 0 && len(h.commands) > 0 {
		h.position--
		if h.position < len(h.commands) {
			return h.commands[h.position], true
		}
	}
	return "", false
}

// Next returns the next command in history
func (h *History) Next() (string, bool) {
	if h.position < len(h.commands)-1 {
		h.position++
		return h.commands[h.position], true
	}
	h.position = len(h.commands)
	return "", false
}

// Reset resets the history position
func (h *History) Reset() {
	h.position = len(h.commands)
}

// load loads history from file
func (h *History) load() {
	file, err := os.Open(h.file)
	if err != nil {
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line != "" {
			h.commands = append(h.commands, line)
		}
	}

	// Trim to max size
	if len(h.commands) > h.maxSize {
		h.commands = h.commands[len(h.commands)-h.maxSize:]
	}

	h.position = len(h.commands)
	h.totalCount = len(h.commands) // Initialize total count from loaded history
}

// save saves history to file
func (h *History) save() {
	file, err := os.Create(h.file)
	if err != nil {
		return
	}
	defer file.Close()

	writer := bufio.NewWriter(file)
	defer writer.Flush()

	for _, cmd := range h.commands {
		writer.WriteString(cmd + "\n")
	}
}

// GetCommand returns the command at the given position (1-based)
func (h *History) GetCommand(position int) string {
	// Convert from 1-based to 0-based index
	idx := position - 1
	if idx >= 0 && idx < len(h.commands) {
		return h.commands[idx]
	}
	return ""
}

// FindByPrefix finds the most recent command starting with the given prefix
func (h *History) FindByPrefix(prefix string) string {
	// Search backwards for most recent match
	for i := len(h.commands) - 1; i >= 0; i-- {
		if strings.HasPrefix(h.commands[i], prefix) {
			return h.commands[i]
		}
	}
	return ""
}

// GetLastN returns the last n commands from history
func (h *History) GetLastN(n int) []string {
	if n <= 0 {
		return nil
	}

	start := len(h.commands) - n
	if start < 0 {
		start = 0
	}

	return h.commands[start:]
}

// GetTotalCount returns the total number of commands ever entered
func (h *History) GetTotalCount() int {
	return h.totalCount
}
