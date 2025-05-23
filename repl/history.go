package repl

import (
	"bufio"
	"os"
	"path/filepath"
	"strings"
)

// History manages REPL command history
type History struct {
	commands []string
	maxSize  int
	position int
	file     string
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