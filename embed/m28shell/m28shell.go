package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/chzyer/readline"
	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/embed"
)

const (
	rcFileName = ".m28rc"
)

func main() {
	// Create a new M28 engine
	engine := embed.NewM28Engine()

	// Setup custom shell command handling
	engine.ShellExecutor = executeShellCommand

	// Attempt to load RC file
	loadRCFile(engine)

	// Setup readline for nice shell experience
	rl, err := readline.NewEx(&readline.Config{
		Prompt:          "m28sh> ",
		HistoryFile:     filepath.Join(os.Getenv("HOME"), ".m28sh_history"),
		InterruptPrompt: "^C",
		EOFPrompt:       "exit",
	})
	if err != nil {
		fmt.Println("Error setting up shell:", err)
		os.Exit(1)
	}
	defer rl.Close()

	fmt.Println("M28 Shell - Type 'exit' to quit")
	
	// Main shell loop
	for {
		line, err := rl.Readline()
		if err != nil {
			break
		}

		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}

		if line == "exit" {
			break
		}

		// Handle shell command shorthands (starting with !)
		if strings.HasPrefix(line, "!") {
			shellCmd := strings.TrimPrefix(line, "!")
			output, err := executeShellCommand(shellCmd)
			if err != nil {
				fmt.Printf("Error: %v\n", err)
			} else {
				fmt.Print(output)
			}
			continue
		}

		// Evaluate the line as M28 code
		result, err := engine.Evaluate(line)
		if err != nil {
			fmt.Printf("Error: %v\n", err)
		} else if _, ok := result.(core.PythonicNone); !ok {
			// Only print the result if it's not None
			fmt.Println(core.PrintValue(result))
		}
	}
}

// executeShellCommand runs a shell command and returns its output
func executeShellCommand(command string) (string, error) {
	// Process special shell built-ins here if needed
	// For example: cd, etc.

	// For standard commands, use the default executor
	return embed.DefaultShellExecutor(command)
}

// loadRCFile attempts to load the user's RC file
func loadRCFile(engine *embed.M28Engine) {
	// Try user's home directory first
	homeDir, err := os.UserHomeDir()
	if err == nil {
		rcPath := filepath.Join(homeDir, rcFileName)
		if fileExists(rcPath) {
			err := engine.ExecuteFile(rcPath)
			if err != nil {
				fmt.Printf("Error loading %s: %v\n", rcPath, err)
			}
			return
		}
	}

	// Try current directory
	currentDir, err := os.Getwd()
	if err == nil {
		rcPath := filepath.Join(currentDir, rcFileName)
		if fileExists(rcPath) {
			err := engine.ExecuteFile(rcPath)
			if err != nil {
				fmt.Printf("Error loading %s: %v\n", rcPath, err)
			}
		}
	}
}

// fileExists checks if a file exists
func fileExists(path string) bool {
	_, err := os.Stat(path)
	return err == nil
}