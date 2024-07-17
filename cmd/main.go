package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/mmichie/m28"
)

func main() {
	interpreter := m28.New()
	args := os.Args[1:]

	if len(args) == 0 {
		interpreter.REPL()
		return
	}

	ext := filepath.Ext(args[0])
	if ext == ".m28" || ext == ".lisp" {
		err := interpreter.ExecuteFile(args[0])
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error executing file: %v\n", err)
			os.Exit(1)
		}
		return
	}

	result, err := interpreter.Execute(strings.Join(args, " "))
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
	fmt.Println(result)
}
