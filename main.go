package main

import (
	"flag"
	"fmt"
	"os"

	"github.com/mmichie/m28/builtin"
	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/eval"
	"github.com/mmichie/m28/parser"
	"github.com/mmichie/m28/repl"
	"github.com/mmichie/m28/special_forms"
)

// Command line flags
var (
	evalExpr    = flag.String("e", "", "Evaluate expression")
	showHelp    = flag.Bool("help", false, "Show help")
	showVersion = flag.Bool("version", false, "Show version")
	debugMode   = flag.Bool("debug", false, "Enable debug mode")
)

const version = "0.1.0-fresh-start"

func main() {
	// Parse command line flags
	flag.Parse()

	// Show help
	if *showHelp {
		printHelp()
		return
	}

	// Show version
	if *showVersion {
		fmt.Printf("M28 version %s\n", version)
		return
	}

	// Create the global context
	globalCtx := core.NewContext(nil)

	// Initialize the global context with built-in values and functions
	initializeGlobalContext(globalCtx)

	// Register all special forms
	special_forms.RegisterAllForms()

	// Evaluate an expression
	if *evalExpr != "" {
		result, err := eval.EvalString(*evalExpr, globalCtx)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error: %v\n", err)
			os.Exit(1)
		}
		fmt.Println(core.PrintValue(result))
		return
	}

	// Get the file to execute from positional arguments
	args := flag.Args()
	if len(args) > 0 {
		err := executeFile(args[0], globalCtx)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error: %v\n", err)
			os.Exit(1)
		}
		return
	}

	// No file or expression provided, start the REPL
	r := repl.NewREPL(globalCtx)
	r.Start()
}

// printHelp prints the usage instructions
func printHelp() {
	fmt.Println("Usage: m28 [options] [file]")
	fmt.Println("Options:")
	flag.PrintDefaults()
}

// initializeGlobalContext sets up the global context with built-in values and functions
func initializeGlobalContext(ctx *core.Context) {
	// Register built-in values
	ctx.Define("true", core.BoolValue(true))
	ctx.Define("false", core.BoolValue(false))
	ctx.Define("nil", core.NilValue{})

	// Register all built-in functions
	builtin.RegisterAllBuiltins(ctx)
}

// executeFile executes a file with the given context
func executeFile(filename string, ctx *core.Context) error {
	// Read the file content
	content, err := os.ReadFile(filename)
	if err != nil {
		return fmt.Errorf("error reading file: %v", err)
	}

	// Create a parser
	p := parser.NewParser()
	p.SetFilename(filename)

	// Parse the content
	expr, err := p.Parse(string(content))
	if err != nil {
		return fmt.Errorf("parse error: %v", err)
	}

	// Evaluate the expressions
	_, err = eval.Eval(expr, ctx)
	if err != nil {
		return fmt.Errorf("evaluation error: %v", err)
	}

	return nil
}
