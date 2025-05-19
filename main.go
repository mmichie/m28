package main

import (
	"flag"
	"fmt"
	"os"
	
	"m28/core"
	"m28/repl"
)

// Command line flags
var (
	evalExpr   = flag.String("e", "", "Evaluate expression")
	showHelp   = flag.Bool("help", false, "Show help")
	showVersion = flag.Bool("version", false, "Show version")
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
	
	// Evaluate an expression
	if *evalExpr != "" {
		// TODO: Implement expression evaluation
		fmt.Println("Expression evaluation not implemented yet")
		return
	}
	
	// Get the file to execute from positional arguments
	args := flag.Args()
	if len(args) > 0 {
		// TODO: Implement file execution
		fmt.Println("File execution not implemented yet")
		return
	}
	
	// No file or expression provided, start the REPL
	repl := repl.NewREPL(globalCtx)
	repl.Start()
}

// printHelp prints the usage instructions
func printHelp() {
	fmt.Println("Usage: m28 [options] [file]")
	fmt.Println("Options:")
	flag.PrintDefaults()
}

// initializeGlobalContext sets up the global context with built-in values and functions
func initializeGlobalContext(ctx *core.Context) {
	// Add built-in values
	ctx.Define("true", core.True)
	ctx.Define("false", core.False)
	ctx.Define("nil", core.Nil)
	
	// Add built-in functions
	ctx.Define("print", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		for i, arg := range args {
			if i > 0 {
				fmt.Print(" ")
			}
			fmt.Print(arg.String())
		}
		fmt.Println()
		return core.Nil, nil
	}))
}