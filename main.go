package main

import (
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/mmichie/m28/builtin"
	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/eval"
	"github.com/mmichie/m28/parser"
	"github.com/mmichie/m28/repl"
	"github.com/mmichie/m28/special_forms"
)

// Command line flags
var (
	evalExpr     = flag.String("e", "", "Evaluate expression")
	showHelp     = flag.Bool("help", false, "Show help")
	showVersion  = flag.Bool("version", false, "Show version")
	debugMode    = flag.Bool("debug", false, "Enable debug mode")
	modulePath   = flag.String("path", "", "Additional module search paths (colon-separated)")
	interactive  = flag.Bool("i", false, "Enter interactive mode after running file")
	command      = flag.String("c", "", "Execute program passed as string")
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

	// Set up module paths
	setupModulePaths()

	// Initialize the global context with built-in values and functions
	initializeGlobalContext(globalCtx)

	// Register all special forms
	special_forms.RegisterAllForms()

	// Create error reporter for enhanced error messages
	errorReporter := repl.NewErrorReporter()

	// Evaluate an expression from -e
	if *evalExpr != "" {
		errorReporter.AddSource("<command-line>", *evalExpr)
		result, err := eval.EvalString(*evalExpr, globalCtx)
		if err != nil {
			errorReporter.ReportError(err, globalCtx, os.Stderr)
			os.Exit(1)
		}
		fmt.Println(core.PrintValue(result))
		if !*interactive {
			return
		}
	}

	// Execute program from -c
	if *command != "" {
		errorReporter.AddSource("<command-line>", *command)
		result, err := eval.EvalString(*command, globalCtx)
		if err != nil {
			errorReporter.ReportError(err, globalCtx, os.Stderr)
			os.Exit(1)
		}
		fmt.Println(core.PrintValue(result))
		if !*interactive {
			return
		}
	}

	// Get the file to execute from positional arguments
	args := flag.Args()
	if len(args) > 0 {
		// Store script arguments in sys.argv
		argv := make(core.ListValue, len(args))
		for i, arg := range args {
			argv[i] = core.StringValue(arg)
		}
		globalCtx.Define("ARGV", argv)

		// Execute the file
		err := executeFile(args[0], globalCtx, errorReporter)
		if err != nil {
			errorReporter.ReportError(err, globalCtx, os.Stderr)
			os.Exit(1)
		}
		
		if !*interactive {
			return
		}
	}

	// Start the REPL (either no args or -i flag)
	r := repl.NewREPL(globalCtx)
	r.Start()
}

// printHelp prints the usage instructions
func printHelp() {
	fmt.Println("Usage: m28 [options] [file] [args...]")
	fmt.Println()
	fmt.Println("Options:")
	fmt.Println("  -e EXPR      Evaluate expression")
	fmt.Println("  -c PROGRAM   Execute program string")
	fmt.Println("  -i           Interactive mode after file execution")
	fmt.Println("  -path PATHS  Additional module search paths (colon-separated)")
	fmt.Println("  -debug       Enable debug mode")
	fmt.Println("  -version     Show version")
	fmt.Println("  -help        Show this help")
	fmt.Println()
	fmt.Println("Environment Variables:")
	fmt.Println("  M28_PATH     Module search paths (colon-separated)")
	fmt.Println()
	fmt.Println("Examples:")
	fmt.Println("  m28                    Start interactive REPL")
	fmt.Println("  m28 script.m28         Run a script")
	fmt.Println("  m28 -e '(+ 1 2)'       Evaluate expression")
	fmt.Println("  m28 -i script.m28      Run script then enter REPL")
}

// setupModulePaths sets up the module search paths
func setupModulePaths() {
	var paths []string

	// Add current directory
	paths = append(paths, ".")

	// Add paths from environment variable
	if envPath := os.Getenv("M28_PATH"); envPath != "" {
		paths = append(paths, strings.Split(envPath, ":")...)
	}

	// Add paths from command line
	if *modulePath != "" {
		paths = append(paths, strings.Split(*modulePath, ":")...)
	}

	// Add standard library path (relative to executable)
	if exePath, err := os.Executable(); err == nil {
		stdlibPath := filepath.Join(filepath.Dir(exePath), "..", "lib", "m28")
		paths = append(paths, stdlibPath)
	}

	// Set the module paths
	core.SetModulePaths(paths)
}

// initializeGlobalContext sets up the global context with built-in values and functions
func initializeGlobalContext(ctx *core.Context) {
	// Register built-in values
	ctx.Define("true", core.BoolValue(true))
	ctx.Define("false", core.BoolValue(false))
	ctx.Define("nil", core.NilValue{})

	// Register all built-in functions
	builtin.RegisterAllBuiltins(ctx)
	
	// Initialize module loader
	moduleLoader := core.NewDefaultModuleLoader(ctx, eval.Eval, func(code string) (core.Value, error) {
		p := parser.NewParser()
		return p.Parse(code)
	})
	core.SetModuleLoader(moduleLoader)
}

// executeFile executes a file with the given context
func executeFile(filename string, ctx *core.Context, errorReporter *repl.ErrorReporter) error {
	// Read the file content
	content, err := os.ReadFile(filename)
	if err != nil {
		return fmt.Errorf("error reading file: %v", err)
	}

	// Add source to error reporter
	errorReporter.AddSource(filename, string(content))

	// Create a parser
	p := parser.NewParser()
	p.SetFilename(filename)

	// Parse the content
	expr, err := p.Parse(string(content))
	if err != nil {
		return fmt.Errorf("parse error: %v", err)
	}

	// Set __file__ in context
	ctx.Define("__file__", core.StringValue(filename))
	ctx.Define("__name__", core.StringValue("__main__"))

	// Evaluate the expressions
	_, err = eval.Eval(expr, ctx)
	return err
}