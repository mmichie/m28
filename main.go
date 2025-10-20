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
	"github.com/mmichie/m28/modules"
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
	modulePath  = flag.String("path", "", "Additional module search paths (colon-separated)")
	interactive = flag.Bool("i", false, "Enter interactive mode after running file")
	command     = flag.String("c", "", "Execute program passed as string")
	parseOnly   = flag.Bool("parse", false, "Parse only, print AST and exit")
	printAST    = flag.Bool("ast", false, "Print AST (same as -parse)")
	pythonMode  = flag.Bool("python", false, "Enable Python mode for REPL")
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
	colorManager := repl.NewColorManager(true)
	errorReporter := repl.NewErrorReporter(colorManager)

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
		argvSlice := make([]core.Value, len(args))
		for i, arg := range args {
			argvSlice[i] = core.StringValue(arg)
		}
		globalCtx.Define("ARGV", core.NewList(argvSlice...))

		// Parse-only mode: just parse and print AST
		if *parseOnly || *printAST {
			err := parseAndPrintAST(args[0])
			if err != nil {
				fmt.Fprintf(os.Stderr, "Error: %v\n", err)
				os.Exit(1)
			}
			return
		}

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
	if *pythonMode {
		r.SetPythonMode(true)
	}
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
	fmt.Println("  -python      Enable Python mode for REPL")
	fmt.Println("  -parse       Parse file and print AST (don't execute)")
	fmt.Println("  -ast         Print AST (same as -parse)")
	fmt.Println("  -path PATHS  Additional module search paths (colon-separated)")
	fmt.Println("  -debug       Enable debug mode")
	fmt.Println("  -version     Show version")
	fmt.Println("  -help        Show this help")
	fmt.Println()
	fmt.Println("Supported File Types:")
	fmt.Println("  .m28         M28 S-expression syntax")
	fmt.Println("  .py          Python syntax (experimental)")
	fmt.Println()
	fmt.Println("Environment Variables:")
	fmt.Println("  M28_PATH     Module search paths (colon-separated)")
	fmt.Println()
	fmt.Println("Examples:")
	fmt.Println("  m28                    Start interactive REPL (M28 syntax)")
	fmt.Println("  m28 -python            Start interactive REPL (Python syntax)")
	fmt.Println("  m28 script.m28         Run M28 script")
	fmt.Println("  m28 script.py          Run Python script")
	fmt.Println("  m28 -e '(+ 1 2)'       Evaluate expression")
	fmt.Println("  m28 -i script.m28      Run script then enter REPL")
	fmt.Println("  m28 -parse script.py   Parse and show AST")
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
	ctx.Define("True", core.BoolValue(true))
	ctx.Define("False", core.BoolValue(false))
	ctx.Define("nil", core.NilValue{})

	// Register all built-in functions
	builtin.RegisterAllBuiltins(ctx)

	// Initialize enhanced module loader with builtin Go modules
	moduleLoader := core.NewModuleLoaderEnhanced(ctx, eval.Eval, func(code string) (core.Value, error) {
		p := parser.NewParser()
		return p.Parse(code)
	}, modules.GetBuiltinModule)
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

	// Detect file type and parse accordingly
	ext := filepath.Ext(filename)
	isPython := ext == ".py"

	// Set __file__ and __name__ in context
	ctx.Define("__file__", core.StringValue(filename))
	ctx.Define("__name__", core.StringValue("__main__"))

	if isPython {
		// Use Python parser for .py files
		return executePythonFile(filename, string(content), ctx)
	}

	// Use M28 parser for .m28 files (default)
	return executeM28File(filename, string(content), ctx)
}

// executeM28File executes an M28 (.m28) file
func executeM28File(filename, content string, ctx *core.Context) error {
	// Create M28 parser
	p := parser.NewParser()
	p.SetFilename(filename)

	// Parse the content
	expr, err := p.Parse(content)
	if err != nil {
		// Check if it's a ParseError and format with context
		if parseErr, ok := err.(*parser.ParseError); ok {
			fmt.Fprintln(os.Stderr, parseErr.FormatWithContext())
			os.Exit(1)
		}
		return fmt.Errorf("parse error: %v", err)
	}

	// Evaluate the expression
	_, err = eval.Eval(expr, ctx)
	return err
}

// executePythonFile executes a Python (.py) file
func executePythonFile(filename, content string, ctx *core.Context) error {
	// Tokenize Python source
	tokenizer := parser.NewPythonTokenizer(content)
	tokens, err := tokenizer.Tokenize()
	if err != nil {
		return fmt.Errorf("tokenization error in %s: %v", filename, err)
	}

	// Parse Python tokens into AST
	pythonParser := parser.NewPythonParser(tokens)
	nodes, err := pythonParser.Parse()
	if err != nil {
		return fmt.Errorf("parse error in %s: %v", filename, err)
	}

	// Lower AST to IR and evaluate each statement
	for _, node := range nodes {
		ir := node.ToIR()
		_, err = eval.Eval(ir, ctx)
		if err != nil {
			return err
		}
	}

	return nil
}

// parseAndPrintAST parses a file and prints its AST representation
func parseAndPrintAST(filename string) error {
	// Read the file content
	content, err := os.ReadFile(filename)
	if err != nil {
		return fmt.Errorf("error reading file: %v", err)
	}

	// Detect file type
	ext := filepath.Ext(filename)
	isPython := ext == ".py"

	if isPython {
		// Parse Python file
		tokenizer := parser.NewPythonTokenizer(string(content))
		tokens, err := tokenizer.Tokenize()
		if err != nil {
			return fmt.Errorf("tokenization error: %v", err)
		}

		pythonParser := parser.NewPythonParser(tokens)
		nodes, err := pythonParser.Parse()
		if err != nil {
			return fmt.Errorf("parse error: %v", err)
		}

		// Print AST nodes
		fmt.Printf("=== Python AST for %s ===\n", filename)
		for i, node := range nodes {
			fmt.Printf("Statement %d:\n", i+1)
			fmt.Printf("  Type: %T\n", node)
			fmt.Printf("  AST: %s\n", node.String())
			ir := node.ToIR()
			fmt.Printf("  IR: %s\n", core.PrintValue(ir))
			fmt.Println()
		}
	} else {
		// Parse M28 file
		p := parser.NewParser()
		p.SetFilename(filename)
		expr, err := p.Parse(string(content))
		if err != nil {
			return fmt.Errorf("parse error: %v", err)
		}

		// Print AST
		fmt.Printf("=== M28 AST for %s ===\n", filename)
		fmt.Println(core.PrintValue(expr))
	}

	return nil
}
