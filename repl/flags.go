package repl

import (
	"flag"
	"fmt"
	"os"
)

// CommandFlags holds the parsed command line arguments
type CommandFlags struct {
	ShowHelp    bool
	EvalCode    string
	Command     string
	Filenames   []string
	Interactive bool
}

// ParseFlags parses command line arguments and returns the configuration
func ParseFlags() *CommandFlags {
	// Create a custom FlagSet to have more control over the behavior
	flagSet := flag.NewFlagSet("m28", flag.ContinueOnError)

	// Define flags
	help := flagSet.Bool("h", false, "Display help information")
	helpLong := flagSet.Bool("help", false, "Display help information")
	eval := flagSet.String("e", "", "Evaluate string and print result")
	evalLong := flagSet.String("eval", "", "Evaluate string and print result")
	cmd := flagSet.String("c", "", "Execute single command (no printing of result)")
	cmdLong := flagSet.String("command", "", "Execute single command (no printing of result)")
	interactive := flagSet.Bool("i", false, "Enter interactive mode after executing commands/files")

	// Set custom usage
	flagSet.Usage = displayHelp

	// Parse the flags
	err := flagSet.Parse(os.Args[1:])
	if err != nil {
		if err == flag.ErrHelp {
			displayHelp()
			os.Exit(0)
		}
		fmt.Fprintf(os.Stderr, "Error parsing flags: %v\n", err)
		os.Exit(1)
	}

	// Collect the unparsed arguments (considered as filenames)
	filenames := flagSet.Args()

	// Combine short and long flag variants
	showHelp := *help || *helpLong
	evalString := *eval
	if evalString == "" {
		evalString = *evalLong
	}
	cmdString := *cmd
	if cmdString == "" {
		cmdString = *cmdLong
	}

	// If help is requested, show help and exit
	if showHelp {
		displayHelp()
		os.Exit(0)
	}

	return &CommandFlags{
		ShowHelp:    showHelp,
		EvalCode:    evalString,
		Command:     cmdString,
		Filenames:   filenames,
		Interactive: *interactive,
	}
}

// displayHelp shows usage information
func displayHelp() {
	help := `Usage: m28 [options] [file]

Options:
  -h, --help             Display this help message
  -e, --eval CODE        Evaluate M28 expression and print result
  -c, --command CODE     Execute M28 command (no result printing)
  -i                     Enter interactive mode after executing commands/files

Examples:
  m28                    Start interactive REPL
  m28 file.m28           Execute M28 code from file
  m28 -e "(+ 2 3)"       Evaluate expression and print result
  m28 -c "(print \"Hello\")" Execute command without printing result
  m28 -i file.m28        Execute file then enter interactive mode

M28 is a Lisp interpreter with Python-inspired syntax and features.
`
	fmt.Print(help)
}
