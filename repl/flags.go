package repl

import (
	"flag"
	"fmt"
	"os"
)

// CommandFlags holds the parsed command line arguments
type CommandFlags struct {
	ShowHelp      bool
	EvalCode      string
	Command       string
	Filenames     []string
	Interactive   bool
	HistorySize   int
	NoHistoryFile bool
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
	historySize := flagSet.Int("history-size", 1000, "Maximum number of entries to store in history file")
	noHistory := flagSet.Bool("no-history", false, "Disable history file")

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
		ShowHelp:      showHelp,
		EvalCode:      evalString,
		Command:       cmdString,
		Filenames:     filenames,
		Interactive:   *interactive,
		HistorySize:   *historySize,
		NoHistoryFile: *noHistory,
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
  --history-size N       Maximum number of entries to store in history (default: 1000)
  --no-history           Disable history file

Examples:
  m28                    Start interactive REPL
  m28 file.m28           Execute M28 code from file
  m28 -e "(+ 2 3)"       Evaluate expression and print result
  m28 -c "(print \"Hello\")" Execute command without printing result
  m28 -i file.m28        Execute file then enter interactive mode
  m28 --history-size 2000 Start REPL with increased history capacity

M28 is a Lisp interpreter with Python-inspired syntax and features.
`
	fmt.Print(help)
}
