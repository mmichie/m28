package builtin

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strings"

	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/parser"
)

func RegisterIOOps() {
	core.RegisterBuiltin("print", printFunc)
	core.RegisterBuiltin("read", readFunc)
	core.RegisterBuiltin("princ", princFunc)
	core.RegisterBuiltin("terpri", terpriFun)
	core.RegisterBuiltin("read-line", readLineFunc)
	core.RegisterBuiltin("write", writeFunc)
	core.RegisterBuiltin("fresh-line", freshLineFunc)
}

func printFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	for _, arg := range args {
		fmt.Print(core.PrintValue(arg), " ")
	}
	fmt.Println()
	return args[len(args)-1], nil // Return the last printed value
}

func readFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) > 1 {
		return nil, fmt.Errorf("read: too many arguments")
	}

	var input string
	if len(args) == 0 {
		fmt.Print("> ")
		_, err := fmt.Scanln(&input)
		if err != nil {
			return nil, err
		}
	} else {
		stream, ok := args[0].(io.Reader)
		if !ok {
			return nil, fmt.Errorf("read: invalid stream argument")
		}
		scanner := bufio.NewScanner(stream)
		if scanner.Scan() {
			input = scanner.Text()
		} else {
			return nil, scanner.Err()
		}
	}

	p := parser.NewParser()
	return p.Parse(input)
}

func princFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	for _, arg := range args {
		fmt.Print(core.PrintValueWithoutQuotes(arg))
	}
	return args[len(args)-1], nil
}

func terpriFun(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) > 0 {
		return nil, fmt.Errorf("terpri: too many arguments")
	}
	fmt.Println()
	return nil, nil
}

func readLineFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) > 0 {
		return nil, fmt.Errorf("read-line: too many arguments")
	}
	reader := bufio.NewReader(os.Stdin)
	line, err := reader.ReadString('\n')
	if err != nil {
		return nil, err
	}
	return strings.TrimSpace(line), nil
}

func writeFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) == 0 {
		return nil, fmt.Errorf("write: missing argument")
	}
	fmt.Print(core.PrintValueWithoutQuotes(args[0]))
	return args[0], nil
}

func freshLineFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) > 0 {
		return nil, fmt.Errorf("fresh-line: too many arguments")
	}
	fmt.Println()
	return core.LispSymbol("t"), nil
}
