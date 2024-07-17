package repl

import (
	"bufio"
	"fmt"
	"os"
	"strings"

	"github.com/mmichie/m28/core"
)

func (r *REPL) ExecuteFile(filename string) error {
	file, err := os.Open(filename)
	if err != nil {
		return fmt.Errorf("error opening file: %v", err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var input strings.Builder

	for scanner.Scan() {
		line := scanner.Text()
		input.WriteString(line)
		input.WriteString("\n")

		if strings.Count(input.String(), "(") == strings.Count(input.String(), ")") {
			result, err := r.EvaluateString(input.String())
			if err != nil {
				return fmt.Errorf("error evaluating expression: %v", err)
			}
			if result != nil {
				fmt.Println(core.PrintValue(result))
			}
			input.Reset()
		}
	}

	if err := scanner.Err(); err != nil {
		return fmt.Errorf("error reading file: %v", err)
	}

	return nil
}
