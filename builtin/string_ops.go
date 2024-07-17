package builtin

import (
	"fmt"
	"strings"

	"github.com/mmichie/m28/core"
)

func RegisterStringOps() {
	core.RegisterBuiltin("string-append", stringAppend)
}

func stringAppend(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	var parts []string
	for _, arg := range args {
		switch v := arg.(type) {
		case string:
			parts = append(parts, v)
		case core.LispSymbol:
			parts = append(parts, string(v))
		default:
			parts = append(parts, fmt.Sprint(v))
		}
	}
	return strings.Join(parts, ""), nil
}
