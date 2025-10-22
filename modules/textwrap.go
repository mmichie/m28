package modules

import (
	"strings"

	"github.com/mmichie/m28/core"
)

// InitTextwrapModule creates a minimal textwrap module stub
// This is needed because Python's textwrap.py uses regex features (lookbehind)
// that Go's regexp package doesn't support
func InitTextwrapModule() *core.DictValue {
	textwrapModule := core.NewDict()

	// dedent - remove common leading whitespace from all lines
	textwrapModule.SetWithKey("dedent", core.StringValue("dedent"),
		core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) != 1 {
				return nil, core.NewTypeError("str", nil, "dedent() argument")
			}

			str, ok := args[0].(core.StringValue)
			if !ok {
				return nil, core.NewTypeError("str", args[0], "dedent() argument")
			}

			text := string(str)
			if text == "" {
				return core.StringValue(""), nil
			}

			// Split into lines
			lines := strings.Split(text, "\n")

			// Find minimum indentation (ignoring empty lines)
			minIndent := -1
			for _, line := range lines {
				if strings.TrimSpace(line) == "" {
					continue
				}

				// Count leading whitespace
				indent := 0
				for _, ch := range line {
					if ch == ' ' || ch == '\t' {
						indent++
					} else {
						break
					}
				}

				if minIndent == -1 || indent < minIndent {
					minIndent = indent
				}
			}

			// If no non-empty lines, return original
			if minIndent == -1 {
				return str, nil
			}

			// Remove the common indentation from each line
			var result []string
			for _, line := range lines {
				if strings.TrimSpace(line) == "" {
					result = append(result, "")
				} else {
					if len(line) >= minIndent {
						result = append(result, line[minIndent:])
					} else {
						result = append(result, line)
					}
				}
			}

			return core.StringValue(strings.Join(result, "\n")), nil
		}))

	// indent - add prefix to each line (minimal implementation)
	textwrapModule.SetWithKey("indent", core.StringValue("indent"),
		core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) < 2 {
				return nil, core.NewTypeError("str", nil, "indent() arguments")
			}

			text, ok := args[0].(core.StringValue)
			if !ok {
				return nil, core.NewTypeError("str", args[0], "indent() text argument")
			}

			prefix, ok := args[1].(core.StringValue)
			if !ok {
				return nil, core.NewTypeError("str", args[1], "indent() prefix argument")
			}

			lines := strings.Split(string(text), "\n")
			var result []string
			for _, line := range lines {
				if line != "" {
					result = append(result, string(prefix)+line)
				} else {
					result = append(result, line)
				}
			}

			return core.StringValue(strings.Join(result, "\n")), nil
		}))

	return textwrapModule
}
