package modules

import (
	"fmt"
	"regexp"
	"strings"
	"unicode"

	"github.com/mmichie/m28/core"
)

// Init_SREModule initializes the _sre C extension stub module
// This provides minimal regex engine support for Python stdlib modules
func Init_SREModule() *core.DictValue {
	module := core.NewDict()

	// MAGIC - version identifier for the SRE module
	// Must match the MAGIC in re._constants (Python 3.12 = 20221023)
	module.Set("MAGIC", core.NumberValue(20221023))

	// CODESIZE - size of regex opcode in bytes (typically 2 or 4)
	module.Set("CODESIZE", core.NumberValue(4))

	// MAXREPEAT - maximum repeat count
	module.Set("MAXREPEAT", core.NumberValue(4294967295)) // 2^32 - 1

	// MAXGROUPS - maximum number of groups
	module.Set("MAXGROUPS", core.NumberValue(2147483647)) // 2^31 - 1

	// unicode_iscased - check if a Unicode character has case
	module.Set("unicode_iscased", core.NewNamedBuiltinFunction("unicode_iscased", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, core.NewTypeError("unicode_iscased", nil, "unicode_iscased() takes exactly 1 argument")
		}

		// Get the character code
		var ch rune
		switch v := args[0].(type) {
		case core.NumberValue:
			ch = rune(v)
		case core.StringValue:
			if len(v) > 0 {
				ch = rune(v[0])
			} else {
				return core.BoolValue(false), nil
			}
		default:
			return nil, core.NewTypeError("unicode_iscased", args[0], "argument must be int or str")
		}

		// A character is cased if it has both uppercase and lowercase forms
		// or if it is already uppercase or lowercase
		isCased := unicode.IsUpper(ch) || unicode.IsLower(ch) || unicode.ToUpper(ch) != ch || unicode.ToLower(ch) != ch
		return core.BoolValue(isCased), nil
	}))

	// unicode_tolower - convert Unicode character to lowercase
	module.Set("unicode_tolower", core.NewNamedBuiltinFunction("unicode_tolower", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, core.NewTypeError("unicode_tolower", nil, "unicode_tolower() takes exactly 1 argument")
		}

		// Get the character code
		var ch rune
		switch v := args[0].(type) {
		case core.NumberValue:
			ch = rune(v)
		case core.StringValue:
			if len(v) > 0 {
				ch = rune(v[0])
			} else {
				return core.NumberValue(0), nil
			}
		default:
			return nil, core.NewTypeError("unicode_tolower", args[0], "argument must be int or str")
		}

		lower := unicode.ToLower(ch)
		return core.NumberValue(lower), nil
	}))

	// ascii_iscased - check if an ASCII character has case
	module.Set("ascii_iscased", core.NewNamedBuiltinFunction("ascii_iscased", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, core.NewTypeError("ascii_iscased", nil, "ascii_iscased() takes exactly 1 argument")
		}

		// Get the character code
		var ch rune
		switch v := args[0].(type) {
		case core.NumberValue:
			ch = rune(v)
		case core.StringValue:
			if len(v) > 0 {
				ch = rune(v[0])
			} else {
				return core.BoolValue(false), nil
			}
		default:
			return nil, core.NewTypeError("ascii_iscased", args[0], "argument must be int or str")
		}

		// ASCII cased characters are A-Z and a-z
		isCased := (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z')
		return core.BoolValue(isCased), nil
	}))

	// ascii_tolower - convert ASCII character to lowercase
	module.Set("ascii_tolower", core.NewNamedBuiltinFunction("ascii_tolower", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, core.NewTypeError("ascii_tolower", nil, "ascii_tolower() takes exactly 1 argument")
		}

		// Get the character code
		var ch rune
		switch v := args[0].(type) {
		case core.NumberValue:
			ch = rune(v)
		case core.StringValue:
			if len(v) > 0 {
				ch = rune(v[0])
			} else {
				return core.NumberValue(0), nil
			}
		default:
			return nil, core.NewTypeError("ascii_tolower", args[0], "argument must be int or str")
		}

		// ASCII lowercase
		if ch >= 'A' && ch <= 'Z' {
			ch = ch + 32
		}
		return core.NumberValue(ch), nil
	}))

	// compile - compile a regex pattern (stub)
	// Returns a minimal pattern object
	module.Set("compile", core.NewNamedBuiltinFunction("compile", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, core.NewTypeError("compile", nil, "compile() takes at least 1 argument")
		}

		// Create a minimal pattern object (dict with basic attributes)
		pattern := core.NewDict()

		// Store the pattern string - use SetValue for proper key formatting
		if patternStr, ok := args[0].(core.StringValue); ok {
			pattern.SetValue(core.StringValue("pattern"), patternStr)
		}

		// Add flags (default 0)
		flags := core.NumberValue(0)
		if len(args) >= 2 {
			if f, ok := args[1].(core.NumberValue); ok {
				flags = f
			}
		}
		pattern.SetValue(core.StringValue("flags"), flags)

		// Add stub methods - use SetValue for proper key formatting
		pattern.SetValue(core.StringValue("match"), core.NewNamedBuiltinFunction("match", func(matchArgs []core.Value, matchCtx *core.Context) (core.Value, error) {
			// Get the pattern string from the pattern object
			patternKey := core.ValueToKey(core.StringValue("pattern"))
			patternVal, ok := pattern.Get(patternKey)
			if !ok {
				return nil, fmt.Errorf("pattern object has no pattern string")
			}
			patternStr, ok := patternVal.(core.StringValue)
			if !ok {
				return nil, fmt.Errorf("pattern must be a string")
			}

			// Get the string to match
			if len(matchArgs) < 1 {
				return nil, fmt.Errorf("match() takes at least 1 argument")
			}
			searchStr, ok := matchArgs[0].(core.StringValue)
			if !ok {
				return nil, fmt.Errorf("match() argument must be a string")
			}

			// Convert pattern to Go-compatible regex
			goPattern := string(patternStr)
			if strings.Contains(goPattern, `(?=\s|$)`) {
				goPattern = strings.ReplaceAll(goPattern, `+(?=\s|$)`, ``)
			}

			// Compile and execute the regex
			re, err := regexp.Compile(goPattern)
			if err != nil {
				return nil, fmt.Errorf("invalid regex pattern: %v", err)
			}

			// Match from the beginning of the string
			loc := re.FindStringSubmatchIndex(string(searchStr))
			if loc == nil || loc[0] != 0 {
				// No match or match doesn't start at beginning
				return core.None, nil
			}

			// Create a match object (simplified - just a dict with basic info)
			matchObj := core.NewDict()

			// Extract the full match and groups
			matches := re.FindStringSubmatch(string(searchStr))
			if len(matches) > 0 {
				// groups() returns just the captured groups (not the full match)
				groupsList := core.NewList()
				for i := 1; i < len(matches); i++ {
					groupsList.Append(core.StringValue(matches[i]))
				}

				matchObj.SetValue(core.StringValue("_groups"), groupsList)
				matchObj.SetValue(core.StringValue("_match"), core.StringValue(matches[0]))
				matchObj.SetValue(core.StringValue("_all_matches"), core.NewList(
					func() []core.Value {
						result := make([]core.Value, len(matches))
						for i, m := range matches {
							result[i] = core.StringValue(m)
						}
						return result
					}()...,
				))

				// Add groups() method
				matchObj.SetValue(core.StringValue("groups"), core.NewNamedBuiltinFunction("groups", func(groupArgs []core.Value, groupCtx *core.Context) (core.Value, error) {
					key := core.ValueToKey(core.StringValue("_groups"))
					if groups, ok := matchObj.Get(key); ok {
						// Convert list to tuple
						if groupsList, ok := groups.(*core.ListValue); ok {
							return core.TupleValue(groupsList.Items()), nil
						}
						return groups, nil
					}
					return core.TupleValue([]core.Value{}), nil
				}))

				// Add group() method
				matchObj.SetValue(core.StringValue("group"), core.NewNamedBuiltinFunction("group", func(groupArgs []core.Value, groupCtx *core.Context) (core.Value, error) {
					// group(0) returns the full match, group(1) returns first group, etc.
					var index int
					if len(groupArgs) == 0 {
						index = 0 // default to full match
					} else if n, ok := groupArgs[0].(core.NumberValue); ok {
						index = int(n)
					} else {
						return nil, fmt.Errorf("group() argument must be an integer")
					}

					key := core.ValueToKey(core.StringValue("_all_matches"))
					if allMatches, ok := matchObj.Get(key); ok {
						if matchList, ok := allMatches.(*core.ListValue); ok {
							if index >= 0 && index < matchList.Len() {
								return matchList.Items()[index], nil
							}
						}
					}
					return core.None, nil
				}))
			}

			return matchObj, nil
		}))

		pattern.SetValue(core.StringValue("search"), core.NewNamedBuiltinFunction("search", func(searchArgs []core.Value, searchCtx *core.Context) (core.Value, error) {
			// Get the pattern string from the pattern object
			patternKey := core.ValueToKey(core.StringValue("pattern"))
			patternVal, ok := pattern.Get(patternKey)
			if !ok {
				return nil, fmt.Errorf("pattern object has no pattern string")
			}
			patternStr, ok := patternVal.(core.StringValue)
			if !ok {
				return nil, fmt.Errorf("pattern must be a string")
			}

			// Get the string to search
			if len(searchArgs) < 1 {
				return nil, fmt.Errorf("search() takes at least 1 argument")
			}
			searchStr, ok := searchArgs[0].(core.StringValue)
			if !ok {
				return nil, fmt.Errorf("search() argument must be a string")
			}

			// Convert pattern to Go-compatible regex (same logic as findall)
			goPattern := string(patternStr)
			if strings.Contains(goPattern, `(?=\s|$)`) {
				goPattern = strings.ReplaceAll(goPattern, `+(?=\s|$)`, ``)
			}

			// Compile and execute the regex
			re, err := regexp.Compile(goPattern)
			if err != nil {
				return nil, fmt.Errorf("invalid regex pattern: %v", err)
			}

			// Find first match
			match := re.FindStringIndex(string(searchStr))
			if match == nil {
				return core.None, nil
			}

			// Create a Match object
			matchObj := core.NewDict()
			matchObj.Set("start", core.NumberValue(float64(match[0])))
			matchObj.Set("end", core.NumberValue(float64(match[1])))
			matchText := string(searchStr)[match[0]:match[1]]
			matchObj.Set("group", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
				// group() or group(0) returns the full match
				if len(args) == 0 || (len(args) == 1 && args[0] == core.NumberValue(0)) {
					return core.StringValue(matchText), nil
				}
				return core.StringValue(matchText), nil
			}))

			return matchObj, nil
		}))

		pattern.SetValue(core.StringValue("findall"), core.NewNamedBuiltinFunction("findall", func(findallArgs []core.Value, findallCtx *core.Context) (core.Value, error) {
			// Get the pattern string from the pattern object
			patternKey := core.ValueToKey(core.StringValue("pattern"))
			patternVal, ok := pattern.Get(patternKey)
			if !ok {
				return nil, fmt.Errorf("pattern object has no pattern string")
			}
			patternStr, ok := patternVal.(core.StringValue)
			if !ok {
				return nil, fmt.Errorf("pattern must be a string")
			}

			// Get the string to search
			if len(findallArgs) < 1 {
				return nil, fmt.Errorf("findall() takes at least 1 argument")
			}
			searchStr, ok := findallArgs[0].(core.StringValue)
			if !ok {
				return nil, fmt.Errorf("findall() argument must be a string")
			}

			// Convert pattern to Go-compatible regex
			// Go's regexp doesn't support lookaheads (?=...), so we need to convert them
			goPattern := string(patternStr)

			// Common pattern: (?=\s|$) - lookahead for whitespace or end
			// This can often be removed when followed by |\S+ which handles non-whitespace
			// For argparse's specific pattern: \(.*?\)+(?=\s|$)|\[.*?\]+(?=\s|$)|\S+
			// We can simplify to: \[.*?\]|\(.*?\)|\S+
			if strings.Contains(goPattern, `(?=\s|$)`) {
				// Remove the lookahead - works for patterns that alternate with \S+
				goPattern = strings.ReplaceAll(goPattern, `+(?=\s|$)`, ``)
			}

			// Compile and execute the regex
			re, err := regexp.Compile(goPattern)
			if err != nil {
				return nil, fmt.Errorf("invalid regex pattern: %v", err)
			}

			// Find all matches
			matches := re.FindAllString(string(searchStr), -1)

			// Convert to M28 list
			result := core.NewList()
			for _, match := range matches {
				result.Append(core.StringValue(match))
			}

			return result, nil
		}))

		pattern.SetValue(core.StringValue("sub"), core.NewNamedBuiltinFunction("sub", func(subArgs []core.Value, subCtx *core.Context) (core.Value, error) {
			// Just return the original string for now
			if len(subArgs) >= 2 {
				if str, ok := subArgs[1].(core.StringValue); ok {
					return str, nil
				}
			}
			return core.StringValue(""), nil
		}))

		pattern.SetValue(core.StringValue("split"), core.NewNamedBuiltinFunction("split", func(splitArgs []core.Value, splitCtx *core.Context) (core.Value, error) {
			// Stub implementation: just split on whitespace for now
			if len(splitArgs) < 1 {
				return core.NewList(), nil
			}

			str, ok := splitArgs[0].(core.StringValue)
			if !ok {
				return nil, fmt.Errorf("split() argument must be a string")
			}

			// For now, just split on whitespace (this is a simplified stub)
			// TODO(M28-580c): Actually use the pattern to split
			parts := strings.Fields(string(str))
			result := core.NewList()
			for _, part := range parts {
				result.Append(core.StringValue(part))
			}

			return result, nil
		}))

		return pattern, nil
	}))

	// getlower - get lowercase version of character (with locale)
	module.Set("getlower", core.NewNamedBuiltinFunction("getlower", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 2 {
			return nil, core.NewTypeError("getlower", nil, "getlower() takes at least 2 arguments")
		}

		// Get the character code
		var ch rune
		switch v := args[0].(type) {
		case core.NumberValue:
			ch = rune(v)
		case core.StringValue:
			if len(v) > 0 {
				ch = rune(v[0])
			} else {
				return core.NumberValue(0), nil
			}
		default:
			return nil, core.NewTypeError("getlower", args[0], "first argument must be int or str")
		}

		// Simple lowercase conversion
		lower := unicode.ToLower(ch)
		return core.NumberValue(lower), nil
	}))

	// getcodesize - get the size of compiled regex code
	module.Set("getcodesize", core.NewNamedBuiltinFunction("getcodesize", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.NumberValue(4), nil
	}))

	// getlower_locale - lowercase with locale (stub - just uses regular lowercase)
	module.Set("getlower_locale", core.NewNamedBuiltinFunction("getlower_locale", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, core.NewTypeError("getlower_locale", nil, "getlower_locale() takes at least 1 argument")
		}

		var ch rune
		switch v := args[0].(type) {
		case core.NumberValue:
			ch = rune(v)
		case core.StringValue:
			if len(v) > 0 {
				ch = rune(v[0])
			} else {
				return core.NumberValue(0), nil
			}
		default:
			return nil, core.NewTypeError("getlower_locale", args[0], "argument must be int or str")
		}

		lower := strings.ToLower(string(ch))
		if len(lower) > 0 {
			return core.NumberValue(rune(lower[0])), nil
		}
		return core.NumberValue(ch), nil
	}))

	return module
}
