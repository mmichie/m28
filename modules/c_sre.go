package modules

import (
	"fmt"
	"regexp"
	"strings"
	"unicode"

	"github.com/mmichie/m28/core"
)

// processVerbosePattern converts a VERBOSE mode regex pattern to a standard pattern
// by stripping comments (# to end of line) and unescaped whitespace
func processVerbosePattern(pattern string) string {
	var result strings.Builder
	i := 0
	inCharClass := false

	for i < len(pattern) {
		ch := pattern[i]

		// Handle escape sequences
		if ch == '\\' && i+1 < len(pattern) {
			result.WriteByte(ch)
			result.WriteByte(pattern[i+1])
			i += 2
			continue
		}

		// Track character class state
		if ch == '[' && !inCharClass {
			inCharClass = true
			result.WriteByte(ch)
			i++
			continue
		}
		if ch == ']' && inCharClass {
			inCharClass = false
			result.WriteByte(ch)
			i++
			continue
		}

		// Inside character class, keep everything
		if inCharClass {
			result.WriteByte(ch)
			i++
			continue
		}

		// Handle comments (# to end of line)
		if ch == '#' {
			// Skip to end of line
			for i < len(pattern) && pattern[i] != '\n' {
				i++
			}
			continue
		}

		// Handle whitespace (skip unescaped whitespace outside char class)
		if ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' {
			i++
			continue
		}

		// Keep all other characters
		result.WriteByte(ch)
		i++
	}

	return result.String()
}

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
		// Handle various input types
		switch p := args[0].(type) {
		case core.StringValue:
			pattern.SetValue(core.StringValue("pattern"), p)
		case core.BytesValue:
			// Convert bytes to string for regex pattern
			pattern.SetValue(core.StringValue("pattern"), core.StringValue(string(p)))
		default:
			// Convert to string representation
			pattern.SetValue(core.StringValue("pattern"), core.StringValue(p.String()))
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
			var searchStr string
			switch v := matchArgs[0].(type) {
			case core.StringValue:
				searchStr = string(v)
			case core.BytesValue:
				searchStr = string(v)
			case core.NilValue:
				return core.None, nil // No match on None
			default:
				return nil, fmt.Errorf("match() argument must be a string or bytes, not %s", matchArgs[0].Type())
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
			loc := re.FindStringSubmatchIndex(searchStr)
			if loc == nil || loc[0] != 0 {
				// No match or match doesn't start at beginning
				return core.None, nil
			}

			// Create a match object (simplified - just a dict with basic info)
			matchObj := core.NewDict()

			// Extract the full match and groups
			matches := re.FindStringSubmatch(searchStr)
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
			var searchStr string
			switch v := searchArgs[0].(type) {
			case core.StringValue:
				searchStr = string(v)
			case core.BytesValue:
				searchStr = string(v)
			case core.NilValue:
				return core.None, nil // No match on None
			default:
				return nil, fmt.Errorf("search() argument must be a string or bytes, not %s", searchArgs[0].Type())
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
			match := re.FindStringIndex(searchStr)
			if match == nil {
				return core.None, nil
			}

			// Create a Match object
			matchObj := core.NewDict()
			matchObj.Set("start", core.NumberValue(float64(match[0])))
			matchObj.Set("end", core.NumberValue(float64(match[1])))
			matchText := searchStr[match[0]:match[1]]
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

			// Get the flags from the pattern object
			flagsKey := core.ValueToKey(core.StringValue("flags"))
			flagsVal, _ := pattern.Get(flagsKey)
			flags := 0
			if f, ok := flagsVal.(core.NumberValue); ok {
				flags = int(f)
			}

			// Get the string to search
			if len(findallArgs) < 1 {
				return nil, fmt.Errorf("findall() takes at least 1 argument")
			}
			var searchStr string
			switch v := findallArgs[0].(type) {
			case core.StringValue:
				searchStr = string(v)
			case core.BytesValue:
				searchStr = string(v)
			case core.NilValue:
				return core.NewList(), nil // Empty list on None
			default:
				return nil, fmt.Errorf("findall() argument must be a string or bytes, not %s", findallArgs[0].Type())
			}

			// Convert pattern to Go-compatible regex
			// Go's regexp doesn't support lookaheads (?=...), so we need to convert them
			goPattern := string(patternStr)

			// Handle VERBOSE flag (64) - strip comments and whitespace
			if flags&64 != 0 {
				goPattern = processVerbosePattern(goPattern)
			}

			// Apply Python regex flags as Go inline flags
			// Python flags: IGNORECASE=2, MULTILINE=8, DOTALL=16, VERBOSE=64
			flagPrefix := ""
			if flags&2 != 0 { // IGNORECASE
				flagPrefix += "i"
			}
			if flags&8 != 0 { // MULTILINE
				flagPrefix += "m"
			}
			if flags&16 != 0 { // DOTALL
				flagPrefix += "s"
			}
			if flagPrefix != "" {
				goPattern = "(?" + flagPrefix + ")" + goPattern
			}

			// Common pattern: (?=\s|$) - lookahead for whitespace or end
			// This can often be removed when followed by |\S+ which handles non-whitespace
			// For argparse's specific pattern: \(.*?\)+(?=\s|$)|\[.*?\]+(?=\s|$)|\S+
			// We can simplify to: \[.*?\]|\(.*?\)|\S+
			if strings.Contains(goPattern, `(?=\s|$)`) {
				// Remove the lookahead - works for patterns that alternate with \S+
				goPattern = strings.ReplaceAll(goPattern, `+(?=\s|$)`, ``)
			}

			// Handle doctest's _INDENT_RE pattern: ^([ ]*)(?=\S)
			// Transform to ^([ ]*)\S and then extract only the group
			hasIndentLookahead := false
			if strings.Contains(goPattern, `(?=\S)`) {
				// Replace lookahead with actual character match (we'll extract group later)
				goPattern = strings.ReplaceAll(goPattern, `(?=\S)`, `\S`)
				hasIndentLookahead = true
			}

			// Handle other common lookahead patterns
			// (?=...) positive lookahead - for now, try to remove simple ones
			if strings.Contains(goPattern, `(?=`) {
				// Try to compile anyway - some patterns might work after other transformations
				// If it still fails, we'll get an error below
			}

			// Handle negative lookahead patterns - these are harder to convert
			if strings.Contains(goPattern, `(?!`) {
				// Remove negative lookahead assertions - not ideal but allows pattern to compile
				for strings.Contains(goPattern, `(?!`) {
					start := strings.Index(goPattern, `(?!`)
					if start < 0 {
						break
					}
					// Find matching paren
					depth := 1
					end := start + 3
					for end < len(goPattern) && depth > 0 {
						if goPattern[end] == '(' {
							depth++
						} else if goPattern[end] == ')' {
							depth--
						}
						end++
					}
					if depth == 0 {
						goPattern = goPattern[:start] + goPattern[end:]
					} else {
						break // Malformed pattern
					}
				}
			}

			// Compile and execute the regex
			re, err := regexp.Compile(goPattern)
			if err != nil {
				// If pattern still can't compile, return empty list instead of error
				return core.NewList(), nil
			}

			// Find all matches
			result := core.NewList()

			if hasIndentLookahead {
				// For transformed indent pattern, use FindAllStringSubmatch to get groups
				matches := re.FindAllStringSubmatch(searchStr, -1)
				for _, match := range matches {
					if len(match) > 1 {
						// Return the captured group (the indent spaces)
						result.Append(core.StringValue(match[1]))
					} else if len(match) > 0 {
						result.Append(core.StringValue(match[0]))
					}
				}
			} else {
				// Check if pattern has capturing groups
				matches := re.FindAllStringSubmatch(searchStr, -1)
				for _, match := range matches {
					if re.NumSubexp() > 0 && len(match) > 1 {
						// Return captured groups like Python
						if re.NumSubexp() == 1 {
							result.Append(core.StringValue(match[1]))
						} else {
							// Multiple groups - return as tuple
							groups := make(core.TupleValue, re.NumSubexp())
							for i := 1; i <= re.NumSubexp(); i++ {
								if i < len(match) {
									groups[i-1] = core.StringValue(match[i])
								} else {
									groups[i-1] = core.StringValue("")
								}
							}
							result.Append(groups)
						}
					} else if len(match) > 0 {
						result.Append(core.StringValue(match[0]))
					}
				}
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
			if len(splitArgs) < 1 {
				return core.NewList(), nil
			}

			var str string
			switch v := splitArgs[0].(type) {
			case core.StringValue:
				str = string(v)
			case core.BytesValue:
				str = string(v)
			case core.NilValue:
				return core.NewList(), nil // Empty list on None
			default:
				return nil, fmt.Errorf("split() argument must be a string or bytes, not %s", splitArgs[0].Type())
			}

			// Get maxsplit parameter (0 means unlimited)
			maxsplit := 0
			if len(splitArgs) >= 2 {
				if ms, ok := splitArgs[1].(core.NumberValue); ok {
					maxsplit = int(ms)
				}
			}

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

			// Convert pattern to Go-compatible regex
			goPattern := string(patternStr)
			if strings.Contains(goPattern, `(?=\s|$)`) {
				goPattern = strings.ReplaceAll(goPattern, `+(?=\s|$)`, ``)
			}

			// Compile the regex
			re, err := regexp.Compile(goPattern)
			if err != nil {
				return nil, fmt.Errorf("invalid regex pattern: %v", err)
			}

			// If pattern doesn't match at all, return original string in list
			if !re.MatchString(str) {
				result := core.NewList()
				result.Append(core.StringValue(str))
				return result, nil
			}

			// Find all matches with their submatch indices
			// This gives us: [match_start, match_end, group1_start, group1_end, ...]
			allMatches := re.FindAllStringSubmatchIndex(str, -1)
			if len(allMatches) == 0 {
				result := core.NewList()
				result.Append(core.StringValue(str))
				return result, nil
			}

			// Build result list
			result := core.NewList()
			lastEnd := 0
			numSplits := 0

			for _, match := range allMatches {
				// Check if we've reached maxsplit
				if maxsplit > 0 && numSplits >= maxsplit {
					break
				}

				matchStart := match[0]
				matchEnd := match[1]

				// Add the part before this match
				result.Append(core.StringValue(str[lastEnd:matchStart]))

				// Add any captured groups (indices 2+ in the match array)
				// match[0:2] is the full match, match[2:4] is group 1, etc.
				for i := 2; i < len(match); i += 2 {
					groupStart := match[i]
					groupEnd := match[i+1]
					if groupStart >= 0 && groupEnd >= 0 {
						result.Append(core.StringValue(str[groupStart:groupEnd]))
					} else {
						// Group didn't participate in match (optional group)
						result.Append(core.None)
					}
				}

				lastEnd = matchEnd
				numSplits++
			}

			// Add the remainder after the last match
			result.Append(core.StringValue(str[lastEnd:]))

			return result, nil
		}))

		// finditer - return iterator over all matches (returns list of match objects for now)
		pattern.SetValue(core.StringValue("finditer"), core.NewNamedBuiltinFunction("finditer", func(finditerArgs []core.Value, finditerCtx *core.Context) (core.Value, error) {
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

			// Get the flags from the pattern object
			flagsKey := core.ValueToKey(core.StringValue("flags"))
			flagsVal, _ := pattern.Get(flagsKey)
			flags := 0
			if f, ok := flagsVal.(core.NumberValue); ok {
				flags = int(f)
			}

			// Get the string to search
			if len(finditerArgs) < 1 {
				return nil, fmt.Errorf("finditer() takes at least 1 argument")
			}
			var searchStr string
			switch v := finditerArgs[0].(type) {
			case core.StringValue:
				searchStr = string(v)
			case core.BytesValue:
				searchStr = string(v)
			case core.NilValue:
				return core.NewList(), nil // Empty list on None
			default:
				return nil, fmt.Errorf("finditer() argument must be a string or bytes, not %s", finditerArgs[0].Type())
			}

			// Convert pattern to Go-compatible regex
			goPattern := string(patternStr)

			// Handle VERBOSE flag (64) - strip comments and whitespace
			if flags&64 != 0 {
				goPattern = processVerbosePattern(goPattern)
			}

			// Apply Python regex flags as Go inline flags
			flagPrefix := ""
			if flags&2 != 0 { // IGNORECASE
				flagPrefix += "i"
			}
			if flags&8 != 0 { // MULTILINE
				flagPrefix += "m"
			}
			if flags&16 != 0 { // DOTALL
				flagPrefix += "s"
			}
			if flagPrefix != "" {
				goPattern = "(?" + flagPrefix + ")" + goPattern
			}

			// Handle lookahead patterns
			if strings.Contains(goPattern, `(?=\S)`) {
				goPattern = strings.ReplaceAll(goPattern, `(?=\S)`, `\S`)
			}
			if strings.Contains(goPattern, `(?=\s|$)`) {
				goPattern = strings.ReplaceAll(goPattern, `+(?=\s|$)`, ``)
			}

			// Handle negative lookahead patterns - these are harder to convert
			// For doctest's pattern (?![ ]*$) (not blank line) and (?![ ]*>>>) (not PS1)
			// We can try to remove them and rely on other parts of the pattern
			// This is a workaround - results may not be identical to Python
			if strings.Contains(goPattern, `(?!`) {
				// Remove negative lookahead assertions - not ideal but allows pattern to compile
				for strings.Contains(goPattern, `(?!`) {
					start := strings.Index(goPattern, `(?!`)
					if start < 0 {
						break
					}
					// Find matching paren
					depth := 1
					end := start + 3
					for end < len(goPattern) && depth > 0 {
						if goPattern[end] == '(' {
							depth++
						} else if goPattern[end] == ')' {
							depth--
						}
						end++
					}
					if depth == 0 {
						goPattern = goPattern[:start] + goPattern[end:]
					} else {
						break // Malformed pattern
					}
				}
			}

			// Compile the regex
			re, err := regexp.Compile(goPattern)
			if err != nil {
				// If pattern still can't compile, return empty list instead of error
				// This allows code to continue even with unsupported regex features
				return core.NewList(), nil
			}

			// Find all matches with indices
			allMatches := re.FindAllStringSubmatchIndex(searchStr, -1)

			// Create a list of match objects
			result := core.NewList()
			for _, match := range allMatches {
				matchObj := core.NewDict()
				if len(match) >= 2 {
					// Store internal values with underscore prefix
					matchStart := match[0]
					matchEnd := match[1]
					matchObj.SetValue(core.StringValue("_start"), core.NumberValue(matchStart))
					matchObj.SetValue(core.StringValue("_end"), core.NumberValue(matchEnd))
					matchObj.SetValue(core.StringValue("_match"), core.StringValue(searchStr[matchStart:matchEnd]))

					// Add groups
					groups := core.NewList()
					for i := 2; i < len(match); i += 2 {
						if match[i] >= 0 && match[i+1] >= 0 {
							groups.Append(core.StringValue(searchStr[match[i]:match[i+1]]))
						} else {
							groups.Append(core.None)
						}
					}
					matchObj.SetValue(core.StringValue("_groups"), groups)

					// Add group method
					matchObj.SetValue(core.StringValue("group"), core.NewNamedBuiltinFunction("group", func(groupArgs []core.Value, groupCtx *core.Context) (core.Value, error) {
						if len(groupArgs) == 0 {
							if m, ok := matchObj.GetValue(core.StringValue("_match")); ok {
								return m, nil
							}
							return core.StringValue(""), nil
						}
						// Handle named groups
						if name, ok := groupArgs[0].(core.StringValue); ok {
							if g, ok := matchObj.GetValue(core.StringValue("_groups")); ok {
								if groupsList, ok := g.(*core.ListValue); ok {
									// Try to find group by name - for now return first group or match
									_ = name
									if groupsList.Len() > 0 {
										if val, err := groupsList.GetItem(0); err == nil {
											return val, nil
										}
									}
								}
							}
							// If named group not found, return the full match
							if m, ok := matchObj.GetValue(core.StringValue("_match")); ok {
								return m, nil
							}
							return core.StringValue(""), nil
						}
						groupNum := 0
						if n, ok := groupArgs[0].(core.NumberValue); ok {
							groupNum = int(n)
						}
						if groupNum == 0 {
							if m, ok := matchObj.GetValue(core.StringValue("_match")); ok {
								return m, nil
							}
							return core.StringValue(""), nil
						}
						if g, ok := matchObj.GetValue(core.StringValue("_groups")); ok {
							if groupsList, ok := g.(*core.ListValue); ok {
								if groupNum-1 < groupsList.Len() {
									if val, err := groupsList.GetItem(groupNum - 1); err == nil {
										return val, nil
									}
								}
							}
						}
						return core.None, nil
					}))

					// Add start method - capture start value in closure
					startVal := core.NumberValue(matchStart)
					matchObj.SetValue(core.StringValue("start"), core.NewNamedBuiltinFunction("start", func(startArgs []core.Value, startCtx *core.Context) (core.Value, error) {
						return startVal, nil
					}))

					// Add end method - capture end value in closure
					endVal := core.NumberValue(matchEnd)
					matchObj.SetValue(core.StringValue("end"), core.NewNamedBuiltinFunction("end", func(endArgs []core.Value, endCtx *core.Context) (core.Value, error) {
						return endVal, nil
					}))
				}
				result.Append(matchObj)
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
