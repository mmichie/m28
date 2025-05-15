package builtin

import (
	"fmt"
	"regexp"
	"strings"

	"github.com/mmichie/m28/core"
)

// PrintValueOverride is a helper function to properly format Pythonic types for display
func PrintValueOverride(val core.LispValue) string {
	// First, unwrap if it's a LocatedValue
	if locatedVal, isLocated := val.(core.LocatedValue); isLocated {
		val = locatedVal.Value
	}

	// Special handling for dictionaries
	if dict, ok := val.(*core.PythonicDict); ok {
		// Get all keys from dictionary
		keys := dict.SortedKeys()

		// Format as Python-style dictionary
		pairs := make([]string, 0, len(keys))
		for _, k := range keys {
			v, _ := dict.Get(k)

			// Special handling for quoted key patterns
			keyStr := core.PrintValue(k)

			// Debug: Add quotes around the key if it looks like it contains quoted syntax
			if strings.Contains(keyStr, "{") && strings.Contains(keyStr, "'") {
				// Simple transformation to display in a more Python-like format
				// Extract the character between { and '
				re := regexp.MustCompile(`{([^']+)'`)
				matches := re.FindStringSubmatch(keyStr)
				if len(matches) >= 2 {
					// Found a match, use the quoted value
					keyStr = fmt.Sprintf("'%s'", matches[1])
				}
			}

			pairs = append(pairs, fmt.Sprintf("%s: %s", keyStr, core.PrintValue(v)))
		}

		return "{" + strings.Join(pairs, ", ") + "}"
	}

	// For all other types, use the standard PrintValue
	return core.PrintValue(val)
}
