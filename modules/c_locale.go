// Package modules provides the _locale C extension module for M28
// Implements minimal locale support with UTF-8 defaults
package modules

import (
	"fmt"
	"os"
	"strings"

	"github.com/mmichie/m28/core"
)

// currentLocale stores the current locale setting (default "C" for POSIX)
var currentLocale = "C"

// Init_LocaleModule creates and returns the _locale module
func Init_LocaleModule() *core.DictValue {
	localeModule := core.NewDict()

	// LC_* category constants (match Python's locale module)
	localeModule.Set("LC_ALL", core.NumberValue(0))
	localeModule.Set("LC_COLLATE", core.NumberValue(1))
	localeModule.Set("LC_CTYPE", core.NumberValue(2))
	localeModule.Set("LC_MONETARY", core.NumberValue(3))
	localeModule.Set("LC_NUMERIC", core.NumberValue(4))
	localeModule.Set("LC_TIME", core.NumberValue(5))
	localeModule.Set("LC_MESSAGES", core.NumberValue(6))

	// getencoding() - Return current text encoding
	// New in Python 3.11 - returns current locale's encoding
	localeModule.Set("getencoding", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Default to UTF-8 on modern systems
		// Check environment variables that might indicate encoding
		if lang := os.Getenv("LANG"); lang != "" {
			// Extract encoding from LANG (e.g., "en_US.UTF-8" -> "UTF-8")
			if parts := strings.Split(lang, "."); len(parts) > 1 {
				return core.StringValue(parts[1]), nil
			}
		}
		// Default to UTF-8
		return core.StringValue("UTF-8"), nil
	}))

	// setlocale(category, locale=None) - Get or set locale for category
	localeModule.Set("setlocale", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("setlocale() takes at least 1 argument (0 given)")
		}

		// Category argument (LC_ALL, LC_CTYPE, etc.) - not used in minimal implementation
		// In a full implementation, we'd track per-category locales

		// If no locale argument or None, return current locale
		if len(args) < 2 || args[1] == core.None {
			return core.StringValue(currentLocale), nil
		}

		// Set new locale
		localeStr, ok := args[1].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("setlocale() argument 2 must be string or None")
		}

		newLocale := string(localeStr)

		// Handle empty string - means "use user's default locale"
		if newLocale == "" {
			// Try to get from environment
			if lang := os.Getenv("LANG"); lang != "" {
				currentLocale = lang
			} else if lc_all := os.Getenv("LC_ALL"); lc_all != "" {
				currentLocale = lc_all
			} else {
				currentLocale = "en_US.UTF-8" // Reasonable default
			}
		} else {
			// Validate and set locale
			// In minimal implementation, we accept: "C", "POSIX", and any UTF-8 locale
			if newLocale == "C" || newLocale == "POSIX" || strings.Contains(newLocale, "UTF-8") ||
				strings.Contains(newLocale, "utf-8") || strings.Contains(newLocale, ".") {
				currentLocale = newLocale
			} else {
				// Try to be lenient - accept the locale but maybe normalize it
				currentLocale = newLocale
			}
		}

		return core.StringValue(currentLocale), nil
	}))

	// localeconv() - Return locale-specific formatting conventions
	// Returns a dict with decimal_point, thousands_sep, grouping, etc.
	localeModule.Set("localeconv", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		result := core.NewDict()

		// For UTF-8/modern locales, use standard formatting
		// In a full implementation, this would query the system's locale database
		result.SetWithKey("decimal_point", core.StringValue("decimal_point"), core.StringValue("."))
		result.SetWithKey("thousands_sep", core.StringValue("thousands_sep"), core.StringValue(","))
		result.SetWithKey("grouping", core.StringValue("grouping"), core.NewList(core.NumberValue(3), core.NumberValue(0))) // Group by 3 digits
		result.SetWithKey("int_curr_symbol", core.StringValue("int_curr_symbol"), core.StringValue(""))
		result.SetWithKey("currency_symbol", core.StringValue("currency_symbol"), core.StringValue(""))
		result.SetWithKey("mon_decimal_point", core.StringValue("mon_decimal_point"), core.StringValue("."))
		result.SetWithKey("mon_thousands_sep", core.StringValue("mon_thousands_sep"), core.StringValue(","))
		result.SetWithKey("mon_grouping", core.StringValue("mon_grouping"), core.NewList(core.NumberValue(3), core.NumberValue(0)))
		result.SetWithKey("positive_sign", core.StringValue("positive_sign"), core.StringValue(""))
		result.SetWithKey("negative_sign", core.StringValue("negative_sign"), core.StringValue("-"))
		result.SetWithKey("int_frac_digits", core.StringValue("int_frac_digits"), core.NumberValue(2))
		result.SetWithKey("frac_digits", core.StringValue("frac_digits"), core.NumberValue(2))
		result.SetWithKey("p_cs_precedes", core.StringValue("p_cs_precedes"), core.NumberValue(1))
		result.SetWithKey("p_sep_by_space", core.StringValue("p_sep_by_space"), core.NumberValue(0))
		result.SetWithKey("n_cs_precedes", core.StringValue("n_cs_precedes"), core.NumberValue(1))
		result.SetWithKey("n_sep_by_space", core.StringValue("n_sep_by_space"), core.NumberValue(0))
		result.SetWithKey("p_sign_posn", core.StringValue("p_sign_posn"), core.NumberValue(1))
		result.SetWithKey("n_sign_posn", core.StringValue("n_sign_posn"), core.NumberValue(1))

		return result, nil
	}))

	// strcoll(string1, string2) - Compare strings using locale collation
	// Stub: falls back to regular string comparison
	localeModule.Set("strcoll", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 2 {
			return nil, fmt.Errorf("strcoll() takes exactly 2 arguments (%d given)", len(args))
		}

		s1, ok1 := args[0].(core.StringValue)
		s2, ok2 := args[1].(core.StringValue)
		if !ok1 || !ok2 {
			return nil, fmt.Errorf("strcoll() arguments must be strings")
		}

		// Simple comparison (in full implementation, would use locale collation)
		if s1 < s2 {
			return core.NumberValue(-1), nil
		} else if s1 > s2 {
			return core.NumberValue(1), nil
		}
		return core.NumberValue(0), nil
	}))

	// strxfrm(string) - Transform string for locale-aware comparison
	// Stub: returns the string unchanged
	localeModule.Set("strxfrm", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("strxfrm() takes exactly 1 argument (0 given)")
		}

		str, ok := args[0].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("strxfrm() argument must be a string")
		}

		// In full implementation, would transform string for collation
		// For now, just return it unchanged
		return str, nil
	}))

	return localeModule
}
