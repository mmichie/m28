package modules

import (
	"regexp"

	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// InitReModule creates and returns a minimal re (regex) module stub
// This is a stub implementation to support basic regex operations needed by difflib
func InitReModule() *core.DictValue {
	reModule := core.NewDict()

	// compile - compile a regex pattern
	reModule.Set("compile", core.NewNamedBuiltinFunction("compile", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("compile", args)
		if err := v.Range(1, 2); err != nil {
			return nil, err
		}

		pattern, err := v.GetString(0)
		if err != nil {
			return nil, err
		}

		// Ignore flags for now (second argument)
		// TODO: Handle regex flags properly

		// Compile the Go regex
		re, compileErr := regexp.Compile(pattern)
		if compileErr != nil {
			return nil, compileErr
		}

		// Return a regex object (dict with match method)
		regexObj := core.NewDict()

		// match method
		regexObj.Set("match", core.NewBuiltinFunction(func(matchArgs []core.Value, matchCtx *core.Context) (core.Value, error) {
			mv := validation.NewArgs("match", matchArgs)
			if err := mv.Exact(1); err != nil {
				return nil, err
			}

			text, err := mv.GetString(0)
			if err != nil {
				return nil, err
			}

			// Python's match() only matches at the beginning
			loc := re.FindStringIndex(text)
			if loc == nil || loc[0] != 0 {
				return core.None, nil
			}

			// Return a match object (dict with groups, group methods)
			matchObj := core.NewDict()
			matchObj.Set("group", core.NewBuiltinFunction(func(groupArgs []core.Value, groupCtx *core.Context) (core.Value, error) {
				// Return the matched string (group 0)
				return core.StringValue(text[loc[0]:loc[1]]), nil
			}))

			return matchObj, nil
		}))

		// sub method
		regexObj.Set("sub", core.NewBuiltinFunction(func(subArgs []core.Value, subCtx *core.Context) (core.Value, error) {
			sv := validation.NewArgs("sub", subArgs)
			if err := sv.Range(2, 3); err != nil {
				return nil, err
			}

			repl, err := sv.GetString(0)
			if err != nil {
				return nil, err
			}

			text, err := sv.GetString(1)
			if err != nil {
				return nil, err
			}

			// Simple replacement (doesn't handle count argument)
			result := re.ReplaceAllString(text, repl)
			return core.StringValue(result), nil
		}))

		// search method
		regexObj.Set("search", core.NewBuiltinFunction(func(searchArgs []core.Value, searchCtx *core.Context) (core.Value, error) {
			sv := validation.NewArgs("search", searchArgs)
			if err := sv.Exact(1); err != nil {
				return nil, err
			}

			text, err := sv.GetString(0)
			if err != nil {
				return nil, err
			}

			// Python's search() finds anywhere in the string
			loc := re.FindStringIndex(text)
			if loc == nil {
				return core.None, nil
			}

			// Return a match object
			matchObj := core.NewDict()
			matchObj.Set("group", core.NewBuiltinFunction(func(groupArgs []core.Value, groupCtx *core.Context) (core.Value, error) {
				return core.StringValue(text[loc[0]:loc[1]]), nil
			}))

			return matchObj, nil
		}))

		return regexObj, nil
	}))

	// match - match pattern at beginning of string
	reModule.Set("match", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("match", args)
		if err := v.Range(2, 3); err != nil {
			return nil, err
		}

		pattern, err := v.GetString(0)
		if err != nil {
			return nil, err
		}

		text, err := v.GetString(1)
		if err != nil {
			return nil, err
		}

		re, compileErr := regexp.Compile(pattern)
		if compileErr != nil {
			return nil, compileErr
		}

		loc := re.FindStringIndex(text)
		if loc == nil || loc[0] != 0 {
			return core.None, nil
		}

		matchObj := core.NewDict()
		matchObj.Set("group", core.NewBuiltinFunction(func(groupArgs []core.Value, groupCtx *core.Context) (core.Value, error) {
			return core.StringValue(text[loc[0]:loc[1]]), nil
		}))

		return matchObj, nil
	}))

	// search - search for pattern anywhere in string
	reModule.Set("search", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("search", args)
		if err := v.Range(2, 3); err != nil {
			return nil, err
		}

		pattern, err := v.GetString(0)
		if err != nil {
			return nil, err
		}

		text, err := v.GetString(1)
		if err != nil {
			return nil, err
		}

		re, compileErr := regexp.Compile(pattern)
		if compileErr != nil {
			return nil, compileErr
		}

		loc := re.FindStringIndex(text)
		if loc == nil {
			return core.None, nil
		}

		matchObj := core.NewDict()
		matchObj.Set("group", core.NewBuiltinFunction(func(groupArgs []core.Value, groupCtx *core.Context) (core.Value, error) {
			return core.StringValue(text[loc[0]:loc[1]]), nil
		}))

		return matchObj, nil
	}))

	return reModule
}
