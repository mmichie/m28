package modules

import (
	"github.com/mmichie/m28/common/errors"
	"github.com/mmichie/m28/core"
)

// TokenizerIter implements the _tokenize.TokenizerIter class
// This is a stub implementation that provides basic tokenization for Python code
type TokenizerIter struct {
	source      core.Value
	encoding    string
	extraTokens bool
	exhausted   bool
}

func (t *TokenizerIter) Type() core.Type {
	return "TokenizerIter"
}

func (t *TokenizerIter) String() string {
	return "<_tokenize.TokenizerIter object>"
}

func (t *TokenizerIter) GetAttr(name string) (core.Value, bool) {
	switch name {
	case "__iter__":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return t, nil
		}), true
	case "__next__":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			// For now, just raise StopIteration to indicate we're done
			// A full implementation would tokenize the source
			if t.exhausted {
				return nil, &core.StopIteration{}
			}
			t.exhausted = true
			return nil, &core.StopIteration{}
		}), true
	}
	return nil, false
}

// Init_TokenizeModule creates and returns the _tokenize module
func Init_TokenizeModule() *core.DictValue {
	tokenizeModule := core.NewDict()

	// TokenizerIter class constructor
	tokenizerIterClass := core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Parse arguments: source, encoding=None, extra_tokens=False
		if len(args) < 1 {
			return nil, errors.NewTypeError("TokenizerIter", "at least 1 argument", "0 arguments")
		}

		source := args[0]
		encoding := "utf-8" // default
		extraTokens := false

		// Check for keyword arguments if provided
		// For now, just handle positional arguments
		if len(args) > 1 {
			// Second arg could be encoding
			if str, ok := args[1].(core.StringValue); ok {
				encoding = string(str)
			}
		}
		if len(args) > 2 {
			// Third arg could be extra_tokens
			if b, ok := args[2].(core.BoolValue); ok {
				extraTokens = bool(b)
			}
		}

		return &TokenizerIter{
			source:      source,
			encoding:    encoding,
			extraTokens: extraTokens,
			exhausted:   false,
		}, nil
	})

	tokenizeModule.Set("TokenizerIter", tokenizerIterClass)

	return tokenizeModule
}
