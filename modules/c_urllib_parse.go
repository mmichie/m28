package modules

import (
	"net/url"

	"github.com/mmichie/m28/core"
)

// Init_UrllibParseModule creates the urllib.parse module
// Minimal implementation providing only what pathlib needs
func Init_UrllibParseModule() *core.DictValue {
	module := core.NewDict()

	// quote_from_bytes: URL-encode bytes
	// pathlib uses this for file:// URIs
	module.Set("quote_from_bytes", core.NewBuiltinFunction(
		func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) < 1 {
				return nil, core.NewTypeError("bytes", nil, "quote_from_bytes() missing required positional argument")
			}

			// Get the bytes argument
			var bytes []byte
			switch v := args[0].(type) {
			case core.BytesValue:
				bytes = []byte(v)
			case core.StringValue:
				bytes = []byte(string(v))
			default:
				return nil, core.NewTypeError("bytes", args[0], "quote_from_bytes() argument")
			}

			// URL encode
			encoded := url.QueryEscape(string(bytes))
			return core.StringValue(encoded), nil
		}))

	// unquote_to_bytes: URL-decode to bytes
	module.Set("unquote_to_bytes", core.NewBuiltinFunction(
		func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) < 1 {
				return nil, core.NewTypeError("str", nil, "unquote_to_bytes() missing required positional argument")
			}

			strVal, ok := args[0].(core.StringValue)
			if !ok {
				return nil, core.NewTypeError("str", args[0], "unquote_to_bytes() argument")
			}

			decoded, err := url.QueryUnescape(string(strVal))
			if err != nil {
				return nil, err
			}

			return core.BytesValue([]byte(decoded)), nil
		}))

	// urlparse: Parse a URL into components
	// Minimal implementation for basic use
	module.Set("urlparse", core.NewBuiltinFunction(
		func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) < 1 {
				return nil, core.NewTypeError("str", nil, "urlparse() missing required positional argument")
			}

			urlStr, ok := args[0].(core.StringValue)
			if !ok {
				return nil, core.NewTypeError("str", args[0], "urlparse() argument")
			}

			parsed, err := url.Parse(string(urlStr))
			if err != nil {
				return core.StringValue(""), nil // Return empty on error for compatibility
			}

			// Return a simple namespace-like dict with url components
			result := core.NewDict()
			result.Set("scheme", core.StringValue(parsed.Scheme))
			result.Set("netloc", core.StringValue(parsed.Host))
			result.Set("path", core.StringValue(parsed.Path))
			result.Set("query", core.StringValue(parsed.RawQuery))
			result.Set("fragment", core.StringValue(parsed.Fragment))

			return result, nil
		}))

	return module
}
