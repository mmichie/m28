package modules

import (
	"crypto/sha512"
	"fmt"

	"github.com/mmichie/m28/core"
)

// sha512Object wraps a SHA-512 hash object
type sha512Object struct {
	data []byte
}

func (s *sha512Object) Type() core.Type {
	return "_sha512.sha512"
}

func (s *sha512Object) String() string {
	return "<sha512 _hashlib.HASH object>"
}

func (s *sha512Object) GetAttr(name string) (core.Value, bool) {
	switch name {
	case "digest":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			hash := sha512.Sum512(s.data)
			return core.BytesValue(hash[:]), nil
		}), true

	case "hexdigest":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			hash := sha512.Sum512(s.data)
			return core.StringValue(fmt.Sprintf("%x", hash)), nil
		}), true

	case "update":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("update() takes exactly 1 argument")
			}
			switch v := args[0].(type) {
			case core.BytesValue:
				s.data = append(s.data, []byte(v)...)
			case core.StringValue:
				s.data = append(s.data, []byte(string(v))...)
			default:
				return nil, fmt.Errorf("update() argument must be bytes or string")
			}
			return core.NilValue{}, nil
		}), true

	case "copy":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			newData := make([]byte, len(s.data))
			copy(newData, s.data)
			return &sha512Object{data: newData}, nil
		}), true

	case "digest_size":
		return core.NumberValue(64), true

	case "block_size":
		return core.NumberValue(128), true

	case "name":
		return core.StringValue("sha512"), true
	}
	return nil, false
}

// InitSHA512Module creates the _sha512 C extension module
func InitSHA512Module() *core.DictValue {
	sha512Module := core.NewDict()

	// sha512(data=b'') - create a new SHA-512 hash object
	sha512Module.Set("sha512", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		obj := &sha512Object{data: []byte{}}

		if len(args) >= 1 {
			switch v := args[0].(type) {
			case core.BytesValue:
				obj.data = []byte(v)
			case core.StringValue:
				obj.data = []byte(string(v))
			default:
				return nil, fmt.Errorf("sha512() argument must be bytes or string")
			}
		}

		return obj, nil
	}))

	return sha512Module
}
