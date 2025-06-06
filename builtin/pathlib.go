package builtin

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/mmichie/m28/core"
)

// PathType represents the Path type
var PathType = core.Type("Path")

// Path represents a filesystem path
type Path struct {
	core.BaseObject
	path string
}

// NewPath creates a new Path object
func NewPath(path string) *Path {
	p := &Path{
		BaseObject: *core.NewBaseObject(PathType),
		path:       path,
	}
	return p
}

// Type returns the type of the Path
func (p *Path) Type() core.Type {
	return PathType
}

// String returns the string representation
func (p *Path) String() string {
	return p.path
}

// GetAttr implements the Object interface for Path
func (p *Path) GetAttr(name string) (core.Value, bool) {
	switch name {
	case "name":
		return core.StringValue(filepath.Base(p.path)), true

	case "parent":
		return NewPath(filepath.Dir(p.path)), true

	case "suffix":
		ext := filepath.Ext(p.path)
		return core.StringValue(ext), true

	case "stem":
		base := filepath.Base(p.path)
		ext := filepath.Ext(base)
		if ext != "" {
			base = base[:len(base)-len(ext)]
		}
		return core.StringValue(base), true

	case "parts":
		parts := strings.Split(p.path, string(filepath.Separator))
		values := make([]core.Value, len(parts))
		for i, part := range parts {
			values[i] = core.StringValue(part)
		}
		return core.TupleValue(values), true

	case "exists":
		return &core.BoundMethod{
			Receiver: p,
			Method: &core.MethodDescriptor{
				Name:    "exists",
				Arity:   0,
				Doc:     "Check if the path exists",
				Builtin: true,
				Handler: func(receiver core.Value, args []core.Value, ctx *core.Context) (core.Value, error) {
					path := receiver.(*Path)
					_, err := os.Stat(path.path)
					return core.BoolValue(err == nil), nil
				},
			},
			TypeDesc: &core.TypeDescriptor{
				Name:       string(PathType),
				PythonName: "Path",
			},
		}, true

	case "is_file":
		return &core.BoundMethod{
			Receiver: p,
			Method: &core.MethodDescriptor{
				Name:    "is_file",
				Arity:   0,
				Doc:     "Check if the path is a file",
				Builtin: true,
				Handler: func(receiver core.Value, args []core.Value, ctx *core.Context) (core.Value, error) {
					path := receiver.(*Path)
					info, err := os.Stat(path.path)
					if err != nil {
						return core.False, nil
					}
					return core.BoolValue(!info.IsDir()), nil
				},
			},
			TypeDesc: &core.TypeDescriptor{
				Name:       string(PathType),
				PythonName: "Path",
			},
		}, true

	case "is_dir":
		return &core.BoundMethod{
			Receiver: p,
			Method: &core.MethodDescriptor{
				Name:    "is_dir",
				Arity:   0,
				Doc:     "Check if the path is a directory",
				Builtin: true,
				Handler: func(receiver core.Value, args []core.Value, ctx *core.Context) (core.Value, error) {
					path := receiver.(*Path)
					info, err := os.Stat(path.path)
					if err != nil {
						return core.False, nil
					}
					return core.BoolValue(info.IsDir()), nil
				},
			},
			TypeDesc: &core.TypeDescriptor{
				Name:       string(PathType),
				PythonName: "Path",
			},
		}, true

	case "mkdir":
		return &core.BoundMethod{
			Receiver: p,
			Method: &core.MethodDescriptor{
				Name:    "mkdir",
				Arity:   -1, // Variable args
				Doc:     "Create directory",
				Builtin: true,
				Handler: func(receiver core.Value, args []core.Value, ctx *core.Context) (core.Value, error) {
					path := receiver.(*Path)

					// Check for parents=True kwarg (simplified - just check if any arg is True)
					parents := false
					for _, arg := range args {
						if b, ok := arg.(core.BoolValue); ok && bool(b) {
							parents = true
							break
						}
					}

					var err error
					if parents {
						err = os.MkdirAll(path.path, 0755)
					} else {
						err = os.Mkdir(path.path, 0755)
					}

					if err != nil {
						return nil, fmt.Errorf("mkdir: %v", err)
					}
					return core.None, nil
				},
			},
			TypeDesc: &core.TypeDescriptor{
				Name:       string(PathType),
				PythonName: "Path",
			},
		}, true

	case "unlink":
		return &core.BoundMethod{
			Receiver: p,
			Method: &core.MethodDescriptor{
				Name:    "unlink",
				Arity:   0,
				Doc:     "Remove file",
				Builtin: true,
				Handler: func(receiver core.Value, args []core.Value, ctx *core.Context) (core.Value, error) {
					path := receiver.(*Path)
					err := os.Remove(path.path)
					if err != nil {
						return nil, fmt.Errorf("unlink: %v", err)
					}
					return core.None, nil
				},
			},
			TypeDesc: &core.TypeDescriptor{
				Name:       string(PathType),
				PythonName: "Path",
			},
		}, true

	case "rmdir":
		return &core.BoundMethod{
			Receiver: p,
			Method: &core.MethodDescriptor{
				Name:    "rmdir",
				Arity:   0,
				Doc:     "Remove empty directory",
				Builtin: true,
				Handler: func(receiver core.Value, args []core.Value, ctx *core.Context) (core.Value, error) {
					path := receiver.(*Path)
					err := os.Remove(path.path)
					if err != nil {
						return nil, fmt.Errorf("rmdir: %v", err)
					}
					return core.None, nil
				},
			},
			TypeDesc: &core.TypeDescriptor{
				Name:       string(PathType),
				PythonName: "Path",
			},
		}, true

	case "glob":
		return &core.BoundMethod{
			Receiver: p,
			Method: &core.MethodDescriptor{
				Name:    "glob",
				Arity:   1,
				Doc:     "Glob pattern matching",
				Builtin: true,
				Handler: func(receiver core.Value, args []core.Value, ctx *core.Context) (core.Value, error) {
					if len(args) != 1 {
						return nil, fmt.Errorf("glob() takes exactly 1 argument")
					}

					pattern, ok := args[0].(core.StringValue)
					if !ok {
						return nil, fmt.Errorf("glob() argument must be str")
					}

					path := receiver.(*Path)
					fullPattern := filepath.Join(path.path, string(pattern))

					matches, err := filepath.Glob(fullPattern)
					if err != nil {
						return nil, fmt.Errorf("glob: %v", err)
					}

					result := make([]core.Value, len(matches))
					for i, match := range matches {
						result[i] = NewPath(match)
					}

					return core.ListValue(result), nil
				},
			},
			TypeDesc: &core.TypeDescriptor{
				Name:       string(PathType),
				PythonName: "Path",
			},
		}, true

	case "joinpath":
		return &core.BoundMethod{
			Receiver: p,
			Method: &core.MethodDescriptor{
				Name:    "joinpath",
				Arity:   -1, // Variable args
				Doc:     "Join path components",
				Builtin: true,
				Handler: func(receiver core.Value, args []core.Value, ctx *core.Context) (core.Value, error) {
					path := receiver.(*Path)
					parts := []string{path.path}

					for _, arg := range args {
						str, ok := arg.(core.StringValue)
						if !ok {
							return nil, fmt.Errorf("joinpath() arguments must be strings")
						}
						parts = append(parts, string(str))
					}

					return NewPath(filepath.Join(parts...)), nil
				},
			},
			TypeDesc: &core.TypeDescriptor{
				Name:       string(PathType),
				PythonName: "Path",
			},
		}, true

	case "resolve":
		return &core.BoundMethod{
			Receiver: p,
			Method: &core.MethodDescriptor{
				Name:    "resolve",
				Arity:   0,
				Doc:     "Resolve to absolute path",
				Builtin: true,
				Handler: func(receiver core.Value, args []core.Value, ctx *core.Context) (core.Value, error) {
					path := receiver.(*Path)
					abs, err := filepath.Abs(path.path)
					if err != nil {
						return nil, fmt.Errorf("resolve: %v", err)
					}
					return NewPath(abs), nil
				},
			},
			TypeDesc: &core.TypeDescriptor{
				Name:       string(PathType),
				PythonName: "Path",
			},
		}, true

	case "read_text":
		return &core.BoundMethod{
			Receiver: p,
			Method: &core.MethodDescriptor{
				Name:    "read_text",
				Arity:   0,
				Doc:     "Read file contents as text",
				Builtin: true,
				Handler: func(receiver core.Value, args []core.Value, ctx *core.Context) (core.Value, error) {
					path := receiver.(*Path)
					data, err := os.ReadFile(path.path)
					if err != nil {
						return nil, fmt.Errorf("read_text: %v", err)
					}
					return core.StringValue(string(data)), nil
				},
			},
			TypeDesc: &core.TypeDescriptor{
				Name:       string(PathType),
				PythonName: "Path",
			},
		}, true

	case "write_text":
		return &core.BoundMethod{
			Receiver: p,
			Method: &core.MethodDescriptor{
				Name:    "write_text",
				Arity:   1,
				Doc:     "Write text to file",
				Builtin: true,
				Handler: func(receiver core.Value, args []core.Value, ctx *core.Context) (core.Value, error) {
					if len(args) != 1 {
						return nil, fmt.Errorf("write_text() takes exactly 1 argument")
					}

					text, ok := args[0].(core.StringValue)
					if !ok {
						return nil, fmt.Errorf("write_text() argument must be str")
					}

					path := receiver.(*Path)
					err := os.WriteFile(path.path, []byte(text), 0644)
					if err != nil {
						return nil, fmt.Errorf("write_text: %v", err)
					}

					return core.NumberValue(len(text)), nil
				},
			},
			TypeDesc: &core.TypeDescriptor{
				Name:       string(PathType),
				PythonName: "Path",
			},
		}, true

	case "__str__":
		return &core.BoundMethod{
			Receiver: p,
			Method: &core.MethodDescriptor{
				Name:    "__str__",
				Arity:   0,
				Doc:     "String representation",
				Builtin: true,
				Handler: func(receiver core.Value, args []core.Value, ctx *core.Context) (core.Value, error) {
					path := receiver.(*Path)
					return core.StringValue(path.path), nil
				},
			},
			TypeDesc: &core.TypeDescriptor{
				Name:       string(PathType),
				PythonName: "Path",
			},
		}, true

	case "__truediv__":
		return &core.BoundMethod{
			Receiver: p,
			Method: &core.MethodDescriptor{
				Name:    "__truediv__",
				Arity:   1,
				Doc:     "Path division (join)",
				Builtin: true,
				Handler: func(receiver core.Value, args []core.Value, ctx *core.Context) (core.Value, error) {
					if len(args) != 1 {
						return nil, fmt.Errorf("__truediv__ takes exactly 1 argument")
					}

					other, ok := args[0].(core.StringValue)
					if !ok {
						return nil, fmt.Errorf("unsupported operand type(s) for /: 'Path' and '%s'", args[0].Type())
					}

					path := receiver.(*Path)
					return NewPath(filepath.Join(path.path, string(other))), nil
				},
			},
			TypeDesc: &core.TypeDescriptor{
				Name:       string(PathType),
				PythonName: "Path",
			},
		}, true
	}

	return nil, false
}

// RegisterPathlib registers the pathlib module
func RegisterPathlib(ctx *core.Context) error {
	pathlib := make(map[string]core.Value)

	// Path constructor
	pathFunc := func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) == 0 {
			return NewPath("."), nil
		}

		if len(args) == 1 {
			str, ok := args[0].(core.StringValue)
			if !ok {
				return nil, fmt.Errorf("Path() argument must be str, not %s", args[0].Type())
			}
			return NewPath(string(str)), nil
		}

		// Multiple arguments - join them
		parts := make([]string, len(args))
		for i, arg := range args {
			str, ok := arg.(core.StringValue)
			if !ok {
				return nil, fmt.Errorf("Path() argument %d must be str, not %s", i+1, arg.Type())
			}
			parts[i] = string(str)
		}

		return NewPath(filepath.Join(parts...)), nil
	}
	pathlib["Path"] = core.NewBuiltinFunction(pathFunc)

	// PurePath (alias for Path for now)
	pathlib["PurePath"] = pathlib["Path"]

	// PosixPath (alias for Path for now)
	pathlib["PosixPath"] = pathlib["Path"]

	// WindowsPath (alias for Path for now)
	pathlib["WindowsPath"] = pathlib["Path"]

	// Create module as a dict
	pathlibModule := core.NewDict()
	for name, fn := range pathlib {
		pathlibModule.SetWithKey(name, core.StringValue(name), fn)
	}

	// Register the module in the module registry
	registry := core.GetModuleRegistry()
	registry.StoreModule("pathlib", pathlibModule, "<builtin>", []string{})

	// Register Path type
	core.RegisterType(&core.TypeDescriptor{
		Name: string(PathType),
	})

	return nil
}
