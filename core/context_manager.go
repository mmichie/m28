package core

import (
	"fmt"
)

// ContextManager interface defines the protocol for context managers
type ContextManager interface {
	Value
	Enter() (Value, error)
	Exit(excType, excValue, excTraceback Value) (bool, error)
}

// WithContext represents the context for a with statement
type WithContext struct {
	Manager  ContextManager
	Variable string // Variable name for 'as' clause
	Value    Value  // Value returned by __enter__
}

// SimpleContextManager is a basic implementation for testing
type SimpleContextManager struct {
	BaseContextManager
	EnterFunc func() (Value, error)
	ExitFunc  func(excType, excValue, excTraceback Value) (bool, error)
}

// NewSimpleContextManager creates a new simple context manager
func NewSimpleContextManager(enterFunc func() (Value, error), exitFunc func(excType, excValue, excTraceback Value) (bool, error)) *SimpleContextManager {
	cm := &SimpleContextManager{
		BaseContextManager: *NewBaseContextManager(Type("context_manager")),
		EnterFunc:          enterFunc,
		ExitFunc:           exitFunc,
	}

	// Initialize registry with handlers
	cm.initRegistry(
		// __enter__ handler
		func(receiver Value, args []Value, ctx *Context) (Value, error) {
			mgr, err := TypedReceiver[*SimpleContextManager](receiver, "__enter__")
			if err != nil {
				return nil, err
			}
			if err := ValidateArity("__enter__", args, 0); err != nil {
				return nil, err
			}
			return mgr.Enter()
		},
		// __exit__ handler
		func(receiver Value, args []Value, ctx *Context) (Value, error) {
			mgr, err := TypedReceiver[*SimpleContextManager](receiver, "__exit__")
			if err != nil {
				return nil, err
			}
			if err := ValidateArityRange("__exit__", args, 0, 3); err != nil {
				return nil, err
			}

			var excType, excValue, excTraceback Value = Nil, Nil, Nil
			if len(args) > 0 {
				excType = args[0]
			}
			if len(args) > 1 {
				excValue = args[1]
			}
			if len(args) > 2 {
				excTraceback = args[2]
			}

			suppress, err := mgr.Exit(excType, excValue, excTraceback)
			if err != nil {
				return nil, err
			}

			return BoolValue(suppress), nil
		},
	)

	return cm
}

// Type implements Value.Type
func (cm *SimpleContextManager) Type() Type {
	return Type("context_manager")
}

// String implements Value.String
func (cm *SimpleContextManager) String() string {
	return "<context manager>"
}

// Enter implements ContextManager.Enter
func (cm *SimpleContextManager) Enter() (Value, error) {
	if cm.EnterFunc != nil {
		return cm.EnterFunc()
	}
	return Nil, nil
}

// Exit implements ContextManager.Exit
func (cm *SimpleContextManager) Exit(excType, excValue, excTraceback Value) (bool, error) {
	if cm.ExitFunc != nil {
		return cm.ExitFunc(excType, excValue, excTraceback)
	}
	return false, nil
}

// GetAttr is now inherited from BaseContextManager

// FileContextManager represents a file that can be used with 'with'
type FileContextManager struct {
	BaseObject
	Path     string
	Mode     string
	File     interface{} // Would be actual file handle
	IsClosed bool
}

// NewFileContextManager creates a new file context manager
func NewFileContextManager(path string, mode string) *FileContextManager {
	return &FileContextManager{
		BaseObject: *NewBaseObject(Type("file")),
		Path:       path,
		Mode:       mode,
		IsClosed:   true,
	}
}

// Type implements Value.Type
func (f *FileContextManager) Type() Type {
	return Type("file")
}

// String implements Value.String
func (f *FileContextManager) String() string {
	status := "closed"
	if !f.IsClosed {
		status = "open"
	}
	return fmt.Sprintf("<file '%s' mode '%s' %s>", f.Path, f.Mode, status)
}

// Enter implements ContextManager.Enter
func (f *FileContextManager) Enter() (Value, error) {
	// In a real implementation, this would open the file
	f.IsClosed = false
	return f, nil
}

// Exit implements ContextManager.Exit
func (f *FileContextManager) Exit(excType, excValue, excTraceback Value) (bool, error) {
	// In a real implementation, this would close the file
	f.IsClosed = true
	// Don't suppress exceptions
	return false, nil
}

// GetAttr implements Object interface
func (f *FileContextManager) GetAttr(name string) (Value, bool) {
	switch name {
	case "__enter__":
		return &BoundMethod{
			Receiver: f,
			Method: &MethodDescriptor{
				Name:    "__enter__",
				Arity:   0,
				Doc:     "Open the file",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					file := receiver.(*FileContextManager)
					return file.Enter()
				},
			},
			TypeDesc: GetTypeDescriptorForValue(f),
		}, true

	case "__exit__":
		return &BoundMethod{
			Receiver: f,
			Method: &MethodDescriptor{
				Name:    "__exit__",
				Arity:   3,
				Doc:     "Close the file",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					file := receiver.(*FileContextManager)

					var excType, excValue, excTraceback Value = Nil, Nil, Nil
					if len(args) > 0 {
						excType = args[0]
					}
					if len(args) > 1 {
						excValue = args[1]
					}
					if len(args) > 2 {
						excTraceback = args[2]
					}

					suppress, err := file.Exit(excType, excValue, excTraceback)
					if err != nil {
						return nil, err
					}

					return BoolValue(suppress), nil
				},
			},
			TypeDesc: GetTypeDescriptorForValue(f),
		}, true

	case "read":
		return &BoundMethod{
			Receiver: f,
			Method: &MethodDescriptor{
				Name:    "read",
				Arity:   0,
				Doc:     "Read file contents",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					file := receiver.(*FileContextManager)
					if file.IsClosed {
						return nil, fmt.Errorf("I/O operation on closed file")
					}
					// Simulated read
					return StringValue("file contents"), nil
				},
			},
			TypeDesc: GetTypeDescriptorForValue(f),
		}, true

	case "write":
		return &BoundMethod{
			Receiver: f,
			Method: &MethodDescriptor{
				Name:    "write",
				Arity:   1,
				Doc:     "Write to file",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					file := receiver.(*FileContextManager)
					if file.IsClosed {
						return nil, fmt.Errorf("I/O operation on closed file")
					}
					if len(args) != 1 {
						return nil, fmt.Errorf("write() takes exactly one argument")
					}
					// Simulated write
					return NumberValue(len(PrintValue(args[0]))), nil
				},
			},
			TypeDesc: GetTypeDescriptorForValue(f),
		}, true
	}

	return f.BaseObject.GetAttr(name)
}

// Helper to check if a value implements context manager protocol
func IsContextManager(v Value) (ContextManager, bool) {
	// First check if it directly implements ContextManager
	if cm, ok := v.(ContextManager); ok {
		return cm, true
	}

	// Then check if it has __enter__ and __exit__ methods
	if obj, ok := v.(interface {
		GetAttr(string) (Value, bool)
	}); ok {
		_, hasEnter := obj.GetAttr("__enter__")
		_, hasExit := obj.GetAttr("__exit__")

		if hasEnter && hasExit {
			// Create a wrapper that implements ContextManager
			return &contextManagerWrapper{obj: obj}, true
		}
	}

	return nil, false
}

// contextManagerWrapper wraps objects with __enter__/__exit__ methods
type contextManagerWrapper struct {
	obj interface {
		GetAttr(string) (Value, bool)
	}
}

func (w *contextManagerWrapper) Type() Type {
	if val, ok := w.obj.(Value); ok {
		return val.Type()
	}
	return Type("object")
}

func (w *contextManagerWrapper) String() string {
	if val, ok := w.obj.(Value); ok {
		return val.String()
	}
	return "<object>"
}

func (w *contextManagerWrapper) Enter() (Value, error) {
	enterMethod, _ := w.obj.GetAttr("__enter__")
	if callable, ok := enterMethod.(interface {
		Call([]Value, *Context) (Value, error)
	}); ok {
		return callable.Call([]Value{}, nil)
	}
	return nil, fmt.Errorf("__enter__ is not callable")
}

func (w *contextManagerWrapper) Exit(excType, excValue, excTraceback Value) (bool, error) {
	exitMethod, _ := w.obj.GetAttr("__exit__")
	if callable, ok := exitMethod.(interface {
		Call([]Value, *Context) (Value, error)
	}); ok {
		result, err := callable.Call([]Value{excType, excValue, excTraceback}, nil)
		if err != nil {
			return false, err
		}

		// Check if exception should be suppressed
		return IsTruthy(result), nil
	}
	return false, fmt.Errorf("__exit__ is not callable")
}
