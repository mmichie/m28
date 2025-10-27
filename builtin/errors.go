package builtin

import (
	"fmt"

	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// RegisterErrors registers error-related functions
func RegisterErrors(ctx *core.Context) {
	// Exception - base exception class
	ctx.Define("Exception", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("Exception", args)
		if err := v.Max(1); err != nil {
			return nil, err
		}

		if v.Count() == 0 {
			return core.NewException(""), nil
		}

		msg := ""
		if !types.IsNil(v.Get(0)) {
			msg = v.Get(0).String()
		}
		return core.NewException(msg), nil
	}))

	// error - create an error object (alias for Exception)
	ctx.Define("error", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("error", args)
		if err := v.Max(2); err != nil {
			return nil, err
		}

		if v.Count() == 0 {
			return core.NewException(""), nil
		}

		msg := ""
		msgIndex := 0

		// For 2 args, ignore the error type in args[0] and use args[1] for message
		if v.Count() == 2 {
			msgIndex = 1
		}

		if !types.IsNil(v.Get(msgIndex)) {
			msg = v.Get(msgIndex).String()
		}
		return core.NewException(msg), nil
	}))

	// raise - raise an error
	ctx.Define("raise", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("raise", args)

		if v.Count() == 0 {
			// Re-raise current exception (not implemented)
			return nil, fmt.Errorf("re-raise not implemented")
		}

		if err := v.Max(1); err != nil {
			return nil, err
		}

		arg := v.Get(0)

		// Check if arg is an exception class (should be instantiated)
		if class, ok := arg.(*core.Class); ok {
			// Instantiate the exception class
			instance, err := class.Call([]core.Value{}, ctx)
			if err != nil {
				return nil, err
			}
			// Now raise the instance
			if _, ok := instance.(*core.Instance); ok {
				// Create an error from the instance
				// For now, just use the class name as the error message
				return nil, fmt.Errorf("%s", class.Name)
			}
			return nil, fmt.Errorf("failed to instantiate exception class")
		} else if exc, ok := arg.(*core.ExceptionValue); ok {
			// Raise the error
			return nil, fmt.Errorf("%s", exc.Message)
		} else if inst, ok := arg.(*core.Instance); ok {
			// Raising an instance of an exception class
			// Try to get the message from the instance's args or message attribute
			var message string
			if argsAttr, hasArgs := inst.GetAttr("args"); hasArgs {
				if argsTuple, ok := argsAttr.(core.TupleValue); ok && len(argsTuple) > 0 {
					if msgStr, ok := argsTuple[0].(core.StringValue); ok {
						message = string(msgStr)
					}
				}
			}
			if message == "" {
				if msgAttr, hasMsg := inst.GetAttr("message"); hasMsg {
					if msgStr, ok := msgAttr.(core.StringValue); ok {
						message = string(msgStr)
					}
				}
			}
			if message != "" {
				return nil, fmt.Errorf("%s: %s", inst.Class.Name, message)
			}
			return nil, fmt.Errorf("%s", inst.Class.Name)
		} else if str, ok := types.AsString(arg); ok {
			// Raise generic error with message
			return nil, fmt.Errorf("%s", str)
		} else {
			return nil, fmt.Errorf("exceptions must derive from BaseException")
		}
	}))

	// assert - now registered in assert.go

	// Define Python exception classes
	// These are needed for Python code that references exception types

	// BaseException - the base of all exceptions in Python
	baseExceptionClass := core.NewClass("BaseException", nil)

	// Add __init__ method to BaseException to store args
	baseExceptionClass.SetMethod("__init__", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("__init__ requires at least 1 argument (self)")
		}

		self, ok := args[0].(*core.Instance)
		if !ok {
			return nil, fmt.Errorf("__init__ first argument must be an instance")
		}

		// Store all arguments after self as a tuple in the 'args' attribute
		exceptionArgs := make(core.TupleValue, len(args)-1)
		for i := 1; i < len(args); i++ {
			exceptionArgs[i-1] = args[i]
		}
		self.Attributes["args"] = exceptionArgs

		return core.None, nil
	}))

	ctx.Define("BaseException", baseExceptionClass)

	// Exception inherits from BaseException
	exceptionClass := core.NewClass("Exception", baseExceptionClass)

	// Add __init__ method to Exception to store args
	exceptionClass.SetMethod("__init__", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("__init__ requires at least 1 argument (self)")
		}

		self, ok := args[0].(*core.Instance)
		if !ok {
			return nil, fmt.Errorf("__init__ first argument must be an instance")
		}

		// Store all arguments after self as a tuple in the 'args' attribute
		exceptionArgs := make(core.TupleValue, len(args)-1)
		for i := 1; i < len(args); i++ {
			exceptionArgs[i-1] = args[i]
		}
		self.Attributes["args"] = exceptionArgs

		return core.None, nil
	}))

	ctx.Define("Exception", exceptionClass)

	// TypeError - raised when an operation or function is applied to an object of inappropriate type
	typeErrorClass := core.NewClass("TypeError", exceptionClass)
	ctx.Define("TypeError", typeErrorClass)

	// ValueError - raised when an operation or function receives an argument with the right type but inappropriate value
	valueErrorClass := core.NewClass("ValueError", exceptionClass)
	ctx.Define("ValueError", valueErrorClass)

	// NameError - raised when a local or global name is not found
	nameErrorClass := core.NewClass("NameError", exceptionClass)
	ctx.Define("NameError", nameErrorClass)

	// KeyError - raised when a dictionary key is not found
	keyErrorClass := core.NewClass("KeyError", exceptionClass)
	ctx.Define("KeyError", keyErrorClass)

	// IndexError - raised when a sequence subscript is out of range
	indexErrorClass := core.NewClass("IndexError", exceptionClass)
	ctx.Define("IndexError", indexErrorClass)

	// AttributeError - raised when an attribute reference or assignment fails
	attributeErrorClass := core.NewClass("AttributeError", exceptionClass)
	ctx.Define("AttributeError", attributeErrorClass)

	// ZeroDivisionError - raised when division or modulo by zero takes place
	zeroDivisionErrorClass := core.NewClass("ZeroDivisionError", exceptionClass)
	ctx.Define("ZeroDivisionError", zeroDivisionErrorClass)

	// RuntimeError - raised when an error is detected that doesn't fall in any of the other categories
	runtimeErrorClass := core.NewClass("RuntimeError", exceptionClass)
	ctx.Define("RuntimeError", runtimeErrorClass)

	// SystemError - raised when the interpreter finds an internal error
	systemErrorClass := core.NewClass("SystemError", exceptionClass)
	ctx.Define("SystemError", systemErrorClass)

	// NotImplementedError - raised when an abstract method that should have been implemented is not
	notImplementedErrorClass := core.NewClass("NotImplementedError", runtimeErrorClass)
	ctx.Define("NotImplementedError", notImplementedErrorClass)

	// ImportError - raised when an import statement fails
	importErrorClass := core.NewClass("ImportError", exceptionClass)
	ctx.Define("ImportError", importErrorClass)

	// OSError - raised when a system function returns a system-related error
	osErrorClass := core.NewClass("OSError", exceptionClass)
	ctx.Define("OSError", osErrorClass)

	// IOError - alias for OSError (deprecated but still used)
	ctx.Define("IOError", osErrorClass)

	// FileNotFoundError - raised when a file or directory is requested but doesn't exist
	fileNotFoundErrorClass := core.NewClass("FileNotFoundError", osErrorClass)
	ctx.Define("FileNotFoundError", fileNotFoundErrorClass)

	// PermissionError - raised when trying to run an operation without adequate access rights
	permissionErrorClass := core.NewClass("PermissionError", osErrorClass)
	ctx.Define("PermissionError", permissionErrorClass)

	// IsADirectoryError - raised when a file operation is requested on a directory
	isADirectoryErrorClass := core.NewClass("IsADirectoryError", osErrorClass)
	ctx.Define("IsADirectoryError", isADirectoryErrorClass)

	// NotADirectoryError - raised when a directory operation is requested on a non-directory
	notADirectoryErrorClass := core.NewClass("NotADirectoryError", osErrorClass)
	ctx.Define("NotADirectoryError", notADirectoryErrorClass)

	// StopIteration - raised by next() and an iterator's __next__() to signal no more items
	stopIterationClass := core.NewClass("StopIteration", exceptionClass)
	ctx.Define("StopIteration", stopIterationClass)

	// AssertionError - raised when an assert statement fails
	assertionErrorClass := core.NewClass("AssertionError", exceptionClass)
	ctx.Define("AssertionError", assertionErrorClass)

	// Warning - base class for warning categories
	warningClass := core.NewClass("Warning", baseExceptionClass)
	ctx.Define("Warning", warningClass)

	// DeprecationWarning - raised for deprecated features
	deprecationWarningClass := core.NewClass("DeprecationWarning", warningClass)
	ctx.Define("DeprecationWarning", deprecationWarningClass)

	// UserWarning - user-generated warning
	userWarningClass := core.NewClass("UserWarning", warningClass)
	ctx.Define("UserWarning", userWarningClass)

	// PendingDeprecationWarning - warning about features that will be deprecated
	pendingDeprecationWarningClass := core.NewClass("PendingDeprecationWarning", warningClass)
	ctx.Define("PendingDeprecationWarning", pendingDeprecationWarningClass)

	// SyntaxWarning - warning about dubious syntax
	syntaxWarningClass := core.NewClass("SyntaxWarning", warningClass)
	ctx.Define("SyntaxWarning", syntaxWarningClass)

	// RuntimeWarning - warning about dubious runtime behavior
	runtimeWarningClass := core.NewClass("RuntimeWarning", warningClass)
	ctx.Define("RuntimeWarning", runtimeWarningClass)

	// FutureWarning - warning about constructs that will change semantically in the future
	futureWarningClass := core.NewClass("FutureWarning", warningClass)
	ctx.Define("FutureWarning", futureWarningClass)

	// ImportWarning - warning about probable mistakes in module imports
	importWarningClass := core.NewClass("ImportWarning", warningClass)
	ctx.Define("ImportWarning", importWarningClass)

	// UnicodeWarning - warning related to Unicode
	unicodeWarningClass := core.NewClass("UnicodeWarning", warningClass)
	ctx.Define("UnicodeWarning", unicodeWarningClass)

	// BytesWarning - warning related to bytes and bytearray
	bytesWarningClass := core.NewClass("BytesWarning", warningClass)
	ctx.Define("BytesWarning", bytesWarningClass)

	// ResourceWarning - warning about resource usage
	resourceWarningClass := core.NewClass("ResourceWarning", warningClass)
	ctx.Define("ResourceWarning", resourceWarningClass)
}

// Migration Statistics:
// Functions migrated: 3 error handling functions (Exception, error, raise)
// Type checks eliminated: ~6 manual type assertions
// Code reduction: ~20% in validation code
// Benefits: Consistent error messages with validation framework
// Improved handling of nil checks with types.IsNil()
