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
			// Use PythonError to preserve the instance for proper try/except matching
			return nil, core.NewPythonError(inst)
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

		// Initialize exception chaining attributes (PEP 3134)
		self.Attributes["__cause__"] = core.None             // Explicit exception chaining (raise ... from ...)
		self.Attributes["__context__"] = core.None           // Implicit exception chaining
		self.Attributes["__suppress_context__"] = core.False // Whether to suppress context display
		self.Attributes["__traceback__"] = core.None         // Traceback object

		return core.None, nil
	}))

	// Add __str__ method to BaseException to show the first arg
	baseExceptionClass.SetMethod("__str__", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("__str__() missing 1 required positional argument: 'self'")
		}

		self, ok := args[0].(*core.Instance)
		if !ok {
			return core.StringValue(""), nil
		}

		// Get the args attribute
		if argsVal, hasArgs := self.Attributes["args"]; hasArgs {
			if argsTuple, ok := argsVal.(core.TupleValue); ok && len(argsTuple) > 0 {
				// Return the first argument as the string representation
				return core.StringValue(argsTuple[0].String()), nil
			}
		}

		// Fall back to empty string
		return core.StringValue(""), nil
	}))

	// Add __repr__ method to BaseException for proper repr() output
	baseExceptionClass.SetMethod("__repr__", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("__repr__() missing 1 required positional argument: 'self'")
		}

		self, ok := args[0].(*core.Instance)
		if !ok {
			return core.StringValue(""), nil
		}

		// Get class name
		className := self.Class.Name

		// Get the args attribute for display
		if argsVal, hasArgs := self.Attributes["args"]; hasArgs {
			if argsTuple, ok := argsVal.(core.TupleValue); ok && len(argsTuple) > 0 {
				// Format like: ImportError('message')
				msg := argsTuple[0].String()
				return core.StringValue(fmt.Sprintf("%s(%s)", className, msg)), nil
			}
		}

		// Fall back to just class name with empty parens
		return core.StringValue(fmt.Sprintf("%s()", className)), nil
	}))

	// Add with_traceback method to BaseException
	// This method sets the traceback and returns self
	baseExceptionClass.SetMethod("with_traceback", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 2 {
			return nil, fmt.Errorf("with_traceback() missing required argument: 'tb' (pos 2)")
		}

		self, ok := args[0].(*core.Instance)
		if !ok {
			return nil, fmt.Errorf("with_traceback() first argument must be an instance")
		}

		tb := args[1]

		// Store the traceback (for now we just store it, not fully implemented)
		self.Attributes["__traceback__"] = tb

		// Return self
		return self, nil
	}))

	ctx.Define("BaseException", baseExceptionClass)

	// BaseExceptionGroup - base class for exception groups (Python 3.11+)
	// Important: inherits from BaseException, not Exception
	baseExceptionGroupClass := core.NewClass("BaseExceptionGroup", baseExceptionClass)
	ctx.Define("BaseExceptionGroup", baseExceptionGroupClass)

	// KeyboardInterrupt - raised when user hits interrupt key (Ctrl-C)
	// Important: inherits from BaseException, not Exception
	keyboardInterruptClass := core.NewClass("KeyboardInterrupt", baseExceptionClass)
	ctx.Define("KeyboardInterrupt", keyboardInterruptClass)

	// SystemExit - raised by sys.exit()
	// Important: inherits from BaseException, not Exception
	systemExitClass := core.NewClass("SystemExit", baseExceptionClass)
	ctx.Define("SystemExit", systemExitClass)

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

		// Initialize exception chaining attributes (PEP 3134)
		self.Attributes["__cause__"] = core.None             // Explicit exception chaining (raise ... from ...)
		self.Attributes["__context__"] = core.None           // Implicit exception chaining
		self.Attributes["__suppress_context__"] = core.False // Whether to suppress context display
		self.Attributes["__traceback__"] = core.None         // Traceback object

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

	// UnboundLocalError - raised when referencing a local variable before it has been assigned
	unboundLocalErrorClass := core.NewClass("UnboundLocalError", nameErrorClass)
	ctx.Define("UnboundLocalError", unboundLocalErrorClass)

	// SyntaxError - raised when the parser encounters a syntax error
	syntaxErrorClass := core.NewClass("SyntaxError", exceptionClass)
	ctx.Define("SyntaxError", syntaxErrorClass)

	// IndentationError - raised when indentation is not correct (subclass of SyntaxError)
	indentationErrorClass := core.NewClass("IndentationError", syntaxErrorClass)
	ctx.Define("IndentationError", indentationErrorClass)

	// TabError - raised when indentation contains mixed tabs and spaces (subclass of IndentationError)
	tabErrorClass := core.NewClass("TabError", indentationErrorClass)
	ctx.Define("TabError", tabErrorClass)

	// LookupError - raised when a key or index used on a mapping or sequence is invalid
	lookupErrorClass := core.NewClass("LookupError", exceptionClass)
	ctx.Define("LookupError", lookupErrorClass)

	// KeyError - raised when a dictionary key is not found (inherits from LookupError)
	keyErrorClass := core.NewClass("KeyError", lookupErrorClass)
	ctx.Define("KeyError", keyErrorClass)

	// IndexError - raised when a sequence subscript is out of range (inherits from LookupError)
	indexErrorClass := core.NewClass("IndexError", lookupErrorClass)
	ctx.Define("IndexError", indexErrorClass)

	// AttributeError - raised when an attribute reference or assignment fails
	attributeErrorClass := core.NewClass("AttributeError", exceptionClass)
	ctx.Define("AttributeError", attributeErrorClass)

	// ArithmeticError - base class for arithmetic errors
	arithmeticErrorClass := core.NewClass("ArithmeticError", exceptionClass)
	ctx.Define("ArithmeticError", arithmeticErrorClass)

	// ZeroDivisionError - raised when division or modulo by zero takes place
	zeroDivisionErrorClass := core.NewClass("ZeroDivisionError", arithmeticErrorClass)
	ctx.Define("ZeroDivisionError", zeroDivisionErrorClass)

	// OverflowError - raised when arithmetic operation result is too large
	overflowErrorClass := core.NewClass("OverflowError", arithmeticErrorClass)
	ctx.Define("OverflowError", overflowErrorClass)

	// FloatingPointError - raised when floating point operation fails
	floatingPointErrorClass := core.NewClass("FloatingPointError", arithmeticErrorClass)
	ctx.Define("FloatingPointError", floatingPointErrorClass)

	// RuntimeError - raised when an error is detected that doesn't fall in any of the other categories
	runtimeErrorClass := core.NewClass("RuntimeError", exceptionClass)
	ctx.Define("RuntimeError", runtimeErrorClass)

	// RecursionError - raised when maximum recursion depth is exceeded
	recursionErrorClass := core.NewClass("RecursionError", runtimeErrorClass)
	ctx.Define("RecursionError", recursionErrorClass)

	// NotImplementedError - raised when an abstract method that should have been implemented is not
	notImplementedErrorClass := core.NewClass("NotImplementedError", runtimeErrorClass)
	ctx.Define("NotImplementedError", notImplementedErrorClass)

	// SystemError - raised when the interpreter finds an internal error
	systemErrorClass := core.NewClass("SystemError", exceptionClass)
	ctx.Define("SystemError", systemErrorClass)

	// ImportError - raised when an import statement fails
	importErrorClass := core.NewClass("ImportError", exceptionClass)
	ctx.Define("ImportError", importErrorClass)

	// ModuleNotFoundError - raised when a module cannot be found
	moduleNotFoundErrorClass := core.NewClass("ModuleNotFoundError", importErrorClass)
	ctx.Define("ModuleNotFoundError", moduleNotFoundErrorClass)

	// EOFError - raised when input() hits end-of-file without reading data
	eofErrorClass := core.NewClass("EOFError", exceptionClass)
	ctx.Define("EOFError", eofErrorClass)

	// OSError - raised when a system function returns a system-related error
	osErrorClass := core.NewClass("OSError", exceptionClass)

	// Add special __init__ for OSError to handle (errno, strerror, filename) pattern
	// Python's OSError has several constructor patterns:
	// - OSError(msg) - just message
	// - OSError(errno, strerror) - errno and strerror
	// - OSError(errno, strerror, filename) - adds filename
	// - OSError(errno, strerror, filename, winerror, filename2) - full form
	osErrorClass.SetMethod("__init__", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("__init__ requires at least 1 argument (self)")
		}

		self, ok := args[0].(*core.Instance)
		if !ok {
			return nil, fmt.Errorf("__init__ first argument must be an instance")
		}

		// Initialize default values for OSError attributes
		self.Attributes["errno"] = core.None
		self.Attributes["strerror"] = core.None
		self.Attributes["filename"] = core.None
		self.Attributes["filename2"] = core.None

		// Initialize exception chaining attributes (PEP 3134)
		self.Attributes["__cause__"] = core.None
		self.Attributes["__context__"] = core.None
		self.Attributes["__suppress_context__"] = core.False
		self.Attributes["__traceback__"] = core.None

		numArgs := len(args) - 1 // excluding self

		if numArgs == 0 {
			// No arguments - empty args tuple
			self.Attributes["args"] = core.TupleValue{}
			return core.None, nil
		}

		// Check if first arg (after self) is a number (errno pattern)
		// In Python, OSError(errno, strerror) pattern is detected when first arg is numeric
		firstArg := args[1]
		isErrnoPattern := false
		if types.IsNumeric(firstArg) && numArgs >= 2 {
			isErrnoPattern = true
		}

		if isErrnoPattern {
			// Pattern: (errno, strerror[, filename[, winerror[, filename2]]])
			self.Attributes["errno"] = args[1]

			if numArgs >= 2 {
				self.Attributes["strerror"] = args[2]
			}

			if numArgs >= 3 {
				self.Attributes["filename"] = args[3]
			}

			// winerror (args[4]) is Windows-specific, we just ignore it

			if numArgs >= 5 {
				self.Attributes["filename2"] = args[5]
			}

			// For errno pattern, args tuple is (errno, strerror)
			// NOT including filename - this matches Python behavior
			if numArgs >= 2 {
				self.Attributes["args"] = core.TupleValue{args[1], args[2]}
			} else {
				self.Attributes["args"] = core.TupleValue{args[1]}
			}
		} else {
			// Pattern: just a message string or other args
			// Store all arguments after self as a tuple in args
			exceptionArgs := make(core.TupleValue, numArgs)
			for i := 1; i < len(args); i++ {
				exceptionArgs[i-1] = args[i]
			}
			self.Attributes["args"] = exceptionArgs
		}

		return core.None, nil
	}))

	// Override __str__ for OSError to format properly
	osErrorClass.SetMethod("__str__", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("__str__() missing 1 required positional argument: 'self'")
		}

		self, ok := args[0].(*core.Instance)
		if !ok {
			return core.StringValue(""), nil
		}

		// Check for errno pattern
		errno, hasErrno := self.Attributes["errno"]
		strerror, hasStrerror := self.Attributes["strerror"]
		filename, hasFilename := self.Attributes["filename"]

		if hasErrno && !types.IsNil(errno) {
			// Format: [Errno N] message or [Errno N] message: 'filename'
			msg := ""
			if hasStrerror && !types.IsNil(strerror) {
				msg = strerror.String()
			}

			result := fmt.Sprintf("[Errno %s] %s", errno.String(), msg)

			if hasFilename && !types.IsNil(filename) {
				result += fmt.Sprintf(": '%s'", filename.String())
			}

			return core.StringValue(result), nil
		}

		// Fall back to default behavior - first arg
		if argsVal, hasArgs := self.Attributes["args"]; hasArgs {
			if argsTuple, ok := argsVal.(core.TupleValue); ok && len(argsTuple) > 0 {
				return core.StringValue(argsTuple[0].String()), nil
			}
		}

		return core.StringValue(""), nil
	}))

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

	// FileExistsError - raised when trying to create a file or directory that already exists
	fileExistsErrorClass := core.NewClass("FileExistsError", osErrorClass)
	ctx.Define("FileExistsError", fileExistsErrorClass)

	// TimeoutError - raised when a system function timed out at the system level
	timeoutErrorClass := core.NewClass("TimeoutError", osErrorClass)
	ctx.Define("TimeoutError", timeoutErrorClass)

	// InterruptedError - raised when a system call is interrupted
	interruptedErrorClass := core.NewClass("InterruptedError", osErrorClass)
	ctx.Define("InterruptedError", interruptedErrorClass)

	// ConnectionError - base class for connection-related errors
	connectionErrorClass := core.NewClass("ConnectionError", osErrorClass)
	ctx.Define("ConnectionError", connectionErrorClass)

	// BrokenPipeError - raised when trying to write to a pipe while the other end has been closed
	brokenPipeErrorClass := core.NewClass("BrokenPipeError", connectionErrorClass)
	ctx.Define("BrokenPipeError", brokenPipeErrorClass)

	// ConnectionAbortedError - raised when a connection attempt is aborted by the peer
	connectionAbortedErrorClass := core.NewClass("ConnectionAbortedError", connectionErrorClass)
	ctx.Define("ConnectionAbortedError", connectionAbortedErrorClass)

	// ConnectionRefusedError - raised when a connection attempt is refused by the peer
	connectionRefusedErrorClass := core.NewClass("ConnectionRefusedError", connectionErrorClass)
	ctx.Define("ConnectionRefusedError", connectionRefusedErrorClass)

	// ConnectionResetError - raised when a connection is reset by the peer
	connectionResetErrorClass := core.NewClass("ConnectionResetError", connectionErrorClass)
	ctx.Define("ConnectionResetError", connectionResetErrorClass)

	// UnicodeError - base class for Unicode-related errors
	unicodeErrorClass := core.NewClass("UnicodeError", valueErrorClass)
	ctx.Define("UnicodeError", unicodeErrorClass)

	// UnicodeDecodeError - raised when a Unicode decoding error occurs
	unicodeDecodeErrorClass := core.NewClass("UnicodeDecodeError", unicodeErrorClass)
	ctx.Define("UnicodeDecodeError", unicodeDecodeErrorClass)

	// UnicodeEncodeError - raised when a Unicode encoding error occurs
	unicodeEncodeErrorClass := core.NewClass("UnicodeEncodeError", unicodeErrorClass)
	ctx.Define("UnicodeEncodeError", unicodeEncodeErrorClass)

	// UnicodeTranslateError - raised when a Unicode translation error occurs
	unicodeTranslateErrorClass := core.NewClass("UnicodeTranslateError", unicodeErrorClass)
	ctx.Define("UnicodeTranslateError", unicodeTranslateErrorClass)

	// StopIteration - raised by next() and an iterator's __next__() to signal no more items
	stopIterationClass := core.NewClass("StopIteration", exceptionClass)
	ctx.Define("StopIteration", stopIterationClass)

	// StopAsyncIteration - raised by an async iterator's __anext__() to signal no more items
	stopAsyncIterationClass := core.NewClass("StopAsyncIteration", exceptionClass)
	ctx.Define("StopAsyncIteration", stopAsyncIterationClass)

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
