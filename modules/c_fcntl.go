package modules

import (
	"fmt"
	"syscall"

	"github.com/mmichie/m28/core"
)

// InitFcntlModule creates the fcntl C extension module
// This provides file control and I/O control operations on file descriptors
func InitFcntlModule() *core.DictValue {
	fcntlModule := core.NewDict()

	// fcntl(fd, cmd, arg=0) - perform file control operation
	fcntlModule.Set("fcntl", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 2 {
			return nil, fmt.Errorf("fcntl() requires at least 2 arguments")
		}

		fd, ok := args[0].(core.NumberValue)
		if !ok {
			return nil, fmt.Errorf("fcntl() first argument must be an integer")
		}

		cmd, ok := args[1].(core.NumberValue)
		if !ok {
			return nil, fmt.Errorf("fcntl() second argument must be an integer")
		}

		arg := 0
		if len(args) >= 3 {
			if argVal, ok := args[2].(core.NumberValue); ok {
				arg = int(argVal)
			}
		}

		result, _, err := syscall.Syscall(syscall.SYS_FCNTL, uintptr(fd), uintptr(cmd), uintptr(arg))
		if err != 0 {
			return nil, fmt.Errorf("fcntl failed: %v", err)
		}

		return core.NumberValue(result), nil
	}))

	// ioctl(fd, request, arg=0) - perform I/O control operation
	fcntlModule.Set("ioctl", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 2 {
			return nil, fmt.Errorf("ioctl() requires at least 2 arguments")
		}

		// Stub implementation - return 0 for now
		return core.NumberValue(0), nil
	}))

	// flock(fd, operation) - apply or remove advisory lock
	fcntlModule.Set("flock", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 2 {
			return nil, fmt.Errorf("flock() requires 2 arguments")
		}

		fd, ok := args[0].(core.NumberValue)
		if !ok {
			return nil, fmt.Errorf("flock() first argument must be an integer")
		}

		operation, ok := args[1].(core.NumberValue)
		if !ok {
			return nil, fmt.Errorf("flock() second argument must be an integer")
		}

		err := syscall.Flock(int(fd), int(operation))
		if err != nil {
			return nil, fmt.Errorf("flock failed: %v", err)
		}

		return core.None, nil
	}))

	// lockf(fd, cmd, len) - apply, test, or remove POSIX lock
	fcntlModule.Set("lockf", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 2 {
			return nil, fmt.Errorf("lockf() requires at least 2 arguments")
		}
		// Stub - return None
		return core.None, nil
	}))

	// Constants for fcntl operations
	fcntlModule.Set("F_DUPFD", core.NumberValue(syscall.F_DUPFD))
	fcntlModule.Set("F_GETFD", core.NumberValue(syscall.F_GETFD))
	fcntlModule.Set("F_SETFD", core.NumberValue(syscall.F_SETFD))
	fcntlModule.Set("F_GETFL", core.NumberValue(syscall.F_GETFL))
	fcntlModule.Set("F_SETFL", core.NumberValue(syscall.F_SETFL))
	fcntlModule.Set("F_GETLK", core.NumberValue(syscall.F_GETLK))
	fcntlModule.Set("F_SETLK", core.NumberValue(syscall.F_SETLK))
	fcntlModule.Set("F_SETLKW", core.NumberValue(syscall.F_SETLKW))
	fcntlModule.Set("F_GETOWN", core.NumberValue(syscall.F_GETOWN))
	fcntlModule.Set("F_SETOWN", core.NumberValue(syscall.F_SETOWN))

	// Constants for flock operations
	fcntlModule.Set("LOCK_SH", core.NumberValue(syscall.LOCK_SH))
	fcntlModule.Set("LOCK_EX", core.NumberValue(syscall.LOCK_EX))
	fcntlModule.Set("LOCK_NB", core.NumberValue(syscall.LOCK_NB))
	fcntlModule.Set("LOCK_UN", core.NumberValue(syscall.LOCK_UN))

	// FD_CLOEXEC flag
	fcntlModule.Set("FD_CLOEXEC", core.NumberValue(syscall.FD_CLOEXEC))

	// Constants for lockf
	fcntlModule.Set("F_LOCK", core.NumberValue(1))
	fcntlModule.Set("F_TLOCK", core.NumberValue(2))
	fcntlModule.Set("F_ULOCK", core.NumberValue(0))
	fcntlModule.Set("F_TEST", core.NumberValue(3))

	return fcntlModule
}
