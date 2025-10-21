package modules

import (
	"github.com/mmichie/m28/core"
)

// Init_SignalModule creates and returns the _signal C extension stub
// This provides signal constants and basic signal handling functions
func Init_SignalModule() *core.DictValue {
	signalModule := core.NewDict()

	// Signal constants (POSIX signals)
	signalModule.Set("SIGABRT", core.NumberValue(6))
	signalModule.Set("SIGALRM", core.NumberValue(14))
	signalModule.Set("SIGBUS", core.NumberValue(7))
	signalModule.Set("SIGCHLD", core.NumberValue(17))
	signalModule.Set("SIGCONT", core.NumberValue(18))
	signalModule.Set("SIGFPE", core.NumberValue(8))
	signalModule.Set("SIGHUP", core.NumberValue(1))
	signalModule.Set("SIGILL", core.NumberValue(4))
	signalModule.Set("SIGINT", core.NumberValue(2))
	signalModule.Set("SIGKILL", core.NumberValue(9))
	signalModule.Set("SIGPIPE", core.NumberValue(13))
	signalModule.Set("SIGQUIT", core.NumberValue(3))
	signalModule.Set("SIGSEGV", core.NumberValue(11))
	signalModule.Set("SIGSTOP", core.NumberValue(19))
	signalModule.Set("SIGTERM", core.NumberValue(15))
	signalModule.Set("SIGTRAP", core.NumberValue(5))
	signalModule.Set("SIGTSTP", core.NumberValue(20))
	signalModule.Set("SIGTTIN", core.NumberValue(21))
	signalModule.Set("SIGTTOU", core.NumberValue(22))
	signalModule.Set("SIGUSR1", core.NumberValue(10))
	signalModule.Set("SIGUSR2", core.NumberValue(12))

	// Signal handler constants
	signalModule.Set("SIG_DFL", core.NumberValue(0)) // Default signal handling
	signalModule.Set("SIG_IGN", core.NumberValue(1)) // Ignore signal
	signalModule.Set("SIG_BLOCK", core.NumberValue(0))
	signalModule.Set("SIG_UNBLOCK", core.NumberValue(1))
	signalModule.Set("SIG_SETMASK", core.NumberValue(2))

	// NSIG - number of signals
	signalModule.Set("NSIG", core.NumberValue(32))

	// signal() - Set signal handler (stub)
	signalModule.Set("signal", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Stub: just return SIG_DFL
		return core.NumberValue(0), nil
	}))

	// getsignal() - Get current signal handler (stub)
	signalModule.Set("getsignal", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Stub: just return SIG_DFL
		return core.NumberValue(0), nil
	}))

	// alarm() - Set alarm (stub)
	signalModule.Set("alarm", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Stub: just return 0
		return core.NumberValue(0), nil
	}))

	// pause() - Wait for signal (stub)
	signalModule.Set("pause", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Stub: do nothing
		return core.NilValue{}, nil
	}))

	// default_int_handler - Default SIGINT handler (stub)
	signalModule.Set("default_int_handler", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Stub: do nothing
		return core.NilValue{}, nil
	}))

	// set_wakeup_fd() - Set wake-up file descriptor (stub)
	signalModule.Set("set_wakeup_fd", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Stub: return -1 (no previous fd)
		return core.NumberValue(-1), nil
	}))

	return signalModule
}
