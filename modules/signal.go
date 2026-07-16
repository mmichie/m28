package modules

import (
	"github.com/mmichie/m28/core"
)

// Init_SignalModule creates and returns the _signal C extension stub
// This provides signal constants and basic signal handling functions
func Init_SignalModule() *core.DictValue {
	signalModule := core.NewDict()

	// Signal constants (POSIX signals)
	signalModule.SetStr("SIGABRT", core.NumberValue(6))
	signalModule.SetStr("SIGALRM", core.NumberValue(14))
	signalModule.SetStr("SIGBUS", core.NumberValue(7))
	signalModule.SetStr("SIGCHLD", core.NumberValue(17))
	signalModule.SetStr("SIGCONT", core.NumberValue(18))
	signalModule.SetStr("SIGFPE", core.NumberValue(8))
	signalModule.SetStr("SIGHUP", core.NumberValue(1))
	signalModule.SetStr("SIGILL", core.NumberValue(4))
	signalModule.SetStr("SIGINT", core.NumberValue(2))
	signalModule.SetStr("SIGKILL", core.NumberValue(9))
	signalModule.SetStr("SIGPIPE", core.NumberValue(13))
	signalModule.SetStr("SIGQUIT", core.NumberValue(3))
	signalModule.SetStr("SIGSEGV", core.NumberValue(11))
	signalModule.SetStr("SIGSTOP", core.NumberValue(19))
	signalModule.SetStr("SIGTERM", core.NumberValue(15))
	signalModule.SetStr("SIGTRAP", core.NumberValue(5))
	signalModule.SetStr("SIGTSTP", core.NumberValue(20))
	signalModule.SetStr("SIGTTIN", core.NumberValue(21))
	signalModule.SetStr("SIGTTOU", core.NumberValue(22))
	signalModule.SetStr("SIGUSR1", core.NumberValue(10))
	signalModule.SetStr("SIGUSR2", core.NumberValue(12))

	// Signal handler constants
	signalModule.SetStr("SIG_DFL", core.NumberValue(0)) // Default signal handling
	signalModule.SetStr("SIG_IGN", core.NumberValue(1)) // Ignore signal
	signalModule.SetStr("SIG_BLOCK", core.NumberValue(0))
	signalModule.SetStr("SIG_UNBLOCK", core.NumberValue(1))
	signalModule.SetStr("SIG_SETMASK", core.NumberValue(2))

	// NSIG - number of signals
	signalModule.SetStr("NSIG", core.NumberValue(32))

	// signal() - Set signal handler (stub)
	signalModule.SetStr("signal", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Stub: just return SIG_DFL
		return core.NumberValue(0), nil
	}))

	// getsignal() - Get current signal handler (stub)
	signalModule.SetStr("getsignal", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Stub: just return SIG_DFL
		return core.NumberValue(0), nil
	}))

	// alarm() - Set alarm (stub)
	signalModule.SetStr("alarm", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Stub: just return 0
		return core.NumberValue(0), nil
	}))

	// pause() - Wait for signal (stub)
	signalModule.SetStr("pause", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Stub: do nothing
		return core.NilValue{}, nil
	}))

	// default_int_handler - Default SIGINT handler (stub)
	signalModule.SetStr("default_int_handler", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Stub: do nothing
		return core.NilValue{}, nil
	}))

	// set_wakeup_fd() - Set wake-up file descriptor (stub)
	signalModule.SetStr("set_wakeup_fd", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Stub: return -1 (no previous fd)
		return core.NumberValue(-1), nil
	}))

	return signalModule
}
