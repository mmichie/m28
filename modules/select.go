package modules

import (
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// InitSelectModule creates the select module stub
// This is a C extension module that provides I/O multiplexing
func InitSelectModule() *core.DictValue {
	selectModule := core.NewDict()

	// Poll constants
	selectModule.SetStr("POLLIN", core.NumberValue(0x0001))
	selectModule.SetStr("POLLOUT", core.NumberValue(0x0004))
	selectModule.SetStr("POLLERR", core.NumberValue(0x0008))
	selectModule.SetStr("POLLHUP", core.NumberValue(0x0010))
	selectModule.SetStr("POLLNVAL", core.NumberValue(0x0020))
	selectModule.SetStr("POLLPRI", core.NumberValue(0x0002))
	selectModule.SetStr("POLLRDNORM", core.NumberValue(0x0040))
	selectModule.SetStr("POLLRDBAND", core.NumberValue(0x0080))
	selectModule.SetStr("POLLWRNORM", core.NumberValue(0x0100))
	selectModule.SetStr("POLLWRBAND", core.NumberValue(0x0200))

	// Kqueue constants (macOS/BSD)
	selectModule.SetStr("KQ_EV_ADD", core.NumberValue(0x0001))
	selectModule.SetStr("KQ_EV_DELETE", core.NumberValue(0x0002))
	selectModule.SetStr("KQ_EV_ENABLE", core.NumberValue(0x0004))
	selectModule.SetStr("KQ_EV_DISABLE", core.NumberValue(0x0008))
	selectModule.SetStr("KQ_FILTER_READ", core.NumberValue(-1))
	selectModule.SetStr("KQ_FILTER_WRITE", core.NumberValue(-2))

	// PIPE_BUF constant
	selectModule.SetStr("PIPE_BUF", core.NumberValue(512))

	// error exception
	selectModule.SetStr("error", core.StringValue("OSError"))

	// select(rlist, wlist, xlist[, timeout]) - stub
	selectModule.SetStr("select", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("select", args)
		if err := v.Range(3, 4); err != nil {
			return nil, err
		}

		// Return empty lists (no file descriptors ready)
		emptyList := core.NewList()
		return core.TupleValue{emptyList, emptyList, emptyList}, nil
	}))

	// poll() - create a poll object
	selectModule.SetStr("poll", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("poll", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}

		// Return a stub poll object
		pollObj := core.NewDict()
		pollObj.SetStr("__class__", core.StringValue("poll"))

		pollObj.SetStr("register", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.Nil, nil
		}))

		pollObj.SetStr("unregister", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.Nil, nil
		}))

		pollObj.SetStr("poll", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			// Return empty list (no events)
			return core.NewList(), nil
		}))

		return pollObj, nil
	}))

	// kqueue() - create a kqueue object (macOS/BSD)
	selectModule.SetStr("kqueue", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("kqueue", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}

		// Return a stub kqueue object
		kqueueObj := core.NewDict()
		kqueueObj.SetStr("__class__", core.StringValue("kqueue"))

		kqueueObj.SetStr("control", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			// Return empty list (no events)
			return core.NewList(), nil
		}))

		kqueueObj.SetStr("close", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.Nil, nil
		}))

		kqueueObj.SetStr("fileno", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.NumberValue(3), nil // Dummy fd
		}))

		return kqueueObj, nil
	}))

	// kevent() - create a kevent object
	selectModule.SetStr("kevent", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Return a stub kevent object
		keventObj := core.NewDict()
		keventObj.SetStr("__class__", core.StringValue("kevent"))
		return keventObj, nil
	}))

	return selectModule
}
