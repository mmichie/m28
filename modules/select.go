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
	selectModule.SetWithKey("POLLIN", core.StringValue("POLLIN"), core.NumberValue(0x0001))
	selectModule.SetWithKey("POLLOUT", core.StringValue("POLLOUT"), core.NumberValue(0x0004))
	selectModule.SetWithKey("POLLERR", core.StringValue("POLLERR"), core.NumberValue(0x0008))
	selectModule.SetWithKey("POLLHUP", core.StringValue("POLLHUP"), core.NumberValue(0x0010))
	selectModule.SetWithKey("POLLNVAL", core.StringValue("POLLNVAL"), core.NumberValue(0x0020))
	selectModule.SetWithKey("POLLPRI", core.StringValue("POLLPRI"), core.NumberValue(0x0002))
	selectModule.SetWithKey("POLLRDNORM", core.StringValue("POLLRDNORM"), core.NumberValue(0x0040))
	selectModule.SetWithKey("POLLRDBAND", core.StringValue("POLLRDBAND"), core.NumberValue(0x0080))
	selectModule.SetWithKey("POLLWRNORM", core.StringValue("POLLWRNORM"), core.NumberValue(0x0100))
	selectModule.SetWithKey("POLLWRBAND", core.StringValue("POLLWRBAND"), core.NumberValue(0x0200))

	// Kqueue constants (macOS/BSD)
	selectModule.SetWithKey("KQ_EV_ADD", core.StringValue("KQ_EV_ADD"), core.NumberValue(0x0001))
	selectModule.SetWithKey("KQ_EV_DELETE", core.StringValue("KQ_EV_DELETE"), core.NumberValue(0x0002))
	selectModule.SetWithKey("KQ_EV_ENABLE", core.StringValue("KQ_EV_ENABLE"), core.NumberValue(0x0004))
	selectModule.SetWithKey("KQ_EV_DISABLE", core.StringValue("KQ_EV_DISABLE"), core.NumberValue(0x0008))
	selectModule.SetWithKey("KQ_FILTER_READ", core.StringValue("KQ_FILTER_READ"), core.NumberValue(-1))
	selectModule.SetWithKey("KQ_FILTER_WRITE", core.StringValue("KQ_FILTER_WRITE"), core.NumberValue(-2))

	// PIPE_BUF constant
	selectModule.SetWithKey("PIPE_BUF", core.StringValue("PIPE_BUF"), core.NumberValue(512))

	// error exception
	selectModule.SetWithKey("error", core.StringValue("error"), core.StringValue("OSError"))

	// select(rlist, wlist, xlist[, timeout]) - stub
	selectModule.SetWithKey("select", core.StringValue("select"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("select", args)
		if err := v.Range(3, 4); err != nil {
			return nil, err
		}

		// Return empty lists (no file descriptors ready)
		emptyList := core.NewList()
		return core.TupleValue{emptyList, emptyList, emptyList}, nil
	}))

	// poll() - create a poll object
	selectModule.SetWithKey("poll", core.StringValue("poll"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("poll", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}

		// Return a stub poll object
		pollObj := core.NewDict()
		pollObj.SetWithKey("__class__", core.StringValue("__class__"), core.StringValue("poll"))

		pollObj.SetWithKey("register", core.StringValue("register"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.Nil, nil
		}))

		pollObj.SetWithKey("unregister", core.StringValue("unregister"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.Nil, nil
		}))

		pollObj.SetWithKey("poll", core.StringValue("poll"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			// Return empty list (no events)
			return core.NewList(), nil
		}))

		return pollObj, nil
	}))

	// kqueue() - create a kqueue object (macOS/BSD)
	selectModule.SetWithKey("kqueue", core.StringValue("kqueue"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("kqueue", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}

		// Return a stub kqueue object
		kqueueObj := core.NewDict()
		kqueueObj.SetWithKey("__class__", core.StringValue("__class__"), core.StringValue("kqueue"))

		kqueueObj.SetWithKey("control", core.StringValue("control"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			// Return empty list (no events)
			return core.NewList(), nil
		}))

		kqueueObj.SetWithKey("close", core.StringValue("close"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.Nil, nil
		}))

		kqueueObj.SetWithKey("fileno", core.StringValue("fileno"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.NumberValue(3), nil // Dummy fd
		}))

		return kqueueObj, nil
	}))

	// kevent() - create a kevent object
	selectModule.SetWithKey("kevent", core.StringValue("kevent"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Return a stub kevent object
		keventObj := core.NewDict()
		keventObj.SetWithKey("__class__", core.StringValue("__class__"), core.StringValue("kevent"))
		return keventObj, nil
	}))

	return selectModule
}
