package modules

import (
	"fmt"

	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// Init_SocketModule creates the _socket module stub
// This is a C extension module that provides low-level socket operations
// For now, we provide constants and minimal stub implementations
func Init_SocketModule() *core.DictValue {
	socketModule := core.NewDict()

	// Socket type constants
	socketModule.SetWithKey("SOCK_STREAM", core.StringValue("SOCK_STREAM"), core.NumberValue(1))
	socketModule.SetWithKey("SOCK_DGRAM", core.StringValue("SOCK_DGRAM"), core.NumberValue(2))
	socketModule.SetWithKey("SOCK_RAW", core.StringValue("SOCK_RAW"), core.NumberValue(3))
	socketModule.SetWithKey("SOCK_RDM", core.StringValue("SOCK_RDM"), core.NumberValue(4))
	socketModule.SetWithKey("SOCK_SEQPACKET", core.StringValue("SOCK_SEQPACKET"), core.NumberValue(5))

	// Address family constants
	socketModule.SetWithKey("AF_UNIX", core.StringValue("AF_UNIX"), core.NumberValue(1))
	socketModule.SetWithKey("AF_INET", core.StringValue("AF_INET"), core.NumberValue(2))
	socketModule.SetWithKey("AF_INET6", core.StringValue("AF_INET6"), core.NumberValue(30))
	socketModule.SetWithKey("AF_UNSPEC", core.StringValue("AF_UNSPEC"), core.NumberValue(0))

	// Protocol constants
	socketModule.SetWithKey("IPPROTO_TCP", core.StringValue("IPPROTO_TCP"), core.NumberValue(6))
	socketModule.SetWithKey("IPPROTO_UDP", core.StringValue("IPPROTO_UDP"), core.NumberValue(17))
	socketModule.SetWithKey("IPPROTO_IP", core.StringValue("IPPROTO_IP"), core.NumberValue(0))

	// Socket options
	socketModule.SetWithKey("SOL_SOCKET", core.StringValue("SOL_SOCKET"), core.NumberValue(0xffff))
	socketModule.SetWithKey("SO_REUSEADDR", core.StringValue("SO_REUSEADDR"), core.NumberValue(0x0004))
	socketModule.SetWithKey("SO_KEEPALIVE", core.StringValue("SO_KEEPALIVE"), core.NumberValue(0x0008))
	socketModule.SetWithKey("SO_BROADCAST", core.StringValue("SO_BROADCAST"), core.NumberValue(0x0020))

	// Shutdown constants
	socketModule.SetWithKey("SHUT_RD", core.StringValue("SHUT_RD"), core.NumberValue(0))
	socketModule.SetWithKey("SHUT_WR", core.StringValue("SHUT_WR"), core.NumberValue(1))
	socketModule.SetWithKey("SHUT_RDWR", core.StringValue("SHUT_RDWR"), core.NumberValue(2))

	// AI (Address Info) constants for getaddrinfo
	socketModule.SetWithKey("AI_PASSIVE", core.StringValue("AI_PASSIVE"), core.NumberValue(0x0001))
	socketModule.SetWithKey("AI_CANONNAME", core.StringValue("AI_CANONNAME"), core.NumberValue(0x0002))
	socketModule.SetWithKey("AI_NUMERICHOST", core.StringValue("AI_NUMERICHOST"), core.NumberValue(0x0004))

	// has_ipv6 - indicates IPv6 support
	socketModule.SetWithKey("has_ipv6", core.StringValue("has_ipv6"), core.BoolValue(true))

	// error - socket error exception
	socketModule.SetWithKey("error", core.StringValue("error"), core.StringValue("OSError"))
	socketModule.SetWithKey("gaierror", core.StringValue("gaierror"), core.StringValue("OSError"))
	socketModule.SetWithKey("herror", core.StringValue("herror"), core.StringValue("OSError"))
	socketModule.SetWithKey("timeout", core.StringValue("timeout"), core.StringValue("OSError"))

	// socket(family, type, proto) - create a socket object (stub)
	socketModule.SetWithKey("socket", core.StringValue("socket"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Return a stub socket object
		socketObj := core.NewDict()
		socketObj.SetWithKey("__class__", core.StringValue("__class__"), core.StringValue("socket"))

		// Add stub methods
		socketObj.SetWithKey("close", core.StringValue("close"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.Nil, nil
		}))

		socketObj.SetWithKey("bind", core.StringValue("bind"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.Nil, nil
		}))

		socketObj.SetWithKey("listen", core.StringValue("listen"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.Nil, nil
		}))

		socketObj.SetWithKey("accept", core.StringValue("accept"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			// Return (socket, address) tuple
			return core.TupleValue{socketObj, core.StringValue("127.0.0.1")}, nil
		}))

		socketObj.SetWithKey("connect", core.StringValue("connect"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.Nil, nil
		}))

		socketObj.SetWithKey("send", core.StringValue("send"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) > 0 {
				if data, ok := args[0].(core.StringValue); ok {
					return core.NumberValue(len(data)), nil
				}
			}
			return core.NumberValue(0), nil
		}))

		socketObj.SetWithKey("recv", core.StringValue("recv"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.StringValue(""), nil
		}))

		socketObj.SetWithKey("fileno", core.StringValue("fileno"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.NumberValue(3), nil // Return dummy fd
		}))

		socketObj.SetWithKey("setblocking", core.StringValue("setblocking"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.Nil, nil
		}))

		return socketObj, nil
	}))

	// getaddrinfo(host, port, family, type, proto, flags) - stub
	socketModule.SetWithKey("getaddrinfo", core.StringValue("getaddrinfo"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("getaddrinfo", args)
		if err := v.Range(2, 6); err != nil {
			return nil, err
		}

		// Return empty list for now
		return core.NewList(), nil
	}))

	// gethostname() - return the local hostname
	socketModule.SetWithKey("gethostname", core.StringValue("gethostname"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("gethostname", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}

		return core.StringValue("localhost"), nil
	}))

	// gethostbyname(hostname) - resolve hostname to IP
	socketModule.SetWithKey("gethostbyname", core.StringValue("gethostbyname"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("gethostbyname", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		return core.StringValue("127.0.0.1"), nil
	}))

	// inet_aton(ip_string) - convert IP string to binary
	socketModule.SetWithKey("inet_aton", core.StringValue("inet_aton"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("inet_aton", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		// Return dummy bytes
		return core.StringValue("\x7f\x00\x00\x01"), nil
	}))

	// inet_ntoa(packed_ip) - convert binary to IP string
	socketModule.SetWithKey("inet_ntoa", core.StringValue("inet_ntoa"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("inet_ntoa", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		return core.StringValue("127.0.0.1"), nil
	}))

	// getdefaulttimeout() - get default timeout
	socketModule.SetWithKey("getdefaulttimeout", core.StringValue("getdefaulttimeout"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("getdefaulttimeout", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}

		return core.Nil, nil // None means no timeout
	}))

	// setdefaulttimeout(timeout) - set default timeout
	socketModule.SetWithKey("setdefaulttimeout", core.StringValue("setdefaulttimeout"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("setdefaulttimeout", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		return core.Nil, nil
	}))

	// SocketType - the socket class
	socketModule.SetWithKey("SocketType", core.StringValue("SocketType"), core.StringValue("socket"))

	// dup(fd) - duplicate a file descriptor
	socketModule.SetWithKey("dup", core.StringValue("dup"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("dup", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		fd, err := v.GetNumber(0)
		if err != nil {
			return nil, err
		}

		// Return same fd for stub
		return core.NumberValue(fd), nil
	}))

	// close(fd) - close a file descriptor
	socketModule.SetWithKey("close", core.StringValue("close"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("close", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		return core.Nil, nil
	}))

	// htonl, htons, ntohl, ntohs - byte order conversion (stubs)
	socketModule.SetWithKey("htonl", core.StringValue("htonl"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("htonl() takes exactly 1 argument")
		}
		return args[0], nil // Just return same value
	}))

	socketModule.SetWithKey("htons", core.StringValue("htons"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("htons() takes exactly 1 argument")
		}
		return args[0], nil
	}))

	socketModule.SetWithKey("ntohl", core.StringValue("ntohl"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("ntohl() takes exactly 1 argument")
		}
		return args[0], nil
	}))

	socketModule.SetWithKey("ntohs", core.StringValue("ntohs"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("ntohs() takes exactly 1 argument")
		}
		return args[0], nil
	}))

	return socketModule
}
