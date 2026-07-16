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
	socketModule.SetStr("SOCK_STREAM", core.NumberValue(1))
	socketModule.SetStr("SOCK_DGRAM", core.NumberValue(2))
	socketModule.SetStr("SOCK_RAW", core.NumberValue(3))
	socketModule.SetStr("SOCK_RDM", core.NumberValue(4))
	socketModule.SetStr("SOCK_SEQPACKET", core.NumberValue(5))

	// Address family constants
	socketModule.SetStr("AF_UNIX", core.NumberValue(1))
	socketModule.SetStr("AF_INET", core.NumberValue(2))
	socketModule.SetStr("AF_INET6", core.NumberValue(30))
	socketModule.SetStr("AF_UNSPEC", core.NumberValue(0))

	// Protocol constants
	socketModule.SetStr("IPPROTO_TCP", core.NumberValue(6))
	socketModule.SetStr("IPPROTO_UDP", core.NumberValue(17))
	socketModule.SetStr("IPPROTO_IP", core.NumberValue(0))

	// Socket options
	socketModule.SetStr("SOL_SOCKET", core.NumberValue(0xffff))
	socketModule.SetStr("SO_REUSEADDR", core.NumberValue(0x0004))
	socketModule.SetStr("SO_KEEPALIVE", core.NumberValue(0x0008))
	socketModule.SetStr("SO_BROADCAST", core.NumberValue(0x0020))

	// Shutdown constants
	socketModule.SetStr("SHUT_RD", core.NumberValue(0))
	socketModule.SetStr("SHUT_WR", core.NumberValue(1))
	socketModule.SetStr("SHUT_RDWR", core.NumberValue(2))

	// AI (Address Info) constants for getaddrinfo
	socketModule.SetStr("AI_PASSIVE", core.NumberValue(0x0001))
	socketModule.SetStr("AI_CANONNAME", core.NumberValue(0x0002))
	socketModule.SetStr("AI_NUMERICHOST", core.NumberValue(0x0004))

	// has_ipv6 - indicates IPv6 support
	socketModule.SetStr("has_ipv6", core.BoolValue(true))

	// error - socket error exception
	socketModule.SetStr("error", core.StringValue("OSError"))
	socketModule.SetStr("gaierror", core.StringValue("OSError"))
	socketModule.SetStr("herror", core.StringValue("OSError"))
	socketModule.SetStr("timeout", core.StringValue("OSError"))

	// socket(family, type, proto) - create a socket object (stub)
	socketModule.SetStr("socket", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Return a stub socket object
		socketObj := core.NewDict()
		socketObj.SetStr("__class__", core.StringValue("socket"))

		// Add stub methods
		socketObj.SetStr("close", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.Nil, nil
		}))

		socketObj.SetStr("bind", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.Nil, nil
		}))

		socketObj.SetStr("listen", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.Nil, nil
		}))

		socketObj.SetStr("accept", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			// Return (socket, address) tuple
			return core.TupleValue{socketObj, core.StringValue("127.0.0.1")}, nil
		}))

		socketObj.SetStr("connect", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.Nil, nil
		}))

		socketObj.SetStr("send", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) > 0 {
				if data, ok := args[0].(core.StringValue); ok {
					return core.NumberValue(len(data)), nil
				}
			}
			return core.NumberValue(0), nil
		}))

		socketObj.SetStr("recv", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.StringValue(""), nil
		}))

		socketObj.SetStr("fileno", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.NumberValue(3), nil // Return dummy fd
		}))

		socketObj.SetStr("setblocking", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.Nil, nil
		}))

		return socketObj, nil
	}))

	// getaddrinfo(host, port, family, type, proto, flags) - stub
	socketModule.SetStr("getaddrinfo", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("getaddrinfo", args)
		if err := v.Range(2, 6); err != nil {
			return nil, err
		}

		// Return empty list for now
		return core.NewList(), nil
	}))

	// gethostname() - return the local hostname
	socketModule.SetStr("gethostname", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("gethostname", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}

		return core.StringValue("localhost"), nil
	}))

	// gethostbyname(hostname) - resolve hostname to IP
	socketModule.SetStr("gethostbyname", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("gethostbyname", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		return core.StringValue("127.0.0.1"), nil
	}))

	// inet_aton(ip_string) - convert IP string to binary
	socketModule.SetStr("inet_aton", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("inet_aton", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		// Return dummy bytes
		return core.StringValue("\x7f\x00\x00\x01"), nil
	}))

	// inet_ntoa(packed_ip) - convert binary to IP string
	socketModule.SetStr("inet_ntoa", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("inet_ntoa", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		return core.StringValue("127.0.0.1"), nil
	}))

	// getdefaulttimeout() - get default timeout
	socketModule.SetStr("getdefaulttimeout", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("getdefaulttimeout", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}

		return core.Nil, nil // None means no timeout
	}))

	// setdefaulttimeout(timeout) - set default timeout
	socketModule.SetStr("setdefaulttimeout", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("setdefaulttimeout", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		return core.Nil, nil
	}))

	// SocketType - the socket class
	socketModule.SetStr("SocketType", core.StringValue("socket"))

	// dup(fd) - duplicate a file descriptor
	socketModule.SetStr("dup", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
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
	socketModule.SetStr("close", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("close", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		return core.Nil, nil
	}))

	// htonl, htons, ntohl, ntohs - byte order conversion (stubs)
	socketModule.SetStr("htonl", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("htonl() takes exactly 1 argument")
		}
		return args[0], nil // Just return same value
	}))

	socketModule.SetStr("htons", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("htons() takes exactly 1 argument")
		}
		return args[0], nil
	}))

	socketModule.SetStr("ntohl", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("ntohl() takes exactly 1 argument")
		}
		return args[0], nil
	}))

	socketModule.SetStr("ntohs", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("ntohs() takes exactly 1 argument")
		}
		return args[0], nil
	}))

	return socketModule
}
