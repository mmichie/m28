package modules

import (
	"fmt"
	"os"
	"os/user"
	"strconv"

	"github.com/mmichie/m28/core"
)

// InitTermiosModule creates a stub for the termios Unix terminal I/O module
func InitTermiosModule() *core.DictValue {
	termiosModule := core.NewDict()

	// Common termios constants (values from Linux)
	termiosModule.Set("TCSANOW", core.NumberValue(0))
	termiosModule.Set("TCSADRAIN", core.NumberValue(1))
	termiosModule.Set("TCSAFLUSH", core.NumberValue(2))

	// Input modes
	termiosModule.Set("IGNBRK", core.NumberValue(0o0000001))
	termiosModule.Set("BRKINT", core.NumberValue(0o0000002))
	termiosModule.Set("IGNPAR", core.NumberValue(0o0000004))
	termiosModule.Set("PARMRK", core.NumberValue(0o0000010))
	termiosModule.Set("INPCK", core.NumberValue(0o0000020))
	termiosModule.Set("ISTRIP", core.NumberValue(0o0000040))
	termiosModule.Set("INLCR", core.NumberValue(0o0000100))
	termiosModule.Set("IGNCR", core.NumberValue(0o0000200))
	termiosModule.Set("ICRNL", core.NumberValue(0o0000400))
	termiosModule.Set("IXON", core.NumberValue(0o0002000))
	termiosModule.Set("IXOFF", core.NumberValue(0o0010000))

	// Output modes
	termiosModule.Set("OPOST", core.NumberValue(0o0000001))

	// Control modes
	termiosModule.Set("CSIZE", core.NumberValue(0o0000060))
	termiosModule.Set("CS5", core.NumberValue(0o0000000))
	termiosModule.Set("CS6", core.NumberValue(0o0000020))
	termiosModule.Set("CS7", core.NumberValue(0o0000040))
	termiosModule.Set("CS8", core.NumberValue(0o0000060))
	termiosModule.Set("CSTOPB", core.NumberValue(0o0000100))
	termiosModule.Set("CREAD", core.NumberValue(0o0000200))
	termiosModule.Set("PARENB", core.NumberValue(0o0000400))
	termiosModule.Set("PARODD", core.NumberValue(0o0001000))
	termiosModule.Set("HUPCL", core.NumberValue(0o0002000))
	termiosModule.Set("CLOCAL", core.NumberValue(0o0004000))

	// Local modes
	termiosModule.Set("ISIG", core.NumberValue(0o0000001))
	termiosModule.Set("ICANON", core.NumberValue(0o0000002))
	termiosModule.Set("ECHO", core.NumberValue(0o0000010))
	termiosModule.Set("ECHOE", core.NumberValue(0o0000020))
	termiosModule.Set("ECHOK", core.NumberValue(0o0000040))
	termiosModule.Set("ECHONL", core.NumberValue(0o0000100))
	termiosModule.Set("NOFLSH", core.NumberValue(0o0000200))
	termiosModule.Set("TOSTOP", core.NumberValue(0o0000400))
	termiosModule.Set("IEXTEN", core.NumberValue(0o0100000))

	// Control characters indices
	termiosModule.Set("VMIN", core.NumberValue(6))
	termiosModule.Set("VTIME", core.NumberValue(5))

	// tcgetattr(fd) - get terminal attributes
	termiosModule.Set("tcgetattr", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Return a default terminal attributes list
		// [iflag, oflag, cflag, lflag, ispeed, ospeed, cc]
		cc := make([]core.Value, 32)
		for i := range cc {
			cc[i] = core.NumberValue(0)
		}
		return core.NewList(
			core.NumberValue(0), // iflag
			core.NumberValue(0), // oflag
			core.NumberValue(0), // cflag
			core.NumberValue(0), // lflag
			core.NumberValue(0), // ispeed
			core.NumberValue(0), // ospeed
			core.NewList(cc...), // cc
		), nil
	}))

	// tcsetattr(fd, when, attributes) - set terminal attributes
	termiosModule.Set("tcsetattr", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Stub: do nothing
		return core.None, nil
	}))

	// tcsendbreak(fd, duration) - send break
	termiosModule.Set("tcsendbreak", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	// tcdrain(fd) - wait until all output written
	termiosModule.Set("tcdrain", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	// tcflush(fd, queue) - flush queued data
	termiosModule.Set("tcflush", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	// tcflow(fd, action) - suspend/resume I/O
	termiosModule.Set("tcflow", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	// error exception
	errorClass := core.NewClassWithParents("error", []*core.Class{})
	termiosModule.Set("error", errorClass)

	return termiosModule
}

// StructPasswd represents a pwd.struct_passwd with both tuple and attribute access
type StructPasswd struct {
	core.BaseObject
	values []core.Value // Tuple values for indexed access
	names  []string     // Field names for attribute access
}

// Type returns the type name
func (s *StructPasswd) Type() core.Type {
	return "pwd.struct_passwd"
}

// String returns a string representation
func (s *StructPasswd) String() string {
	return fmt.Sprintf("pwd.struct_passwd(pw_name=%q, pw_passwd=%q, pw_uid=%v, pw_gid=%v, pw_gecos=%q, pw_dir=%q, pw_shell=%q)",
		s.values[0], s.values[1], s.values[2], s.values[3], s.values[4], s.values[5], s.values[6])
}

// GetAttr returns named attributes
func (s *StructPasswd) GetAttr(name string) (core.Value, bool) {
	for i, n := range s.names {
		if n == name {
			return s.values[i], true
		}
	}
	// Special attributes
	switch name {
	case "__len__":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.NumberValue(len(s.values)), nil
		}), true
	case "__getitem__":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) < 1 {
				return nil, fmt.Errorf("__getitem__ requires an index")
			}
			idx, ok := args[0].(core.NumberValue)
			if !ok {
				return nil, fmt.Errorf("indices must be integers")
			}
			i := int(idx)
			if i < 0 {
				i = len(s.values) + i
			}
			if i < 0 || i >= len(s.values) {
				return nil, fmt.Errorf("index out of range")
			}
			return s.values[i], nil
		}), true
	case "__iter__":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return &structPasswdIterator{values: s.values, index: 0}, nil
		}), true
	case "n_fields":
		return core.NumberValue(len(s.values)), true
	case "n_sequence_fields":
		return core.NumberValue(len(s.values)), true
	case "n_unnamed_fields":
		return core.NumberValue(0), true
	}
	return nil, false
}

// GetItem implements indexed access
func (s *StructPasswd) GetItem(key core.Value) (core.Value, error) {
	idx, ok := key.(core.NumberValue)
	if !ok {
		return nil, fmt.Errorf("indices must be integers, not %s", key.Type())
	}
	i := int(idx)
	if i < 0 {
		i = len(s.values) + i
	}
	if i < 0 || i >= len(s.values) {
		return nil, fmt.Errorf("index out of range")
	}
	return s.values[i], nil
}

// Len returns the number of fields
func (s *StructPasswd) Len() int {
	return len(s.values)
}

// structPasswdIterator for iterating over struct_passwd
type structPasswdIterator struct {
	core.BaseObject
	values []core.Value
	index  int
}

func (it *structPasswdIterator) Type() core.Type {
	return "struct_passwd_iterator"
}

func (it *structPasswdIterator) String() string {
	return "<struct_passwd_iterator>"
}

func (it *structPasswdIterator) GetAttr(name string) (core.Value, bool) {
	if name == "__next__" {
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if it.index >= len(it.values) {
				return nil, &core.StopIteration{}
			}
			val := it.values[it.index]
			it.index++
			return val, nil
		}), true
	}
	return nil, false
}

// InitPwdModule creates a stub for the pwd Unix password database module
func InitPwdModule() *core.DictValue {
	pwdModule := core.NewDict()

	// struct_passwd class
	structPasswdClass := core.NewClass("struct_passwd", nil)
	pwdModule.Set("struct_passwd", structPasswdClass)

	// getpwuid(uid) - get password database entry by UID
	pwdModule.Set("getpwuid", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("getpwuid() missing required argument: uid")
		}
		uid, ok := args[0].(core.NumberValue)
		if !ok {
			return nil, fmt.Errorf("getpwuid() argument must be an integer")
		}

		// Try to get the current user if uid matches
		currentUser, err := user.Current()
		if err == nil {
			currentUID, _ := strconv.Atoi(currentUser.Uid)
			if int(uid) == currentUID {
				return makePasswdStruct(currentUser), nil
			}
		}

		// For other UIDs, return a stub
		return nil, fmt.Errorf("getpwuid(): uid not found: %d", int(uid))
	}))

	// getpwnam(name) - get password database entry by name
	pwdModule.Set("getpwnam", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("getpwnam() missing required argument: name")
		}
		name, ok := args[0].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("getpwnam() argument must be a string")
		}

		// Try to look up the user
		u, err := user.Lookup(string(name))
		if err != nil {
			return nil, fmt.Errorf("getpwnam(): name not found: %s", name)
		}
		return makePasswdStruct(u), nil
	}))

	// getpwall() - get all password database entries
	pwdModule.Set("getpwall", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Return list with just current user
		currentUser, err := user.Current()
		if err != nil {
			return core.NewList(), nil
		}
		return core.NewList(makePasswdStruct(currentUser)), nil
	}))

	return pwdModule
}

// makePasswdStruct creates a passwd struct from a user
func makePasswdStruct(u *user.User) *StructPasswd {
	uid, _ := strconv.Atoi(u.Uid)
	gid, _ := strconv.Atoi(u.Gid)
	homeDir := u.HomeDir
	if homeDir == "" {
		homeDir = os.Getenv("HOME")
	}

	// struct_passwd fields: pw_name, pw_passwd, pw_uid, pw_gid, pw_gecos, pw_dir, pw_shell
	return &StructPasswd{
		values: []core.Value{
			core.StringValue(u.Username), // pw_name
			core.StringValue("x"),        // pw_passwd (placeholder)
			core.NumberValue(uid),        // pw_uid
			core.NumberValue(gid),        // pw_gid
			core.StringValue(u.Name),     // pw_gecos
			core.StringValue(homeDir),    // pw_dir
			core.StringValue("/bin/sh"),  // pw_shell (placeholder)
		},
		names: []string{"pw_name", "pw_passwd", "pw_uid", "pw_gid", "pw_gecos", "pw_dir", "pw_shell"},
	}
}

// InitGrpModule creates a stub for the grp Unix group database module
func InitGrpModule() *core.DictValue {
	grpModule := core.NewDict()

	// struct_group class
	structGroupClass := core.NewClass("struct_group", nil)
	grpModule.Set("struct_group", structGroupClass)

	// getgrgid(gid) - get group database entry by GID
	grpModule.Set("getgrgid", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("getgrgid() missing required argument: gid")
		}
		gid, ok := args[0].(core.NumberValue)
		if !ok {
			return nil, fmt.Errorf("getgrgid() argument must be an integer")
		}

		// Try to get current user's group
		currentUser, err := user.Current()
		if err == nil {
			currentGID, _ := strconv.Atoi(currentUser.Gid)
			if int(gid) == currentGID {
				return makeGroupStruct(int(gid), "users"), nil
			}
		}

		return nil, fmt.Errorf("getgrgid(): gid not found: %d", int(gid))
	}))

	// getgrnam(name) - get group database entry by name
	grpModule.Set("getgrnam", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("getgrnam() missing required argument: name")
		}
		name, ok := args[0].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("getgrnam() argument must be a string")
		}

		// Try to look up group
		g, err := user.LookupGroup(string(name))
		if err != nil {
			return nil, fmt.Errorf("getgrnam(): name not found: %s", name)
		}
		gid, _ := strconv.Atoi(g.Gid)
		return makeGroupStruct(gid, g.Name), nil
	}))

	// getgrall() - get all group database entries
	grpModule.Set("getgrall", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Return empty list (can't easily enumerate all groups)
		return core.NewList(), nil
	}))

	return grpModule
}

// makeGroupStruct creates a group struct tuple
func makeGroupStruct(gid int, name string) core.TupleValue {
	// struct_group: (gr_name, gr_passwd, gr_gid, gr_mem)
	return core.TupleValue{
		core.StringValue(name),    // gr_name
		core.StringValue("x"),     // gr_passwd (placeholder)
		core.NumberValue(gid),     // gr_gid
		core.NewList(),            // gr_mem (empty list)
	}
}

// InitMsvcrtModule creates a stub for the msvcrt Windows-specific module
func InitMsvcrtModule() *core.DictValue {
	msvcrtModule := core.NewDict()

	// Constants
	msvcrtModule.Set("LK_LOCK", core.NumberValue(1))
	msvcrtModule.Set("LK_NBLCK", core.NumberValue(2))
	msvcrtModule.Set("LK_NBRLCK", core.NumberValue(3))
	msvcrtModule.Set("LK_RLCK", core.NumberValue(4))
	msvcrtModule.Set("LK_UNLCK", core.NumberValue(0))

	// Console I/O functions - all stubs that do nothing on Unix
	msvcrtModule.Set("kbhit", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Return False - no key pressed (not available on Unix)
		return core.BoolValue(false), nil
	}))

	msvcrtModule.Set("getch", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("msvcrt.getch() is not available on this platform")
	}))

	msvcrtModule.Set("getwch", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("msvcrt.getwch() is not available on this platform")
	}))

	msvcrtModule.Set("getche", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("msvcrt.getche() is not available on this platform")
	}))

	msvcrtModule.Set("getwche", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("msvcrt.getwche() is not available on this platform")
	}))

	msvcrtModule.Set("putch", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("msvcrt.putch() is not available on this platform")
	}))

	msvcrtModule.Set("putwch", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("msvcrt.putwch() is not available on this platform")
	}))

	msvcrtModule.Set("ungetch", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("msvcrt.ungetch() is not available on this platform")
	}))

	msvcrtModule.Set("ungetwch", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("msvcrt.ungetwch() is not available on this platform")
	}))

	// File operations
	msvcrtModule.Set("locking", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("msvcrt.locking() is not available on this platform")
	}))

	msvcrtModule.Set("setmode", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("msvcrt.setmode() is not available on this platform")
	}))

	msvcrtModule.Set("open_osfhandle", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("msvcrt.open_osfhandle() is not available on this platform")
	}))

	msvcrtModule.Set("get_osfhandle", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("msvcrt.get_osfhandle() is not available on this platform")
	}))

	// Heap operations
	msvcrtModule.Set("heapmin", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("msvcrt.heapmin() is not available on this platform")
	}))

	return msvcrtModule
}
