package modules

import (
	"fmt"
	"math"
	"os"
	"os/user"
	"strconv"
	"strings"

	"github.com/mmichie/m28/core"
)

// InitTermiosModule creates a stub for the termios Unix terminal I/O module
func InitTermiosModule() *core.DictValue {
	termiosModule := core.NewDict()

	// Common termios constants (values from Linux)
	termiosModule.SetStr("TCSANOW", core.NumberValue(0))
	termiosModule.SetStr("TCSADRAIN", core.NumberValue(1))
	termiosModule.SetStr("TCSAFLUSH", core.NumberValue(2))

	// Input modes
	termiosModule.SetStr("IGNBRK", core.NumberValue(0o0000001))
	termiosModule.SetStr("BRKINT", core.NumberValue(0o0000002))
	termiosModule.SetStr("IGNPAR", core.NumberValue(0o0000004))
	termiosModule.SetStr("PARMRK", core.NumberValue(0o0000010))
	termiosModule.SetStr("INPCK", core.NumberValue(0o0000020))
	termiosModule.SetStr("ISTRIP", core.NumberValue(0o0000040))
	termiosModule.SetStr("INLCR", core.NumberValue(0o0000100))
	termiosModule.SetStr("IGNCR", core.NumberValue(0o0000200))
	termiosModule.SetStr("ICRNL", core.NumberValue(0o0000400))
	termiosModule.SetStr("IXON", core.NumberValue(0o0002000))
	termiosModule.SetStr("IXOFF", core.NumberValue(0o0010000))

	// Output modes
	termiosModule.SetStr("OPOST", core.NumberValue(0o0000001))

	// Control modes
	termiosModule.SetStr("CSIZE", core.NumberValue(0o0000060))
	termiosModule.SetStr("CS5", core.NumberValue(0o0000000))
	termiosModule.SetStr("CS6", core.NumberValue(0o0000020))
	termiosModule.SetStr("CS7", core.NumberValue(0o0000040))
	termiosModule.SetStr("CS8", core.NumberValue(0o0000060))
	termiosModule.SetStr("CSTOPB", core.NumberValue(0o0000100))
	termiosModule.SetStr("CREAD", core.NumberValue(0o0000200))
	termiosModule.SetStr("PARENB", core.NumberValue(0o0000400))
	termiosModule.SetStr("PARODD", core.NumberValue(0o0001000))
	termiosModule.SetStr("HUPCL", core.NumberValue(0o0002000))
	termiosModule.SetStr("CLOCAL", core.NumberValue(0o0004000))

	// Local modes
	termiosModule.SetStr("ISIG", core.NumberValue(0o0000001))
	termiosModule.SetStr("ICANON", core.NumberValue(0o0000002))
	termiosModule.SetStr("ECHO", core.NumberValue(0o0000010))
	termiosModule.SetStr("ECHOE", core.NumberValue(0o0000020))
	termiosModule.SetStr("ECHOK", core.NumberValue(0o0000040))
	termiosModule.SetStr("ECHONL", core.NumberValue(0o0000100))
	termiosModule.SetStr("NOFLSH", core.NumberValue(0o0000200))
	termiosModule.SetStr("TOSTOP", core.NumberValue(0o0000400))
	termiosModule.SetStr("IEXTEN", core.NumberValue(0o0100000))

	// Control characters indices
	termiosModule.SetStr("VMIN", core.NumberValue(6))
	termiosModule.SetStr("VTIME", core.NumberValue(5))

	// tcgetattr(fd) - get terminal attributes
	termiosModule.SetStr("tcgetattr", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
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
	termiosModule.SetStr("tcsetattr", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Stub: do nothing
		return core.None, nil
	}))

	// tcsendbreak(fd, duration) - send break
	termiosModule.SetStr("tcsendbreak", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	// tcdrain(fd) - wait until all output written
	termiosModule.SetStr("tcdrain", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	// tcflush(fd, queue) - flush queued data
	termiosModule.SetStr("tcflush", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	// tcflow(fd, action) - suspend/resume I/O
	termiosModule.SetStr("tcflow", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	// error exception
	errorClass := core.NewClassWithParents("error", []*core.Class{})
	termiosModule.SetStr("error", errorClass)

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

// EqualsValue compares a struct_passwd by value, like the tuple it subclasses
// in CPython: equal to another struct_passwd or a plain tuple with the same
// fields. Without this, two getpwnam() results compared only by identity.
func (s *StructPasswd) EqualsValue(other core.Value) bool {
	var otherVals []core.Value
	switch o := other.(type) {
	case *StructPasswd:
		otherVals = o.values
	case core.TupleValue:
		otherVals = []core.Value(o)
	default:
		return false
	}
	if len(s.values) != len(otherVals) {
		return false
	}
	for i := range s.values {
		if !core.EqualValues(s.values[i], otherVals[i]) {
			return false
		}
	}
	return true
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

// Iterator implements core.Iterable so struct_passwd unpacks and iterates as a
// 7-element sequence, e.g. `for n, p, u, g, gecos, d, s in getpwall(): ...`.
func (s *StructPasswd) Iterator() core.Iterator {
	return &structPasswdIterator{values: s.values, index: 0}
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

// Next implements core.Iterator so struct_passwd works with Go-side iteration
// (sequence unpacking, list(), etc.).
func (it *structPasswdIterator) Next() (core.Value, bool) {
	if it.index >= len(it.values) {
		return nil, false
	}
	val := it.values[it.index]
	it.index++
	return val, true
}

// Reset restarts iteration from the first field.
func (it *structPasswdIterator) Reset() {
	it.index = 0
}

// InitPwdModule creates a stub for the pwd Unix password database module
func InitPwdModule() *core.DictValue {
	pwdModule := core.NewDict()

	// struct_passwd class
	structPasswdClass := core.NewClass("struct_passwd", nil)
	pwdModule.SetStr("struct_passwd", structPasswdClass)

	// getpwuid(uid) - get password database entry by UID
	pwdModule.SetStr("getpwuid", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, &core.TypeError{Message: fmt.Sprintf("getpwuid() takes exactly one argument (%d given)", len(args))}
		}
		switch uid := args[0].(type) {
		case core.NumberValue:
			if float64(uid) != math.Trunc(float64(uid)) {
				return nil, &core.TypeError{Message: "getpwuid(): an integer is required"}
			}
			// Try to get the current user if uid matches
			currentUser, err := user.Current()
			if err == nil {
				currentUID, _ := strconv.Atoi(currentUser.Uid)
				if int(uid) == currentUID {
					return makePasswdStruct(currentUser), nil
				}
			}
			// Unknown uid, like CPython, is a KeyError.
			return nil, &core.KeyError{Key: core.NumberValue(uid), Message: fmt.Sprintf("getpwuid(): uid not found: %d", int(uid))}
		case core.BigIntValue:
			// A valid integer too large for any real uid: never the current user.
			return nil, &core.KeyError{Key: uid, Message: "getpwuid(): uid not found"}
		default:
			return nil, &core.TypeError{Message: "getpwuid(): an integer is required"}
		}
	}))

	// getpwnam(name) - get password database entry by name
	pwdModule.SetStr("getpwnam", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, &core.TypeError{Message: fmt.Sprintf("getpwnam() takes exactly one argument (%d given)", len(args))}
		}
		name, ok := args[0].(core.StringValue)
		if !ok {
			return nil, &core.TypeError{Message: "getpwnam() argument must be str"}
		}
		if strings.IndexByte(string(name), 0) >= 0 {
			return nil, &core.ValueError{Message: "embedded null character"}
		}

		// Try to look up the user
		u, err := user.Lookup(string(name))
		if err != nil {
			return nil, &core.KeyError{Key: name, Message: fmt.Sprintf("getpwnam(): name not found: %s", string(name))}
		}
		return makePasswdStruct(u), nil
	}))

	// getpwall() - get all password database entries
	pwdModule.SetStr("getpwall", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 0 {
			return nil, &core.TypeError{Message: fmt.Sprintf("getpwall() takes no arguments (%d given)", len(args))}
		}
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
	grpModule.SetStr("struct_group", structGroupClass)

	// getgrgid(gid) - get group database entry by GID
	grpModule.SetStr("getgrgid", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, &core.TypeError{Message: fmt.Sprintf("getgrgid() takes exactly one argument (%d given)", len(args))}
		}
		switch gid := args[0].(type) {
		case core.NumberValue:
			if float64(gid) != math.Trunc(float64(gid)) {
				return nil, &core.TypeError{Message: "getgrgid(): an integer is required"}
			}
			// Try to get current user's group
			currentUser, err := user.Current()
			if err == nil {
				currentGID, _ := strconv.Atoi(currentUser.Gid)
				if int(gid) == currentGID {
					return makeGroupStruct(int(gid), "users"), nil
				}
			}
			return nil, &core.KeyError{Key: core.NumberValue(gid), Message: fmt.Sprintf("getgrgid(): gid not found: %d", int(gid))}
		case core.BigIntValue:
			return nil, &core.KeyError{Key: gid, Message: "getgrgid(): gid not found"}
		default:
			return nil, &core.TypeError{Message: "getgrgid(): an integer is required"}
		}
	}))

	// getgrnam(name) - get group database entry by name
	grpModule.SetStr("getgrnam", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, &core.TypeError{Message: fmt.Sprintf("getgrnam() takes exactly one argument (%d given)", len(args))}
		}
		name, ok := args[0].(core.StringValue)
		if !ok {
			return nil, &core.TypeError{Message: "getgrnam() argument must be str"}
		}
		if strings.IndexByte(string(name), 0) >= 0 {
			return nil, &core.ValueError{Message: "embedded null character"}
		}

		// Try to look up group
		g, err := user.LookupGroup(string(name))
		if err != nil {
			return nil, &core.KeyError{Key: name, Message: fmt.Sprintf("getgrnam(): name not found: %s", string(name))}
		}
		gid, _ := strconv.Atoi(g.Gid)
		return makeGroupStruct(gid, g.Name), nil
	}))

	// getgrall() - get all group database entries
	grpModule.SetStr("getgrall", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 0 {
			return nil, &core.TypeError{Message: fmt.Sprintf("getgrall() takes no arguments (%d given)", len(args))}
		}
		// We cannot enumerate the whole group database portably, but returning at
		// least the current user's primary group lets callers iterate.
		currentUser, err := user.Current()
		if err != nil {
			return core.NewList(), nil
		}
		g, err := user.LookupGroupId(currentUser.Gid)
		if err != nil {
			return core.NewList(), nil
		}
		gid, _ := strconv.Atoi(g.Gid)
		return core.NewList(makeGroupStruct(gid, g.Name)), nil
	}))

	return grpModule
}

// StructGroup represents a grp.struct_group with both tuple and attribute
// access, mirroring StructPasswd.
type StructGroup struct {
	core.BaseObject
	values []core.Value
	names  []string
}

func (s *StructGroup) Type() core.Type { return "grp.struct_group" }

func (s *StructGroup) String() string {
	return fmt.Sprintf("grp.struct_group(gr_name=%q, gr_passwd=%q, gr_gid=%v, gr_mem=%v)",
		s.values[0], s.values[1], s.values[2], s.values[3])
}

func (s *StructGroup) GetAttr(name string) (core.Value, bool) {
	for i, n := range s.names {
		if n == name {
			return s.values[i], true
		}
	}
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
			return s.GetItem(args[0])
		}), true
	case "__iter__":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return &structPasswdIterator{values: s.values, index: 0}, nil
		}), true
	case "n_fields", "n_sequence_fields":
		return core.NumberValue(len(s.values)), true
	case "n_unnamed_fields":
		return core.NumberValue(0), true
	}
	return nil, false
}

func (s *StructGroup) GetItem(key core.Value) (core.Value, error) {
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

func (s *StructGroup) Len() int { return len(s.values) }

// Iterator implements core.Iterable so struct_group unpacks/iterates as a
// 4-element sequence.
func (s *StructGroup) Iterator() core.Iterator {
	return &structPasswdIterator{values: s.values, index: 0}
}

// EqualsValue compares struct_group by value, like the tuple it subclasses.
func (s *StructGroup) EqualsValue(other core.Value) bool {
	var otherVals []core.Value
	switch o := other.(type) {
	case *StructGroup:
		otherVals = o.values
	case core.TupleValue:
		otherVals = []core.Value(o)
	default:
		return false
	}
	if len(s.values) != len(otherVals) {
		return false
	}
	for i := range s.values {
		if !core.EqualValues(s.values[i], otherVals[i]) {
			return false
		}
	}
	return true
}

// makeGroupStruct creates a struct_group: (gr_name, gr_passwd, gr_gid, gr_mem).
func makeGroupStruct(gid int, name string) *StructGroup {
	return &StructGroup{
		values: []core.Value{
			core.StringValue(name),
			core.StringValue("x"),
			core.NumberValue(gid),
			core.NewList(),
		},
		names: []string{"gr_name", "gr_passwd", "gr_gid", "gr_mem"},
	}
}

// InitMsvcrtModule creates a stub for the msvcrt Windows-specific module
func InitMsvcrtModule() *core.DictValue {
	msvcrtModule := core.NewDict()

	// Constants
	msvcrtModule.SetStr("LK_LOCK", core.NumberValue(1))
	msvcrtModule.SetStr("LK_NBLCK", core.NumberValue(2))
	msvcrtModule.SetStr("LK_NBRLCK", core.NumberValue(3))
	msvcrtModule.SetStr("LK_RLCK", core.NumberValue(4))
	msvcrtModule.SetStr("LK_UNLCK", core.NumberValue(0))

	// Console I/O functions - all stubs that do nothing on Unix
	msvcrtModule.SetStr("kbhit", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Return False - no key pressed (not available on Unix)
		return core.BoolValue(false), nil
	}))

	msvcrtModule.SetStr("getch", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("msvcrt.getch() is not available on this platform")
	}))

	msvcrtModule.SetStr("getwch", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("msvcrt.getwch() is not available on this platform")
	}))

	msvcrtModule.SetStr("getche", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("msvcrt.getche() is not available on this platform")
	}))

	msvcrtModule.SetStr("getwche", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("msvcrt.getwche() is not available on this platform")
	}))

	msvcrtModule.SetStr("putch", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("msvcrt.putch() is not available on this platform")
	}))

	msvcrtModule.SetStr("putwch", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("msvcrt.putwch() is not available on this platform")
	}))

	msvcrtModule.SetStr("ungetch", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("msvcrt.ungetch() is not available on this platform")
	}))

	msvcrtModule.SetStr("ungetwch", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("msvcrt.ungetwch() is not available on this platform")
	}))

	// File operations
	msvcrtModule.SetStr("locking", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("msvcrt.locking() is not available on this platform")
	}))

	msvcrtModule.SetStr("setmode", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("msvcrt.setmode() is not available on this platform")
	}))

	msvcrtModule.SetStr("open_osfhandle", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("msvcrt.open_osfhandle() is not available on this platform")
	}))

	msvcrtModule.SetStr("get_osfhandle", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("msvcrt.get_osfhandle() is not available on this platform")
	}))

	// Heap operations
	msvcrtModule.SetStr("heapmin", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("msvcrt.heapmin() is not available on this platform")
	}))

	return msvcrtModule
}
