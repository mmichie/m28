package modules

import (
	"syscall"

	"github.com/mmichie/m28/core"
)

// InitErrnoModule creates the errno module with error codes
func InitErrnoModule() *core.DictValue {
	errnoModule := core.NewDict()
	errorcode := core.NewDict()

	// Single source of truth: each entry becomes both a module attribute
	// (errno.EXXX) and an errorcode[value] = "EXXX" entry, so the two stay in
	// sync (CPython's errno.errorcode covers every constant). When two names
	// share a value (e.g. EAGAIN and EWOULDBLOCK on Linux), the first listed
	// wins the errorcode entry, matching CPython.
	codes := []struct {
		name string
		num  syscall.Errno
	}{
		{"EPERM", syscall.EPERM},
		{"ENOENT", syscall.ENOENT},
		{"ESRCH", syscall.ESRCH},
		{"EINTR", syscall.EINTR},
		{"EIO", syscall.EIO},
		{"ENXIO", syscall.ENXIO},
		{"E2BIG", syscall.E2BIG},
		{"ENOEXEC", syscall.ENOEXEC},
		{"EBADF", syscall.EBADF},
		{"ECHILD", syscall.ECHILD},
		{"EAGAIN", syscall.EAGAIN},
		{"ENOMEM", syscall.ENOMEM},
		{"EACCES", syscall.EACCES},
		{"EFAULT", syscall.EFAULT},
		{"ENOTBLK", syscall.ENOTBLK},
		{"EBUSY", syscall.EBUSY},
		{"EEXIST", syscall.EEXIST},
		{"EXDEV", syscall.EXDEV},
		{"ENODEV", syscall.ENODEV},
		{"ENOTDIR", syscall.ENOTDIR},
		{"EISDIR", syscall.EISDIR},
		{"EINVAL", syscall.EINVAL},
		{"ENFILE", syscall.ENFILE},
		{"EMFILE", syscall.EMFILE},
		{"ENOTTY", syscall.ENOTTY},
		{"ETXTBSY", syscall.ETXTBSY},
		{"EFBIG", syscall.EFBIG},
		{"ENOSPC", syscall.ENOSPC},
		{"ESPIPE", syscall.ESPIPE},
		{"EROFS", syscall.EROFS},
		{"EMLINK", syscall.EMLINK},
		{"EPIPE", syscall.EPIPE},
		{"EDOM", syscall.EDOM},
		{"ERANGE", syscall.ERANGE},
		{"EDEADLK", syscall.EDEADLK},
		{"ENAMETOOLONG", syscall.ENAMETOOLONG},
		{"ENOLCK", syscall.ENOLCK},
		{"ENOSYS", syscall.ENOSYS},
		{"ENOTEMPTY", syscall.ENOTEMPTY},
		{"ELOOP", syscall.ELOOP},
		{"EWOULDBLOCK", syscall.EWOULDBLOCK},
		{"ENOMSG", syscall.ENOMSG},
		{"EIDRM", syscall.EIDRM},
	}
	for _, c := range codes {
		val := core.NumberValue(int(c.num))
		errnoModule.Set(c.name, val)
		// errorcode is keyed by the integer value (errorcode[errno.EXXX]).
		if _, exists := errorcode.GetValue(val); !exists {
			_ = errorcode.SetValue(val, core.StringValue(c.name))
		}
	}

	errnoModule.Set("errorcode", errorcode)

	return errnoModule
}
