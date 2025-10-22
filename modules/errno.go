package modules

import (
	"syscall"

	"github.com/mmichie/m28/core"
)

// InitErrnoModule creates the errno module with error codes
func InitErrnoModule() *core.DictValue {
	errnoModule := core.NewDict()

	// Common error codes from errno.h
	errnoModule.Set("EPERM", core.NumberValue(int(syscall.EPERM)))
	errnoModule.Set("ENOENT", core.NumberValue(int(syscall.ENOENT)))
	errnoModule.Set("ESRCH", core.NumberValue(int(syscall.ESRCH)))
	errnoModule.Set("EINTR", core.NumberValue(int(syscall.EINTR)))
	errnoModule.Set("EIO", core.NumberValue(int(syscall.EIO)))
	errnoModule.Set("ENXIO", core.NumberValue(int(syscall.ENXIO)))
	errnoModule.Set("E2BIG", core.NumberValue(int(syscall.E2BIG)))
	errnoModule.Set("ENOEXEC", core.NumberValue(int(syscall.ENOEXEC)))
	errnoModule.Set("EBADF", core.NumberValue(int(syscall.EBADF)))
	errnoModule.Set("ECHILD", core.NumberValue(int(syscall.ECHILD)))
	errnoModule.Set("EAGAIN", core.NumberValue(int(syscall.EAGAIN)))
	errnoModule.Set("ENOMEM", core.NumberValue(int(syscall.ENOMEM)))
	errnoModule.Set("EACCES", core.NumberValue(int(syscall.EACCES)))
	errnoModule.Set("EFAULT", core.NumberValue(int(syscall.EFAULT)))
	errnoModule.Set("ENOTBLK", core.NumberValue(int(syscall.ENOTBLK)))
	errnoModule.Set("EBUSY", core.NumberValue(int(syscall.EBUSY)))
	errnoModule.Set("EEXIST", core.NumberValue(int(syscall.EEXIST)))
	errnoModule.Set("EXDEV", core.NumberValue(int(syscall.EXDEV)))
	errnoModule.Set("ENODEV", core.NumberValue(int(syscall.ENODEV)))
	errnoModule.Set("ENOTDIR", core.NumberValue(int(syscall.ENOTDIR)))
	errnoModule.Set("EISDIR", core.NumberValue(int(syscall.EISDIR)))
	errnoModule.Set("EINVAL", core.NumberValue(int(syscall.EINVAL)))
	errnoModule.Set("ENFILE", core.NumberValue(int(syscall.ENFILE)))
	errnoModule.Set("EMFILE", core.NumberValue(int(syscall.EMFILE)))
	errnoModule.Set("ENOTTY", core.NumberValue(int(syscall.ENOTTY)))
	errnoModule.Set("ETXTBSY", core.NumberValue(int(syscall.ETXTBSY)))
	errnoModule.Set("EFBIG", core.NumberValue(int(syscall.EFBIG)))
	errnoModule.Set("ENOSPC", core.NumberValue(int(syscall.ENOSPC)))
	errnoModule.Set("ESPIPE", core.NumberValue(int(syscall.ESPIPE)))
	errnoModule.Set("EROFS", core.NumberValue(int(syscall.EROFS)))
	errnoModule.Set("EMLINK", core.NumberValue(int(syscall.EMLINK)))
	errnoModule.Set("EPIPE", core.NumberValue(int(syscall.EPIPE)))
	errnoModule.Set("EDOM", core.NumberValue(int(syscall.EDOM)))
	errnoModule.Set("ERANGE", core.NumberValue(int(syscall.ERANGE)))
	errnoModule.Set("EDEADLK", core.NumberValue(int(syscall.EDEADLK)))
	errnoModule.Set("ENAMETOOLONG", core.NumberValue(int(syscall.ENAMETOOLONG)))
	errnoModule.Set("ENOLCK", core.NumberValue(int(syscall.ENOLCK)))
	errnoModule.Set("ENOSYS", core.NumberValue(int(syscall.ENOSYS)))
	errnoModule.Set("ENOTEMPTY", core.NumberValue(int(syscall.ENOTEMPTY)))
	errnoModule.Set("ELOOP", core.NumberValue(int(syscall.ELOOP)))

	// Additional common errors (cross-platform)
	errnoModule.Set("EWOULDBLOCK", core.NumberValue(int(syscall.EWOULDBLOCK)))
	errnoModule.Set("ENOMSG", core.NumberValue(int(syscall.ENOMSG)))
	errnoModule.Set("EIDRM", core.NumberValue(int(syscall.EIDRM)))

	// errorcode dict - maps errno to name
	errorcode := core.NewDict()
	addErrorCode := func(num syscall.Errno, name string) {
		errorcode.Set(core.NumberValue(int(num)).String(), core.StringValue(name))
	}

	addErrorCode(syscall.EPERM, "EPERM")
	addErrorCode(syscall.ENOENT, "ENOENT")
	addErrorCode(syscall.EACCES, "EACCES")
	addErrorCode(syscall.EEXIST, "EEXIST")
	addErrorCode(syscall.EISDIR, "EISDIR")
	addErrorCode(syscall.ENOTDIR, "ENOTDIR")
	addErrorCode(syscall.EINVAL, "EINVAL")
	// Add more as needed...

	errnoModule.Set("errorcode", errorcode)

	return errnoModule
}
