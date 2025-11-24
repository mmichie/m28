package modules

import (
	"github.com/mmichie/m28/core"
)

// InitSSLModule creates and returns the ssl module stub
// This module provides SSL exception classes needed by asyncio.sslproto
// which is imported by unittest.mock and many CPython regression tests.
//
// The ssl module is primarily a C extension (_ssl), but Python code expects
// to import exception classes from the pure Python 'ssl' module wrapper.
// We provide stub exception classes to support the stdlib dependency chain:
// test files → unittest.mock → asyncio → asyncio.sslproto → ssl exceptions
func InitSSLModule() *core.DictValue {
	sslModule := core.NewDict()

	// Get OSError and ValueError from context (they're defined in builtin/errors.go)
	// We need to look them up when the module is first used, not at init time
	// For now, we'll create the exception hierarchy using NewClass directly

	// SSLError - base SSL exception (inherits from OSError)
	// In a real implementation, we'd need to get OSError from the context
	// For now, create a standalone class that will be compatible
	sslErrorClass := core.NewClass("SSLError", nil)

	// Add library attribute (required by some code that checks SSLError.library)
	sslErrorClass.SetAttr("library", core.None)
	// Add reason attribute (required by some code that checks SSLError.reason)
	sslErrorClass.SetAttr("reason", core.None)

	sslModule.Set("SSLError", sslErrorClass)

	// SSLZeroReturnError - SSL connection has been closed cleanly
	sslZeroReturnErrorClass := core.NewClass("SSLZeroReturnError", sslErrorClass)
	sslModule.Set("SSLZeroReturnError", sslZeroReturnErrorClass)

	// SSLWantReadError - non-blocking SSL socket needs to read more data before the operation can be completed
	sslWantReadErrorClass := core.NewClass("SSLWantReadError", sslErrorClass)
	sslModule.Set("SSLWantReadError", sslWantReadErrorClass)

	// SSLWantWriteError - non-blocking SSL socket needs to write more data before the operation can be completed
	sslWantWriteErrorClass := core.NewClass("SSLWantWriteError", sslErrorClass)
	sslModule.Set("SSLWantWriteError", sslWantWriteErrorClass)

	// SSLSyscallError - system error occurred (errno)
	sslSyscallErrorClass := core.NewClass("SSLSyscallError", sslErrorClass)
	sslModule.Set("SSLSyscallError", sslSyscallErrorClass)

	// SSLEOFError - EOF occurred in violation of protocol
	sslEOFErrorClass := core.NewClass("SSLEOFError", sslErrorClass)
	sslModule.Set("SSLEOFError", sslEOFErrorClass)

	// SSLCertVerificationError - certificate verification failed
	// In Python, this inherits from both SSLError and ValueError
	// For now, we'll inherit from SSLError only (M28 doesn't fully support multiple inheritance yet)
	sslCertVerificationErrorClass := core.NewClass("SSLCertVerificationError", sslErrorClass)
	sslModule.Set("SSLCertVerificationError", sslCertVerificationErrorClass)

	// CertificateError - certificate error occurred
	// In Python, this also inherits from both SSLError and ValueError
	certificateErrorClass := core.NewClass("CertificateError", sslErrorClass)
	sslModule.Set("CertificateError", certificateErrorClass)

	// SSLErrorNumber - not actually an exception but a constant/enum
	// Used by some code to check SSL error numbers
	// For now, just define it as an integer constant
	sslModule.Set("SSLErrorNumber", core.NumberValue(0))

	// Add module metadata
	sslModule.Set("__name__", core.StringValue("ssl"))
	sslModule.Set("__doc__", core.StringValue("Stub implementation of ssl module with exception classes"))

	return sslModule
}
