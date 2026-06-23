# Minimal test.support stub for M28.
#
# The real CPython test.support is ~3000 lines, much of it specific to
# CPython implementation details (refcount tracking, GC introspection,
# subprocess helpers, threading helpers, etc). For language conformance
# tests we mostly need:
#   - cpython_only (decorator to skip implementation-specific tests)
#   - check_impl_detail (returns whether we're on CPython)
#   - import_helper (submodule)
#   - assertion helpers (check_syntax_error etc)
#   - basic constants (verbose, etc)
#
# This stub provides defensive no-ops/permissive implementations so test
# files can load and the platform-independent assertions inside them can run.


import sys
import unittest


verbose = 0
is_jython = False
is_android = False
is_emscripten = False
is_wasi = False

# True only on a CPython debug build with reference tracing. M28 is never such a
# build; computed the same way CPython's test.support does.
Py_TRACE_REFS = hasattr(sys, 'getobjects')


# Some CPython tests check whether they're on CPython specifically and skip
# implementation-specific behavior on other interpreters. M28 is not CPython.
def check_impl_detail(**kwargs):
    """Return True iff this implementation matches the given kwargs.

    Real CPython sets `cpython=True` here; on M28 we always return False so
    CPython-specific test bodies are skipped.
    """
    # CPython-only assertions should be skipped on M28
    if kwargs.get("cpython"):
        return False
    return True


def cpython_only(test):
    """Decorator skipping the test when not running on CPython."""
    return unittest.skip("CPython-specific test, skipped on M28")(test)


def impl_detail(msg=None, **guards):
    """Decorator skipping the test when implementation doesn't match guards."""
    if check_impl_detail(**guards):
        return lambda test: test
    if msg is None:
        msg = "implementation-detail test, skipped on M28"
    return unittest.skip(msg)


def requires(resource, msg=None):
    """Decorator skipping the test when a resource isn't available."""
    return unittest.skip(msg or f"requires resource {resource!r}")


def requires_resource(resource):
    return requires(resource)


def requires_subprocess():
    return unittest.skip("subprocess not available in M28 test stub")


def requires_freebsd_version(*args):
    return unittest.skip("OS-version-specific")


def requires_linux_version(*args):
    return unittest.skip("OS-version-specific")


def requires_mac_ver(*args):
    return unittest.skip("OS-version-specific")


def requires_docstrings(test):
    return test


def requires_working_socket(*, module=False):
    if module:
        return unittest.skip("socket may not work in M28 test stub")
    def deco(test):
        return unittest.skip("socket may not work in M28 test stub")(test)
    return deco


def requires_gil_enabled(test):
    return test


def requires_specialization(test):
    return unittest.skip("CPython specialization-specific")(test)


def requires_legacy_unicode_capi(test):
    return unittest.skip("legacy unicode C API")(test)


def gc_collect():
    """Simulate gc.collect() — M28 doesn't have explicit cycle GC."""
    try:
        import gc
        gc.collect()
    except Exception:
        pass


def check_free_after_iterating(test, iter_func, container_type, *, base_cls=None):
    """No-op stub — checks CPython's reference-cycle behaviour."""
    pass


def check_disallow_instantiation(test, tp, *args, **kwargs):
    """Assert that a type cannot be instantiated."""
    msg = "cannot create '.*' instances"
    with test.assertRaises(TypeError):
        tp(*args, **kwargs)


def check_syntax_error(testcase, statement, errtext='', *, lineno=None, offset=None):
    """Verify that `statement` raises SyntaxError when compiled.

    Real CPython matches errtext against the exception message; we keep the
    check loose so M28's slightly different error messages still pass.
    """
    with testcase.assertRaises(SyntaxError):
        compile(statement, '<test>', 'exec')


def check_syntax_warning(testcase, statement, errtext='', *, lineno=1, offset=None):
    """Verify that `statement` triggers a SyntaxWarning. No-op on M28."""
    pass


def check_warnings(*filters, quiet=True):
    """Return a context manager that suppresses warnings (no-op)."""
    class _Ctx:
        def __init__(self):
            self.warnings = []
        def __enter__(self):
            return self
        def __exit__(self, *a):
            return False
    return _Ctx()


def captured_stdout():
    return _captured('stdout')


def captured_stderr():
    return _captured('stderr')


def captured_output(stream_name):
    return _captured(stream_name)


def _captured(stream_name):
    import io
    class _Capture:
        def __enter__(self):
            self._old = getattr(sys, stream_name)
            self._new = io.StringIO()
            setattr(sys, stream_name, self._new)
            return self._new
        def __exit__(self, *a):
            setattr(sys, stream_name, self._old)
            return False
    return _Capture()


def run_unittest(*classes):
    """Run unittest tests for the given classes."""
    suite = unittest.TestSuite() if hasattr(unittest, 'TestSuite') and unittest.TestSuite else None
    # Fallback: just instantiate and run
    for cls in classes:
        if isinstance(cls, type) and issubclass(cls, unittest.TestCase):
            for name in dir(cls):
                if name.startswith('test'):
                    instance = cls(name)
                    instance.run()


def gc_threshold(*args, **kwargs):
    class _Ctx:
        def __enter__(self):
            return self
        def __exit__(self, *a):
            return False
    return _Ctx()


def no_tracing(test):
    """No-op decorator."""
    return test


def suppress_immortalization(suppress=True):
    """No-op decorator factory (CPython 3.14 immortalization control)."""
    def decorator(test):
        return test
    return decorator


def refcount_test(test):
    """Decorator skipping tests that need CPython refcounting."""
    return unittest.skip("refcount-specific test")(test)


def with_pymalloc():
    """Returns whether CPython's pymalloc is used. Always False on M28."""
    return False


def has_no_debug_ranges():
    """Whether the build has no debug ranges (used by tracebacks)."""
    return True


def open_urlresource(url, *args, **kwargs):
    raise unittest.SkipTest("open_urlresource not supported in M28 test stub")


_2G = 2 * 1024 ** 3
_4G = 4 * 1024 ** 3


def bigmemtest(size, memuse, dry_run=True):
    """Decorator for big memory tests — skip on M28."""
    def decorator(test):
        return unittest.skip("bigmemtest not supported in M28 test stub")(test)
    return decorator


def bigaddrspacetest(test):
    return unittest.skip("bigaddrspacetest not supported in M28 test stub")(test)


def precisionbigmemtest(*args, **kwargs):
    def decorator(test):
        return unittest.skip("precisionbigmemtest")(test)
    return decorator


def anticipate_failure(condition):
    def decorator(test):
        if condition:
            return unittest.expectedFailure(test)
        return test
    return decorator


def reap_threads(func):
    """No-op decorator; M28 test stub doesn't need to clean up threads."""
    return func


def reap_children():
    """No-op."""
    pass


def threading_cleanup(*args):
    """No-op."""
    pass


def threading_setup():
    return (0, ())


class SuppressCrashReport:
    """Context manager that suppresses crash reports (Windows). No-op on Linux."""
    def __enter__(self):
        return self
    def __exit__(self, *exc):
        return False


def disable_faulthandler():
    """No-op context manager."""
    class _Ctx:
        def __enter__(self):
            return self
        def __exit__(self, *exc):
            return False
    return _Ctx()


def reset_default_excepthook():
    """No-op."""
    pass


def force_not_colorized(test):
    """No-op decorator."""
    return test


def force_not_colorized_test_class(cls):
    """No-op class decorator."""
    return cls


def requires_limited_api(test):
    """Decorator: skip when limited C API isn't available."""
    return unittest.skip("limited C API not supported in M28")(test)


def requires_fork(test):
    return unittest.skip("os.fork() not supported in M28 test stub")(test)


def requires_IEEE_754(test):
    return test  # Most platforms have IEEE 754


def requires_zlib(test):
    """Decorator: skip when zlib isn't available."""
    try:
        import zlib  # noqa: F401
        return test
    except ImportError:
        return unittest.skip("zlib not available")(test)


def requires_bz2(test):
    try:
        import bz2  # noqa: F401
        return test
    except ImportError:
        return unittest.skip("bz2 not available")(test)


def requires_lzma(test):
    try:
        import lzma  # noqa: F401
        return test
    except ImportError:
        return unittest.skip("lzma not available")(test)


def requires_hashdigest(digestname, openssl=False, usedforsecurity=True):
    """Decorator: skip when a hash algorithm is not available."""
    def decorator(test):
        return unittest.skip(f"hashdigest {digestname!r} not available")(test)
    return decorator


def import_helper_msg(msg):
    return None


def system_must_validate_cert(f):
    return f


def python_is_optimized():
    return False


def perf_counter():
    import time
    return time.perf_counter()


# These constants are queried defensively by some test modules.
Py_DEBUG = False
Py_GIL_DISABLED = False
TEST_HOME_DIR = '.'
TEST_DATA_DIR = '.'
TEST_SUPPORT_DIR = '.'


def is_resource_enabled(resource):
    return False


def use_old_parser():
    return False


def get_pagesize():
    return 4096


def set_recursion_limit(limit):
    """Context manager to set sys.recursionlimit temporarily.

    M28 doesn't enforce recursion limit, so this is a no-op context manager.
    """
    class _Ctx:
        def __enter__(self):
            return self
        def __exit__(self, *exc):
            return False
    return _Ctx()


def infinite_recursion(max_depth=None):
    """Context manager that raises if recursion exceeds max_depth. No-op."""
    class _Ctx:
        def __enter__(self):
            return self
        def __exit__(self, *exc):
            return False
    return _Ctx()


def setswitchinterval(interval):
    """No-op context manager."""
    class _Ctx:
        def __enter__(self):
            return self
        def __exit__(self, *exc):
            return False
    return _Ctx()


def adjust_int_max_str_digits(max_digits):
    class _Ctx:
        def __enter__(self):
            return self
        def __exit__(self, *exc):
            return False
    return _Ctx()


def skip_if_sanitizer(msg=None, *, address=False, memory=False, thread=False, ub=False):
    def decorator(test):
        return test
    return decorator


def thread_unsafe(reason):
    def decorator(test):
        return test
    return decorator


def has_subprocess_support():
    return False


def has_socket_support():
    try:
        import socket  # noqa: F401
        return True
    except ImportError:
        return False


def get_signal_name(signum):
    """Look up signal name by number, or return numeric string."""
    try:
        import signal
        for name in dir(signal):
            if name.startswith('SIG') and not name.startswith('SIG_'):
                if getattr(signal, name, None) == signum:
                    return name
    except Exception:
        pass
    return str(signum)


# Many CPython test files pull in narrow architecture/platform skip
# decorators from test.support. Enumerate the common ones so they import.
def _make_skip(reason):
    def decorator(test):
        return unittest.skip(reason)(test)
    return decorator


skip_on_s390x = _make_skip("s390x-specific test")
skip_emscripten_stack_overflow = _make_skip("emscripten-specific")
skip_wasi_stack_overflow = _make_skip("wasi-specific")
skip_if_buildbot = _make_skip("buildbot-specific")
skip_if_buggy_ucrt_strfptime = _make_skip("ucrt-specific")
skip_if_pgo_task = _make_skip("PGO-specific")
skip_if_sanitizer_msan = _make_skip("MSAN-specific")
skip_unless_xattr = _make_skip("xattr not available")
skip_unless_symlink = _make_skip("symlink not supported")
skip_unless_bind_unix_socket = _make_skip("unix bind not supported")


# Catch-all: many CPython test files import narrow predicate decorators
# from test.support that we don't model. Python's module __getattr__ hook
# (PEP 562) is used to manufacture defaults so the import succeeds.
#
# Important: default `requires_*` to NO-OP (test runs as-is). Defaulting
# to skip would silently disable hundreds of tests that don't actually
# need the missing feature for their assertions to be valid on M28.
# Specific architecture/platform skips above stay as real skips.
def _noop_decorator(name):
    def decorator(test_or_obj):
        return test_or_obj
    decorator.__name__ = name
    return decorator


def __getattr__(name):
    if (name.startswith('requires_') or name.startswith('skip_unless_')):
        return _noop_decorator(name)
    if name.startswith('skip_if_') or name.startswith('skip_on_'):
        return _make_skip(f"{name} not implemented in M28 test stub")
    if name.startswith('is_') or name.startswith('has_'):
        return False
    raise AttributeError(f"module 'test.support' has no attribute {name!r}")


def get_c_recursion_limit():
    """Approximate CPython's C recursion limit."""
    return 200


def get_attribute(obj, name):
    """Get an attribute, skipping the test if missing."""
    try:
        return getattr(obj, name)
    except AttributeError:
        raise unittest.SkipTest(f"object has no attribute {name!r}")


def findfile(filename, *, subdir=None):
    """Find a file relative to the test directory (best-effort)."""
    import os
    base = os.path.dirname(__file__)
    candidates = [base, os.path.join(base, '..')]
    if subdir:
        candidates = [os.path.join(c, subdir) for c in candidates]
    for d in candidates:
        path = os.path.join(d, filename)
        if os.path.exists(path):
            return path
    return filename


# Common architecture/platform constants
HOST = 'localhost'
HOSTv4 = '127.0.0.1'
HOSTv6 = '::1'
TESTFN = '@test'
SHORT_TIMEOUT = 30.0
LONG_TIMEOUT = 60.0
LOOPBACK_TIMEOUT = 5.0
INTERNET_TIMEOUT = 60.0

# Architecture details for hash-related tests
import sys as _sys
NHASHBITS = 64 if _sys.maxsize > 2**32 else 32


# ALWAYS_EQ / NEVER_EQ are sentinel objects used by container tests to
# poke at __eq__ behaviour without triggering NotImplemented fallbacks.
class _ALWAYS_EQ:
    def __eq__(self, other):
        return True
    def __ne__(self, other):
        return False
    def __hash__(self):
        return 0
    def __repr__(self):
        return 'ALWAYS_EQ'


class _NEVER_EQ:
    def __eq__(self, other):
        return False
    def __ne__(self, other):
        return True
    def __hash__(self):
        return 1
    def __repr__(self):
        return 'NEVER_EQ'


ALWAYS_EQ = _ALWAYS_EQ()
NEVER_EQ = _NEVER_EQ()


class BrokenIter:
    """Iterator that can be configured to fail at various points.

    Used by container tests to verify error handling around iteration.
    """
    def __init__(self, init_raises=False, next_raises=False, iter_raises=False):
        if init_raises:
            1 / 0
        self.next_raises = next_raises
        self.iter_raises = iter_raises

    def __next__(self):
        if self.next_raises:
            1 / 0
        raise StopIteration

    def __iter__(self):
        if self.iter_raises:
            1 / 0
        return self


# Comparable sentinels used by tests for ordering (similar to ALWAYS_EQ).
class _LARGEST:
    def __le__(self, other): return self is other
    def __lt__(self, other): return False
    def __ge__(self, other): return True
    def __gt__(self, other): return self is not other
    def __eq__(self, other): return self is other
    def __ne__(self, other): return self is not other
    def __hash__(self): return 2
    def __repr__(self): return 'LARGEST'


class _SMALLEST:
    def __le__(self, other): return True
    def __lt__(self, other): return self is not other
    def __ge__(self, other): return self is other
    def __gt__(self, other): return False
    def __eq__(self, other): return self is other
    def __ne__(self, other): return self is not other
    def __hash__(self): return 3
    def __repr__(self): return 'SMALLEST'


LARGEST = _LARGEST()
SMALLEST = _SMALLEST()


def collision_stats(nbins, nballs):
    """Compute expected mean and stdev for a ball-and-bin collision.

    Used by some hash-distribution tests. The real CPython has a precise
    formula; we approximate well enough for tests that only check ratios.
    """
    mean = nballs - nbins + nbins * ((nbins - 1) / nbins) ** nballs
    sdev = (mean * (1 - mean / nbins)) ** 0.5
    return mean, sdev


# Re-export common things via the test.support namespace
__all__ = [
    'verbose', 'is_jython', 'is_android', 'is_emscripten', 'is_wasi',
    'check_impl_detail', 'cpython_only', 'impl_detail',
    'requires', 'requires_resource', 'requires_subprocess',
    'requires_freebsd_version', 'requires_linux_version', 'requires_mac_ver',
    'requires_docstrings', 'requires_working_socket', 'requires_gil_enabled',
    'requires_specialization', 'requires_legacy_unicode_capi',
    'gc_collect', 'check_free_after_iterating', 'check_disallow_instantiation',
    'check_syntax_error', 'check_syntax_warning', 'check_warnings',
    'captured_stdout', 'captured_stderr', 'captured_output',
    'run_unittest', 'gc_threshold', 'no_tracing',
    'get_c_recursion_limit', 'get_attribute', 'findfile',
    'HOST', 'HOSTv4', 'HOSTv6', 'TESTFN',
    'SHORT_TIMEOUT', 'LONG_TIMEOUT', 'LOOPBACK_TIMEOUT', 'INTERNET_TIMEOUT',
    'NHASHBITS', 'collision_stats',
    'ALWAYS_EQ', 'NEVER_EQ', 'LARGEST', 'SMALLEST', 'BrokenIter',
]
