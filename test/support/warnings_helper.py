# Minimal stub for test.support.warnings_helper.

import contextlib
import warnings


def check_warnings(*filters, quiet=True):
    """Context manager that captures warnings."""
    return warnings.catch_warnings(record=True)


def check_no_warnings(testcase, msg=None, *, category=Warning):
    """Context manager checking that no warnings of `category` are raised."""
    class _Ctx:
        def __init__(self):
            self.warning_list = []
        def __enter__(self):
            self._cm = warnings.catch_warnings(record=True)
            self.warning_list = self._cm.__enter__()
            warnings.simplefilter("always")
            return self
        def __exit__(self, *exc):
            self._cm.__exit__(*exc)
            for w in self.warning_list:
                if issubclass(w.category, category):
                    testcase.fail(msg or f"unexpected warning: {w.message}")
            return False
    return _Ctx()


def check_syntax_warning(testcase, statement, errtext='', *, lineno=1, offset=None):
    """Stub for check_syntax_warning."""
    pass


def ignore_warnings(*, category=DeprecationWarning):
    """Decorator that runs the wrapped function with warnings suppressed."""
    def decorator(func):
        def wrapper(*args, **kwargs):
            with warnings.catch_warnings():
                warnings.simplefilter("ignore", category=category)
                return func(*args, **kwargs)
        return wrapper
    return decorator


def import_deprecated(name):
    """Import a deprecated module, ignoring the warning."""
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", category=DeprecationWarning)
        return __import__(name)


@contextlib.contextmanager
def save_restore_warnings_filters():
    """Save the warnings filters on entry and restore them on exit."""
    old_filters = warnings.filters[:]
    try:
        yield
    finally:
        warnings.filters[:] = old_filters
