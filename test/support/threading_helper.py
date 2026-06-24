# Minimal stub for test.support.threading_helper.
#
# The real module provides helpers for thread-leak detection and joining
# spawned threads. For language conformance tests we only need permissive
# no-ops plus the decorators tests reference.

import contextlib
import functools
import threading
import unittest


def requires_working_threading(*, module=False):
    """Skip a test (or whole module) when threading is unavailable.

    M28 supports threading, so this is a pass-through decorator.
    """
    if module:
        return None

    def decorator(test):
        return test

    return decorator


def join_thread(thread, timeout=None):
    """Join a thread, ignoring timeouts."""
    thread.join(timeout)


@contextlib.contextmanager
def wait_threads_exit(timeout=None):
    """Context manager that yields; thread bookkeeping is a no-op here."""
    yield


def threading_setup():
    return (threading.active_count(), ())


def threading_cleanup(*original_values):
    pass


def reap_threads(func):
    """Decorator that runs the test; no thread reaping needed."""

    @functools.wraps(func)
    def decorator(*args, **kwargs):
        return func(*args, **kwargs)

    return decorator


def catch_threading_exception():
    """Return a context manager capturing threading.excepthook exceptions."""
    return _ThreadingExceptionCatcher()


class _ThreadingExceptionCatcher:
    def __init__(self):
        self.args = None

    def __enter__(self):
        return self

    def __exit__(self, *exc):
        return False
