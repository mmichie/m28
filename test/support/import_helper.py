# Minimal stub for test.support.import_helper.

import sys
import unittest


def import_module(name, deprecated=False, *, required_on=()):
    """Import a module, skipping the test if it fails."""
    try:
        __import__(name)
        return sys.modules[name]
    except ImportError:
        raise unittest.SkipTest(f"module {name!r} not available")


def import_fresh_module(name, fresh=(), blocked=(), *, deprecated=False):
    """Import a "fresh" copy of `name`. Best-effort: returns the cached one."""
    return import_module(name)


def unload(name):
    """Remove a module from sys.modules."""
    try:
        del sys.modules[name]
    except KeyError:
        pass


class CleanImport:
    """Context manager that ensures named modules can be freshly imported."""

    def __init__(self, *module_names):
        self.original_modules = sys.modules.copy()
        self.module_names = module_names

    def __enter__(self):
        for name in self.module_names:
            if name in sys.modules:
                del sys.modules[name]
        return self

    def __exit__(self, *exc):
        sys.modules.clear()
        sys.modules.update(self.original_modules)
        return False


class DirsOnSysPath:
    """Context manager: temporarily add directories to sys.path."""

    def __init__(self, *paths):
        self.paths = paths
        self.original_value = None

    def __enter__(self):
        self.original_value = sys.path[:]
        for p in self.paths:
            sys.path.insert(0, p)
        return self

    def __exit__(self, *exc):
        sys.path[:] = self.original_value
        return False


def modules_setup():
    """Return a snapshot of currently-loaded modules."""
    return list(sys.modules)


def modules_cleanup(saved_modules):
    """Restore sys.modules to a saved snapshot."""
    for name in list(sys.modules):
        if name not in saved_modules:
            del sys.modules[name]


def make_legacy_pyc(source):
    """No-op: M28 doesn't produce .pyc files."""
    return None


def forget(modname):
    """Remove a module and its compiled equivalents."""
    unload(modname)
