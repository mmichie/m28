# Minimal stub for test.support.os_helper.

import os
import unittest


TESTFN = '@test_file'
SAVEDCWD = os.getcwd()


class EnvironmentVarGuard:
    """Class to help protect the environment variable properly.

    Can be used as a context manager. Restores os.environ to its original
    state on exit, like CPython's test.support.os_helper.EnvironmentVarGuard.
    """

    def __init__(self):
        self._environ = os.environ
        self._changed = {}

    def __getitem__(self, envvar):
        return self._environ[envvar]

    def __setitem__(self, envvar, value):
        if envvar not in self._changed:
            self._changed[envvar] = self._environ.get(envvar)
        self._environ[envvar] = value

    def __delitem__(self, envvar):
        if envvar not in self._changed:
            self._changed[envvar] = self._environ.get(envvar)
        if envvar in self._environ:
            del self._environ[envvar]

    def __iter__(self):
        return iter(self._environ)

    def __len__(self):
        return len(self._environ)

    def keys(self):
        return self._environ.keys()

    def get(self, envvar, default=None):
        return self._environ.get(envvar, default)

    def set(self, envvar, value):
        self[envvar] = value

    def unset(self, envvar):
        if envvar in self._environ:
            del self[envvar]

    def __enter__(self):
        return self

    def __exit__(self, *ignore_exc):
        for envvar, value in self._changed.items():
            if value is None:
                if envvar in self._environ:
                    del self._environ[envvar]
            else:
                self._environ[envvar] = value


def can_symlink():
    """Return True if symlinks work on this OS."""
    return hasattr(os, 'symlink')


def skip_unless_symlink(test):
    if not can_symlink():
        return unittest.skip("symlinks not supported")(test)
    return test


def can_chmod():
    return hasattr(os, 'chmod')


def skip_unless_working_chmod(test):
    """Skip tests that require a working os.chmod()."""
    if not can_chmod():
        return unittest.skip("requires working os.chmod()")(test)
    return test


def can_xattr():
    return hasattr(os, 'getxattr')


def unlink(filename):
    try:
        os.unlink(filename)
    except (FileNotFoundError, IsADirectoryError):
        pass


def rmtree(path):
    import shutil
    try:
        shutil.rmtree(path)
    except FileNotFoundError:
        pass


def temp_dir(path=None, quiet=False):
    """Context manager that creates a temporary directory."""
    import tempfile
    class _TempDir:
        def __enter__(self):
            self.path = tempfile.mkdtemp()
            return self.path
        def __exit__(self, *exc):
            rmtree(self.path)
            return False
    return _TempDir()


def temp_cwd(name=None, quiet=False):
    """Context manager that changes cwd to a temporary dir."""
    import tempfile
    class _TempCwd:
        def __enter__(self):
            self.old = os.getcwd()
            self.path = tempfile.mkdtemp()
            os.chdir(self.path)
            return self.path
        def __exit__(self, *exc):
            os.chdir(self.old)
            rmtree(self.path)
            return False
    return _TempCwd()


def create_empty_file(filename):
    open(filename, 'w').close()
