# Minimal stub for test.support.os_helper.

import os
import unittest


TESTFN = '@test_file'
SAVEDCWD = os.getcwd()


def can_symlink():
    """Return True if symlinks work on this OS."""
    return hasattr(os, 'symlink')


def skip_unless_symlink(test):
    if not can_symlink():
        return unittest.skip("symlinks not supported")(test)
    return test


def can_chmod():
    return hasattr(os, 'chmod')


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
