# Minimal stub for test.support.script_helper.

import unittest


def assert_python_ok(*args, **env_vars):
    """Stub: would run Python with args and assert it exits OK.
    Returns (rc, stdout, stderr). M28 returns success unconditionally.
    """
    return (0, b'', b'')


def assert_python_failure(*args, **env_vars):
    return (1, b'', b'')


def run_python_until_end(*args, **env_vars):
    return _Result(0, b'', b'')


class _Result:
    def __init__(self, rc, out, err):
        self.rc = rc
        self.out = out
        self.err = err


def make_script(script_dir, script_basename, source):
    import os
    script_filename = os.path.join(script_dir, script_basename + '.py')
    with open(script_filename, 'w') as f:
        f.write(source)
    return script_filename


def spawn_python(*args, **env_vars):
    raise unittest.SkipTest("subprocess spawning not supported in M28 test stub")
