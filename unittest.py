# Minimal unittest stub for M28
#
# CPython's unittest module depends on a long chain of modules (inspect, dis,
# opcode, dataclasses, pprint, traceback) that require CPython bytecode
# introspection - a feature M28 doesn't have. This stub provides the most
# commonly used unittest API so real Python tests can run in M28.
#
# Supported:
#   - TestCase base class with the standard assert methods
#   - main() with automatic test discovery in __main__
#   - skip / skipIf / skipUnless decorators
#   - expectedFailure decorator
#   - setUp / tearDown / setUpClass / tearDownClass
#   - SkipTest exception
#
# Not yet supported (raises NotImplementedError or no-ops):
#   - TestSuite / TestLoader / TestRunner customization
#   - assertWarns / assertLogs context managers
#   - subTest


import sys


class SkipTest(Exception):
    """Raised to skip a test"""
    pass


class _AssertRaisesContext:
    """Context manager for assertRaises."""

    def __init__(self, expected, test_case, expected_regex=None):
        self.expected = expected
        self.test_case = test_case
        self.expected_regex = expected_regex
        self.exception = None

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_value, tb):
        if exc_type is None:
            try:
                exc_name = self.expected.__name__
            except AttributeError:
                exc_name = str(self.expected)
            raise AssertionError(f"{exc_name} not raised")
        if not issubclass(exc_type, self.expected):
            # Let unexpected exceptions propagate
            return False
        self.exception = exc_value
        if self.expected_regex is not None:
            import re
            pattern = self.expected_regex
            if not re.search(pattern, str(exc_value)):
                raise AssertionError(
                    f"\"{pattern}\" does not match \"{exc_value}\""
                )
        return True


class _AssertWarnsContext:
    """Context manager for assertWarns / assertWarnsRegex."""

    def __init__(self, expected, test_case, expected_regex=None):
        self.expected = expected
        self.test_case = test_case
        self.expected_regex = expected_regex
        self.warning = None
        self.filename = None
        self.lineno = None

    def __enter__(self):
        import warnings
        # Record all warnings raised inside the block.
        self.warnings_manager = warnings.catch_warnings(record=True)
        self.warnings = self.warnings_manager.__enter__()
        warnings.resetwarnings()
        warnings.simplefilter("always")
        return self

    def __exit__(self, exc_type, exc_value, tb):
        self.warnings_manager.__exit__(exc_type, exc_value, tb)
        if exc_type is not None:
            # Let unexpected exceptions propagate.
            return False
        try:
            exc_name = self.expected.__name__
        except AttributeError:
            exc_name = str(self.expected)
        first_matching = None
        for m in self.warnings:
            w = m.message
            if not isinstance(w, self.expected):
                continue
            if first_matching is None:
                first_matching = w
            if self.expected_regex is not None:
                import re
                if not re.search(self.expected_regex, str(w)):
                    continue
            # Found a matching warning; store details for later inspection.
            self.warning = w
            self.filename = m.filename
            self.lineno = m.lineno
            return True
        if first_matching is not None:
            raise AssertionError(
                f'"{self.expected_regex}" does not match "{first_matching}"'
            )
        raise AssertionError(f"{exc_name} not triggered")


class TestCase:
    """Base class for all M28 unittest tests."""

    # Override in subclasses to change the default failure exception type
    failureException = AssertionError

    # Default longMessage behavior matches CPython
    longMessage = True

    # The currently running test method name
    _testMethodName = None

    def __init__(self, methodName='runTest'):
        self._testMethodName = methodName
        self._cleanups = []
        try:
            self._testMethodDoc = getattr(self, methodName).__doc__
        except AttributeError:
            self._testMethodDoc = None

    # ---- Lifecycle hooks ----

    def setUp(self):
        """Hook method for setting up the test fixture before each test."""
        pass

    def tearDown(self):
        """Hook method for cleanup after each test."""
        pass

    @classmethod
    def setUpClass(cls):
        """Hook method run once before any tests in the class run."""
        pass

    @classmethod
    def tearDownClass(cls):
        """Hook method run once after all tests in the class have run."""
        pass

    def addCleanup(self, function, *args, **kwargs):
        """Add a function to be called after tearDown()."""
        self._cleanups.append((function, args, kwargs))

    def doCleanups(self):
        """Run any cleanup functions added with addCleanup()."""
        while self._cleanups:
            function, args, kwargs = self._cleanups.pop()
            function(*args, **kwargs)

    def enterContext(self, cm):
        """Enter the context manager and register its __exit__ as a cleanup,
        returning the result of __enter__ (like unittest's enterContext)."""
        result = cm.__enter__()
        self.addCleanup(cm.__exit__, None, None, None)
        return result

    def subTest(self, msg=None, **params):
        """Context manager for parameterised sub-tests.

        Real unittest reports each sub-test separately and continues past
        failures. M28's stub just runs the block; if the block raises, the
        exception propagates as a normal test failure. This is enough for
        most CPython tests to make progress instead of skipping the whole
        method.
        """
        class _SubTest:
            def __enter__(self_inner):
                return self_inner
            def __exit__(self_inner, exc_type, exc_val, exc_tb):
                return False
        return _SubTest()

    # ---- Test execution ----

    def skipTest(self, reason):
        """Skip the test, providing a reason."""
        raise SkipTest(reason)

    def fail(self, msg=None):
        """Fail immediately with the given message."""
        raise self.failureException(msg or "fail() called")

    def run(self, result=None):
        """Run the test, calling setUp/test/tearDown.

        Returns a tuple (status, message) where status is one of
        'pass', 'fail', 'error', 'skip', 'expected_failure',
        'unexpected_success'.
        """
        method = getattr(self, self._testMethodName)
        expected_failure = getattr(method, '__unittest_expecting_failure__', False)
        skip_reason = getattr(method, '__unittest_skip_reason__', None)
        if skip_reason is None:
            skip_reason = getattr(type(self), '__unittest_skip_reason__', None)
        if skip_reason is not None:
            return ('skip', skip_reason)

        try:
            self.setUp()
        except SkipTest as e:
            return ('skip', str(e))
        except Exception as e:
            return ('error', _format_exception(e))

        status = 'pass'
        message = None
        try:
            method()
        except SkipTest as e:
            status = 'skip'
            message = str(e)
        except self.failureException as e:
            if expected_failure:
                status = 'expected_failure'
                message = str(e)
            else:
                status = 'fail'
                message = _format_exception(e)
        except Exception as e:
            if expected_failure:
                status = 'expected_failure'
                message = str(e)
            else:
                status = 'error'
                message = _format_exception(e)
        else:
            if expected_failure:
                status = 'unexpected_success'
                message = 'Expected failure but test passed'

        try:
            self.tearDown()
        except Exception as e:
            if status == 'pass':
                status = 'error'
                message = _format_exception(e)

        try:
            self.doCleanups()
        except Exception as e:
            if status == 'pass':
                status = 'error'
                message = _format_exception(e)

        return (status, message)

    # ---- Assertion helpers ----

    def _formatMessage(self, msg, standardMsg):
        if not self.longMessage:
            return msg or standardMsg
        if msg is None:
            return standardMsg
        return f"{standardMsg} : {msg}"

    def assertEqual(self, first, second, msg=None):
        if first != second:
            std = f"{first!r} != {second!r}"
            raise self.failureException(self._formatMessage(msg, std))

    def assertNotEqual(self, first, second, msg=None):
        if first == second:
            std = f"{first!r} == {second!r}"
            raise self.failureException(self._formatMessage(msg, std))

    def assertTrue(self, expr, msg=None):
        if not expr:
            std = f"{expr!r} is not true"
            raise self.failureException(self._formatMessage(msg, std))

    def assertFalse(self, expr, msg=None):
        if expr:
            std = f"{expr!r} is not false"
            raise self.failureException(self._formatMessage(msg, std))

    def assertIs(self, first, second, msg=None):
        if first is not second:
            std = f"{first!r} is not {second!r}"
            raise self.failureException(self._formatMessage(msg, std))

    def assertIsNot(self, first, second, msg=None):
        if first is second:
            std = f"unexpectedly identical: {first!r}"
            raise self.failureException(self._formatMessage(msg, std))

    def assertIsNone(self, expr, msg=None):
        if expr is not None:
            std = f"{expr!r} is not None"
            raise self.failureException(self._formatMessage(msg, std))

    def assertIsNotNone(self, expr, msg=None):
        if expr is None:
            std = "unexpectedly None"
            raise self.failureException(self._formatMessage(msg, std))

    def assertIn(self, member, container, msg=None):
        if member not in container:
            std = f"{member!r} not found in {container!r}"
            raise self.failureException(self._formatMessage(msg, std))

    def assertNotIn(self, member, container, msg=None):
        if member in container:
            std = f"{member!r} unexpectedly found in {container!r}"
            raise self.failureException(self._formatMessage(msg, std))

    def assertIsInstance(self, obj, cls, msg=None):
        if not isinstance(obj, cls):
            std = f"{obj!r} is not an instance of {cls!r}"
            raise self.failureException(self._formatMessage(msg, std))

    def assertNotIsInstance(self, obj, cls, msg=None):
        if isinstance(obj, cls):
            std = f"{obj!r} is an instance of {cls!r}"
            raise self.failureException(self._formatMessage(msg, std))

    def assertGreater(self, first, second, msg=None):
        if not first > second:
            std = f"{first!r} not greater than {second!r}"
            raise self.failureException(self._formatMessage(msg, std))

    def assertGreaterEqual(self, first, second, msg=None):
        if not first >= second:
            std = f"{first!r} not greater than or equal to {second!r}"
            raise self.failureException(self._formatMessage(msg, std))

    def assertLess(self, first, second, msg=None):
        if not first < second:
            std = f"{first!r} not less than {second!r}"
            raise self.failureException(self._formatMessage(msg, std))

    def assertLessEqual(self, first, second, msg=None):
        if not first <= second:
            std = f"{first!r} not less than or equal to {second!r}"
            raise self.failureException(self._formatMessage(msg, std))

    def assertAlmostEqual(self, first, second, places=7, msg=None, delta=None):
        if first == second:
            return
        if delta is not None:
            if abs(first - second) <= delta:
                return
            std = f"{first!r} != {second!r} within {delta!r} delta"
        else:
            if round(abs(second - first), places) == 0:
                return
            std = f"{first!r} != {second!r} within {places} places"
        raise self.failureException(self._formatMessage(msg, std))

    def assertNotAlmostEqual(self, first, second, places=7, msg=None, delta=None):
        if delta is not None:
            if not (first == second) and abs(first - second) > delta:
                return
            std = f"{first!r} == {second!r} within {delta!r} delta"
        else:
            if not (first == second) and round(abs(second - first), places) != 0:
                return
            std = f"{first!r} == {second!r} within {places} places"
        raise self.failureException(self._formatMessage(msg, std))

    def assertRaises(self, expected_exception, *args, **kwargs):
        """Assert that the callable raises expected_exception.

        Usage:
          with self.assertRaises(ValueError):
              do_something()

          self.assertRaises(ValueError, do_something, arg1, arg2)
        """
        context = _AssertRaisesContext(expected_exception, self)
        if not args:
            return context
        callable_obj = args[0]
        with context:
            callable_obj(*args[1:], **kwargs)

    def assertRaisesRegex(self, expected_exception, expected_regex, *args, **kwargs):
        context = _AssertRaisesContext(expected_exception, self, expected_regex)
        if not args:
            return context
        callable_obj = args[0]
        with context:
            callable_obj(*args[1:], **kwargs)

    def assertWarns(self, expected_warning, *args, **kwargs):
        """Assert that the block / callable triggers expected_warning.

        Usage:
          with self.assertWarns(UserWarning):
              do_something()

          self.assertWarns(UserWarning, do_something, arg1, arg2)
        """
        context = _AssertWarnsContext(expected_warning, self)
        if not args:
            return context
        callable_obj = args[0]
        with context:
            callable_obj(*args[1:], **kwargs)

    def assertWarnsRegex(self, expected_warning, expected_regex, *args, **kwargs):
        """Like assertWarns but also checks the warning message matches a regex."""
        context = _AssertWarnsContext(expected_warning, self, expected_regex)
        if not args:
            return context
        callable_obj = args[0]
        with context:
            callable_obj(*args[1:], **kwargs)

    def assertListEqual(self, first, second, msg=None):
        self.assertEqual(list(first), list(second), msg)

    def assertTupleEqual(self, first, second, msg=None):
        self.assertEqual(tuple(first), tuple(second), msg)

    def assertSetEqual(self, first, second, msg=None):
        self.assertEqual(set(first), set(second), msg)

    def assertDictEqual(self, first, second, msg=None):
        self.assertEqual(dict(first), dict(second), msg)

    def assertSequenceEqual(self, first, second, msg=None, seq_type=None):
        if seq_type is not None:
            if not isinstance(first, seq_type) or not isinstance(second, seq_type):
                raise self.failureException(
                    self._formatMessage(msg, f"sequences not of type {seq_type!r}")
                )
        if len(first) != len(second):
            std = f"sequence lengths differ: {len(first)} != {len(second)}"
            raise self.failureException(self._formatMessage(msg, std))
        for i in range(len(first)):
            if first[i] != second[i]:
                std = f"sequences differ at index {i}: {first[i]!r} != {second[i]!r}"
                raise self.failureException(self._formatMessage(msg, std))

    def assertMultiLineEqual(self, first, second, msg=None):
        self.assertEqual(first, second, msg)

    def assertCountEqual(self, first, second, msg=None):
        first_list = list(first)
        second_list = list(second)
        if sorted(first_list) != sorted(second_list):
            std = f"Element counts differ: {first_list!r} vs {second_list!r}"
            raise self.failureException(self._formatMessage(msg, std))

    def assertRegex(self, text, expected_regex, msg=None):
        import re
        if not re.search(expected_regex, text):
            std = f"Regex didn't match: {expected_regex!r} not found in {text!r}"
            raise self.failureException(self._formatMessage(msg, std))

    def assertNotRegex(self, text, unexpected_regex, msg=None):
        import re
        if re.search(unexpected_regex, text):
            std = f"Regex matched: {unexpected_regex!r} found in {text!r}"
            raise self.failureException(self._formatMessage(msg, std))

    # ---- Legacy aliases used by older code ----
    failUnless = assertTrue
    assert_ = assertTrue
    failIf = assertFalse
    failUnlessEqual = assertEqual
    failIfEqual = assertNotEqual
    failUnlessRaises = assertRaises
    failUnlessAlmostEqual = assertAlmostEqual
    failIfAlmostEqual = assertNotAlmostEqual

    # ---- Representation ----

    def __str__(self):
        return f"{self._testMethodName} ({type(self).__name__})"

    def __repr__(self):
        return f"<{type(self).__name__} testMethod={self._testMethodName}>"


def _format_exception(exc):
    """Best-effort string formatting for an exception."""
    try:
        exc_type_name = type(exc).__name__
    except AttributeError:
        exc_type_name = "Exception"
    return f"{exc_type_name}: {exc}"


# ---- Decorators ----

def skip(reason):
    """Unconditionally skip the decorated test."""
    def decorator(test_item):
        if isinstance(test_item, type):
            test_item.__unittest_skip_reason__ = reason
            return test_item
        def wrapper(*args, **kwargs):
            raise SkipTest(reason)
        wrapper.__unittest_skip_reason__ = reason
        wrapper.__wrapped__ = test_item
        wrapper.__name__ = getattr(test_item, '__name__', 'wrapper')
        return wrapper
    return decorator


def skipIf(condition, reason):
    """Skip the test if condition is true."""
    if condition:
        return skip(reason)
    return lambda x: x


def skipUnless(condition, reason):
    """Skip the test unless condition is true."""
    if not condition:
        return skip(reason)
    return lambda x: x


def expectedFailure(test_item):
    """Mark the test as an expected failure."""
    test_item.__unittest_expecting_failure__ = True
    return test_item


# ---- Test discovery and execution ----

def _collect_test_methods(cls):
    """Return a list of test method names defined on the class."""
    names = []
    # dir() returns methods inherited too; this is what CPython does.
    for name in dir(cls):
        if not name.startswith('test'):
            continue
        attr = getattr(cls, name, None)
        if callable(attr):
            names.append(name)
    names.sort()
    return names


def _collect_test_classes(module_dict):
    """Return list of TestCase subclasses from a module's dict."""
    classes = []
    for name, value in module_dict.items():
        if isinstance(value, type) and value is not TestCase and issubclass(value, TestCase):
            classes.append((name, value))
    classes.sort(key=lambda nv: nv[0])
    return classes


class _TestRunResult:
    def __init__(self):
        self.tests_run = 0
        self.failures = []
        self.errors = []
        self.skipped = []
        self.expected_failures = []
        self.unexpected_successes = []

    def wasSuccessful(self):
        return not self.failures and not self.errors and not self.unexpected_successes


def _run_class(cls, name, verbosity, result):
    """Run all test methods in a TestCase subclass."""
    method_names = _collect_test_methods(cls)
    if not method_names:
        return

    try:
        cls.setUpClass()
    except SkipTest as e:
        for method_name in method_names:
            result.skipped.append((f"{name}.{method_name}", str(e)))
            if verbosity > 0:
                print(f"{name}.{method_name} ... skipped {str(e)!r}")
        return
    except Exception as e:
        for method_name in method_names:
            result.errors.append((f"{name}.{method_name}", _format_exception(e)))
            if verbosity > 0:
                print(f"{name}.{method_name} ... ERROR (setUpClass)")
        return

    for method_name in method_names:
        result.tests_run += 1
        test_id = f"{name}.{method_name}"
        try:
            instance = cls(method_name)
        except Exception as e:
            result.errors.append((test_id, _format_exception(e)))
            if verbosity > 0:
                print(f"{test_id} ... ERROR (constructor)")
            continue

        status, message = instance.run()
        if status == 'pass':
            if verbosity > 1:
                print(f"{test_id} ... ok")
            elif verbosity > 0:
                print(".", end="")
        elif status == 'fail':
            result.failures.append((test_id, message))
            if verbosity > 1:
                print(f"{test_id} ... FAIL")
            elif verbosity > 0:
                print("F", end="")
        elif status == 'error':
            result.errors.append((test_id, message))
            if verbosity > 1:
                print(f"{test_id} ... ERROR")
            elif verbosity > 0:
                print("E", end="")
        elif status == 'skip':
            result.skipped.append((test_id, message or ""))
            if verbosity > 1:
                print(f"{test_id} ... skipped {message!r}")
            elif verbosity > 0:
                print("s", end="")
        elif status == 'expected_failure':
            result.expected_failures.append((test_id, message))
            if verbosity > 1:
                print(f"{test_id} ... expected failure")
            elif verbosity > 0:
                print("x", end="")
        elif status == 'unexpected_success':
            result.unexpected_successes.append(test_id)
            if verbosity > 1:
                print(f"{test_id} ... unexpected success")
            elif verbosity > 0:
                print("u", end="")

    try:
        cls.tearDownClass()
    except Exception as e:
        result.errors.append((f"{name}.tearDownClass", _format_exception(e)))


def _summarize(result, verbosity):
    if verbosity > 0 and verbosity <= 1:
        print()

    for test_id, message in result.errors:
        print(f"\nERROR: {test_id}")
        print("-" * 70)
        print(message)
    for test_id, message in result.failures:
        print(f"\nFAIL: {test_id}")
        print("-" * 70)
        print(message)

    print()
    print("-" * 70)
    print(f"Ran {result.tests_run} test{'' if result.tests_run == 1 else 's'}")
    print()
    parts = []
    if result.failures:
        parts.append(f"failures={len(result.failures)}")
    if result.errors:
        parts.append(f"errors={len(result.errors)}")
    if result.skipped:
        parts.append(f"skipped={len(result.skipped)}")
    if result.expected_failures:
        parts.append(f"expected failures={len(result.expected_failures)}")
    if result.unexpected_successes:
        parts.append(f"unexpected successes={len(result.unexpected_successes)}")
    if result.wasSuccessful():
        if parts:
            print(f"OK ({', '.join(parts)})")
        else:
            print("OK")
    else:
        print(f"FAILED ({', '.join(parts)})")


def main(module='__main__', exit=True, verbosity=1, argv=None):
    """Discover and run all tests in the given module.

    For now this is a simplified version of unittest.main(): it discovers
    TestCase subclasses in the named module, runs all test methods on each,
    and prints a CPython-style summary.
    """
    # Default behaviour: collect tests from sys.modules[module]
    if module == '__main__' or module is None:
        target_module = sys.modules.get('__main__')
    elif isinstance(module, str):
        target_module = sys.modules.get(module)
        if target_module is None:
            target_module = __import__(module)
    else:
        target_module = module

    if target_module is None:
        raise RuntimeError(f"unittest.main: could not find module {module!r}")

    # Extract dict of names from the module
    module_dict = getattr(target_module, '__dict__', None)
    if module_dict is None:
        # Module objects that lack __dict__ - try a few common attrs
        module_dict = {}
        for attr_name in dir(target_module):
            module_dict[attr_name] = getattr(target_module, attr_name)

    classes = _collect_test_classes(module_dict)
    result = _TestRunResult()

    for name, cls in classes:
        _run_class(cls, name, verbosity, result)

    _summarize(result, verbosity)

    if exit:
        sys.exit(0 if result.wasSuccessful() else 1)

    return result


# Convenience aliases that mirror CPython's public API surface.
TestLoader = None  # Not implemented
TestSuite = None   # Not implemented
TextTestRunner = None  # Not implemented
TextTestResult = None  # Not implemented
defaultTestLoader = None
