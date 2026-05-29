# M28 stub for the CPython `test` package.
#
# CPython ships a `test` package that contains both the regression tests
# themselves (test_*.py) and a substantial test-infrastructure layer
# (test.support, test.list_tests, test.seq_tests, etc). That infrastructure
# pulls in deep CPython-specific modules (inspect → dataclasses → typing
# Protocol metaclass machinery, plus C-extension introspection bits) that
# M28 doesn't currently load.
#
# This stub provides just enough of the `test` package surface that real
# CPython test files (run from tests/cpython-source/Lib/test/) can import
# and execute. Submodules (test.support, test.list_tests, etc) live next
# to this file and are loaded by M28's normal package import.
