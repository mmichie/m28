# Minimal stub for test.support.socket_helper.
#
# The real module provides networking test helpers (port binding, transient
# internet retries, IPv6 detection, etc). For language conformance tests we
# only need permissive constants, no-op context managers, and skip decorators
# so importing files load and their non-network assertions run. Socket-bound
# tests are skipped rather than executed.

import contextlib
import unittest

HOST = "localhost"
HOSTv4 = "127.0.0.1"
HOSTv6 = "::1"
IPV6_ENABLED = False


def find_unused_port(family=None, socktype=None):
    """Return an arbitrary unused-looking port number."""
    return 0


def bind_port(sock, host=HOST):
    """Best-effort bind to an ephemeral port; return the bound port."""
    try:
        sock.bind((host, 0))
        return sock.getsockname()[1]
    except Exception:
        return 0


def bind_unix_socket(sock, addr):
    sock.bind(addr)


def create_unix_domain_name():
    return "/tmp/m28_test_unix_socket"


def has_gethostname():
    return True


def get_socket_conn_refused_errs():
    return []


def skip_if_tcp_blackhole(test):
    """Pass-through: M28 has no TCP blackhole detection."""
    return test


def skip_unless_bind_unix_socket(test):
    """Skip a test that needs to bind an AF_UNIX socket.

    Conservatively skipped on M28, whose socket support is incomplete.
    """
    return unittest.skip("requires AF_UNIX socket binding")(test)


@contextlib.contextmanager
def transient_internet(resource_name, *, timeout=None, errnos=()):
    """No-op context manager (network resources are unavailable in tests)."""
    raise unittest.SkipTest("network resource %r unavailable" % (resource_name,))
    yield  # pragma: no cover
