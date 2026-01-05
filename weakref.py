# Stub implementation of weakref module for M28
# Pure Python weakref.py from CPython uses actual weak references which M28 doesn't support yet
# This provides a simplified WeakKeyDictionary that acts as a regular dict
# until M28 implements proper weak reference semantics

# Simple ref function that returns the object itself (no actual weak reference)
def ref(obj, callback=None):
    """Create a weak reference to obj. Since M28 doesn't have weak references yet,
    this just returns the object itself."""
    return obj

# Simple proxy function that returns the object itself
def proxy(obj, callback=None):
    """Create a weak proxy to obj. Since M28 doesn't have weak references yet,
    this just returns the object itself."""
    return obj

class WeakKeyDictionary:
    """Simplified WeakKeyDictionary that behaves like a regular dict.

    M28 doesn't support weak references yet, so this is a plain dict wrapper.
    This is sufficient for unittest.signals which just needs basic dict operations.
    """

    def __init__(self, d=None):
        self.data = {}
        if d is not None:
            self.update(d)

    def __getitem__(self, key):
        return self.data[key]

    def __setitem__(self, key, value):
        self.data[key] = value

    def __delitem__(self, key):
        del self.data[key]

    def __contains__(self, key):
        return key in self.data

    def __len__(self):
        return len(self.data)

    def __iter__(self):
        return iter(self.data)

    def get(self, key, default=None):
        return self.data.get(key, default)

    def setdefault(self, key, default=None):
        return self.data.setdefault(key, default)

    def pop(self, key, *args):
        return self.data.pop(key, *args)

    def popitem(self):
        return self.data.popitem()

    def clear(self):
        self.data.clear()

    def update(self, other):
        if hasattr(other, 'items'):
            for key, value in other.items():
                self.data[key] = value
        else:
            for key, value in other:
                self.data[key] = value

    def keys(self):
        return self.data.keys()

    def values(self):
        return self.data.values()

    def items(self):
        return self.data.items()

    def __repr__(self):
        return f"<WeakKeyDictionary at {id(self)}>"


class WeakValueDictionary:
    """Simplified WeakValueDictionary that behaves like a regular dict.

    Similar to WeakKeyDictionary but for weak values.
    """

    def __init__(self, d=None):
        self.data = {}
        if d is not None:
            self.update(d)

    def __getitem__(self, key):
        return self.data[key]

    def __setitem__(self, key, value):
        self.data[key] = value

    def __delitem__(self, key):
        del self.data[key]

    def __contains__(self, key):
        return key in self.data

    def __len__(self):
        return len(self.data)

    def __iter__(self):
        return iter(self.data)

    def get(self, key, default=None):
        return self.data.get(key, default)

    def setdefault(self, key, default=None):
        return self.data.setdefault(key, default)

    def pop(self, key, *args):
        return self.data.pop(key, *args)

    def popitem(self):
        return self.data.popitem()

    def clear(self):
        self.data.clear()

    def update(self, other):
        if hasattr(other, 'items'):
            for key, value in other.items():
                self.data[key] = value
        else:
            for key, value in other:
                self.data[key] = value

    def keys(self):
        return self.data.keys()

    def values(self):
        return self.data.values()

    def items(self):
        return self.data.items()

    def __repr__(self):
        return f"<WeakValueDictionary at {id(self)}>"


class finalize:
    """Stub implementation of weakref.finalize for M28.

    This provides a simplified finalize that works for TemporaryDirectory
    and similar use cases. Since M28 doesn't have garbage collection hooks,
    the callback won't be automatically invoked on object destruction -
    it must be called explicitly via __call__() or will be skipped via detach().

    For context managers like TemporaryDirectory, this is fine because cleanup
    is handled by __exit__ -> cleanup() -> detach().
    """

    # Class-level registry to track all finalizers
    _registry = {}
    _index_counter = 0

    class _Info:
        """Internal class to store finalizer info."""
        def __init__(self, obj, func, args, kwargs, atexit, index):
            self.obj = obj  # Strong reference since we don't have weak refs
            self.func = func
            self.args = args
            self.kwargs = kwargs
            self.atexit = atexit
            self.index = index

    def __init__(self, obj, func, *args, **kwargs):
        """Register a finalizer for obj that will call func(*args, **kwargs)."""
        finalize._index_counter += 1
        info = finalize._Info(
            obj=obj,
            func=func,
            args=args,
            kwargs=kwargs,
            atexit=True,
            index=finalize._index_counter
        )
        finalize._registry[id(self)] = info
        self._id = id(self)

    def __call__(self, _=None):
        """If alive then mark as dead and return func(*args, **kwargs);
        otherwise return None."""
        info = finalize._registry.pop(self._id, None)
        if info is not None:
            return info.func(*info.args, **(info.kwargs or {}))
        return None

    def detach(self):
        """If alive then mark as dead and return (obj, func, args, kwargs);
        otherwise return None."""
        info = finalize._registry.get(self._id)
        if info is not None and finalize._registry.pop(self._id, None):
            return (info.obj, info.func, info.args, info.kwargs or {})
        return None

    def peek(self):
        """If alive then return (obj, func, args, kwargs);
        otherwise return None."""
        info = finalize._registry.get(self._id)
        if info is not None:
            return (info.obj, info.func, info.args, info.kwargs or {})
        return None

    @property
    def alive(self):
        """Whether finalizer is alive."""
        return self._id in finalize._registry

    @property
    def atexit(self):
        """Whether finalizer should be called at exit."""
        info = finalize._registry.get(self._id)
        return info is not None and info.atexit

    @atexit.setter
    def atexit(self, value):
        """Set whether finalizer should be called at exit."""
        info = finalize._registry.get(self._id)
        if info is not None:
            info.atexit = bool(value)

    def __repr__(self):
        info = finalize._registry.get(self._id)
        if info is None:
            return f'<finalize object at {hex(id(self))}; dead>'
        else:
            return f'<finalize object at {hex(id(self))}; for {type(info.obj).__name__} at {hex(id(info.obj))}>'
