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
