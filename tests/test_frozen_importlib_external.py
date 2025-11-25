# Test for _frozen_importlib_external C extension stub module
# This verifies that the module can be imported and has expected attributes

# Import the module
import _frozen_importlib_external

# Test: module has __name__ attribute
assert hasattr(_frozen_importlib_external, '__name__'), "_frozen_importlib_external missing __name__"
assert _frozen_importlib_external.__name__ == '_frozen_importlib_external', "Wrong module name"
print("✓ Module __name__ attribute correct")

# Test: module has __doc__ attribute
assert hasattr(_frozen_importlib_external, '__doc__'), "_frozen_importlib_external missing __doc__"
print("✓ Module __doc__ attribute present")

# Test: _install() function exists and is callable
assert hasattr(_frozen_importlib_external, '_install'), "_frozen_importlib_external missing _install function"
result = _frozen_importlib_external._install()
assert result is None, "_install() should return None"
print("✓ _install() function works")

# Test: bootstrap loader stubs exist
assert hasattr(_frozen_importlib_external, 'SourceFileLoader'), "Missing SourceFileLoader"
assert hasattr(_frozen_importlib_external, 'ExtensionFileLoader'), "Missing ExtensionFileLoader"
assert hasattr(_frozen_importlib_external, 'ModuleSpec'), "Missing ModuleSpec"
assert hasattr(_frozen_importlib_external, 'FileFinder'), "Missing FileFinder"
print("✓ Bootstrap loader stubs present")

print("\nAll _frozen_importlib_external tests passed!")
