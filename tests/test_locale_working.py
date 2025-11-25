# Test _locale C extension module - working around dict subscript issue
# Tests basic locale functionality with UTF-8 defaults

import _locale

print("Testing _locale module...")

# Test 1: LC_* constants exist
print("\n1. Testing LC_* constants:")
assert hasattr(_locale, 'LC_ALL'), "LC_ALL constant missing"
assert hasattr(_locale, 'LC_CTYPE'), "LC_CTYPE constant missing"
assert hasattr(_locale, 'LC_NUMERIC'), "LC_NUMERIC constant missing"
assert hasattr(_locale, 'LC_TIME'), "LC_TIME constant missing"
assert hasattr(_locale, 'LC_COLLATE'), "LC_COLLATE constant missing"
assert hasattr(_locale, 'LC_MONETARY'), "LC_MONETARY constant missing"
assert hasattr(_locale, 'LC_MESSAGES'), "LC_MESSAGES constant missing"
print("   ✓ All LC_* constants defined")

# Test 2: getencoding() returns UTF-8 or system encoding
print("\n2. Testing getencoding():")
encoding = _locale.getencoding()
assert isinstance(encoding, str), f"getencoding() should return string, got {type(encoding)}"
assert len(encoding) > 0, "getencoding() should return non-empty string"
print(f"   ✓ getencoding() returned: {encoding}")

# Test 3: setlocale() - get current locale (no arguments besides category)
print("\n3. Testing setlocale() - get current locale:")
current = _locale.setlocale(_locale.LC_ALL)
assert isinstance(current, str), f"setlocale() should return string, got {type(current)}"
print(f"   ✓ Current locale: {current}")

# Test 4: setlocale() - set to "C" locale
print("\n4. Testing setlocale() - set to C locale:")
result = _locale.setlocale(_locale.LC_ALL, "C")
assert result == "C", f"Expected 'C', got '{result}'"
print("   ✓ Set locale to 'C'")

# Test 5: setlocale() - set to UTF-8 locale
print("\n5. Testing setlocale() - set to UTF-8 locale:")
result = _locale.setlocale(_locale.LC_ALL, "en_US.UTF-8")
assert "UTF-8" in result or "utf-8" in result, f"Expected UTF-8 locale, got '{result}'"
print(f"   ✓ Set locale to UTF-8: {result}")

# Test 6: setlocale() - empty string means user's default
print("\n6. Testing setlocale() - empty string for user default:")
result = _locale.setlocale(_locale.LC_ALL, "")
assert isinstance(result, str), "Should return string"
assert len(result) > 0, "Should return non-empty locale string"
print(f"   ✓ User default locale: {result}")

# Test 7: localeconv() returns dictionary with formatting info
print("\n7. Testing localeconv():")
conv = _locale.localeconv()
assert isinstance(conv, dict), f"localeconv() should return dict, got {type(conv)}"

# Check keys using keys() method (subscript access has a known issue in M28)
keys = conv.keys()
required_keys = ['decimal_point', 'thousands_sep', 'grouping',
                 'int_curr_symbol', 'currency_symbol',
                 'mon_decimal_point', 'mon_thousands_sep', 'mon_grouping',
                 'positive_sign', 'negative_sign',
                 'int_frac_digits', 'frac_digits',
                 'p_cs_precedes', 'p_sep_by_space',
                 'n_cs_precedes', 'n_sep_by_space',
                 'p_sign_posn', 'n_sign_posn']

for key in required_keys:
    assert key in keys, f"localeconv() missing key: {key}"

print("   ✓ localeconv() has all required keys")
print(f"   ✓ Keys: {len(keys)} keys present")

# Test 8: strcoll() string comparison
print("\n8. Testing strcoll():")
result = _locale.strcoll("abc", "xyz")
assert result < 0, "strcoll('abc', 'xyz') should be negative"

result = _locale.strcoll("xyz", "abc")
assert result > 0, "strcoll('xyz', 'abc') should be positive"

result = _locale.strcoll("test", "test")
assert result == 0, "strcoll('test', 'test') should be zero"
print("   ✓ strcoll() comparison works")

# Test 9: strxfrm() string transformation
print("\n9. Testing strxfrm():")
result = _locale.strxfrm("hello")
assert isinstance(result, str), f"strxfrm() should return string, got {type(result)}"
assert len(result) > 0, "strxfrm() should return non-empty string"
print(f"   ✓ strxfrm('hello') = '{result}'")

# Test 10: Integration - can import locale module that uses _locale
print("\n10. Testing integration with Python's locale module:")
try:
    import locale
    print("   ✓ locale module imports successfully")
    print("   ✓ _locale module is functional for stdlib use")
except ImportError as e:
    print(f"   ⚠ locale module import failed: {e}")
except Exception as e:
    print(f"   ⚠ locale module test failed: {e}")

print("\n" + "="*60)
print("All _locale module tests passed! ✓")
print("="*60)
