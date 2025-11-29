# Test async method calls with keyword arguments
# This tests the fix for M28-8da4: AsyncFunction.CallWithKeywords

class AsyncProcessor:
    def __init__(self, base):
        self.base = base

    async def process(self, value, multiplier=2):
        return self.base + (value * multiplier)

print("Test: Async method with keyword arguments")

processor = AsyncProcessor(10)

# Test with default kwarg
coro1 = processor.process(5)  # 10 + (5 * 2) = 20
try:
    coro1.send(None)
except StopIteration as e:
    assert e.value == 20, f"Expected 20, got {e.value}"
print("  - Async method with default kwarg: PASSED")

# Test with explicit kwarg
coro2 = processor.process(5, multiplier=3)  # 10 + (5 * 3) = 25
try:
    coro2.send(None)
except StopIteration as e:
    assert e.value == 25, f"Expected 25, got {e.value}"
print("  - Async method with explicit kwarg: PASSED")

# Test with all kwargs
coro3 = processor.process(value=4, multiplier=5)  # 10 + (4 * 5) = 30
try:
    coro3.send(None)
except StopIteration as e:
    assert e.value == 30, f"Expected 30, got {e.value}"
print("  - Async method with all kwargs: PASSED")

print("\nAll async kwarg tests passed!")
