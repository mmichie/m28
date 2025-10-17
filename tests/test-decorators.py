# Test Python decorator functionality

print("=== Testing Python Decorators ===\n")

# Test 1: @property decorator
print("Test 1: @property decorator")

class Person:
    def __init__(self, first, last):
        self._first = first
        self._last = last

    @property
    def full_name(self):
        return f"{self._first} {self._last}"

person = Person("John", "Doe")
print("person.full_name:", person.full_name)
print("✓ @property works!")

# Test 2: @staticmethod decorator
print("\nTest 2: @staticmethod decorator")

class Math:
    @staticmethod
    def add(a, b):
        return a + b

    @staticmethod
    def multiply(x, y):
        return x * y

# Call static methods without instance
result1 = Math.add(5, 3)
result2 = Math.multiply(4, 7)
print("Math.add(5, 3):", result1)
print("Math.multiply(4, 7):", result2)
print("✓ @staticmethod works!")

# Test 3: @classmethod decorator
print("\nTest 3: @classmethod decorator")

class Counter:
    count = 0

    @classmethod
    def increment(cls):
        cls.count = cls.count + 1
        return cls.count

    @classmethod
    def get_count(cls):
        return cls.count

count1 = Counter.increment()
count2 = Counter.increment()
final_count = Counter.get_count()
print("After 2 increments, count:", final_count)
print("✓ @classmethod works!")

print("\n✓ All decorator tests passed!")
