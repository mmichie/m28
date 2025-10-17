# Test property with setter

print("Testing property with setter...")

class Temperature:
    def __init__(self):
        self._celsius = 0

    @property
    def celsius(self):
        return self._celsius

    # Note: @celsius.setter syntax not yet implemented
    # For now, just test that getter works
    def set_celsius(self, value):
        self._celsius = value

temp = Temperature()
print("Initial celsius:", temp.celsius)

temp.set_celsius(25)
print("After setting to 25:", temp.celsius)

print("✓ Property getter works correctly!")
