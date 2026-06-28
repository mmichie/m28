# Subsystem: attribute access and method dispatch (GetAttr + bound methods).
class Point:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def norm2(self):
        return self.x * self.x + self.y * self.y

total = 0
for i in range(6000):
    p = Point(i, i + 1)
    total = total + p.norm2()
print(total)
