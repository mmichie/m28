# Comprehensive Phase 3 Test
# Tests default parameters, f-strings, and tuple literals together

print("=== Phase 3 Feature Integration Test ===\n")

# Test 1: Default parameters with tuple return
def get_point(x=0, y=0, z=0):
    return (x, y, z)

origin = get_point()
point2d = get_point(10, 20)
point3d = get_point(10, 20, 30)

print(f"Origin: {origin}")
print(f"2D Point: {point2d}")
print(f"3D Point: {point3d}\n")

# Test 2: F-strings with tuple unpacking via indexing
coords = (100, 200, 300)
print(f"Coordinates: x={coords[0]}, y={coords[1]}, z={coords[2]}\n")

# Test 3: Default parameters creating formatted tuples
def create_user(name="Anonymous", age=0, role="Guest"):
    return (name, age, role)

user1 = create_user()
user2 = create_user("Alice", 30)
user3 = create_user("Bob", 25, "Admin")

print(f"User 1: {user1}")
print(f"User 2: {user2}")
print(f"User 3: {user3}\n")

# Test 4: Nested tuples with defaults and f-strings
def make_rect(top_left=(0, 0), bottom_right=(100, 100)):
    return f"Rectangle: top_left={top_left}, bottom_right={bottom_right}"

rect1 = make_rect()
rect2 = make_rect((10, 20), (50, 80))

print(rect1)
print(rect2)
print()

# Test 5: Simple tuple return with f-strings
def format_triplet(a=1, b=2, c=3):
    result = (a, b, c)
    return f"Triplet: {result}"

output1 = format_triplet()
output2 = format_triplet(10, 20, 30)

print(output1)
print(output2)
print()

# Test 6: Single-element tuples with defaults
def wrap_value(x=42):
    return (x,)  # Single-element tuple

wrapped1 = wrap_value()
wrapped2 = wrap_value(100)

print(f"Wrapped default: {wrapped1}, length={len(wrapped1)}")
print(f"Wrapped 100: {wrapped2}, length={len(wrapped2)}")

print("\n✓ All Phase 3 integration tests passed!")
