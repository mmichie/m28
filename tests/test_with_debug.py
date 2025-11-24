# Simple debug test for with unpacking
class DualContext:
    def __enter__(self):
        return (1, 2)

    def __exit__(self, exc_type, exc_val, exc_tb):
        return False

with DualContext() as (a, b):
    print(f"a={a}, b={b}")
