# Test Python 3.10+ pattern matching (match/case)

# Test basic literal patterns
def test_literal_patterns():
    def check(x):
        match x:
            case 1:
                return "one"
            case 2:
                return "two"
            case 3:
                return "three"
            case _:
                return "other"

    assert check(1) == "one", "Expected 'one' for 1"
    assert check(2) == "two", "Expected 'two' for 2"
    assert check(3) == "three", "Expected 'three' for 3"
    assert check(42) == "other", "Expected 'other' for 42"

test_literal_patterns()

# Test string patterns
def test_string_patterns():
    def greet(cmd):
        match cmd:
            case "hello":
                return "Hi there!"
            case "bye":
                return "Goodbye!"
            case _:
                return "Unknown command"

    assert greet("hello") == "Hi there!", "Expected greeting for 'hello'"
    assert greet("bye") == "Goodbye!", "Expected farewell for 'bye'"
    assert greet("other") == "Unknown command", "Expected unknown for other"

test_string_patterns()

# Test boolean and None patterns
def test_bool_none_patterns():
    def describe(val):
        match val:
            case True:
                return "true value"
            case False:
                return "false value"
            case None:
                return "nothing"
            case _:
                return "something else"

    assert describe(True) == "true value", "Expected 'true value' for True"
    assert describe(False) == "false value", "Expected 'false value' for False"
    assert describe(None) == "nothing", "Expected 'nothing' for None"
    assert describe(42) == "something else", "Expected 'something else' for 42"

test_bool_none_patterns()

# Test variable binding patterns
def test_variable_patterns():
    def extract(x):
        match x:
            case n:
                return n * 2

    assert extract(5) == 10, "Expected 10 for extract(5)"
    assert extract(0) == 0, "Expected 0 for extract(0)"

test_variable_patterns()

# Test guard conditions
def test_guards():
    def classify(n):
        match n:
            case x if x < 0:
                return "negative"
            case x if x == 0:
                return "zero"
            case x if x < 10:
                return "small positive"
            case _:
                return "large positive"

    assert classify(-5) == "negative", "Expected 'negative' for -5"
    assert classify(0) == "zero", "Expected 'zero' for 0"
    assert classify(5) == "small positive", "Expected 'small positive' for 5"
    assert classify(100) == "large positive", "Expected 'large positive' for 100"

test_guards()

# Test wildcard patterns with guards
def test_wildcard_with_guard():
    def check(val):
        match val:
            case _ if val > 100:
                return "very large"
            case _:
                return "not so large"

    assert check(200) == "very large", "Expected 'very large' for 200"
    assert check(50) == "not so large", "Expected 'not so large' for 50"

test_wildcard_with_guard()

# Test no match returns None
def test_no_match():
    result = None
    x = 42
    match x:
        case 1:
            result = "one"
        case 2:
            result = "two"

    # When no case matches, match returns None (no assignment to result)
    assert result == None, "Expected None when no case matches"

test_no_match()

# Test match in function with return
def test_match_return():
    def process(cmd):
        match cmd:
            case "start":
                return "Starting..."
            case "stop":
                return "Stopping..."
            case action:
                return f"Unknown action: {action}"

    assert process("start") == "Starting...", "Expected 'Starting...' for 'start'"
    assert process("stop") == "Stopping...", "Expected 'Stopping...' for 'stop'"
    assert process("pause") == "Unknown action: pause", "Expected formatted message for 'pause'"

test_match_return()

print("All pattern matching tests passed!")
