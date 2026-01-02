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

# Test OR patterns (pattern1 | pattern2)
def test_or_patterns():
    def categorize(x):
        match x:
            case 1 | 2 | 3:
                return "small"
            case 4 | 5:
                return "medium"
            case _:
                return "other"

    assert categorize(1) == "small", "Expected 'small' for 1"
    assert categorize(2) == "small", "Expected 'small' for 2"
    assert categorize(3) == "small", "Expected 'small' for 3"
    assert categorize(4) == "medium", "Expected 'medium' for 4"
    assert categorize(5) == "medium", "Expected 'medium' for 5"
    assert categorize(10) == "other", "Expected 'other' for 10"

test_or_patterns()

# Test OR patterns with strings
def test_or_string_patterns():
    def day_type(day):
        match day:
            case "sat" | "sun":
                return "weekend"
            case "mon" | "tue" | "wed" | "thu" | "fri":
                return "weekday"
            case _:
                return "unknown"

    assert day_type("sat") == "weekend", "Expected 'weekend' for 'sat'"
    assert day_type("sun") == "weekend", "Expected 'weekend' for 'sun'"
    assert day_type("mon") == "weekday", "Expected 'weekday' for 'mon'"
    assert day_type("fri") == "weekday", "Expected 'weekday' for 'fri'"
    assert day_type("holiday") == "unknown", "Expected 'unknown' for 'holiday'"

test_or_string_patterns()

# Test AS patterns (pattern as name)
def test_as_patterns():
    def describe(x):
        match x:
            case [a, b] as pair:
                return f"pair {pair}"
            case n as val:
                return f"value {val}"

    assert describe([1, 2]) == "pair [1, 2]", "Expected pair pattern"
    assert describe(42) == "value 42", "Expected value pattern"

test_as_patterns()

# Test AS with OR patterns
def test_as_or_patterns():
    def categorize(x):
        match x:
            case 1 | 2 | 3 as num:
                return f"small: {num}"
            case _:
                return "other"

    assert categorize(1) == "small: 1", "Expected 'small: 1'"
    assert categorize(2) == "small: 2", "Expected 'small: 2'"
    assert categorize(10) == "other", "Expected 'other'"

test_as_or_patterns()

# Test grouped OR patterns with parentheses
def test_grouped_or_patterns():
    def check(x):
        match x:
            case (1 | 2) as num:
                return f"one or two: {num}"
            case _:
                return "other"

    assert check(1) == "one or two: 1", "Expected grouped pattern match"
    assert check(2) == "one or two: 2", "Expected grouped pattern match"
    assert check(3) == "other", "Expected 'other'"

test_grouped_or_patterns()

# Test star patterns (*rest)
def test_star_patterns():
    def extract(x):
        match x:
            case [first, *rest]:
                return (first, rest)
            case _:
                return None

    assert extract([1, 2, 3, 4]) == (1, [2, 3, 4]), "Expected (1, [2, 3, 4])"
    assert extract([1]) == (1, []), "Expected (1, [])"
    assert extract([]) == None, "Expected None for empty list"

test_star_patterns()

# Test star at different positions
def test_star_positions():
    def head_tail(x):
        match x:
            case [*init, last]:
                return (init, last)
            case _:
                return None

    assert head_tail([1, 2, 3]) == ([1, 2], 3), "Expected ([1, 2], 3)"
    assert head_tail([1]) == ([], 1), "Expected ([], 1)"

    def first_rest_last(x):
        match x:
            case [first, *middle, last]:
                return (first, middle, last)
            case _:
                return None

    assert first_rest_last([1, 2, 3, 4]) == (1, [2, 3], 4), "Expected (1, [2, 3], 4)"
    assert first_rest_last([1, 2]) == (1, [], 2), "Expected (1, [], 2)"
    assert first_rest_last([1]) == None, "Expected None for single element"

test_star_positions()

# Test dict/mapping patterns
def test_dict_patterns():
    def get_name(x):
        match x:
            case {'name': name}:
                return name
            case _:
                return None

    assert get_name({'name': 'Alice'}) == 'Alice', "Expected 'Alice'"
    assert get_name({'age': 30}) == None, "Expected None"

test_dict_patterns()

# Test multiple key dict patterns
def test_dict_multi_keys():
    def describe(x):
        match x:
            case {'name': name, 'age': age}:
                return f"{name} is {age}"
            case _:
                return None

    assert describe({'name': 'Bob', 'age': 25}) == "Bob is 25", "Expected 'Bob is 25'"
    assert describe({'name': 'Bob'}) == None, "Expected None (missing key)"

test_dict_multi_keys()

# Test nested dict patterns
def test_nested_dict():
    def get_nested(x):
        match x:
            case {'user': {'name': name}}:
                return name
            case _:
                return None

    assert get_nested({'user': {'name': 'Charlie'}}) == 'Charlie', "Expected 'Charlie'"

test_nested_dict()

print("All pattern matching tests passed!")
