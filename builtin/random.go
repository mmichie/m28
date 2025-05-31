package builtin

import (
	"fmt"
	"math/rand"
	"time"

	"github.com/mmichie/m28/core"
)

// Random number generator instance
var rng = rand.New(rand.NewSource(time.Now().UnixNano()))

// RegisterRandomModule creates and registers the random module
func RegisterRandomModule() {
	randomModule := core.NewDict()

	// random - random float in [0.0, 1.0)
	randomModule.Set("random", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 0 {
			return nil, fmt.Errorf("random() takes no arguments (%d given)", len(args))
		}
		return core.NumberValue(rng.Float64()), nil
	}))

	// randint - random integer in [a, b] inclusive
	randomModule.Set("randint", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, fmt.Errorf("randint() takes exactly 2 arguments (%d given)", len(args))
		}

		a, ok1 := args[0].(core.NumberValue)
		b, ok2 := args[1].(core.NumberValue)
		if !ok1 || !ok2 {
			return nil, fmt.Errorf("randint() arguments must be integers")
		}

		// Convert to int
		aInt := int(a)
		bInt := int(b)

		if aInt > bInt {
			return nil, fmt.Errorf("empty range for randint() (%d, %d)", aInt, bInt)
		}

		// Generate random int in [aInt, bInt] inclusive
		result := aInt + rng.Intn(bInt-aInt+1)
		return core.NumberValue(result), nil
	}))

	// uniform - random float in [a, b)
	randomModule.Set("uniform", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, fmt.Errorf("uniform() takes exactly 2 arguments (%d given)", len(args))
		}

		a, ok1 := args[0].(core.NumberValue)
		b, ok2 := args[1].(core.NumberValue)
		if !ok1 || !ok2 {
			return nil, fmt.Errorf("uniform() arguments must be numbers")
		}

		aFloat := float64(a)
		bFloat := float64(b)

		// Generate random float in [a, b)
		result := aFloat + rng.Float64()*(bFloat-aFloat)
		return core.NumberValue(result), nil
	}))

	// choice - choose random element from sequence
	randomModule.Set("choice", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("choice() takes exactly 1 argument (%d given)", len(args))
		}

		switch v := args[0].(type) {
		case core.ListValue:
			if len(v) == 0 {
				return nil, fmt.Errorf("choice() cannot choose from an empty sequence")
			}
			idx := rng.Intn(len(v))
			return v[idx], nil
		case core.TupleValue:
			if len(v) == 0 {
				return nil, fmt.Errorf("choice() cannot choose from an empty sequence")
			}
			idx := rng.Intn(len(v))
			return v[idx], nil
		case core.StringValue:
			str := string(v)
			if len(str) == 0 {
				return nil, fmt.Errorf("choice() cannot choose from an empty sequence")
			}
			idx := rng.Intn(len(str))
			// Return single character as string
			return core.StringValue(string(str[idx])), nil
		default:
			return nil, fmt.Errorf("choice() argument must be a non-empty sequence, not '%s'", v.Type())
		}
	}))

	// shuffle - shuffle list in place
	randomModule.Set("shuffle", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("shuffle() takes exactly 1 argument (%d given)", len(args))
		}

		list, ok := args[0].(core.ListValue)
		if !ok {
			return nil, fmt.Errorf("shuffle() argument must be a list, not '%s'", args[0].Type())
		}

		// Shuffle the list in place using Fisher-Yates
		for i := len(list) - 1; i > 0; i-- {
			j := rng.Intn(i + 1)
			list[i], list[j] = list[j], list[i]
		}

		// Python's shuffle returns None
		return core.Nil, nil
	}))

	// seed - seed the random number generator
	randomModule.Set("seed", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) > 1 {
			return nil, fmt.Errorf("seed() takes at most 1 argument (%d given)", len(args))
		}

		if len(args) == 0 {
			// Use current time
			rng = rand.New(rand.NewSource(time.Now().UnixNano()))
		} else {
			// Use provided seed
			seed, ok := args[0].(core.NumberValue)
			if !ok {
				return nil, fmt.Errorf("seed() argument must be a number, not '%s'", args[0].Type())
			}
			rng = rand.New(rand.NewSource(int64(seed)))
		}

		return core.Nil, nil
	}))

	// sample - choose k unique elements from population
	randomModule.Set("sample", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, fmt.Errorf("sample() takes exactly 2 arguments (%d given)", len(args))
		}

		// Get the population
		var population []core.Value
		switch v := args[0].(type) {
		case core.ListValue:
			population = v
		case core.TupleValue:
			population = v
		case core.StringValue:
			// Convert string to list of characters
			str := string(v)
			population = make([]core.Value, len(str))
			for i, ch := range str {
				population[i] = core.StringValue(string(ch))
			}
		default:
			return nil, fmt.Errorf("sample() population must be a sequence, not '%s'", v.Type())
		}

		// Get k
		kVal, ok := args[1].(core.NumberValue)
		if !ok {
			return nil, fmt.Errorf("sample() k must be an integer, not '%s'", args[1].Type())
		}
		k := int(kVal)

		if k < 0 {
			return nil, fmt.Errorf("sample() k must be non-negative")
		}
		if k > len(population) {
			return nil, fmt.Errorf("sample() sample larger than population")
		}

		// Create a copy of the population
		populationCopy := make([]core.Value, len(population))
		copy(populationCopy, population)

		// Shuffle the first k elements
		for i := 0; i < k; i++ {
			j := i + rng.Intn(len(populationCopy)-i)
			populationCopy[i], populationCopy[j] = populationCopy[j], populationCopy[i]
		}

		// Return the first k elements
		result := make(core.ListValue, k)
		copy(result, populationCopy[:k])
		return result, nil
	}))

	// Register in the global module registry
	registry := core.GetModuleRegistry()
	registry.StoreModule("random", randomModule, "<builtin>", []string{})
}

// RegisterRandomFunctions registers random-related builtin functions and module
func RegisterRandomFunctions(ctx *core.Context) {
	// Register the random module
	RegisterRandomModule()
}