package modules

import (
	"fmt"
	"math/rand"
	"time"

	"github.com/mmichie/m28/core"
)

// InitRandomModule creates the _random C extension module
func InitRandomModule() *core.DictValue {
	randomModule := core.NewDict()

	// Create the Random class - this is the base class for Python's random.Random
	randomClass := core.NewClass("Random", nil)

	// Store the RNG in the instance's __dict__
	randomClass.SetMethod("__init__", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("__init__ requires self argument")
		}
		self := args[0]

		// Initialize the random state with time-based seed
		rng := rand.New(rand.NewSource(time.Now().UnixNano()))

		// Store the RNG in the instance
		if obj, ok := self.(core.Object); ok {
			// Create a wrapper for the RNG
			rngWrapper := &rngWrapper{rng: rng}
			obj.SetAttr("_rng", rngWrapper)
		}

		return core.NilValue{}, nil
	}))

	// random() - return random float in [0.0, 1.0)
	randomClass.SetMethod("random", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("random() requires self argument")
		}
		rng := getRNG(args[0])
		if rng == nil {
			return nil, fmt.Errorf("random state not initialized")
		}
		return core.NumberValue(rng.Float64()), nil
	}))

	// seed(a=None) - seed the random number generator
	randomClass.SetMethod("seed", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("seed() requires self argument")
		}
		self := args[0]

		var seed int64
		if len(args) < 2 || args[1] == core.Nil {
			seed = time.Now().UnixNano()
		} else if num, ok := args[1].(core.NumberValue); ok {
			seed = int64(num)
		} else {
			// Use hash of the value
			seed = int64(len(fmt.Sprintf("%v", args[1])))
		}

		rng := rand.New(rand.NewSource(seed))
		if obj, ok := self.(core.Object); ok {
			obj.SetAttr("_rng", &rngWrapper{rng: rng})
		}

		return core.NilValue{}, nil
	}))

	// getrandbits(k) - generate k random bits
	randomClass.SetMethod("getrandbits", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 2 {
			return nil, fmt.Errorf("getrandbits() requires self and k arguments")
		}
		rng := getRNG(args[0])
		if rng == nil {
			return nil, fmt.Errorf("random state not initialized")
		}

		k, ok := args[1].(core.NumberValue)
		if !ok {
			return nil, fmt.Errorf("getrandbits() argument must be an integer")
		}
		bits := int(k)
		if bits < 0 {
			return nil, fmt.Errorf("number of bits must be non-negative")
		}
		if bits == 0 {
			return core.NumberValue(0), nil
		}
		if bits <= 63 {
			max := int64(1) << bits
			return core.NumberValue(rng.Int63n(max)), nil
		}
		return core.NumberValue(rng.Int63()), nil
	}))

	// getstate() - get internal state
	randomClass.SetMethod("getstate", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Return a dummy state tuple - Go's rand doesn't expose state easily
		return core.TupleValue{core.NumberValue(3), core.NilValue{}, core.NilValue{}}, nil
	}))

	// setstate(state) - restore internal state
	randomClass.SetMethod("setstate", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Ignore state restoration for simplified implementation
		return core.NilValue{}, nil
	}))

	randomModule.Set("Random", randomClass)

	return randomModule
}

// rngWrapper wraps a Go RNG for storage in Python objects
type rngWrapper struct {
	rng *rand.Rand
}

func (r *rngWrapper) Type() core.Type {
	return "_random_state"
}

func (r *rngWrapper) String() string {
	return "<random state>"
}

// getRNG extracts the RNG from a Random instance
func getRNG(self core.Value) *rand.Rand {
	if obj, ok := self.(core.Object); ok {
		if rngVal, found := obj.GetAttr("_rng"); found {
			if wrapper, ok := rngVal.(*rngWrapper); ok {
				return wrapper.rng
			}
		}
	}
	// Fallback: create a new RNG
	return rand.New(rand.NewSource(time.Now().UnixNano()))
}
