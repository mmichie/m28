package modules

import (
	"fmt"
	"math"
	"strings"
	"sync"

	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
	"github.com/shopspring/decimal"
)

// DecimalContext represents a decimal context with precision and rounding settings
type DecimalContext struct {
	Precision int32
	Rounding  string
	Emin      int32
	Emax      int32
}

var (
	// Global context with thread-local storage
	contextMu      sync.RWMutex
	defaultContext = &DecimalContext{
		Precision: 28,
		Rounding:  "ROUND_HALF_EVEN",
		Emin:      -999999,
		Emax:      999999,
	}
	currentContext = defaultContext
)

// Rounding mode constants
const (
	RoundUp       = "ROUND_UP"
	RoundDown     = "ROUND_DOWN"
	RoundCeiling  = "ROUND_CEILING"
	RoundFloor    = "ROUND_FLOOR"
	RoundHalfUp   = "ROUND_HALF_UP"
	RoundHalfDown = "ROUND_HALF_DOWN"
	RoundHalfEven = "ROUND_HALF_EVEN"
	Round05Up     = "ROUND_05UP"
)

// RegisterDecimalModule creates and registers the decimal module
func RegisterDecimalModule(ctx *core.Context) {
	decimalModule := core.NewDict()

	// Decimal constructor
	decimalModule.Set("Decimal", core.NewBuiltinFunction(decimalConstructor))

	// Context management
	decimalModule.Set("getcontext", core.NewBuiltinFunction(getContext))
	decimalModule.Set("setcontext", core.NewBuiltinFunction(setContext))
	decimalModule.Set("Context", core.NewBuiltinFunction(newContextConstructor))

	// Rounding mode constants
	decimalModule.Set("ROUND_UP", core.StringValue(RoundUp))
	decimalModule.Set("ROUND_DOWN", core.StringValue(RoundDown))
	decimalModule.Set("ROUND_CEILING", core.StringValue(RoundCeiling))
	decimalModule.Set("ROUND_FLOOR", core.StringValue(RoundFloor))
	decimalModule.Set("ROUND_HALF_UP", core.StringValue(RoundHalfUp))
	decimalModule.Set("ROUND_HALF_DOWN", core.StringValue(RoundHalfDown))
	decimalModule.Set("ROUND_HALF_EVEN", core.StringValue(RoundHalfEven))
	decimalModule.Set("ROUND_05UP", core.StringValue(Round05Up))

	// Special values
	inf, _ := core.NewDecimalFromString("Infinity")
	negInf, _ := core.NewDecimalFromString("-Infinity")
	nan, _ := core.NewDecimalFromString("NaN")
	decimalModule.Set("Infinity", inf)
	decimalModule.Set("NegInfinity", negInf)
	decimalModule.Set("NaN", nan)

	// Register in global context
	ctx.Define("Decimal", core.NewBuiltinFunction(decimalConstructor))
}

// decimalConstructor implements the Decimal() constructor
func decimalConstructor(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("Decimal", args)
	if err := v.Range(0, 1); err != nil {
		return nil, err
	}

	// No arguments - return Decimal(0)
	if len(args) == 0 {
		return core.NewDecimalFromInt(0), nil
	}

	arg := args[0]

	// Handle different input types
	switch val := arg.(type) {
	case core.NumberValue:
		// Check if it's an integer
		if float64(val) == math.Floor(float64(val)) {
			return core.NewDecimalFromInt(int64(val)), nil
		}
		return core.NewDecimalFromFloat(float64(val)), nil

	case core.StringValue:
		d, err := core.NewDecimalFromString(string(val))
		if err != nil {
			return nil, fmt.Errorf("invalid literal for Decimal: %s", val)
		}
		return d, nil

	case *core.DecimalValue:
		// Already a decimal, return a copy
		return core.NewDecimal(val.GetDecimal()), nil

	case core.TupleValue:
		// Decimal from tuple: (sign, digits, exponent)
		// sign: 0 for positive, 1 for negative
		// digits: tuple of digits
		// exponent: int
		return decimalFromTuple(val)

	default:
		return nil, fmt.Errorf("cannot convert %s to Decimal", arg.Type())
	}
}

// decimalFromTuple creates a Decimal from a tuple (sign, digits, exponent)
func decimalFromTuple(tup core.TupleValue) (core.Value, error) {
	items := []core.Value(tup)
	if len(items) != 3 {
		return nil, fmt.Errorf("Decimal tuple must have 3 elements (sign, digits, exponent)")
	}

	// Extract sign
	signVal, ok := items[0].(core.NumberValue)
	if !ok {
		return nil, fmt.Errorf("sign must be a number")
	}
	sign := int(signVal)
	if sign != 0 && sign != 1 {
		return nil, fmt.Errorf("sign must be 0 or 1")
	}

	// Extract digits
	digitsTuple, ok := items[1].(core.TupleValue)
	if !ok {
		return nil, fmt.Errorf("digits must be a tuple")
	}

	var digitStr strings.Builder
	for _, d := range digitsTuple {
		digitNum, ok := d.(core.NumberValue)
		if !ok {
			return nil, fmt.Errorf("all digits must be numbers")
		}
		digit := int(digitNum)
		if digit < 0 || digit > 9 {
			return nil, fmt.Errorf("digits must be in range 0-9")
		}
		digitStr.WriteString(fmt.Sprintf("%d", digit))
	}

	// Extract exponent
	expVal, ok := items[2].(core.NumberValue)
	if !ok {
		return nil, fmt.Errorf("exponent must be a number")
	}
	exp := int(expVal)

	// Build the decimal string
	var decStr string
	digits := digitStr.String()
	if len(digits) == 0 {
		digits = "0"
	}

	if exp >= 0 {
		// Positive exponent: multiply by 10^exp
		decStr = digits + strings.Repeat("0", exp)
	} else {
		// Negative exponent: move decimal point
		absExp := -exp
		if absExp >= len(digits) {
			// Need leading zeros
			decStr = "0." + strings.Repeat("0", absExp-len(digits)) + digits
		} else {
			// Insert decimal point
			pos := len(digits) - absExp
			decStr = digits[:pos] + "." + digits[pos:]
		}
	}

	if sign == 1 {
		decStr = "-" + decStr
	}

	return core.NewDecimalFromString(decStr)
}

// getContext returns the current decimal context
func getContext(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("getcontext", args)
	if err := v.Exact(0); err != nil {
		return nil, err
	}

	contextMu.RLock()
	defer contextMu.RUnlock()

	// Return a context object
	contextDict := core.NewDict()
	contextDict.Set("precision", core.NumberValue(currentContext.Precision))
	contextDict.Set("rounding", core.StringValue(currentContext.Rounding))
	contextDict.Set("Emin", core.NumberValue(currentContext.Emin))
	contextDict.Set("Emax", core.NumberValue(currentContext.Emax))

	return contextDict, nil
}

// setContext sets the current decimal context
func setContext(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("setcontext", args)
	if err := v.Exact(1); err != nil {
		return nil, err
	}

	contextDict, ok := args[0].(*core.DictValue)
	if !ok {
		return nil, fmt.Errorf("setcontext() argument must be a Context")
	}

	contextMu.Lock()
	defer contextMu.Unlock()

	// Update precision if provided
	if precVal, exists := contextDict.Get("precision"); exists {
		if prec, ok := precVal.(core.NumberValue); ok {
			currentContext.Precision = int32(prec)
		}
	}

	// Update rounding if provided
	if roundVal, exists := contextDict.Get("rounding"); exists {
		if round, ok := roundVal.(core.StringValue); ok {
			currentContext.Rounding = string(round)
		}
	}

	// Update Emin if provided
	if eminVal, exists := contextDict.Get("Emin"); exists {
		if emin, ok := eminVal.(core.NumberValue); ok {
			currentContext.Emin = int32(emin)
		}
	}

	// Update Emax if provided
	if emaxVal, exists := contextDict.Get("Emax"); exists {
		if emax, ok := emaxVal.(core.NumberValue); ok {
			currentContext.Emax = int32(emax)
		}
	}

	return core.Nil, nil
}

// newContextConstructor creates a new Context object
func newContextConstructor(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("Context", args)
	if err := v.Range(0, 4); err != nil {
		return nil, err
	}

	contextDict := core.NewDict()

	// Set default values
	precision := int32(28)
	rounding := "ROUND_HALF_EVEN"
	emin := int32(-999999)
	emax := int32(999999)

	// Parse arguments (precision, rounding, Emin, Emax)
	if len(args) >= 1 {
		if p, ok := args[0].(core.NumberValue); ok {
			precision = int32(p)
		}
	}
	if len(args) >= 2 {
		if r, ok := args[1].(core.StringValue); ok {
			rounding = string(r)
		}
	}
	if len(args) >= 3 {
		if e, ok := args[2].(core.NumberValue); ok {
			emin = int32(e)
		}
	}
	if len(args) >= 4 {
		if e, ok := args[3].(core.NumberValue); ok {
			emax = int32(e)
		}
	}

	contextDict.Set("precision", core.NumberValue(precision))
	contextDict.Set("rounding", core.StringValue(rounding))
	contextDict.Set("Emin", core.NumberValue(emin))
	contextDict.Set("Emax", core.NumberValue(emax))

	return contextDict, nil
}

// Helper function to apply precision to a decimal
func applyPrecision(d decimal.Decimal, precision int32) decimal.Decimal {
	if precision <= 0 {
		return d
	}

	// Get the string representation
	str := d.String()

	// Count significant digits (excluding sign and decimal point)
	var sigDigits int
	for _, ch := range str {
		if ch >= '0' && ch <= '9' {
			sigDigits++
		}
	}

	if sigDigits <= int(precision) {
		return d
	}

	// Round to the specified precision
	// Calculate the number of decimal places needed
	exp := d.Exponent()
	places := int(precision) + int(exp)

	if places < 0 {
		places = 0
	}

	return d.Round(int32(places))
}

// GetCurrentContext returns the current decimal context (for use by other modules)
func GetCurrentContext() *DecimalContext {
	contextMu.RLock()
	defer contextMu.RUnlock()
	return currentContext
}
