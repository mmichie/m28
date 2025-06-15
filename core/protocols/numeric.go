package protocols

import (
	"fmt"
	"math"

	"github.com/mmichie/m28/common/errors"
	"github.com/mmichie/m28/core"
)

// Ensure NumericOps implements Numeric protocol
var _ Numeric = &NumericOps{}

// NumericOps provides Numeric protocol implementation for NumberValue
// We use extension methods pattern since we can't modify core.NumberValue directly
type NumericOps struct {
	value float64
}

// NewNumericOps creates a NumericOps wrapper for a number
func NewNumericOps(n core.NumberValue) *NumericOps {
	return &NumericOps{value: float64(n)}
}

// Add implements Numeric.Add
func (n *NumericOps) Add(other core.Value) (core.Value, error) {
	switch v := other.(type) {
	case core.NumberValue:
		return core.NumberValue(n.value + float64(v)), nil
	default:
		// Try to get __radd__ from other
		if obj, ok := other.(core.Object); ok {
			if method, exists := obj.GetAttr("__radd__"); exists {
				// Call other.__radd__(self)
				if callable, ok := method.(interface {
					Call([]core.Value, *core.Context) (core.Value, error)
				}); ok {
					return callable.Call([]core.Value{core.NumberValue(n.value)}, nil)
				}
			}
		}
		return nil, errors.NewTypeError("+", "unsupported operand type(s)",
			fmt.Sprintf("'float' and '%s'", other.Type()))
	}
}

// Subtract implements Numeric.Subtract
func (n *NumericOps) Subtract(other core.Value) (core.Value, error) {
	switch v := other.(type) {
	case core.NumberValue:
		return core.NumberValue(n.value - float64(v)), nil
	default:
		// Try to get __rsub__ from other
		if obj, ok := other.(core.Object); ok {
			if method, exists := obj.GetAttr("__rsub__"); exists {
				if callable, ok := method.(interface {
					Call([]core.Value, *core.Context) (core.Value, error)
				}); ok {
					return callable.Call([]core.Value{core.NumberValue(n.value)}, nil)
				}
			}
		}
		return nil, errors.NewTypeError("-", "unsupported operand type(s)",
			fmt.Sprintf("'float' and '%s'", other.Type()))
	}
}

// Multiply implements Numeric.Multiply
func (n *NumericOps) Multiply(other core.Value) (core.Value, error) {
	switch v := other.(type) {
	case core.NumberValue:
		return core.NumberValue(n.value * float64(v)), nil
	case core.StringValue:
		// Support string repetition: 3 * "ab" = "ababab"
		if n.value == float64(int(n.value)) && n.value >= 0 {
			count := int(n.value)
			result := ""
			str := string(v)
			for i := 0; i < count; i++ {
				result += str
			}
			return core.StringValue(result), nil
		}
		return nil, errors.NewTypeError("*", "can't multiply sequence by non-int of type 'float'", "")
	case core.ListValue:
		// Support list repetition: 3 * [1,2] = [1,2,1,2,1,2]
		if n.value == float64(int(n.value)) && n.value >= 0 {
			count := int(n.value)
			result := make(core.ListValue, 0, len(v)*count)
			for i := 0; i < count; i++ {
				result = append(result, v...)
			}
			return result, nil
		}
		return nil, errors.NewTypeError("*", "can't multiply sequence by non-int of type 'float'", "")
	default:
		// Try to get __rmul__ from other
		if obj, ok := other.(core.Object); ok {
			if method, exists := obj.GetAttr("__rmul__"); exists {
				if callable, ok := method.(interface {
					Call([]core.Value, *core.Context) (core.Value, error)
				}); ok {
					return callable.Call([]core.Value{core.NumberValue(n.value)}, nil)
				}
			}
		}
		return nil, errors.NewTypeError("*", "unsupported operand type(s)",
			fmt.Sprintf("'float' and '%s'", other.Type()))
	}
}

// Divide implements Numeric.Divide
func (n *NumericOps) Divide(other core.Value) (core.Value, error) {
	switch v := other.(type) {
	case core.NumberValue:
		if float64(v) == 0 {
			return nil, errors.NewRuntimeError("division by zero", "")
		}
		return core.NumberValue(n.value / float64(v)), nil
	default:
		// Try to get __rdiv__ from other
		if obj, ok := other.(core.Object); ok {
			if method, exists := obj.GetAttr("__rdiv__"); exists {
				if callable, ok := method.(interface {
					Call([]core.Value, *core.Context) (core.Value, error)
				}); ok {
					return callable.Call([]core.Value{core.NumberValue(n.value)}, nil)
				}
			}
		}
		return nil, errors.NewTypeError("/", "unsupported operand type(s)",
			fmt.Sprintf("'float' and '%s'", other.Type()))
	}
}

// Modulo implements Numeric.Modulo
func (n *NumericOps) Modulo(other core.Value) (core.Value, error) {
	switch v := other.(type) {
	case core.NumberValue:
		if float64(v) == 0 {
			return nil, errors.NewRuntimeError("modulo by zero", "")
		}
		return core.NumberValue(math.Mod(n.value, float64(v))), nil
	default:
		return nil, errors.NewTypeError("%", "unsupported operand type(s)",
			fmt.Sprintf("'float' and '%s'", other.Type()))
	}
}

// Power implements Numeric.Power
func (n *NumericOps) Power(other core.Value) (core.Value, error) {
	switch v := other.(type) {
	case core.NumberValue:
		return core.NumberValue(math.Pow(n.value, float64(v))), nil
	default:
		return nil, errors.NewTypeError("**", "unsupported operand type(s)",
			fmt.Sprintf("'float' and '%s'", other.Type()))
	}
}

// Negate implements Numeric.Negate
func (n *NumericOps) Negate() (core.Value, error) {
	return core.NumberValue(-n.value), nil
}

// Absolute implements Numeric.Absolute
func (n *NumericOps) Absolute() (core.Value, error) {
	return core.NumberValue(math.Abs(n.value)), nil
}

// GetNumericOps returns a Numeric implementation for a value if possible
func GetNumericOps(v core.Value) (Numeric, bool) {
	switch val := v.(type) {
	case core.NumberValue:
		return NewNumericOps(val), true
	default:
		// Check if value has numeric dunder methods
		if obj, ok := v.(core.Object); ok {
			// For now, we'll check if it has __add__ as a proxy for numeric
			if _, exists := obj.GetAttr("__add__"); exists {
				// TODO: Return a DunderNumeric wrapper
				return nil, false
			}
		}
		return nil, false
	}
}
