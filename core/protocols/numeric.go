package protocols

import (
	"fmt"
	"math"

	"github.com/mmichie/m28/common/errors"
	"github.com/mmichie/m28/common/types"
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
		// Try __radd__ on other
		if result, found, err := types.CallRadd(other, core.NumberValue(n.value), nil); found {
			if err != nil {
				return nil, err
			}
			return result, nil
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
		// Try __rsub__ on other
		if result, found, err := types.CallRsub(other, core.NumberValue(n.value), nil); found {
			if err != nil {
				return nil, err
			}
			return result, nil
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
		// Try __rmul__ on other
		if result, found, err := types.CallRmul(other, core.NumberValue(n.value), nil); found {
			if err != nil {
				return nil, err
			}
			return result, nil
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
		// Try __rtruediv__ on other
		if result, found, err := types.CallRdiv(other, core.NumberValue(n.value), nil); found {
			if err != nil {
				return nil, err
			}
			return result, nil
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

// DunderNumeric wraps objects with arithmetic dunder methods
type DunderNumeric struct {
	obj core.Object
	ctx *core.Context
}

// NewDunderNumeric creates a Numeric wrapper for an object with arithmetic methods
func NewDunderNumeric(obj core.Object, ctx *core.Context) *DunderNumeric {
	return &DunderNumeric{obj: obj, ctx: ctx}
}

// callDunder is a helper to call a dunder method on the wrapped object
func (d *DunderNumeric) callDunder(method string, args []core.Value) (core.Value, error) {
	methodVal, exists := d.obj.GetAttr(method)
	if !exists {
		return nil, fmt.Errorf("object has no %s method", method)
	}

	callable, ok := methodVal.(interface {
		Call([]core.Value, *core.Context) (core.Value, error)
	})
	if !ok {
		return nil, fmt.Errorf("%s is not callable", method)
	}

	return callable.Call(args, d.ctx)
}

// Add implements Numeric.Add by calling __add__
func (d *DunderNumeric) Add(other core.Value) (core.Value, error) {
	return d.callDunder("__add__", []core.Value{other})
}

// Subtract implements Numeric.Subtract by calling __sub__
func (d *DunderNumeric) Subtract(other core.Value) (core.Value, error) {
	return d.callDunder("__sub__", []core.Value{other})
}

// Multiply implements Numeric.Multiply by calling __mul__
func (d *DunderNumeric) Multiply(other core.Value) (core.Value, error) {
	return d.callDunder("__mul__", []core.Value{other})
}

// Divide implements Numeric.Divide by calling __truediv__
func (d *DunderNumeric) Divide(other core.Value) (core.Value, error) {
	return d.callDunder("__truediv__", []core.Value{other})
}

// Modulo implements Numeric.Modulo by calling __mod__
func (d *DunderNumeric) Modulo(other core.Value) (core.Value, error) {
	return d.callDunder("__mod__", []core.Value{other})
}

// Power implements Numeric.Power by calling __pow__
func (d *DunderNumeric) Power(other core.Value) (core.Value, error) {
	return d.callDunder("__pow__", []core.Value{other})
}

// Negate implements Numeric.Negate by calling __neg__
func (d *DunderNumeric) Negate() (core.Value, error) {
	return d.callDunder("__neg__", []core.Value{})
}

// Absolute implements Numeric.Absolute by calling __abs__
func (d *DunderNumeric) Absolute() (core.Value, error) {
	return d.callDunder("__abs__", []core.Value{})
}

// GetNumericOps returns a Numeric implementation for a value if possible
func GetNumericOps(v core.Value) (Numeric, bool) {
	switch val := v.(type) {
	case core.NumberValue:
		return NewNumericOps(val), true
	default:
		// Check if value has numeric dunder methods
		if obj, ok := v.(core.Object); ok {
			// Check if it has __add__ as a proxy for numeric capability
			if _, exists := obj.GetAttr("__add__"); exists {
				// Return a DunderNumeric wrapper
				return NewDunderNumeric(obj, nil), true
			}
		}
		return nil, false
	}
}
