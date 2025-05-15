package concurrency

import (
	"fmt"
	"reflect"
	"time"

	"github.com/mmichie/m28/core"
)

// EvalSelect implements the select special form for channel multiplexing
// The syntax is:
// (select
//
//	[(case expr channel-expr) body...]
//	[(case expr channel-expr) body...]
//	...
//	[(default) default-body...])
//
// Where:
// - expr can be :recv or :send
// - channel-expr is a channel expression for :recv or [channel value] for :send
// - body is the code to execute if the case is selected
// - default is optional and runs if no other case is ready
func EvalSelect(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("select requires at least one case")
	}

	// Parse and validate cases
	var cases []reflect.SelectCase
	var caseHandlers []func() (core.LispValue, error)

	// Track if we have a default case
	hasDefault := false

	for _, arg := range args {
		// Unwrap LocatedValue if present
		if locatedValue, ok := arg.(core.LocatedValue); ok {
			arg = locatedValue.Value
		}

		// Each case should be a list: [(case expr channel-expr) body...]
		caseList, ok := arg.(core.LispList)
		if !ok {
			return nil, fmt.Errorf("select case must be a list")
		}

		if len(caseList) < 1 {
			return nil, fmt.Errorf("select case list must not be empty")
		}

		// The first element should be a case specification
		caseSpec, ok := caseList[0].(core.LispList)
		if !ok {
			return nil, fmt.Errorf("select case specification must be a list")
		}

		// Handle body (everything after the case specification)
		body := caseList[1:]
		if len(body) == 0 {
			return nil, fmt.Errorf("select case must have a body")
		}

		// Create handler function to execute this case's body
		handler := func(caseBody []core.LispValue) func() (core.LispValue, error) {
			return func() (core.LispValue, error) {
				var result core.LispValue = core.PythonicNone{}
				var err error

				// Evaluate each expression in the body
				for _, expr := range caseBody {
					result, err = e.Eval(expr, env)
					if err != nil {
						return nil, err
					}
				}
				return result, nil
			}
		}(body)

		// Process the case specification
		if len(caseSpec) < 1 {
			return nil, fmt.Errorf("case specification must not be empty")
		}

		// Check for default case
		if firstSym, ok := caseSpec[0].(core.LispSymbol); ok && firstSym == "default" {
			if hasDefault {
				return nil, fmt.Errorf("select can have at most one default case")
			}
			hasDefault = true

			// Add default case (nil channel means default)
			cases = append(cases, reflect.SelectCase{
				Dir:  reflect.SelectDefault,
				Chan: reflect.Value{},
				Send: reflect.Value{},
			})
			caseHandlers = append(caseHandlers, handler)
			continue
		}

		// Regular case: [(case :recv|:send channel-expr) body...]
		if len(caseSpec) < 3 {
			return nil, fmt.Errorf("case specification must have at least 3 elements")
		}

		// First element should be 'case' symbol
		if caseSym, ok := caseSpec[0].(core.LispSymbol); !ok || caseSym != "case" {
			return nil, fmt.Errorf("expected 'case' as first element in case specification")
		}

		// Second element should be :recv or :send
		dirSym, ok := caseSpec[1].(core.LispSymbol)
		if !ok {
			return nil, fmt.Errorf("expected :recv or :send in case specification")
		}

		// Third element is channel expression for :recv or [channel value] for :send
		channelExpr := caseSpec[2]

		// Now process the type of case (receive or send)
		switch dirSym {
		case ":recv", "recv":
			// Evaluate the channel expression
			channelVal, err := e.Eval(channelExpr, env)
			if err != nil {
				return nil, err
			}

			// Check if it's a channel
			ch, ok := channelVal.(*PythonicChannel)
			if !ok {
				return nil, fmt.Errorf("receive case requires a channel")
			}

			// Add to cases
			cases = append(cases, reflect.SelectCase{
				Dir:  reflect.SelectRecv,
				Chan: reflect.ValueOf(ch.ch),
			})

			// Wrap the handler to assign received value to a variable
			wrappedHandler := func(origHandler func() (core.LispValue, error), recvCh *PythonicChannel) func() (core.LispValue, error) {
				return func() (core.LispValue, error) {
					// Receive value
					value, ok := <-recvCh.ch

					// Create a temporary environment for the case body
					caseEnv := env.NewEnvironment(nil)

					// Copy all values from parent environment
					if collector, ok := env.(core.EnvironmentCollector); ok {
						collector.ForEachSymbol(func(symbol core.LispSymbol, value core.LispValue) {
							caseEnv.Define(symbol, value)
						})
					}

					// Define received value in the environment for the case body to use
					caseEnv.Define("select-value", value)
					caseEnv.Define("select-ok", core.PythonicBool(ok))

					// Execute the original handler in the new environment
					return origHandler()
				}
			}(handler, ch)
			caseHandlers = append(caseHandlers, wrappedHandler)

		case ":send", "send":
			// For send, the channelExpr should be a list: [channel value]
			sendExpr, ok := channelExpr.(core.LispList)
			if !ok || len(sendExpr) != 2 {
				return nil, fmt.Errorf("send case requires [channel value] expression")
			}

			// Evaluate the channel
			channelVal, err := e.Eval(sendExpr[0], env)
			if err != nil {
				return nil, err
			}

			// Check if it's a channel
			ch, ok := channelVal.(*PythonicChannel)
			if !ok {
				return nil, fmt.Errorf("send case requires a channel")
			}

			// Evaluate the value to send
			sendVal, err := e.Eval(sendExpr[1], env)
			if err != nil {
				return nil, err
			}

			// Add to cases
			cases = append(cases, reflect.SelectCase{
				Dir:  reflect.SelectSend,
				Chan: reflect.ValueOf(ch.ch),
				Send: reflect.ValueOf(sendVal),
			})
			caseHandlers = append(caseHandlers, handler)

		default:
			return nil, fmt.Errorf("expected :recv or :send in case specification, got %s", dirSym)
		}
	}

	// Now perform the select operation
	chosen, _, _ := reflect.Select(cases)

	// Execute the handler for the chosen case
	handler := caseHandlers[chosen]
	return handler()
}

// EvalSelectTimeout implements the select-timeout special form
// This is similar to select but adds a timeout option
// (select-timeout timeout-ms
//
//	[(case expr channel-expr) body...]
//	...
//	[(timeout) timeout-body...])
func EvalSelectTimeout(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("select-timeout requires a timeout and at least one case")
	}

	// First argument is the timeout in milliseconds
	timeoutExpr := args[0]
	timeoutVal, err := e.Eval(timeoutExpr, env)
	if err != nil {
		return nil, err
	}

	// Convert timeout to milliseconds
	var timeoutMs int64
	if timeout, ok := timeoutVal.(float64); ok {
		timeoutMs = int64(timeout)
	} else {
		return nil, fmt.Errorf("timeout must be a number in milliseconds")
	}

	// Create a timeout channel
	timeoutCh := time.After(time.Duration(timeoutMs) * time.Millisecond)

	// Parse and validate cases (similar to EvalSelect)
	var cases []reflect.SelectCase
	var caseHandlers []func() (core.LispValue, error)

	// Add the timeout case first
	cases = append(cases, reflect.SelectCase{
		Dir:  reflect.SelectRecv,
		Chan: reflect.ValueOf(timeoutCh),
	})

	// Handler for timeout case
	timeoutHandler := func() (core.LispValue, error) {
		// Look for a case with (timeout) specification
		for _, arg := range args[1:] {
			// Unwrap LocatedValue if present
			if locatedValue, ok := arg.(core.LocatedValue); ok {
				arg = locatedValue.Value
			}

			caseList, ok := arg.(core.LispList)
			if !ok || len(caseList) < 1 {
				continue
			}

			caseSpec, ok := caseList[0].(core.LispList)
			if !ok || len(caseSpec) < 1 {
				continue
			}

			if firstSym, ok := caseSpec[0].(core.LispSymbol); ok && firstSym == "timeout" {
				// Found timeout case, execute its body
				body := caseList[1:]
				var result core.LispValue = core.PythonicNone{}

				for _, expr := range body {
					var err error
					result, err = e.Eval(expr, env)
					if err != nil {
						return nil, err
					}
				}
				return result, nil
			}
		}

		// No explicit timeout case, return None
		return core.PythonicNone{}, nil
	}
	caseHandlers = append(caseHandlers, timeoutHandler)

	// Process the regular cases (same as in EvalSelect)
	for _, arg := range args[1:] {
		// Unwrap LocatedValue if present
		if locatedValue, ok := arg.(core.LocatedValue); ok {
			arg = locatedValue.Value
		}

		caseList, ok := arg.(core.LispList)
		if !ok {
			return nil, fmt.Errorf("select case must be a list")
		}

		if len(caseList) < 1 {
			return nil, fmt.Errorf("select case list must not be empty")
		}

		// Skip timeout cases as we've handled them separately
		caseSpec, ok := caseList[0].(core.LispList)
		if !ok {
			return nil, fmt.Errorf("select case specification must be a list")
		}

		if len(caseSpec) < 1 {
			return nil, fmt.Errorf("case specification must not be empty")
		}

		if firstSym, ok := caseSpec[0].(core.LispSymbol); ok && firstSym == "timeout" {
			// Skip timeout case
			continue
		}

		// The rest is the same as in EvalSelect
		body := caseList[1:]
		if len(body) == 0 {
			return nil, fmt.Errorf("select case must have a body")
		}

		handler := func(caseBody []core.LispValue) func() (core.LispValue, error) {
			return func() (core.LispValue, error) {
				var result core.LispValue = core.PythonicNone{}
				var err error

				for _, expr := range caseBody {
					result, err = e.Eval(expr, env)
					if err != nil {
						return nil, err
					}
				}
				return result, nil
			}
		}(body)

		// Process the case specification
		if len(caseSpec) < 3 {
			return nil, fmt.Errorf("case specification must have at least 3 elements")
		}

		if caseSym, ok := caseSpec[0].(core.LispSymbol); !ok || caseSym != "case" {
			return nil, fmt.Errorf("expected 'case' as first element in case specification")
		}

		dirSym, ok := caseSpec[1].(core.LispSymbol)
		if !ok {
			return nil, fmt.Errorf("expected :recv or :send in case specification")
		}

		channelExpr := caseSpec[2]

		switch dirSym {
		case ":recv", "recv":
			channelVal, err := e.Eval(channelExpr, env)
			if err != nil {
				return nil, err
			}

			ch, ok := channelVal.(*PythonicChannel)
			if !ok {
				return nil, fmt.Errorf("receive case requires a channel")
			}

			cases = append(cases, reflect.SelectCase{
				Dir:  reflect.SelectRecv,
				Chan: reflect.ValueOf(ch.ch),
			})

			wrappedHandler := func(origHandler func() (core.LispValue, error), recvCh *PythonicChannel) func() (core.LispValue, error) {
				return func() (core.LispValue, error) {
					value, ok := <-recvCh.ch

					caseEnv := env.NewEnvironment(nil)

					if collector, ok := env.(core.EnvironmentCollector); ok {
						collector.ForEachSymbol(func(symbol core.LispSymbol, value core.LispValue) {
							caseEnv.Define(symbol, value)
						})
					}

					caseEnv.Define("select-value", value)
					caseEnv.Define("select-ok", core.PythonicBool(ok))

					return origHandler()
				}
			}(handler, ch)
			caseHandlers = append(caseHandlers, wrappedHandler)

		case ":send", "send":
			sendExpr, ok := channelExpr.(core.LispList)
			if !ok || len(sendExpr) != 2 {
				return nil, fmt.Errorf("send case requires [channel value] expression")
			}

			channelVal, err := e.Eval(sendExpr[0], env)
			if err != nil {
				return nil, err
			}

			ch, ok := channelVal.(*PythonicChannel)
			if !ok {
				return nil, fmt.Errorf("send case requires a channel")
			}

			sendVal, err := e.Eval(sendExpr[1], env)
			if err != nil {
				return nil, err
			}

			cases = append(cases, reflect.SelectCase{
				Dir:  reflect.SelectSend,
				Chan: reflect.ValueOf(ch.ch),
				Send: reflect.ValueOf(sendVal),
			})
			caseHandlers = append(caseHandlers, handler)

		default:
			return nil, fmt.Errorf("expected :recv or :send in case specification, got %s", dirSym)
		}
	}

	// Perform the select operation
	chosen, _, _ := reflect.Select(cases)

	// Execute the handler for the chosen case
	handler := caseHandlers[chosen]
	return handler()
}
