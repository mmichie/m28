package eval

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// asyncForm wraps a function to run asynchronously
// (async def name (args) body...)  - defines an async function
// (async (lambda (args) body...))   - creates an async lambda
func asyncForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() < 1 {
		return nil, &core.TypeError{Message: "async requires at least 1 argument"}
	}

	// Check if it's async def
	if args.Len() >= 2 {
		if sym, ok := args.Items()[0].(core.SymbolValue); ok && string(sym) == "def" {
			// async def form
			return asyncDefForm(args.Items()[1:], ctx)
		}
	}

	// Otherwise, wrap the expression as async
	expr, err := Eval(args.Items()[0], ctx)
	if err != nil {
		return nil, err
	}

	// Check if it's a function
	if _, ok := expr.(interface {
		Call([]core.Value, *core.Context) (core.Value, error)
	}); ok {
		name := ""
		if fn, ok := expr.(*UserFunction); ok {
			name = fn.name
		}
		return core.NewAsyncFunction(expr, name), nil
	}

	return nil, &core.TypeError{Message: "async requires a function"}
}

// asyncDefForm handles async def
func asyncDefForm(args []core.Value, ctx *core.Context) (core.Value, error) {
	// Similar to regular def but creates an async function
	if len(args) < 3 {
		return nil, &core.TypeError{Message: "async def requires name, params, and body"}
	}

	name, ok := args[0].(core.SymbolValue)
	if !ok {
		return nil, &core.TypeError{Message: "function name must be a symbol"}
	}

	params, ok := args[1].(*core.ListValue)
	if !ok {
		return nil, &core.TypeError{Message: "parameters must be a list"}
	}

	// Create a regular function first
	defArgs := append([]core.Value{name, params}, args[2:]...)
	fn, err := DefForm(core.NewList(defArgs...), ctx)
	if err != nil {
		return nil, err
	}

	// Wrap it as async
	asyncFn := core.NewAsyncFunction(fn, string(name))
	ctx.Define(string(name), asyncFn)

	return asyncFn, nil
}

// awaitForm waits for an async task to complete
func awaitForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() != 1 {
		return nil, &core.TypeError{Message: "await requires exactly 1 argument"}
	}

	// Evaluate the expression
	expr, err := Eval(args.Items()[0], ctx)
	if err != nil {
		return nil, err
	}

	// Check if it's a task
	if task, ok := expr.(*core.Task); ok {
		return task.Wait()
	}

	// If not a task, return the value as-is (for compatibility)
	return expr, nil
}

// channelForm creates a new channel
// (channel)      - unbuffered channel
// (channel 10)   - buffered channel with capacity 10
func channelForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	capacity := 0

	if args.Len() > 0 {
		// Evaluate capacity
		capVal, err := Eval(args.Items()[0], ctx)
		if err != nil {
			return nil, err
		}

		if num, ok := capVal.(core.NumberValue); ok {
			capacity = int(num)
			if capacity < 0 {
				return nil, &core.ValueError{Message: "channel capacity must be non-negative"}
			}
		} else {
			return nil, &core.TypeError{Message: "channel capacity must be a number"}
		}
	}

	return core.NewChannel(capacity), nil
}

// sendForm sends a value to a channel (blocking)
// (send! channel value)
func sendForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() != 2 {
		return nil, &core.TypeError{Message: "send! requires 2 arguments: channel and value"}
	}

	// Evaluate channel
	chVal, err := Eval(args.Items()[0], ctx)
	if err != nil {
		return nil, err
	}

	ch, ok := chVal.(*core.Channel)
	if !ok {
		return nil, &core.TypeError{Message: "first argument must be a channel"}
	}

	// Evaluate value
	value, err := Eval(args.Items()[1], ctx)
	if err != nil {
		return nil, err
	}

	// Send value
	err = ch.Send(value)
	if err != nil {
		return nil, err
	}

	return value, nil
}

// receiveForm receives a value from a channel (blocking)
// (recv! channel)
func receiveForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() != 1 {
		return nil, &core.TypeError{Message: "recv! requires 1 argument: channel"}
	}

	// Evaluate channel
	chVal, err := Eval(args.Items()[0], ctx)
	if err != nil {
		return nil, err
	}

	ch, ok := chVal.(*core.Channel)
	if !ok {
		return nil, &core.TypeError{Message: "argument must be a channel"}
	}

	// Receive value
	return ch.Receive()
}

// selectForm implements channel selection
// (select
//
//	((recv! ch1) (lambda (val) ...))
//	((send! ch2 value) (lambda () ...))
//	(default (lambda () ...)))
func selectForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() == 0 {
		return nil, &core.TypeError{Message: "select requires at least one case"}
	}

	var cases []core.SelectCase
	var defaultCase func() (core.Value, error)

	for _, arg := range args.Items() {
		caseList, ok := arg.(*core.ListValue)
		if !ok || caseList.Len() != 2 {
			return nil, &core.TypeError{Message: "select case must be a list of (channel-op handler)"}
		}

		// Check for default case
		if sym, ok := caseList.Items()[0].(core.SymbolValue); ok && string(sym) == "default" {
			// Default case
			handler, err := Eval(caseList.Items()[1], ctx)
			if err != nil {
				return nil, err
			}

			if fn, ok := handler.(interface {
				Call([]core.Value, *core.Context) (core.Value, error)
			}); ok {
				defaultCase = func() (core.Value, error) {
					return fn.Call([]core.Value{}, ctx)
				}
			} else {
				return nil, &core.TypeError{Message: "default handler must be a function"}
			}
			continue
		}

		// Parse channel operation
		opList, ok := caseList.Items()[0].(*core.ListValue)
		if !ok || opList.Len() < 1 {
			return nil, &core.TypeError{Message: "invalid channel operation in select"}
		}

		opSym, ok := opList.Items()[0].(core.SymbolValue)
		if !ok {
			return nil, &core.TypeError{Message: "channel operation must start with a symbol"}
		}

		// Get handler
		handler, err := Eval(caseList.Items()[1], ctx)
		if err != nil {
			return nil, err
		}

		handlerFn, ok := handler.(interface {
			Call([]core.Value, *core.Context) (core.Value, error)
		})
		if !ok {
			return nil, &core.TypeError{Message: "case handler must be a function"}
		}

		switch string(opSym) {
		case "recv!", "receive":
			if opList.Len() != 2 {
				return nil, &core.TypeError{Message: "recv! requires 1 argument"}
			}

			chVal, err := Eval(opList.Items()[1], ctx)
			if err != nil {
				return nil, err
			}

			ch, ok := chVal.(*core.Channel)
			if !ok {
				return nil, &core.TypeError{Message: "recv! argument must be a channel"}
			}

			cases = append(cases, core.SelectCase{
				Channel: ch,
				IsSend:  false,
				Body: func(val core.Value) (core.Value, error) {
					return handlerFn.Call([]core.Value{val}, ctx)
				},
			})

		case "send!", "send":
			if opList.Len() != 3 {
				return nil, &core.TypeError{Message: "send! requires 2 arguments"}
			}

			chVal, err := Eval(opList.Items()[1], ctx)
			if err != nil {
				return nil, err
			}

			ch, ok := chVal.(*core.Channel)
			if !ok {
				return nil, &core.TypeError{Message: "send! first argument must be a channel"}
			}

			value, err := Eval(opList.Items()[2], ctx)
			if err != nil {
				return nil, err
			}

			cases = append(cases, core.SelectCase{
				Channel: ch,
				Value:   value,
				IsSend:  true,
				Body: func(_ core.Value) (core.Value, error) {
					return handlerFn.Call([]core.Value{}, ctx)
				},
			})

		default:
			return nil, &core.ValueError{Message: fmt.Sprintf("unknown channel operation: %s", string(opSym))}
		}
	}

	// Perform select
	return core.Select(cases, defaultCase)
}

// goForm starts a goroutine
// (go expr) - evaluates expr in a new goroutine
func goForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() != 1 {
		return nil, &core.TypeError{Message: "go requires exactly 1 argument"}
	}

	// Create a task that evaluates the expression
	task := core.NewTask("", nil, nil)

	// Start the goroutine
	go func() {
		defer func() {
			task.Mu.Lock()
			task.Finished = true
			task.Mu.Unlock()
			close(task.Done)
		}()

		// Evaluate in a new context
		goCtx := core.NewContext(ctx)
		result, err := Eval(args.Items()[0], goCtx)

		task.Mu.Lock()
		task.Result = result
		task.Err = err
		task.Mu.Unlock()
	}()

	task.Mu.Lock()
	task.Started = true
	task.Mu.Unlock()

	return task, nil
}

// sleepForm pauses execution
// (sleep seconds)
func sleepForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() != 1 {
		return nil, &core.TypeError{Message: "sleep requires exactly 1 argument"}
	}

	// Evaluate duration
	durVal, err := Eval(args.Items()[0], ctx)
	if err != nil {
		return nil, err
	}

	dur, ok := durVal.(core.NumberValue)
	if !ok {
		return nil, &core.TypeError{Message: "sleep duration must be a number"}
	}

	core.Sleep(float64(dur))
	return core.Nil, nil
}

// RegisterAsyncForms registers async/concurrent forms
func RegisterAsyncForms() {
	RegisterSpecialForm("async", asyncForm)
	RegisterSpecialForm("await", awaitForm)
	RegisterSpecialForm("channel", channelForm)
	RegisterSpecialForm("send!", sendForm)
	RegisterSpecialForm("recv!", receiveForm)
	RegisterSpecialForm("receive", receiveForm) // alias
	RegisterSpecialForm("select", selectForm)
	RegisterSpecialForm("go", goForm)
	RegisterSpecialForm("sleep", sleepForm)
}
