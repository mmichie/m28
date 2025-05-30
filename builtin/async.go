package builtin

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// RegisterAsyncBuiltins registers async/concurrent builtin functions
func RegisterAsyncBuiltins(ctx *core.Context) {
	// Channel constructor function
	ctx.Define("Channel", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		capacity := 0

		if len(args) > 0 {
			if num, ok := args[0].(core.NumberValue); ok {
				capacity = int(num)
				if capacity < 0 {
					return nil, fmt.Errorf("channel capacity must be non-negative")
				}
			} else {
				return nil, fmt.Errorf("channel capacity must be a number")
			}
		}

		return core.NewChannel(capacity), nil
	}))

	// asyncio.run equivalent
	ctx.Define("run_async", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("run_async requires exactly 1 argument")
		}

		// If it's a task, wait for it
		if task, ok := args[0].(*core.Task); ok {
			return task.Wait()
		}

		// If it's an async function, call it and wait
		if asyncFn, ok := args[0].(*core.AsyncFunction); ok {
			task, err := asyncFn.Call([]core.Value{}, ctx)
			if err != nil {
				return nil, err
			}
			if t, ok := task.(*core.Task); ok {
				return t.Wait()
			}
		}

		return nil, fmt.Errorf("run_async requires a task or async function")
	}))

	// gather - run multiple async tasks concurrently
	ctx.Define("gather", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		tasks := make([]*core.Task, 0, len(args))

		// Start all tasks
		for i, arg := range args {
			var task *core.Task

			switch v := arg.(type) {
			case *core.Task:
				task = v
			case *core.AsyncFunction:
				t, err := v.Call([]core.Value{}, ctx)
				if err != nil {
					return nil, fmt.Errorf("error starting task %d: %v", i, err)
				}
				task = t.(*core.Task)
			default:
				// Try to call it if it's callable
				if _, ok := v.(interface {
					Call([]core.Value, *core.Context) (core.Value, error)
				}); ok {
					t := core.NewTask("", v, []core.Value{})
					t.Start(ctx)
					task = t
				} else {
					return nil, fmt.Errorf("argument %d is not a task or callable", i)
				}
			}

			tasks = append(tasks, task)
		}

		// Wait for all tasks and collect results
		results := make(core.ListValue, len(tasks))
		for i, task := range tasks {
			result, err := task.Wait()
			if err != nil {
				return nil, fmt.Errorf("task %d failed: %v", i, err)
			}
			results[i] = result
		}

		return results, nil
	}))

	// create_task - create a task from a callable
	ctx.Define("create_task", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("create_task requires at least 1 argument")
		}

		fn := args[0]
		fnArgs := args[1:]

		if _, ok := fn.(interface {
			Call([]core.Value, *core.Context) (core.Value, error)
		}); ok {
			task := core.NewTask("", fn, fnArgs)
			task.Start(ctx)
			return task, nil
		}

		return nil, fmt.Errorf("create_task requires a callable")
	}))

	// sleep function (also available as special form)
	ctx.Define("sleep", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("sleep requires exactly 1 argument")
		}

		dur, ok := args[0].(core.NumberValue)
		if !ok {
			return nil, fmt.Errorf("sleep duration must be a number")
		}

		core.Sleep(float64(dur))
		return core.Nil, nil
	}))
}
