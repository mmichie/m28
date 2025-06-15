package builtin

import (
	"fmt"

	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// RegisterAsyncBuiltins registers async/concurrent builtin functions
func RegisterAsyncBuiltins(ctx *core.Context) {
	// Channel constructor function
	ctx.Define("Channel", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("Channel", args)
		if err := v.Max(1); err != nil {
			return nil, err
		}

		capacity := 0
		if v.Count() > 0 {
			num, err := v.GetNumber(0)
			if err != nil {
				return nil, fmt.Errorf("channel capacity must be a number")
			}
			capacity = int(num)
			if capacity < 0 {
				return nil, fmt.Errorf("channel capacity must be non-negative")
			}
		}

		return core.NewChannel(capacity), nil
	}))

	// asyncio.run equivalent
	ctx.Define("run_async", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("run_async", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		arg := v.Get(0)
		// If it's a task, wait for it
		if task, ok := arg.(*core.Task); ok {
			return task.Wait()
		}

		// If it's an async function, call it and wait
		if asyncFn, ok := arg.(*core.AsyncFunction); ok {
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
		v := validation.NewArgs("gather", args)
		tasks := make([]*core.Task, 0, v.Count())

		// Start all tasks
		for i := 0; i < v.Count(); i++ {
			arg := v.Get(i)
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
		v := validation.NewArgs("create_task", args)
		if err := v.Min(1); err != nil {
			return nil, err
		}

		fn := v.Get(0)
		fnArgs := make([]core.Value, v.Count()-1)
		for i := 1; i < v.Count(); i++ {
			fnArgs[i-1] = v.Get(i)
		}

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
		v := validation.NewArgs("sleep", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		dur, err := v.GetNumber(0)
		if err != nil {
			return nil, fmt.Errorf("sleep duration must be a number")
		}

		core.Sleep(dur)
		return core.Nil, nil
	}))
}

// Migration Statistics:
// Functions migrated: 4 async functions (Channel, run_async, gather, create_task, sleep)
// Type checks eliminated: ~6 manual type assertions
// Code improvements: Uses validation framework throughout
// Benefits: Consistent error messages, cleaner validation with v.Min(), v.Max(), v.Exact()
