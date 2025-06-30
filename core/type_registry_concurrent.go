package core

// registerConcurrentTypes registers task, channel, and generator type descriptors
func registerConcurrentTypes() {
	registerTaskType()
	registerChannelType()
	registerGeneratorType()
}

// registerTaskType registers the task type descriptor
func registerTaskType() {
	// Note: Task type already uses method registry pattern in core/async.go
	RegisterType(&TypeDescriptor{
		Name:       "task",
		PythonName: "Task",
		BaseType:   Type("task"),
		Methods: map[string]*MethodDescriptor{
			"result": {
				Name:    "result",
				Arity:   0,
				Doc:     "Get the result of the task (blocks until complete)",
				Builtin: true,
			},
			"done": {
				Name:    "done",
				Arity:   0,
				Doc:     "Check if the task is finished",
				Builtin: true,
			},
		},
	})
}

// registerChannelType registers the channel type descriptor
func registerChannelType() {
	// Note: Channel type already uses method registry pattern in core/async.go
	RegisterType(&TypeDescriptor{
		Name:       "channel",
		PythonName: "Channel",
		BaseType:   Type("channel"),
		Methods: map[string]*MethodDescriptor{
			"send": {
				Name:    "send",
				Arity:   1,
				Doc:     "Send a value to the channel",
				Builtin: true,
			},
			"put": {
				Name:    "put",
				Arity:   1,
				Doc:     "Send a value to the channel (alias for send)",
				Builtin: true,
			},
			"receive": {
				Name:    "receive",
				Arity:   0,
				Doc:     "Receive a value from the channel",
				Builtin: true,
			},
			"recv": {
				Name:    "recv",
				Arity:   0,
				Doc:     "Receive a value from the channel (alias for receive)",
				Builtin: true,
			},
			"get": {
				Name:    "get",
				Arity:   0,
				Doc:     "Receive a value from the channel (alias for receive)",
				Builtin: true,
			},
			"close": {
				Name:    "close",
				Arity:   0,
				Doc:     "Close the channel",
				Builtin: true,
			},
			"__len__": {
				Name:    "__len__",
				Arity:   0,
				Doc:     "Get the number of values in the channel buffer",
				Builtin: true,
			},
		},
	})
}

// registerGeneratorType registers the generator type descriptor
func registerGeneratorType() {
	// Note: Generator type already uses method registry pattern in core/generator.go
	RegisterType(&TypeDescriptor{
		Name:       "generator",
		PythonName: "generator",
		BaseType:   Type("generator"),
		Methods: map[string]*MethodDescriptor{
			"__next__": {
				Name:    "__next__",
				Arity:   0,
				Doc:     "Retrieve the next value from the generator",
				Builtin: true,
			},
			"next": {
				Name:    "next",
				Arity:   0,
				Doc:     "Retrieve the next value from the generator (Python 2 compatibility)",
				Builtin: true,
			},
			"send": {
				Name:    "send",
				Arity:   1,
				Doc:     "Send a value into the generator",
				Builtin: true,
			},
			"close": {
				Name:    "close",
				Arity:   0,
				Doc:     "Close the generator",
				Builtin: true,
			},
			"__iter__": {
				Name:    "__iter__",
				Arity:   0,
				Doc:     "Return self (generators are iterators)",
				Builtin: true,
			},
		},
	})
}
