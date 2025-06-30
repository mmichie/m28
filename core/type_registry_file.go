package core

// registerFileType registers the file type descriptor
func registerFileType() {
	// Note: File type already uses method registry pattern in core/file.go
	// This registration is for the TypeDescriptor system
	RegisterType(&TypeDescriptor{
		Name:       "file",
		PythonName: "file",
		BaseType:   Type("file"),
		Methods: map[string]*MethodDescriptor{
			"read": {
				Name:    "read",
				Arity:   -1,
				Doc:     "Read from file. read() reads entire file, read(n) reads n bytes",
				Builtin: true,
			},
			"write": {
				Name:    "write",
				Arity:   1,
				Doc:     "Write string to file",
				Builtin: true,
			},
			"readline": {
				Name:    "readline",
				Arity:   0,
				Doc:     "Read a single line from file",
				Builtin: true,
			},
			"readlines": {
				Name:    "readlines",
				Arity:   0,
				Doc:     "Read all lines from file into a list",
				Builtin: true,
			},
			"writelines": {
				Name:    "writelines",
				Arity:   1,
				Doc:     "Write a list of strings to file",
				Builtin: true,
			},
			"close": {
				Name:    "close",
				Arity:   0,
				Doc:     "Close the file",
				Builtin: true,
			},
			"seek": {
				Name:    "seek",
				Arity:   -1,
				Doc:     "Change file position. seek(offset, whence=0)",
				Builtin: true,
			},
			"tell": {
				Name:    "tell",
				Arity:   0,
				Doc:     "Get current file position",
				Builtin: true,
			},
			"__enter__": {
				Name:    "__enter__",
				Arity:   0,
				Doc:     "Enter context manager",
				Builtin: true,
			},
			"__exit__": {
				Name:    "__exit__",
				Arity:   3,
				Doc:     "Exit context manager",
				Builtin: true,
			},
			"__iter__": {
				Name:    "__iter__",
				Arity:   0,
				Doc:     "Return iterator over lines",
				Builtin: true,
			},
		},
		Properties: map[string]*PropertyDescriptor{
			"closed": {
				Name: "closed",
				Doc:  "Whether the file is closed",
			},
			"name": {
				Name: "name",
				Doc:  "File path",
			},
			"mode": {
				Name: "mode",
				Doc:  "File mode",
			},
		},
	})
}
