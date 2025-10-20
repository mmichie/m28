package core

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strings"
)

// File represents a file object
type File struct {
	BaseObject
	Path     string
	Mode     string
	file     *os.File
	reader   *bufio.Reader
	writer   *bufio.Writer
	closed   bool
	isText   bool
	registry *MethodRegistry
}

// FileIterator implements the Iterator interface for files
type FileIterator struct {
	file     *File
	finished bool
}

// Next advances the iterator and returns the next line
func (fi *FileIterator) Next() (Value, bool) {
	if fi.finished || fi.file.closed {
		return nil, false
	}

	line, err := fi.file.ReadLine()
	if err != nil {
		if err == io.EOF {
			fi.finished = true
			// If we got a line with EOF, return it
			if line != nil && line.String() != "" {
				return line, true
			}
			return nil, false
		}
		// For other errors, just stop iteration
		fi.finished = true
		return nil, false
	}

	return line, true
}

// Reset resets the iterator to the beginning
func (fi *FileIterator) Reset() {
	// For files, reset would require seeking to the beginning
	// This is only possible for seekable files
	if fi.file.file != nil && !fi.file.closed {
		fi.file.Seek(0, 0)
		fi.finished = false
	}
}

// Iterator returns an iterator over the file's lines
func (f *File) Iterator() Iterator {
	return &FileIterator{
		file:     f,
		finished: false,
	}
}

// NewFile creates a new file object
func NewFile(path string, mode string) (*File, error) {
	f := &File{
		BaseObject: *NewBaseObject(Type("file")),
		Path:       path,
		Mode:       mode,
		closed:     false,
		isText:     !strings.Contains(mode, "b"),
	}

	var err error

	// Parse mode
	switch mode {
	case "r", "rb":
		f.file, err = os.Open(path)
		if err != nil {
			return nil, fmt.Errorf("cannot open file '%s': %v", path, err)
		}
		f.reader = bufio.NewReader(f.file)

	case "w", "wb":
		f.file, err = os.Create(path)
		if err != nil {
			return nil, fmt.Errorf("cannot create file '%s': %v", path, err)
		}
		f.writer = bufio.NewWriter(f.file)

	case "a", "ab":
		f.file, err = os.OpenFile(path, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0644)
		if err != nil {
			return nil, fmt.Errorf("cannot open file '%s' for append: %v", path, err)
		}
		f.writer = bufio.NewWriter(f.file)

	case "r+", "rb+", "r+b":
		f.file, err = os.OpenFile(path, os.O_RDWR, 0644)
		if err != nil {
			return nil, fmt.Errorf("cannot open file '%s' for read/write: %v", path, err)
		}
		f.reader = bufio.NewReader(f.file)
		f.writer = bufio.NewWriter(f.file)

	case "w+", "wb+", "w+b":
		f.file, err = os.OpenFile(path, os.O_RDWR|os.O_CREATE|os.O_TRUNC, 0644)
		if err != nil {
			return nil, fmt.Errorf("cannot create file '%s' for read/write: %v", path, err)
		}
		f.reader = bufio.NewReader(f.file)
		f.writer = bufio.NewWriter(f.file)

	default:
		return nil, fmt.Errorf("invalid file mode: %s", mode)
	}

	// Initialize the method registry
	f.registry = f.createRegistry()

	return f, nil
}

// Type returns the file type
func (f *File) Type() Type {
	return Type("file")
}

// String returns the string representation
func (f *File) String() string {
	status := "open"
	if f.closed {
		status = "closed"
	}
	return fmt.Sprintf("<file '%s' mode '%s' %s>", f.Path, f.Mode, status)
}

// Read reads from the file
func (f *File) Read(size int) (Value, error) {
	if f.closed {
		return nil, fmt.Errorf("I/O operation on closed file")
	}

	if f.reader == nil {
		return nil, fmt.Errorf("file not open for reading")
	}

	if size < 0 {
		// Read entire file
		content, err := io.ReadAll(f.reader)
		if err != nil {
			return nil, fmt.Errorf("error reading file: %v", err)
		}

		if f.isText {
			return StringValue(string(content)), nil
		}
		// For binary mode, we'd return bytes, but for now return string
		return StringValue(string(content)), nil
	}

	// Read specified number of bytes
	buf := make([]byte, size)
	n, err := f.reader.Read(buf)
	if err != nil && err != io.EOF {
		return nil, fmt.Errorf("error reading file: %v", err)
	}

	if f.isText {
		return StringValue(string(buf[:n])), nil
	}
	return StringValue(string(buf[:n])), nil
}

// Write writes to the file
func (f *File) Write(data string) error {
	if f.closed {
		return fmt.Errorf("I/O operation on closed file")
	}

	if f.writer == nil {
		return fmt.Errorf("file not open for writing")
	}

	_, err := f.writer.WriteString(data)
	if err != nil {
		return fmt.Errorf("error writing to file: %v", err)
	}

	// Flush to ensure data is written
	return f.writer.Flush()
}

// ReadLine reads a single line
func (f *File) ReadLine() (Value, error) {
	if f.closed {
		return nil, fmt.Errorf("I/O operation on closed file")
	}

	if f.reader == nil {
		return nil, fmt.Errorf("file not open for reading")
	}

	line, err := f.reader.ReadString('\n')
	if err != nil {
		if err == io.EOF {
			if line != "" {
				return StringValue(line), nil
			}
			return StringValue(""), io.EOF
		}
		return nil, fmt.Errorf("error reading line: %v", err)
	}

	return StringValue(line), nil
}

// ReadLines reads all lines
func (f *File) ReadLines() (Value, error) {
	if f.closed {
		return nil, fmt.Errorf("I/O operation on closed file")
	}

	if f.reader == nil {
		return nil, fmt.Errorf("file not open for reading")
	}

	lines := NewList()
	scanner := bufio.NewScanner(f.reader)

	for scanner.Scan() {
		lines.Append(StringValue(scanner.Text() + "\n"))
	}

	if err := scanner.Err(); err != nil {
		return nil, fmt.Errorf("error reading lines: %v", err)
	}

	return lines, nil
}

// Close closes the file
func (f *File) Close() error {
	if f.closed {
		return nil // Already closed
	}

	if f.writer != nil {
		if err := f.writer.Flush(); err != nil {
			return fmt.Errorf("error flushing file: %v", err)
		}
	}

	if err := f.file.Close(); err != nil {
		return fmt.Errorf("error closing file: %v", err)
	}

	f.closed = true
	return nil
}

// Seek changes the file position
func (f *File) Seek(offset int64, whence int) (int64, error) {
	if f.closed {
		return 0, fmt.Errorf("I/O operation on closed file")
	}

	pos, err := f.file.Seek(offset, whence)
	if err != nil {
		return 0, fmt.Errorf("error seeking: %v", err)
	}

	// Reset readers/writers after seek
	if f.reader != nil {
		f.reader = bufio.NewReader(f.file)
	}
	if f.writer != nil {
		f.writer = bufio.NewWriter(f.file)
	}

	return pos, nil
}

// Tell returns the current file position
func (f *File) Tell() (int64, error) {
	if f.closed {
		return 0, fmt.Errorf("I/O operation on closed file")
	}

	return f.file.Seek(0, io.SeekCurrent)
}

// Enter implements context manager protocol
func (f *File) Enter() (Value, error) {
	return f, nil
}

// Exit implements context manager protocol
func (f *File) Exit(excType, excValue, excTraceback Value) (bool, error) {
	err := f.Close()
	return false, err // Don't suppress exceptions
}

// createRegistry sets up all methods and properties for file
func (f *File) createRegistry() *MethodRegistry {
	registry := NewMethodRegistry()

	// Register properties
	registry.RegisterProperties(
		MakeProperty("closed", "Whether the file is closed", func(receiver Value) (Value, error) {
			return BoolValue(receiver.(*File).closed), nil
		}),
		MakeProperty("name", "File path", func(receiver Value) (Value, error) {
			return StringValue(receiver.(*File).Path), nil
		}),
		MakeProperty("mode", "File mode", func(receiver Value) (Value, error) {
			return StringValue(receiver.(*File).Mode), nil
		}),
	)

	// Register methods
	registry.RegisterMethods(
		// read method
		MakeMethod("read", -1, "Read from file. read() reads entire file, read(n) reads n bytes",
			func(receiver Value, args []Value, ctx *Context) (Value, error) {
				file, err := TypedReceiver[*File](receiver, "read")
				if err != nil {
					return nil, err
				}

				size := -1
				if len(args) > 0 {
					if num, ok := args[0].(NumberValue); ok {
						size = int(num)
					} else {
						return nil, fmt.Errorf("read() argument must be a number")
					}
				}
				return file.Read(size)
			}),

		// write method
		MakeMethod("write", 1, "Write string to file",
			func(receiver Value, args []Value, ctx *Context) (Value, error) {
				file, err := TypedReceiver[*File](receiver, "write")
				if err != nil {
					return nil, err
				}
				if err := ValidateArity("write", args, 1); err != nil {
					return nil, err
				}

				// For strings, write the actual string content, not the repr
				var data string
				if s, ok := args[0].(StringValue); ok {
					data = string(s)
				} else {
					data = PrintValue(args[0])
				}

				err = file.Write(data)
				if err != nil {
					return nil, err
				}
				return NumberValue(len(data)), nil // Return number of bytes written
			}),

		// readline method
		MakeMethod("readline", 0, "Read a single line from file",
			func(receiver Value, args []Value, ctx *Context) (Value, error) {
				file, err := TypedReceiver[*File](receiver, "readline")
				if err != nil {
					return nil, err
				}
				if err := ValidateArity("readline", args, 0); err != nil {
					return nil, err
				}
				return file.ReadLine()
			}),

		// readlines method
		MakeMethod("readlines", 0, "Read all lines from file into a list",
			func(receiver Value, args []Value, ctx *Context) (Value, error) {
				file, err := TypedReceiver[*File](receiver, "readlines")
				if err != nil {
					return nil, err
				}
				if err := ValidateArity("readlines", args, 0); err != nil {
					return nil, err
				}
				return file.ReadLines()
			}),

		// writelines method
		MakeMethod("writelines", 1, "Write a list of strings to file",
			func(receiver Value, args []Value, ctx *Context) (Value, error) {
				file, err := TypedReceiver[*File](receiver, "writelines")
				if err != nil {
					return nil, err
				}
				if err := ValidateArity("writelines", args, 1); err != nil {
					return nil, err
				}

				// Get the lines to write
				var lines []Value
				switch v := args[0].(type) {
				case *ListValue:
					lines = v.Items()
				case TupleValue:
					lines = v
				default:
					return nil, fmt.Errorf("writelines() argument must be a list or tuple of strings")
				}

				// Write each line
				for i, line := range lines {
					var data string
					if s, ok := line.(StringValue); ok {
						data = string(s)
					} else {
						return nil, fmt.Errorf("writelines() argument must contain strings, found %s at index %d", line.Type(), i)
					}
					err := file.Write(data)
					if err != nil {
						return nil, err
					}
				}

				return Nil, nil
			}),

		// close method
		MakeMethod("close", 0, "Close the file",
			func(receiver Value, args []Value, ctx *Context) (Value, error) {
				file, err := TypedReceiver[*File](receiver, "close")
				if err != nil {
					return nil, err
				}
				if err := ValidateArity("close", args, 0); err != nil {
					return nil, err
				}

				err = file.Close()
				if err != nil {
					return nil, err
				}
				return Nil, nil
			}),

		// seek method
		MakeMethod("seek", -1, "Change file position. seek(offset, whence=0)",
			func(receiver Value, args []Value, ctx *Context) (Value, error) {
				file, err := TypedReceiver[*File](receiver, "seek")
				if err != nil {
					return nil, err
				}
				if err := ValidateArityRange("seek", args, 1, 2); err != nil {
					return nil, err
				}

				offset, ok := args[0].(NumberValue)
				if !ok {
					return nil, fmt.Errorf("seek() offset must be a number")
				}

				whence := 0
				if len(args) > 1 {
					if w, ok := args[1].(NumberValue); ok {
						whence = int(w)
					} else {
						return nil, fmt.Errorf("seek() whence must be a number")
					}
				}

				pos, err := file.Seek(int64(offset), whence)
				if err != nil {
					return nil, err
				}
				return NumberValue(pos), nil
			}),

		// tell method
		MakeMethod("tell", 0, "Get current file position",
			func(receiver Value, args []Value, ctx *Context) (Value, error) {
				file, err := TypedReceiver[*File](receiver, "tell")
				if err != nil {
					return nil, err
				}
				if err := ValidateArity("tell", args, 0); err != nil {
					return nil, err
				}

				pos, err := file.Tell()
				if err != nil {
					return nil, err
				}
				return NumberValue(pos), nil
			}),

		// __enter__ method
		MakeMethod("__enter__", 0, "Enter context manager",
			func(receiver Value, args []Value, ctx *Context) (Value, error) {
				file, err := TypedReceiver[*File](receiver, "__enter__")
				if err != nil {
					return nil, err
				}
				if err := ValidateArity("__enter__", args, 0); err != nil {
					return nil, err
				}
				return file.Enter()
			}),

		// __exit__ method
		MakeMethod("__exit__", 3, "Exit context manager",
			func(receiver Value, args []Value, ctx *Context) (Value, error) {
				file, err := TypedReceiver[*File](receiver, "__exit__")
				if err != nil {
					return nil, err
				}
				if err := ValidateArityRange("__exit__", args, 0, 3); err != nil {
					return nil, err
				}

				var excType, excValue, excTraceback Value = Nil, Nil, Nil
				if len(args) > 0 {
					excType = args[0]
				}
				if len(args) > 1 {
					excValue = args[1]
				}
				if len(args) > 2 {
					excTraceback = args[2]
				}

				suppress, err := file.Exit(excType, excValue, excTraceback)
				if err != nil {
					return nil, err
				}
				return BoolValue(suppress), nil
			}),

		// __iter__ method
		MakeIterMethod(),
	)

	return registry
}

// GetRegistry implements AttributeProvider
func (f *File) GetRegistry() *MethodRegistry {
	return f.registry
}

// GetBaseObject implements AttributeProvider
func (f *File) GetBaseObject() *BaseObject {
	return &f.BaseObject
}

// GetAttr implements the new simplified GetAttr pattern
func (f *File) GetAttr(name string) (Value, bool) {
	return GetAttrWithRegistry(f, name)
}
