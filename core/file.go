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

	var lines ListValue
	scanner := bufio.NewScanner(f.reader)
	
	for scanner.Scan() {
		lines = append(lines, StringValue(scanner.Text() + "\n"))
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

// GetAttr implements the Object interface
func (f *File) GetAttr(name string) (Value, bool) {
	switch name {
	case "read":
		return &BoundMethod{
			Receiver: f,
			Method: &MethodDescriptor{
				Name:    "read",
				Arity:   -1, // Variable args
				Doc:     "Read from file. read() reads entire file, read(n) reads n bytes",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					file := receiver.(*File)
					size := -1
					if len(args) > 0 {
						if num, ok := args[0].(NumberValue); ok {
							size = int(num)
						} else {
							return nil, fmt.Errorf("read() argument must be a number")
						}
					}
					return file.Read(size)
				},
			},
			TypeDesc: GetTypeDescriptorForValue(f),
		}, true
		
	case "write":
		return &BoundMethod{
			Receiver: f,
			Method: &MethodDescriptor{
				Name:    "write",
				Arity:   1,
				Doc:     "Write string to file",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					if len(args) != 1 {
						return nil, fmt.Errorf("write() takes exactly one argument")
					}
					file := receiver.(*File)
					// For strings, write the actual string content, not the repr
					var data string
					if s, ok := args[0].(StringValue); ok {
						data = string(s)
					} else {
						data = PrintValue(args[0])
					}
					err := file.Write(data)
					if err != nil {
						return nil, err
					}
					return NumberValue(len(data)), nil // Return number of bytes written
				},
			},
			TypeDesc: GetTypeDescriptorForValue(f),
		}, true
		
	case "readline":
		return &BoundMethod{
			Receiver: f,
			Method: &MethodDescriptor{
				Name:    "readline",
				Arity:   0,
				Doc:     "Read a single line from file",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					file := receiver.(*File)
					return file.ReadLine()
				},
			},
			TypeDesc: GetTypeDescriptorForValue(f),
		}, true
		
	case "readlines":
		return &BoundMethod{
			Receiver: f,
			Method: &MethodDescriptor{
				Name:    "readlines",
				Arity:   0,
				Doc:     "Read all lines from file into a list",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					file := receiver.(*File)
					return file.ReadLines()
				},
			},
			TypeDesc: GetTypeDescriptorForValue(f),
		}, true
		
	case "close":
		return &BoundMethod{
			Receiver: f,
			Method: &MethodDescriptor{
				Name:    "close",
				Arity:   0,
				Doc:     "Close the file",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					file := receiver.(*File)
					err := file.Close()
					if err != nil {
						return nil, err
					}
					return Nil, nil
				},
			},
			TypeDesc: GetTypeDescriptorForValue(f),
		}, true
		
	case "seek":
		return &BoundMethod{
			Receiver: f,
			Method: &MethodDescriptor{
				Name:    "seek",
				Arity:   -1,
				Doc:     "Change file position. seek(offset, whence=0)",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					if len(args) < 1 || len(args) > 2 {
						return nil, fmt.Errorf("seek() takes 1 or 2 arguments")
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
					
					file := receiver.(*File)
					pos, err := file.Seek(int64(offset), whence)
					if err != nil {
						return nil, err
					}
					return NumberValue(pos), nil
				},
			},
			TypeDesc: GetTypeDescriptorForValue(f),
		}, true
		
	case "tell":
		return &BoundMethod{
			Receiver: f,
			Method: &MethodDescriptor{
				Name:    "tell",
				Arity:   0,
				Doc:     "Get current file position",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					file := receiver.(*File)
					pos, err := file.Tell()
					if err != nil {
						return nil, err
					}
					return NumberValue(pos), nil
				},
			},
			TypeDesc: GetTypeDescriptorForValue(f),
		}, true
		
	case "closed":
		return BoolValue(f.closed), true
		
	case "name":
		return StringValue(f.Path), true
		
	case "mode":
		return StringValue(f.Mode), true
		
	case "__enter__":
		return &BoundMethod{
			Receiver: f,
			Method: &MethodDescriptor{
				Name:    "__enter__",
				Arity:   0,
				Doc:     "Enter context manager",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					file := receiver.(*File)
					return file.Enter()
				},
			},
			TypeDesc: GetTypeDescriptorForValue(f),
		}, true
		
	case "__exit__":
		return &BoundMethod{
			Receiver: f,
			Method: &MethodDescriptor{
				Name:    "__exit__",
				Arity:   3,
				Doc:     "Exit context manager",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					file := receiver.(*File)
					var excType, excValue, excTraceback Value
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
				},
			},
			TypeDesc: GetTypeDescriptorForValue(f),
		}, true
	}
	
	return f.BaseObject.GetAttr(name)
}