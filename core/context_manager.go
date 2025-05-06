package core

import (
	"fmt"
	"os"
)

// FileContextManager implements the ContextManager interface for file operations
type FileContextManager struct {
	Filename string
	Mode     string
	File     *os.File
}

// NewFileContextManager creates a new FileContextManager
func NewFileContextManager(filename, mode string) *FileContextManager {
	return &FileContextManager{
		Filename: filename,
		Mode:     mode,
	}
}

// Enter opens the file and returns the file object
func (f *FileContextManager) Enter() (LispValue, error) {
	file, err := os.OpenFile(f.Filename, parseFileMode(f.Mode), 0644)
	if err != nil {
		return nil, &Exception{
			Type:    "IOError",
			Message: fmt.Sprintf("Error opening file %s: %v", f.Filename, err),
		}
	}
	f.File = file
	return f, nil
}

// Exit closes the file
func (f *FileContextManager) Exit(exc LispValue) error {
	if f.File != nil {
		err := f.File.Close()
		f.File = nil
		if err != nil {
			return &Exception{
				Type:    "IOError",
				Message: fmt.Sprintf("Error closing file %s: %v", f.Filename, err),
			}
		}
	}
	return nil
}

// Read reads from the file
func (f *FileContextManager) Read() (string, error) {
	if f.File == nil {
		return "", &Exception{
			Type:    "IOError",
			Message: "File is not open",
		}
	}
	data, err := os.ReadFile(f.Filename)
	if err != nil {
		return "", &Exception{
			Type:    "IOError",
			Message: fmt.Sprintf("Error reading file %s: %v", f.Filename, err),
		}
	}
	return string(data), nil
}

// Write writes to the file
func (f *FileContextManager) Write(data string) error {
	if f.File == nil {
		return &Exception{
			Type:    "IOError",
			Message: "File is not open",
		}
	}
	_, err := f.File.WriteString(data)
	if err != nil {
		return &Exception{
			Type:    "IOError",
			Message: fmt.Sprintf("Error writing to file %s: %v", f.Filename, err),
		}
	}
	return nil
}

// String returns a string representation of the FileContextManager
func (f *FileContextManager) String() string {
	if f.File == nil {
		return fmt.Sprintf("<closed file '%s', mode '%s'>", f.Filename, f.Mode)
	}
	return fmt.Sprintf("<open file '%s', mode '%s'>", f.Filename, f.Mode)
}

// Helper function to parse file mode string to os.O_* flags
func parseFileMode(mode string) int {
	switch mode {
	case "r":
		return os.O_RDONLY
	case "w":
		return os.O_WRONLY | os.O_CREATE | os.O_TRUNC
	case "a":
		return os.O_WRONLY | os.O_CREATE | os.O_APPEND
	case "r+":
		return os.O_RDWR
	case "w+":
		return os.O_RDWR | os.O_CREATE | os.O_TRUNC
	case "a+":
		return os.O_RDWR | os.O_CREATE | os.O_APPEND
	default:
		return os.O_RDONLY
	}
}
