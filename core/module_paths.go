package core

import (
	"os"
	"path/filepath"
	"strings"
)

var modulePaths []string

// SetModulePaths sets the module search paths
func SetModulePaths(paths []string) {
	modulePaths = paths
}

// GetModulePaths returns the current module search paths
func GetModulePaths() []string {
	return modulePaths
}

// FindModule searches for a module file in the module paths
func FindModule(name string) (string, error) {
	// Try direct path first
	if filepath.IsAbs(name) {
		if fileExists(name) {
			return name, nil
		}
		return "", os.ErrNotExist
	}
	
	// Convert module name to file path
	// "foo.bar" -> "foo/bar.m28"
	parts := strings.Split(name, ".")
	filename := filepath.Join(parts...) + ".m28"
	
	// Search in module paths
	for _, basePath := range modulePaths {
		fullPath := filepath.Join(basePath, filename)
		if fileExists(fullPath) {
			return fullPath, nil
		}
		
		// Also try without .m28 extension (for directories with __init__.m28)
		dirPath := filepath.Join(basePath, filepath.Join(parts...))
		initPath := filepath.Join(dirPath, "__init__.m28")
		if fileExists(initPath) {
			return initPath, nil
		}
	}
	
	return "", os.ErrNotExist
}

// fileExists checks if a file exists
func fileExists(path string) bool {
	info, err := os.Stat(path)
	return err == nil && !info.IsDir()
}