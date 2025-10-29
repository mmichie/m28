package modules

import (
	"fmt"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"sync"
)

var debugImport = os.Getenv("M28_DEBUG_IMPORT") != ""

// PythonModuleFinder locates Python modules in the standard library
type PythonModuleFinder struct {
	searchPaths []string
	pathCache   map[string]string // module name → file path
	mu          sync.RWMutex
}

var (
	globalFinder    *PythonModuleFinder
	finderInitOnce  sync.Once
	finderInitError error
)

// GetPythonFinder returns the global PythonModuleFinder instance
func GetPythonFinder() (*PythonModuleFinder, error) {
	finderInitOnce.Do(func() {
		globalFinder, finderInitError = NewPythonModuleFinder()
	})
	return globalFinder, finderInitError
}

// NewPythonModuleFinder creates a new Python module finder
func NewPythonModuleFinder() (*PythonModuleFinder, error) {
	paths, err := discoverPythonPaths()
	if err != nil {
		return nil, err
	}

	return &PythonModuleFinder{
		searchPaths: paths,
		pathCache:   make(map[string]string),
	}, nil
}

// discoverPythonPaths finds Python stdlib paths by running Python
func discoverPythonPaths() ([]string, error) {
	// Try to run python3 to get sys.path
	cmd := exec.Command("python3", "-c", "import sys; print('\\n'.join(sys.path))")
	output, err := cmd.Output()
	if err != nil {
		return nil, fmt.Errorf("failed to discover Python paths: %w (is python3 installed?)", err)
	}

	// Parse output into paths
	lines := strings.Split(strings.TrimSpace(string(output)), "\n")
	var paths []string

	for _, line := range lines {
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}

		// Skip current directory
		if line == "." || line == "" {
			continue
		}

		// Check if path exists
		if info, err := os.Stat(line); err == nil && info.IsDir() {
			paths = append(paths, line)
		}
	}

	// Also respect PYTHONPATH environment variable
	if pythonPath := os.Getenv("PYTHONPATH"); pythonPath != "" {
		for _, p := range strings.Split(pythonPath, ":") {
			p = strings.TrimSpace(p)
			if p != "" {
				if info, err := os.Stat(p); err == nil && info.IsDir() {
					paths = append(paths, p)
				}
			}
		}
	}

	if len(paths) == 0 {
		return nil, fmt.Errorf("no Python paths discovered")
	}

	return paths, nil
}

// Find locates a Python module by name
// Returns (path, isPackage, error)
// - path: full path to .py file or __init__.py
// - isPackage: true if module is a package (has __init__.py)
func (f *PythonModuleFinder) Find(moduleName string) (string, bool, error) {
	if debugImport {
		log.Printf("[IMPORT] PythonFinder.Find('%s') called", moduleName)
	}

	// Check cache first
	f.mu.RLock()
	if cached, ok := f.pathCache[moduleName]; ok {
		f.mu.RUnlock()
		// Determine if it's a package
		isPackage := strings.HasSuffix(cached, "__init__.py")
		if debugImport {
			log.Printf("[IMPORT] PythonFinder.Find('%s') -> found in cache: %s (package=%v)", moduleName, cached, isPackage)
		}
		return cached, isPackage, nil
	}
	f.mu.RUnlock()

	// Convert module name to path (e.g., "os.path" → "os/path")
	modulePath := strings.ReplaceAll(moduleName, ".", string(filepath.Separator))
	if debugImport {
		log.Printf("[IMPORT] PythonFinder.Find('%s') -> searching for: %s.py or %s/__init__.py", moduleName, modulePath, modulePath)
	}

	// Search in all paths
	for i, searchPath := range f.searchPaths {
		// Try as module: name.py
		modulePyPath := filepath.Join(searchPath, modulePath+".py")
		if debugImport {
			log.Printf("[IMPORT] PythonFinder.Find('%s') -> trying [%d/%d]: %s", moduleName, i+1, len(f.searchPaths), modulePyPath)
		}
		if fileExists(modulePyPath) {
			f.mu.Lock()
			f.pathCache[moduleName] = modulePyPath
			f.mu.Unlock()
			if debugImport {
				log.Printf("[IMPORT] PythonFinder.Find('%s') -> FOUND module: %s", moduleName, modulePyPath)
			}
			return modulePyPath, false, nil
		}

		// Try as package: name/__init__.py
		packageInitPath := filepath.Join(searchPath, modulePath, "__init__.py")
		if debugImport {
			log.Printf("[IMPORT] PythonFinder.Find('%s') -> trying package: %s", moduleName, packageInitPath)
		}
		if fileExists(packageInitPath) {
			f.mu.Lock()
			f.pathCache[moduleName] = packageInitPath
			f.mu.Unlock()
			if debugImport {
				log.Printf("[IMPORT] PythonFinder.Find('%s') -> FOUND package: %s", moduleName, packageInitPath)
			}
			return packageInitPath, true, nil
		}
	}

	err := fmt.Errorf("Python module '%s' not found in stdlib paths", moduleName)
	if debugImport {
		log.Printf("[IMPORT] PythonFinder.Find('%s') -> NOT FOUND: %v", moduleName, err)
		log.Printf("[IMPORT] PythonFinder.Find('%s') -> searched %d paths: %v", moduleName, len(f.searchPaths), f.searchPaths)
	}
	return "", false, err
}

// GetSearchPaths returns the list of Python search paths
func (f *PythonModuleFinder) GetSearchPaths() []string {
	f.mu.RLock()
	defer f.mu.RUnlock()

	paths := make([]string, len(f.searchPaths))
	copy(paths, f.searchPaths)
	return paths
}

// fileExists checks if a file exists and is not a directory
func fileExists(path string) bool {
	info, err := os.Stat(path)
	if err != nil {
		return false
	}
	return !info.IsDir()
}
