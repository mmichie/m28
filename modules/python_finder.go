package modules

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"sync"

	"github.com/mmichie/m28/core"
)

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
	return f.FindWithExtraPaths(moduleName, nil)
}

// FindWithExtraPaths locates a Python module, checking extraPaths first, then default paths
func (f *PythonModuleFinder) FindWithExtraPaths(moduleName string, extraPaths []string) (string, bool, error) {
	core.Log.Trace(core.SubsystemImport, "Python finder searching for module", "module", moduleName)

	// Special case: "." refers to the current working directory as a package
	if moduleName == "." {
		core.Log.Debug(core.SubsystemImport, "Handling special case: current directory import", "module", moduleName)
		cwd, err := os.Getwd()
		if err != nil {
			return "", false, fmt.Errorf("cannot import '.': failed to get current working directory: %w", err)
		}

		// Check if current directory has __init__.py (is a package)
		initPath := filepath.Join(cwd, "__init__.py")
		if fileExists(initPath) {
			core.Log.Debug(core.SubsystemImport, "Current directory is importable package", "module", moduleName, "path", initPath)
			f.mu.Lock()
			f.pathCache[moduleName] = initPath
			f.mu.Unlock()
			return initPath, true, nil
		}

		// Current directory is not a package - return it anyway but mark as not a package
		// This allows unittest and other tools to use the directory for test discovery
		// even if it doesn't have __init__.py
		core.Log.Debug(core.SubsystemImport, "Current directory has no __init__.py, treating as implicit package", "module", moduleName, "path", cwd)
		// Return a special marker that indicates this is a directory without __init__.py
		// We'll return the directory path itself
		f.mu.Lock()
		f.pathCache[moduleName] = cwd
		f.mu.Unlock()
		return cwd, false, nil
	}

	// Check cache first
	f.mu.RLock()
	if cached, ok := f.pathCache[moduleName]; ok {
		f.mu.RUnlock()
		// Determine if it's a package
		isPackage := strings.HasSuffix(cached, "__init__.py")
		core.Log.Debug(core.SubsystemImport, "Module path found in cache", "module", moduleName, "path", cached, "is_package", isPackage, "cache_status", "hit")
		return cached, isPackage, nil
	}
	f.mu.RUnlock()

	// Convert module name to path (e.g., "os.path" → "os/path")
	modulePath := strings.ReplaceAll(moduleName, ".", string(filepath.Separator))
	core.Log.Debug(core.SubsystemImport, "Searching for Python file", "module", moduleName, "file_pattern", modulePath+".py", "package_pattern", modulePath+"/__init__.py")

	// Combine extra paths with default search paths (extra paths first)
	allPaths := extraPaths
	allPaths = append(allPaths, f.searchPaths...)

	if len(extraPaths) > 0 {
		core.Log.Debug(core.SubsystemImport, "Using extra search paths", "module", moduleName, "extra_paths", len(extraPaths), "total_paths", len(allPaths))
	}

	// Search in all paths
	for i, searchPath := range allPaths {
		// Try as module: name.py
		modulePyPath := filepath.Join(searchPath, modulePath+".py")
		core.Log.Trace(core.SubsystemImport, "Checking search path", "module", moduleName, "path_index", i+1, "total_paths", len(allPaths), "checking", modulePyPath)
		if fileExists(modulePyPath) {
			f.mu.Lock()
			f.pathCache[moduleName] = modulePyPath
			f.mu.Unlock()
			core.Log.Debug(core.SubsystemImport, "Python module file found", "module", moduleName, "path", modulePyPath, "is_package", false)
			return modulePyPath, false, nil
		}

		// Try as package: name/__init__.py
		packageInitPath := filepath.Join(searchPath, modulePath, "__init__.py")
		core.Log.Trace(core.SubsystemImport, "Checking for package", "module", moduleName, "checking", packageInitPath)
		if fileExists(packageInitPath) {
			f.mu.Lock()
			f.pathCache[moduleName] = packageInitPath
			f.mu.Unlock()
			core.Log.Debug(core.SubsystemImport, "Python package found", "module", moduleName, "path", packageInitPath, "is_package", true)
			return packageInitPath, true, nil
		}
	}

	err := fmt.Errorf("Python module '%s' not found in stdlib paths", moduleName)
	core.Log.Debug(core.SubsystemImport, "Python module not found in any search path", "module", moduleName, "paths_searched", len(allPaths))
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
