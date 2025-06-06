package builtin

import (
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"

	"github.com/mmichie/m28/core"
)

// RegisterShutil registers the shutil module
func RegisterShutil(ctx *core.Context) error {
	shutil := make(map[string]core.Value)

	// copy - Copy a file
	copyFunc := func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, fmt.Errorf("copy() takes exactly 2 arguments (%d given)", len(args))
		}

		src, ok := args[0].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("copy() argument 1 must be str, not %s", args[0].Type())
		}

		dst, ok := args[1].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("copy() argument 2 must be str, not %s", args[1].Type())
		}

		srcPath := string(src)
		dstPath := string(dst)

		// Check if destination is a directory
		dstInfo, err := os.Stat(dstPath)
		if err == nil && dstInfo.IsDir() {
			// If dst is a directory, use the source filename
			dstPath = filepath.Join(dstPath, filepath.Base(srcPath))
		}

		// Open source file
		srcFile, err := os.Open(srcPath)
		if err != nil {
			return nil, fmt.Errorf("copy: %v", err)
		}
		defer srcFile.Close()

		// Create destination file
		dstFile, err := os.Create(dstPath)
		if err != nil {
			return nil, fmt.Errorf("copy: %v", err)
		}
		defer dstFile.Close()

		// Copy the file
		_, err = io.Copy(dstFile, srcFile)
		if err != nil {
			return nil, fmt.Errorf("copy: %v", err)
		}

		// Copy file permissions
		srcInfo, err := os.Stat(srcPath)
		if err == nil {
			err = os.Chmod(dstPath, srcInfo.Mode())
		}

		return core.StringValue(dstPath), nil
	}
	shutil["copy"] = core.NewBuiltinFunction(copyFunc)

	// copy2 - Copy a file preserving metadata
	shutil["copy2"] = shutil["copy"] // For now, same as copy

	// move - Move a file or directory
	moveFunc := func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, fmt.Errorf("move() takes exactly 2 arguments (%d given)", len(args))
		}

		src, ok := args[0].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("move() argument 1 must be str, not %s", args[0].Type())
		}

		dst, ok := args[1].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("move() argument 2 must be str, not %s", args[1].Type())
		}

		srcPath := string(src)
		dstPath := string(dst)

		// Check if destination is a directory
		dstInfo, err := os.Stat(dstPath)
		if err == nil && dstInfo.IsDir() {
			// If dst is a directory, use the source filename
			dstPath = filepath.Join(dstPath, filepath.Base(srcPath))
		}

		err = os.Rename(srcPath, dstPath)
		if err != nil {
			return nil, fmt.Errorf("move: %v", err)
		}

		return core.StringValue(dstPath), nil
	}
	shutil["move"] = core.NewBuiltinFunction(moveFunc)

	// rmtree - Remove a directory tree
	rmtreeFunc := func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 || len(args) > 2 {
			return nil, fmt.Errorf("rmtree() takes 1 or 2 arguments (%d given)", len(args))
		}

		path, ok := args[0].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("rmtree() argument must be str, not %s", args[0].Type())
		}

		// ignore_errors parameter (default False)
		ignoreErrors := false
		if len(args) == 2 {
			if b, ok := args[1].(core.BoolValue); ok {
				ignoreErrors = bool(b)
			}
		}

		err := os.RemoveAll(string(path))
		if err != nil && !ignoreErrors {
			return nil, fmt.Errorf("rmtree: %v", err)
		}

		return core.None, nil
	}
	shutil["rmtree"] = core.NewBuiltinFunction(rmtreeFunc)

	// copytree - Copy an entire directory tree
	shutil["copytree"] = core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, fmt.Errorf("copytree() takes exactly 2 arguments (%d given)", len(args))
		}

		src, ok := args[0].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("copytree() argument 1 must be str, not %s", args[0].Type())
		}

		dst, ok := args[1].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("copytree() argument 2 must be str, not %s", args[1].Type())
		}

		err := copyDir(string(src), string(dst))
		if err != nil {
			return nil, fmt.Errorf("copytree: %v", err)
		}

		return core.StringValue(dst), nil
	})

	// which - Find a program in PATH
	shutil["which"] = core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("which() takes exactly 1 argument (%d given)", len(args))
		}

		cmd, ok := args[0].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("which() argument must be str, not %s", args[0].Type())
		}

		cmdPath, err := exec.LookPath(string(cmd))
		if err != nil {
			return core.None, nil
		}

		return core.StringValue(cmdPath), nil
	})

	// disk_usage - Get disk usage statistics
	shutil["disk_usage"] = core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("disk_usage() takes exactly 1 argument (%d given)", len(args))
		}

		_, ok := args[0].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("disk_usage() argument must be str, not %s", args[0].Type())
		}

		// For now, return a simple tuple with placeholder values
		// In a real implementation, we'd use syscalls to get actual disk usage
		return core.TupleValue{
			core.NumberValue(0), // total
			core.NumberValue(0), // used
			core.NumberValue(0), // free
		}, nil
	})

	// Create module as a dict
	shutilModule := core.NewDict()
	for name, fn := range shutil {
		shutilModule.SetWithKey(name, core.StringValue(name), fn)
	}

	// Register the module in the module registry
	registry := core.GetModuleRegistry()
	registry.StoreModule("shutil", shutilModule, "<builtin>", []string{})

	return nil
}

// copyDir recursively copies a directory
func copyDir(src, dst string) error {
	// Get source directory info
	srcInfo, err := os.Stat(src)
	if err != nil {
		return err
	}

	// Create destination directory
	err = os.MkdirAll(dst, srcInfo.Mode())
	if err != nil {
		return err
	}

	// Read directory contents
	entries, err := os.ReadDir(src)
	if err != nil {
		return err
	}

	// Copy each entry
	for _, entry := range entries {
		srcPath := filepath.Join(src, entry.Name())
		dstPath := filepath.Join(dst, entry.Name())

		if entry.IsDir() {
			// Recursively copy subdirectory
			err = copyDir(srcPath, dstPath)
			if err != nil {
				return err
			}
		} else {
			// Copy file
			err = copyFile(srcPath, dstPath)
			if err != nil {
				return err
			}
		}
	}

	return nil
}

// copyFile copies a single file
func copyFile(src, dst string) error {
	srcFile, err := os.Open(src)
	if err != nil {
		return err
	}
	defer srcFile.Close()

	dstFile, err := os.Create(dst)
	if err != nil {
		return err
	}
	defer dstFile.Close()

	_, err = io.Copy(dstFile, srcFile)
	if err != nil {
		return err
	}

	// Copy file permissions
	srcInfo, err := os.Stat(src)
	if err == nil {
		err = os.Chmod(dst, srcInfo.Mode())
	}

	return err
}
