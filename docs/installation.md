# Installation Guide

This guide will help you install M28 on your system.

## Prerequisites

- Go 1.19 or later
- Git
- Make (optional but recommended)

## Installation Methods

### Method 1: Build from Source (Recommended)

1. **Clone the repository**
   ```bash
   git clone https://github.com/mmichie/m28.git
   cd m28
   ```

2. **Build using Make**
   ```bash
   make build
   ```
   
   Or build directly with Go:
   ```bash
   go build -o bin/m28 main.go
   ```

3. **Verify installation**
   ```bash
   ./bin/m28 --version
   # Output: M28 v0.1.0
   ```

### Method 2: Install to System Path

After building, you can install M28 system-wide:

```bash
# Option 1: Install to /usr/local/bin (requires sudo)
sudo cp bin/m28 /usr/local/bin/

# Option 2: Install to user's local bin
mkdir -p ~/.local/bin
cp bin/m28 ~/.local/bin/
# Add ~/.local/bin to your PATH if not already there
```

### Method 3: Using Go Install (Coming Soon)

In future releases:
```bash
go install github.com/mmichie/m28@latest
```

## Running M28

### Interactive REPL
```bash
m28
# or if not in PATH:
./bin/m28
```

### Run a Script
```bash
m28 script.m28
# or
./bin/m28 script.m28
```

### Evaluate Expression
```bash
m28 -e '(print "Hello, M28!")'
```

### Check Version
```bash
m28 --version
```

## Setting Up Your Environment

### Module Path
M28 looks for modules in these locations:
1. Current directory
2. Directory of the main script
3. `M28_PATH` environment variable directories

Set up module path:
```bash
export M28_PATH="$HOME/.m28/modules:/usr/local/lib/m28"
```

### Editor Support

Currently, M28 files use the `.m28` extension. For syntax highlighting, you can:

1. **Vim/Neovim**: Use Lisp mode
   ```vim
   autocmd BufRead,BufNewFile *.m28 setfiletype lisp
   ```

2. **VS Code**: Install a Lisp extension and associate `.m28` files
3. **Emacs**: Use lisp-mode or scheme-mode

## Verifying Installation

Run the test suite to ensure everything is working:

```bash
# Run all tests
make test

# Run M28 test suite
./run-tests.sh

# Run a simple test
m28 -e '(print (+ 1 2 3))'
# Output: 6
```

## Troubleshooting

### Command not found
Make sure M28 is in your PATH or use the full path to the binary.

### Module not found errors
Check that modules are in the current directory or set `M28_PATH`.

### Build errors
Ensure you have Go 1.19+ installed:
```bash
go version
```

## Next Steps

- Read the [Tutorial](tutorial.md) to learn M28
- Explore the [Examples](../examples/README.md)
- Check the [Quick Reference](quick-reference.md) for syntax help