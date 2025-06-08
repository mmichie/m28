# Readline Support in M28 REPL

The M28 REPL now includes full readline support, providing a modern interactive experience similar to IPython and other advanced REPLs.

## Features

### 1. Command History Navigation
- **Up/Down Arrow Keys**: Navigate through command history
- **Ctrl+P/Ctrl+N**: Alternative history navigation (emacs mode)
- History is automatically saved to `~/.m28_history`

### 2. Line Editing
- **Left/Right Arrow Keys**: Move cursor within the line
- **Home/End**: Jump to beginning/end of line
- **Ctrl+A/Ctrl+E**: Beginning/end of line (emacs mode)
- **Ctrl+U**: Clear line before cursor
- **Ctrl+K**: Clear line after cursor
- **Ctrl+W**: Delete word before cursor

### 3. Tab Completion
- Press **Tab** to complete:
  - Built-in functions (print, len, range, etc.)
  - User-defined functions and variables
  - Keywords (def, if, for, etc.)
  - Module names after import

### 4. Reverse History Search
- **Ctrl+R**: Start reverse incremental search
- Type to search through history
- **Ctrl+R** again: Find next match
- **Enter**: Accept current match
- **Ctrl+G/Esc**: Cancel search

### 5. Vi/Emacs Mode Support
- Default: Emacs keybindings
- Switch modes with `:toggle-keybindings` command
- Vi mode:
  - **Esc**: Enter command mode
  - **i/a**: Enter insert mode
  - **hjkl**: Navigation in command mode
  - **/**: Search in command mode
  - All standard vi commands supported

### 6. Multi-line Editing
- Automatic continuation for incomplete expressions
- Smart indentation based on context
- History saves complete multi-line commands

## REPL Commands

- `:toggle-keybindings` - Switch between vi and emacs modes
- `:history [n]` - Show last n commands (default: 10)
- `:settings` - Show current REPL settings
- `:toggle-colors` - Enable/disable colored output

## Configuration

The REPL automatically:
- Saves history to `~/.m28_history`
- Loads previous history on startup
- Maintains up to 1000 commands in history

## Comparison with Basic Input

Without readline support, the REPL falls back to basic line-by-line input without:
- Arrow key navigation
- Tab completion
- History search
- Line editing capabilities

## Implementation Notes

The readline support is provided by the `github.com/chzyer/readline` library, which provides cross-platform terminal handling with a consistent interface across Linux, macOS, and Windows.