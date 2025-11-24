package core

import (
	"io"
	"log/slog"
	"os"
	"strings"
)

// Subsystem represents a component of M28 that can be independently logged
type Subsystem string

// Available subsystems for logging
const (
	SubsystemParser  Subsystem = "parser"
	SubsystemImport  Subsystem = "import"
	SubsystemEval    Subsystem = "eval"
	SubsystemBuiltin Subsystem = "builtin"
	SubsystemScope   Subsystem = "scope"
)

// AllSubsystems returns a slice of all available subsystems
func AllSubsystems() []Subsystem {
	return []Subsystem{
		SubsystemParser,
		SubsystemImport,
		SubsystemEval,
		SubsystemBuiltin,
		SubsystemScope,
	}
}

// M28Logger wraps slog.Logger with subsystem-aware logging
type M28Logger struct {
	logger            *slog.Logger
	enabledSubsystems map[Subsystem]bool
	traceEnabled      bool
}

// InitLogger creates and initializes a new M28Logger with the specified configuration
// level: logging level (debug, info, warn, error)
// format: output format (json, text)
// subsystems: list of subsystems to enable (empty means all enabled)
// output: where to write logs (nil defaults to os.Stderr)
// trace: whether to enable trace-level logging
func InitLogger(level string, format string, subsystems []string, output io.Writer, trace bool) *M28Logger {
	// Default to stderr if no output specified
	if output == nil {
		output = os.Stderr
	}

	// Parse log level
	var slogLevel slog.Level
	switch strings.ToLower(level) {
	case "debug":
		slogLevel = slog.LevelDebug
	case "info":
		slogLevel = slog.LevelInfo
	case "warn", "warning":
		slogLevel = slog.LevelWarn
	case "error":
		slogLevel = slog.LevelError
	default:
		slogLevel = slog.LevelInfo
	}

	// Create handler based on format
	var handler slog.Handler
	opts := &slog.HandlerOptions{
		Level: slogLevel,
	}

	switch strings.ToLower(format) {
	case "json":
		handler = slog.NewJSONHandler(output, opts)
	case "text", "":
		handler = slog.NewTextHandler(output, opts)
	default:
		handler = slog.NewTextHandler(output, opts)
	}

	// Build enabled subsystems map
	enabledSubsystems := make(map[Subsystem]bool)
	if len(subsystems) == 0 || contains(subsystems, "all") {
		// Enable all subsystems
		for _, s := range AllSubsystems() {
			enabledSubsystems[s] = true
		}
	} else {
		// Enable only specified subsystems
		for _, s := range subsystems {
			enabledSubsystems[Subsystem(s)] = true
		}
	}

	return &M28Logger{
		logger:            slog.New(handler),
		enabledSubsystems: enabledSubsystems,
		traceEnabled:      trace,
	}
}

// contains checks if a string slice contains a specific string
func contains(slice []string, item string) bool {
	for _, s := range slice {
		if s == item {
			return true
		}
	}
	return false
}

// IsEnabled checks if a subsystem is enabled for logging
func (l *M28Logger) IsEnabled(subsystem Subsystem) bool {
	if l == nil {
		return false
	}
	return l.enabledSubsystems[subsystem]
}

// IsTraceEnabled returns whether trace logging is enabled
func (l *M28Logger) IsTraceEnabled() bool {
	if l == nil {
		return false
	}
	return l.traceEnabled
}

// EnableSubsystem enables logging for a specific subsystem
func (l *M28Logger) EnableSubsystem(subsystem Subsystem) {
	if l != nil {
		l.enabledSubsystems[subsystem] = true
	}
}

// DisableSubsystem disables logging for a specific subsystem
func (l *M28Logger) DisableSubsystem(subsystem Subsystem) {
	if l != nil {
		l.enabledSubsystems[subsystem] = false
	}
}

// Debug logs a debug message if the subsystem is enabled
func (l *M28Logger) Debug(subsystem Subsystem, msg string, attrs ...any) {
	if l != nil && l.IsEnabled(subsystem) {
		l.logger.Debug(msg, append([]any{"subsystem", string(subsystem)}, attrs...)...)
	}
}

// Info logs an info message if the subsystem is enabled
func (l *M28Logger) Info(subsystem Subsystem, msg string, attrs ...any) {
	if l != nil && l.IsEnabled(subsystem) {
		l.logger.Info(msg, append([]any{"subsystem", string(subsystem)}, attrs...)...)
	}
}

// Warn logs a warning message if the subsystem is enabled
func (l *M28Logger) Warn(subsystem Subsystem, msg string, attrs ...any) {
	if l != nil && l.IsEnabled(subsystem) {
		l.logger.Warn(msg, append([]any{"subsystem", string(subsystem)}, attrs...)...)
	}
}

// Error logs an error message if the subsystem is enabled
func (l *M28Logger) Error(subsystem Subsystem, msg string, attrs ...any) {
	if l != nil && l.IsEnabled(subsystem) {
		l.logger.Error(msg, append([]any{"subsystem", string(subsystem)}, attrs...)...)
	}
}

// Trace logs a trace message if both the subsystem and trace are enabled
// This is meant for very detailed logging like function entry/exit
func (l *M28Logger) Trace(subsystem Subsystem, msg string, attrs ...any) {
	if l != nil && l.IsEnabled(subsystem) && l.IsTraceEnabled() {
		l.logger.Debug(msg, append([]any{"subsystem", string(subsystem), "level", "trace"}, attrs...)...)
	}
}

// TraceFunc returns a function for tracing function entry/exit with defer
// Usage: defer Log.TraceFunc(SubsystemParser, "ParseFile", "file", filename)()
func (l *M28Logger) TraceFunc(subsystem Subsystem, funcName string, attrs ...any) func() {
	if l != nil && l.IsEnabled(subsystem) && l.IsTraceEnabled() {
		l.Trace(subsystem, "→ "+funcName, attrs...)
		return func() {
			l.Trace(subsystem, "← "+funcName, attrs...)
		}
	}
	// Return no-op function if tracing is disabled
	return func() {}
}

// Log is the global logger instance
// It should be initialized via InitLogger before use
var Log *M28Logger

// init provides a default logger that discards all output
// This prevents nil pointer errors if Log is used before InitLogger is called
func init() {
	// Create a no-op logger that discards everything
	Log = &M28Logger{
		logger:            slog.New(slog.NewTextHandler(io.Discard, nil)),
		enabledSubsystems: make(map[Subsystem]bool),
		traceEnabled:      false,
	}
}
