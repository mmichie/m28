package core

import (
	"fmt"
	"os"
	"strings"
)

// TraceConfig controls what gets traced
type TraceConfig struct {
	Enabled        bool     // Master switch
	FunctionCalls  bool     // Trace function entry/exit
	Variables      bool     // Trace variable assignments
	ModuleFilter   []string // Only trace these modules (empty = all)
	FunctionFilter []string // Only trace these functions (empty = all)
	MaxDepth       int      // Maximum call depth to trace (0 = unlimited)
}

var globalTraceConfig *TraceConfig

// InitTraceConfig initializes tracing from environment variables
func InitTraceConfig() {
	cfg := &TraceConfig{
		MaxDepth: 0,
	}

	// M28_TRACE=1 enables tracing
	if os.Getenv("M28_TRACE") == "1" {
		cfg.Enabled = true
		cfg.FunctionCalls = true
		fmt.Fprintf(os.Stderr, "[TRACE INIT] Tracing enabled via M28_TRACE=1\n")
	}

	// M28_TRACE_VARS=1 enables variable tracing
	if os.Getenv("M28_TRACE_VARS") == "1" {
		cfg.Enabled = true
		cfg.Variables = true
	}

	// M28_TRACE_MODULE=inspect,dataclasses filters by module
	if moduleFilter := os.Getenv("M28_TRACE_MODULE"); moduleFilter != "" {
		cfg.Enabled = true
		cfg.ModuleFilter = strings.Split(moduleFilter, ",")
	}

	// M28_TRACE_FUNC=_signature_from_function filters by function name
	if funcFilter := os.Getenv("M28_TRACE_FUNC"); funcFilter != "" {
		cfg.Enabled = true
		cfg.FunctionFilter = strings.Split(funcFilter, ",")
	}

	// M28_TRACE_DEPTH=5 limits call depth
	if depth := os.Getenv("M28_TRACE_DEPTH"); depth != "" {
		fmt.Sscanf(depth, "%d", &cfg.MaxDepth)
	}

	globalTraceConfig = cfg
}

// GetTraceConfig returns the global trace configuration
func GetTraceConfig() *TraceConfig {
	if globalTraceConfig == nil {
		InitTraceConfig()
	}
	return globalTraceConfig
}

// ShouldTraceModule returns true if this module should be traced
func (cfg *TraceConfig) ShouldTraceModule(moduleName string) bool {
	if !cfg.Enabled {
		return false
	}
	if len(cfg.ModuleFilter) == 0 {
		return true
	}
	for _, m := range cfg.ModuleFilter {
		if strings.Contains(moduleName, m) {
			return true
		}
	}
	return false
}

// ShouldTraceFunction returns true if this function should be traced
func (cfg *TraceConfig) ShouldTraceFunction(funcName string) bool {
	if !cfg.Enabled {
		return false
	}
	if len(cfg.FunctionFilter) == 0 {
		return true
	}
	for _, f := range cfg.FunctionFilter {
		if strings.Contains(funcName, f) {
			return true
		}
	}
	return false
}

var traceDepth int = 0

// TraceEnterFunction logs function entry
func TraceEnterFunction(funcName string, args []Value) {
	cfg := GetTraceConfig()
	if !cfg.Enabled || !cfg.FunctionCalls {
		return
	}
	if !cfg.ShouldTraceFunction(funcName) {
		return
	}
	if cfg.MaxDepth > 0 && traceDepth >= cfg.MaxDepth {
		return
	}

	indent := strings.Repeat("  ", traceDepth)
	fmt.Fprintf(os.Stderr, "[TRACE] %s→ %s(", indent, funcName)
	for i, arg := range args {
		if i > 0 {
			fmt.Fprintf(os.Stderr, ", ")
		}
		fmt.Fprintf(os.Stderr, "%v", PrintValue(arg))
	}
	fmt.Fprintf(os.Stderr, ")\n")

	traceDepth++
}

// TraceExitFunction logs function exit
func TraceExitFunction(funcName string, result Value, err error) {
	cfg := GetTraceConfig()
	if !cfg.Enabled || !cfg.FunctionCalls {
		return
	}
	if !cfg.ShouldTraceFunction(funcName) {
		return
	}

	traceDepth--
	if cfg.MaxDepth > 0 && traceDepth >= cfg.MaxDepth {
		return
	}

	indent := strings.Repeat("  ", traceDepth)
	if err != nil {
		fmt.Fprintf(os.Stderr, "[TRACE] %s← %s = ERROR: %v\n", indent, funcName, err)
	} else {
		fmt.Fprintf(os.Stderr, "[TRACE] %s← %s = %v\n", indent, funcName, PrintValue(result))
	}
}

// TraceVariable logs a variable assignment
func TraceVariable(varName string, value Value, location string) {
	cfg := GetTraceConfig()
	if !cfg.Enabled || !cfg.Variables {
		return
	}

	indent := strings.Repeat("  ", traceDepth)
	fmt.Fprintf(os.Stderr, "[TRACE] %s  %s = %v  [%s]\n", indent, varName, PrintValue(value), location)
}

// TraceValue logs an arbitrary value with a label
func TraceValue(label string, value Value) {
	cfg := GetTraceConfig()
	if !cfg.Enabled {
		return
	}

	indent := strings.Repeat("  ", traceDepth)
	fmt.Fprintf(os.Stderr, "[TRACE] %s  %s: %v (type: %T)\n", indent, label, PrintValue(value), value)
}
