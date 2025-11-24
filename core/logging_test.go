package core

import (
	"bytes"
	"strings"
	"testing"
)

func TestInitLogger_DefaultsToInfo(t *testing.T) {
	buf := &bytes.Buffer{}
	logger := InitLogger("", "text", nil, buf, false)

	if logger == nil {
		t.Fatal("InitLogger returned nil")
	}

	// Info should log
	logger.Info(SubsystemParser, "test message")
	if !strings.Contains(buf.String(), "test message") {
		t.Error("Info message was not logged")
	}

	// Debug should not log at info level
	buf.Reset()
	logger.Debug(SubsystemParser, "debug message")
	if strings.Contains(buf.String(), "debug message") {
		t.Error("Debug message was logged at info level")
	}
}

func TestInitLogger_DebugLevel(t *testing.T) {
	buf := &bytes.Buffer{}
	logger := InitLogger("debug", "text", nil, buf, false)

	// Debug should log at debug level
	logger.Debug(SubsystemParser, "debug message")
	if !strings.Contains(buf.String(), "debug message") {
		t.Error("Debug message was not logged at debug level")
	}
}

func TestInitLogger_ErrorLevel(t *testing.T) {
	buf := &bytes.Buffer{}
	logger := InitLogger("error", "text", nil, buf, false)

	// Info should not log at error level
	logger.Info(SubsystemParser, "info message")
	if strings.Contains(buf.String(), "info message") {
		t.Error("Info message was logged at error level")
	}

	// Error should log
	buf.Reset()
	logger.Error(SubsystemParser, "error message")
	if !strings.Contains(buf.String(), "error message") {
		t.Error("Error message was not logged")
	}
}

func TestInitLogger_JSONFormat(t *testing.T) {
	buf := &bytes.Buffer{}
	logger := InitLogger("info", "json", nil, buf, false)

	logger.Info(SubsystemParser, "test message")
	output := buf.String()

	// JSON output should contain specific fields
	if !strings.Contains(output, `"msg":"test message"`) {
		t.Error("JSON output missing msg field")
	}
	if !strings.Contains(output, `"subsystem":"parser"`) {
		t.Error("JSON output missing subsystem field")
	}
	if !strings.Contains(output, `"level":"INFO"`) {
		t.Error("JSON output missing level field")
	}
}

func TestInitLogger_TextFormat(t *testing.T) {
	buf := &bytes.Buffer{}
	logger := InitLogger("info", "text", nil, buf, false)

	logger.Info(SubsystemParser, "test message")
	output := buf.String()

	// Text output should contain the message and subsystem
	if !strings.Contains(output, "test message") {
		t.Error("Text output missing message")
	}
	if !strings.Contains(output, "subsystem=parser") {
		t.Error("Text output missing subsystem")
	}
}

func TestSubsystemEnableDisable(t *testing.T) {
	buf := &bytes.Buffer{}
	// Only enable parser subsystem
	logger := InitLogger("info", "text", []string{"parser"}, buf, false)

	// Parser subsystem should be enabled
	if !logger.IsEnabled(SubsystemParser) {
		t.Error("Parser subsystem should be enabled")
	}

	// Import subsystem should not be enabled
	if logger.IsEnabled(SubsystemImport) {
		t.Error("Import subsystem should not be enabled")
	}

	// Parser should log
	logger.Info(SubsystemParser, "parser message")
	if !strings.Contains(buf.String(), "parser message") {
		t.Error("Parser message was not logged")
	}

	// Import should not log
	buf.Reset()
	logger.Info(SubsystemImport, "import message")
	if strings.Contains(buf.String(), "import message") {
		t.Error("Import message was logged when subsystem disabled")
	}
}

func TestSubsystemEnableAll(t *testing.T) {
	buf := &bytes.Buffer{}
	// Empty subsystems list should enable all
	logger := InitLogger("info", "text", nil, buf, false)

	for _, subsystem := range AllSubsystems() {
		if !logger.IsEnabled(subsystem) {
			t.Errorf("Subsystem %s should be enabled when all are enabled", subsystem)
		}
	}
}

func TestSubsystemEnableAllExplicit(t *testing.T) {
	buf := &bytes.Buffer{}
	// Explicit "all" should enable all subsystems
	logger := InitLogger("info", "text", []string{"all"}, buf, false)

	for _, subsystem := range AllSubsystems() {
		if !logger.IsEnabled(subsystem) {
			t.Errorf("Subsystem %s should be enabled with 'all'", subsystem)
		}
	}
}

func TestEnableDisableSubsystem(t *testing.T) {
	buf := &bytes.Buffer{}
	logger := InitLogger("info", "text", []string{"parser"}, buf, false)

	// Import should be disabled initially
	if logger.IsEnabled(SubsystemImport) {
		t.Error("Import subsystem should be disabled initially")
	}

	// Enable import
	logger.EnableSubsystem(SubsystemImport)
	if !logger.IsEnabled(SubsystemImport) {
		t.Error("Import subsystem should be enabled after EnableSubsystem")
	}

	// Disable parser
	logger.DisableSubsystem(SubsystemParser)
	if logger.IsEnabled(SubsystemParser) {
		t.Error("Parser subsystem should be disabled after DisableSubsystem")
	}
}

func TestLogLevels(t *testing.T) {
	buf := &bytes.Buffer{}
	logger := InitLogger("debug", "text", []string{"parser"}, buf, false)

	tests := []struct {
		name     string
		logFunc  func(Subsystem, string, ...any)
		message  string
		expected string
	}{
		{"Debug", logger.Debug, "debug message", "level=DEBUG"},
		{"Info", logger.Info, "info message", "level=INFO"},
		{"Warn", logger.Warn, "warn message", "level=WARN"},
		{"Error", logger.Error, "error message", "level=ERROR"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			buf.Reset()
			tt.logFunc(SubsystemParser, tt.message)
			output := buf.String()

			if !strings.Contains(output, tt.message) {
				t.Errorf("Output missing message: %s", tt.message)
			}
			if !strings.Contains(output, tt.expected) {
				t.Errorf("Output missing level: %s", tt.expected)
			}
		})
	}
}

func TestLogAttributes(t *testing.T) {
	buf := &bytes.Buffer{}
	logger := InitLogger("info", "text", []string{"parser"}, buf, false)

	logger.Info(SubsystemParser, "test message", "file", "test.m28", "line", 42)
	output := buf.String()

	if !strings.Contains(output, "file=test.m28") {
		t.Error("Output missing file attribute")
	}
	if !strings.Contains(output, "line=42") {
		t.Error("Output missing line attribute")
	}
}

func TestTraceEnabled(t *testing.T) {
	buf := &bytes.Buffer{}
	logger := InitLogger("debug", "text", []string{"parser"}, buf, true)

	if !logger.IsTraceEnabled() {
		t.Error("Trace should be enabled")
	}

	// Trace should log when enabled
	logger.Trace(SubsystemParser, "trace message")
	if !strings.Contains(buf.String(), "trace message") {
		t.Error("Trace message was not logged")
	}
	if !strings.Contains(buf.String(), "level=trace") {
		t.Error("Trace message missing trace level attribute")
	}
}

func TestTraceDisabled(t *testing.T) {
	buf := &bytes.Buffer{}
	logger := InitLogger("debug", "text", []string{"parser"}, buf, false)

	if logger.IsTraceEnabled() {
		t.Error("Trace should be disabled")
	}

	// Trace should not log when disabled
	logger.Trace(SubsystemParser, "trace message")
	if strings.Contains(buf.String(), "trace message") {
		t.Error("Trace message was logged when trace disabled")
	}
}

func TestTraceFunc(t *testing.T) {
	buf := &bytes.Buffer{}
	logger := InitLogger("debug", "text", []string{"parser"}, buf, true)

	func() {
		defer logger.TraceFunc(SubsystemParser, "TestFunction", "param", "value")()
		// Function body would go here
	}()

	output := buf.String()

	// Should have entry and exit traces
	if !strings.Contains(output, "→ TestFunction") {
		t.Error("Trace entry missing")
	}
	if !strings.Contains(output, "← TestFunction") {
		t.Error("Trace exit missing")
	}
	if !strings.Contains(output, "param=value") {
		t.Error("Trace attributes missing")
	}
}

func TestTraceFuncNoOp(t *testing.T) {
	buf := &bytes.Buffer{}
	logger := InitLogger("debug", "text", []string{"parser"}, buf, false)

	// TraceFunc should return no-op when trace is disabled
	func() {
		defer logger.TraceFunc(SubsystemParser, "TestFunction")()
		// Function body
	}()

	if buf.Len() > 0 {
		t.Error("TraceFunc logged when trace disabled")
	}
}

func TestNilLoggerSafety(t *testing.T) {
	var logger *M28Logger

	// These should not panic
	logger.Debug(SubsystemParser, "test")
	logger.Info(SubsystemParser, "test")
	logger.Warn(SubsystemParser, "test")
	logger.Error(SubsystemParser, "test")
	logger.Trace(SubsystemParser, "test")

	if logger.IsEnabled(SubsystemParser) {
		t.Error("Nil logger should not have enabled subsystems")
	}

	if logger.IsTraceEnabled() {
		t.Error("Nil logger should not have trace enabled")
	}

	// TraceFunc should return no-op
	func() {
		defer logger.TraceFunc(SubsystemParser, "TestFunction")()
	}()
}

func TestGlobalLogInitialized(t *testing.T) {
	// Global Log should be initialized with no-op logger
	if Log == nil {
		t.Fatal("Global Log is nil")
	}

	// Should not panic
	Log.Info(SubsystemParser, "test")

	// Should not be enabled by default (no-op)
	if Log.IsEnabled(SubsystemParser) {
		t.Error("Default global Log should not have subsystems enabled")
	}
}

func TestAllSubsystems(t *testing.T) {
	subsystems := AllSubsystems()

	expectedCount := 5
	if len(subsystems) != expectedCount {
		t.Errorf("Expected %d subsystems, got %d", expectedCount, len(subsystems))
	}

	expected := []Subsystem{
		SubsystemParser,
		SubsystemImport,
		SubsystemEval,
		SubsystemBuiltin,
		SubsystemScope,
	}

	for _, exp := range expected {
		found := false
		for _, sub := range subsystems {
			if sub == exp {
				found = true
				break
			}
		}
		if !found {
			t.Errorf("Expected subsystem %s not found", exp)
		}
	}
}

func TestMultipleSubsystems(t *testing.T) {
	buf := &bytes.Buffer{}
	logger := InitLogger("info", "text", []string{"parser", "import"}, buf, false)

	// Both should be enabled
	if !logger.IsEnabled(SubsystemParser) {
		t.Error("Parser should be enabled")
	}
	if !logger.IsEnabled(SubsystemImport) {
		t.Error("Import should be enabled")
	}

	// Others should not be enabled
	if logger.IsEnabled(SubsystemEval) {
		t.Error("Eval should not be enabled")
	}

	// Both should log
	logger.Info(SubsystemParser, "parser message")
	if !strings.Contains(buf.String(), "parser message") {
		t.Error("Parser message not logged")
	}

	buf.Reset()
	logger.Info(SubsystemImport, "import message")
	if !strings.Contains(buf.String(), "import message") {
		t.Error("Import message not logged")
	}

	// Disabled subsystem should not log
	buf.Reset()
	logger.Info(SubsystemEval, "eval message")
	if strings.Contains(buf.String(), "eval message") {
		t.Error("Eval message logged when subsystem disabled")
	}
}

func BenchmarkLoggingEnabled(b *testing.B) {
	buf := &bytes.Buffer{}
	logger := InitLogger("info", "text", []string{"parser"}, buf, false)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		logger.Info(SubsystemParser, "benchmark message", "iteration", i)
	}
}

func BenchmarkLoggingDisabled(b *testing.B) {
	buf := &bytes.Buffer{}
	logger := InitLogger("info", "text", []string{"parser"}, buf, false)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		// Import subsystem is disabled
		logger.Info(SubsystemImport, "benchmark message", "iteration", i)
	}
}

func BenchmarkTraceFunc(b *testing.B) {
	buf := &bytes.Buffer{}
	logger := InitLogger("debug", "text", []string{"parser"}, buf, true)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		func() {
			defer logger.TraceFunc(SubsystemParser, "BenchmarkFunc")()
		}()
	}
}

func BenchmarkTraceFuncDisabled(b *testing.B) {
	buf := &bytes.Buffer{}
	logger := InitLogger("debug", "text", []string{"parser"}, buf, false)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		func() {
			defer logger.TraceFunc(SubsystemParser, "BenchmarkFunc")()
		}()
	}
}

func ExampleM28Logger_Info() {
	// Initialize logger with text format at info level for parser subsystem
	logger := InitLogger("info", "text", []string{"parser"}, nil, false)

	// Log an info message
	logger.Info(SubsystemParser, "Parsing file", "filename", "test.m28", "tokens", 42)
}

func ExampleM28Logger_TraceFunc() {
	// Initialize logger with trace enabled
	logger := InitLogger("debug", "text", []string{"parser"}, nil, true)

	parseFile := func(filename string) {
		// Automatically logs entry and exit
		defer logger.TraceFunc(SubsystemParser, "parseFile", "file", filename)()

		// Function implementation here
	}

	parseFile("test.m28")
}

func ExampleInitLogger() {
	// Initialize with JSON format at debug level for multiple subsystems
	logger := InitLogger(
		"debug",                      // level
		"json",                       // format
		[]string{"parser", "import"}, // subsystems
		nil,                          // output (nil = stderr)
		true,                         // trace enabled
	)

	logger.Debug(SubsystemParser, "Debug message", "key", "value")
}
