// Package modules provides the warnings module for M28
// Implements Python's warning system for deprecations and other warnings
package modules

import (
	"fmt"
	"log"
	"os"
	"sync"

	"github.com/mmichie/m28/core"
)

var debugWarnings = os.Getenv("M28_DEBUG_WARNINGS") != ""

// WarningFilter represents a single warning filter
type WarningFilter struct {
	Action   string // "default", "error", "ignore", "always", "module", "once"
	Category core.Value
	Module   string
	Lineno   int
}

// WarningRegistry manages warning filters and state
type WarningRegistry struct {
	filters         []*WarningFilter
	onceRegistry    map[string]bool           // Track "once" warnings
	recordingList   *core.ListValue           // When non-nil, record warnings here instead of showing
	showWarningFunc func(string, string, int) // Custom show function
	mu              sync.RWMutex
}

var globalRegistry = &WarningRegistry{
	filters:      make([]*WarningFilter, 0),
	onceRegistry: make(map[string]bool),
}

// InitWarningsModule registers the warnings module
func InitWarningsModule() *core.DictValue {
	warningsModule := core.NewDict()

	// Set default filters to match CPython's behavior
	// Python's default filters (in order of precedence):
	// 1. Show DeprecationWarning from __main__ module
	// 2. Ignore DeprecationWarning from all other modules
	// 3. Ignore PendingDeprecationWarning
	// 4. Ignore ImportWarning
	// 5. Ignore ResourceWarning
	globalRegistry.mu.Lock()
	globalRegistry.filters = []*WarningFilter{
		{Action: "default", Category: core.StringValue("DeprecationWarning"), Module: "__main__", Lineno: 0},
		{Action: "ignore", Category: core.StringValue("DeprecationWarning"), Module: "", Lineno: 0},
		{Action: "ignore", Category: core.StringValue("PendingDeprecationWarning"), Module: "", Lineno: 0},
		{Action: "ignore", Category: core.StringValue("ImportWarning"), Module: "", Lineno: 0},
		{Action: "ignore", Category: core.StringValue("ResourceWarning"), Module: "", Lineno: 0},
	}
	globalRegistry.mu.Unlock()

	// Register functions
	warningsModule.Set("warn", core.NewBuiltinFunction(warnFunc))
	warningsModule.Set("warn_explicit", core.NewBuiltinFunction(warnExplicitFunc))
	warningsModule.Set("simplefilter", core.NewBuiltinFunction(simplefilterFunc))
	warningsModule.Set("filterwarnings", core.NewBuiltinFunction(filterwarningsFunc))
	warningsModule.Set("resetwarnings", core.NewBuiltinFunction(resetwarningsFunc))

	// Expose filters as a list-like object (for compatibility with CPython)
	// Create a FiltersProxy that wraps the global filters
	filtersProxy := createFiltersProxy()
	warningsModule.Set("filters", filtersProxy)

	// catch_warnings is a class-like callable
	catchWarningsClass := createCatchWarningsClass()
	warningsModule.Set("catch_warnings", catchWarningsClass)

	return warningsModule
}

// warnFunc implements warnings.warn(message, category=UserWarning, stacklevel=1)
func warnFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("warn() missing required argument: 'message'")
	}

	message := ""
	if str, ok := args[0].(core.StringValue); ok {
		message = string(str)
	} else {
		message = core.PrintValue(args[0])
	}

	// Default category is UserWarning
	var category core.Value = core.StringValue("UserWarning")
	if len(args) >= 2 {
		category = args[1]
	}

	stacklevel := 1
	if len(args) >= 3 {
		if num, ok := args[2].(core.NumberValue); ok {
			stacklevel = int(num)
		}
	}

	return nil, emitWarning(message, category, stacklevel, ctx)
}

// warnExplicitFunc implements warnings.warn_explicit()
func warnExplicitFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 4 {
		return nil, fmt.Errorf("warn_explicit() requires at least 4 arguments")
	}

	message := core.PrintValue(args[0])
	category := args[1]
	filename := core.PrintValue(args[2])
	lineno := 0
	if num, ok := args[3].(core.NumberValue); ok {
		lineno = int(num)
	}

	return nil, emitWarningExplicit(message, category, filename, lineno, ctx)
}

// simplefilterFunc implements warnings.simplefilter(action, category=Warning, lineno=0, append=False)
func simplefilterFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("simplefilter() missing required argument: 'action'")
	}

	action := ""
	if str, ok := args[0].(core.StringValue); ok {
		action = string(str)
	} else {
		return nil, fmt.Errorf("simplefilter() action must be a string")
	}

	// Validate action
	validActions := map[string]bool{
		"default": true, "error": true, "ignore": true,
		"always": true, "module": true, "once": true,
	}
	if !validActions[action] {
		return nil, fmt.Errorf("invalid action: %s", action)
	}

	// Default category is Warning class (not nil - nil would match all categories)
	var category core.Value = core.StringValue("Warning")
	if len(args) >= 2 {
		category = args[1]
	}

	lineno := 0
	if len(args) >= 3 {
		if num, ok := args[2].(core.NumberValue); ok {
			lineno = int(num)
		}
	}

	appendToEnd := false
	if len(args) >= 4 {
		if b, ok := args[3].(core.BoolValue); ok {
			appendToEnd = bool(b)
		}
	}

	newFilter := &WarningFilter{
		Action:   action,
		Category: category,
		Module:   "",
		Lineno:   lineno,
	}

	globalRegistry.mu.Lock()
	if appendToEnd {
		globalRegistry.filters = append(globalRegistry.filters, newFilter)
	} else {
		// Insert at beginning
		globalRegistry.filters = append([]*WarningFilter{newFilter}, globalRegistry.filters...)
	}
	globalRegistry.mu.Unlock()

	if debugWarnings {
		log.Printf("[WARNINGS] simplefilter(%s) added (category=%v)", action, category)
	}

	return core.Nil, nil
}

// filterwarningsFunc implements warnings.filterwarnings()
func filterwarningsFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	// For now, delegate to simplefilter
	return simplefilterFunc(args, ctx)
}

// resetwarningsFunc implements warnings.resetwarnings()
func resetwarningsFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	globalRegistry.mu.Lock()
	// Reset to CPython's default filters
	globalRegistry.filters = []*WarningFilter{
		{Action: "default", Category: core.StringValue("DeprecationWarning"), Module: "__main__", Lineno: 0},
		{Action: "ignore", Category: core.StringValue("DeprecationWarning"), Module: "", Lineno: 0},
		{Action: "ignore", Category: core.StringValue("PendingDeprecationWarning"), Module: "", Lineno: 0},
		{Action: "ignore", Category: core.StringValue("ImportWarning"), Module: "", Lineno: 0},
		{Action: "ignore", Category: core.StringValue("ResourceWarning"), Module: "", Lineno: 0},
	}
	globalRegistry.onceRegistry = make(map[string]bool)
	globalRegistry.mu.Unlock()

	if debugWarnings {
		log.Printf("[WARNINGS] resetwarnings() called")
	}

	return core.Nil, nil
}

// emitWarning emits a warning based on current filter settings
func emitWarning(message string, category core.Value, stacklevel int, ctx *core.Context) error {
	// Get category name
	categoryName := getCategoryName(category, ctx)

	// Determine module name from context
	// Warnings from unittest code should not be treated as __main__
	moduleName := "<string>"

	// Check filters with module name
	action := getFilterAction(categoryName, moduleName)

	if debugWarnings {
		log.Printf("[WARNINGS] emit: %s: %s (module=%s, action=%s)", categoryName, message, moduleName, action)
	}

	switch action {
	case "error":
		// Turn warning into exception
		return createWarningException(categoryName, message, ctx)
	case "ignore":
		// Do nothing
		return nil
	case "once":
		// Show only first occurrence
		key := fmt.Sprintf("%s:%s", categoryName, message)
		globalRegistry.mu.Lock()
		if globalRegistry.onceRegistry[key] {
			globalRegistry.mu.Unlock()
			return nil // Already shown
		}
		globalRegistry.onceRegistry[key] = true
		globalRegistry.mu.Unlock()
		showWarning(categoryName, message, stacklevel)
	case "always":
		// Always show
		showWarning(categoryName, message, stacklevel)
	case "default", "module":
		// Show (default behavior)
		showWarning(categoryName, message, stacklevel)
	}

	return nil
}

// emitWarningExplicit emits a warning with explicit location info
func emitWarningExplicit(message string, category core.Value, filename string, lineno int, ctx *core.Context) error {
	categoryName := getCategoryName(category, ctx)

	// Extract module name from filename (use filename as module name for now)
	moduleName := filename

	action := getFilterAction(categoryName, moduleName)

	if debugWarnings {
		log.Printf("[WARNINGS] explicit: %s:%d: %s: %s (module=%s, action=%s)", filename, lineno, categoryName, message, moduleName, action)
	}

	switch action {
	case "error":
		return createWarningException(categoryName, message, ctx)
	case "ignore":
		return nil
	case "once":
		key := fmt.Sprintf("%s:%s:%d:%s", filename, categoryName, lineno, message)
		globalRegistry.mu.Lock()
		if globalRegistry.onceRegistry[key] {
			globalRegistry.mu.Unlock()
			return nil
		}
		globalRegistry.onceRegistry[key] = true
		globalRegistry.mu.Unlock()
		showWarningExplicit(categoryName, message, filename, lineno)
	default:
		showWarningExplicit(categoryName, message, filename, lineno)
	}

	return nil
}

// showWarningExplicit displays a warning with file/line info or records it
func showWarningExplicit(categoryName, message, filename string, lineno int) {
	globalRegistry.mu.RLock()
	recordingList := globalRegistry.recordingList
	globalRegistry.mu.RUnlock()

	// If recording mode is active, create a warning message object and append to list
	if recordingList != nil {
		warningMsg := core.NewDict()
		warningMsg.Set("message", core.StringValue(message))
		warningMsg.Set("category", core.StringValue(categoryName))
		warningMsg.Set("filename", core.StringValue(filename))
		warningMsg.Set("lineno", core.NumberValue(float64(lineno)))

		recordingList.Append(warningMsg)
		return
	}

	// Default: print to stderr
	fmt.Fprintf(os.Stderr, "%s:%d: %s: %s\n", filename, lineno, categoryName, message)
}

// getCategoryName extracts the warning category name
func getCategoryName(category core.Value, ctx *core.Context) string {
	if category == nil {
		return "Warning"
	}

	// If it's a string, use it directly
	if str, ok := category.(core.StringValue); ok {
		return string(str)
	}

	// If it's a class, get its name
	if class, ok := category.(*core.Class); ok {
		return class.Name
	}

	// Try to get __name__ attribute via Object interface
	if obj, ok := category.(core.Object); ok {
		if name, found := obj.GetAttr("__name__"); found {
			if str, ok := name.(core.StringValue); ok {
				return string(str)
			}
		}
	}

	return "Warning"
}

// getFilterAction determines what action to take for a warning category and module
func getFilterAction(categoryName string, moduleName string) string {
	globalRegistry.mu.RLock()
	defer globalRegistry.mu.RUnlock()

	// Check filters in order (first match wins)
	for _, filter := range globalRegistry.filters {
		// Check if module matches
		// Empty filter.Module means match all modules
		// Non-empty filter.Module must match exactly
		if filter.Module != "" && filter.Module != moduleName {
			continue // Module doesn't match, try next filter
		}

		// Check if category matches
		if filter.Category == nil {
			// No category specified - matches all categories
			return filter.Action
		}

		filterCategoryName := getCategoryName(filter.Category, nil)
		// Check for exact match or if the warning category is a subclass of the filter category
		if filterCategoryName == categoryName {
			return filter.Action
		}

		// Check if filter is for Warning base class - this should match all warning categories
		if filterCategoryName == "Warning" {
			// All warnings are subclasses of Warning
			if categoryName == "DeprecationWarning" ||
				categoryName == "PendingDeprecationWarning" ||
				categoryName == "ImportWarning" ||
				categoryName == "ResourceWarning" ||
				categoryName == "UserWarning" ||
				categoryName == "SyntaxWarning" ||
				categoryName == "RuntimeWarning" ||
				categoryName == "FutureWarning" ||
				categoryName == "BytesWarning" ||
				categoryName == "UnicodeWarning" {
				return filter.Action
			}
		}
	}

	// Default action
	return "default"
}

// showWarning displays a warning to stderr or records it if recording mode is active
func showWarning(categoryName, message string, stacklevel int) {
	globalRegistry.mu.RLock()
	recordingList := globalRegistry.recordingList
	customShow := globalRegistry.showWarningFunc
	globalRegistry.mu.RUnlock()

	// If recording mode is active, create a warning message object and append to list
	if recordingList != nil {
		// Create a simple warning message dict
		warningMsg := core.NewDict()
		warningMsg.Set("message", core.StringValue(message))
		warningMsg.Set("category", core.StringValue(categoryName))
		warningMsg.Set("filename", core.StringValue("<string>"))
		warningMsg.Set("lineno", core.NumberValue(1))

		recordingList.Append(warningMsg)
		return
	}

	// Use custom show function if provided
	if customShow != nil {
		customShow(categoryName, message, stacklevel)
		return
	}

	// Default: Format and print to stderr
	fmt.Fprintf(os.Stderr, "<string>:1: %s: %s\n", categoryName, message)
}

// createWarningException creates an exception from a warning
func createWarningException(categoryName, message string, ctx *core.Context) error {
	// Look up the warning class
	if warningClass, err := ctx.Lookup(categoryName); err == nil {
		// Try to call it as a constructor
		if constructor, ok := warningClass.(interface {
			Call([]core.Value, *core.Context) (core.Value, error)
		}); ok {
			exc, err := constructor.Call([]core.Value{core.StringValue(message)}, ctx)
			if err != nil {
				return err
			}
			// If it's an ExceptionValue, return it as error
			if excValue, ok := exc.(*core.ExceptionValue); ok {
				return excValue
			}
			// Otherwise wrap in a new exception
			return core.NewException(message)
		}
	}

	// Fallback: just return a regular error
	return fmt.Errorf("%s: %s", categoryName, message)
}

// createCatchWarningsClass creates the catch_warnings context manager class
func createCatchWarningsClass() core.Value {
	return &core.BuiltinFunctionWithKwargs{
		BaseObject: *core.NewBaseObject(core.FunctionType),
		Name:       "catch_warnings",
		Fn: func(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
			// Parse arguments - support both positional and keyword argument
			record := false

			// Check positional argument first
			if len(args) > 0 {
				if b, ok := args[0].(core.BoolValue); ok {
					record = bool(b)
				}
			}

			// Check keyword argument (overrides positional)
			if kwargs != nil {
				if recordVal, ok := kwargs["record"]; ok {
					if b, ok := recordVal.(core.BoolValue); ok {
						record = bool(b)
					}
				}
			}

			// Create instance
			instance := &CatchWarnings{
				record:         record,
				warningsList:   nil,
				savedFilters:   nil,
				savedRecording: nil,
			}

			// Return as dict with __enter__ and __exit__ methods
			obj := core.NewDict()
			obj.Set("__enter__", core.NewBuiltinFunction(instance.enter))
			obj.Set("__exit__", core.NewBuiltinFunction(instance.exit))

			return obj, nil
		},
	}
}

// CatchWarnings implements the catch_warnings context manager
type CatchWarnings struct {
	record         bool
	warningsList   *core.ListValue // The list we return and record to
	savedFilters   []*WarningFilter
	savedRecording *core.ListValue
}

func (c *CatchWarnings) enter(args []core.Value, ctx *core.Context) (core.Value, error) {
	globalRegistry.mu.Lock()
	defer globalRegistry.mu.Unlock()

	// Save current filters and recording state
	c.savedFilters = make([]*WarningFilter, len(globalRegistry.filters))
	copy(c.savedFilters, globalRegistry.filters)
	c.savedRecording = globalRegistry.recordingList

	if c.record {
		// Create a new list to record warnings
		c.warningsList = core.NewList()

		// Set global registry to record to this list
		globalRegistry.recordingList = c.warningsList

		// Install a filter that shows all warnings (so they get recorded)
		globalRegistry.filters = []*WarningFilter{
			{Action: "always", Category: nil, Module: "", Lineno: 0},
		}

		if debugWarnings {
			log.Printf("[WARNINGS] catch_warnings.__enter__() - recording enabled, list=%p", c.warningsList)
		}

		// Return the list
		return c.warningsList, nil
	}

	if debugWarnings {
		log.Printf("[WARNINGS] catch_warnings.__enter__() - recording disabled")
	}

	// Not recording, return None
	return core.Nil, nil
}

func (c *CatchWarnings) exit(args []core.Value, ctx *core.Context) (core.Value, error) {
	globalRegistry.mu.Lock()
	defer globalRegistry.mu.Unlock()

	// Restore previous state
	globalRegistry.filters = c.savedFilters
	globalRegistry.recordingList = c.savedRecording

	if debugWarnings {
		log.Printf("[WARNINGS] catch_warnings.__exit__() - restored state")
	}

	return core.False, nil // Don't suppress exceptions
}

// Warn is a helper function to emit warnings from Go code
func Warn(message string, category string, ctx *core.Context) error {
	var categoryValue core.Value
	if cat, err := ctx.Lookup(category); err == nil {
		categoryValue = cat
	} else {
		categoryValue = core.StringValue(category)
	}

	return emitWarning(message, categoryValue, 1, ctx)
}

// createFiltersProxy creates a list-like proxy for the warnings filters
func createFiltersProxy() core.Value {
	// For now, return a simple list representation
	// In a full implementation, this should be a dynamic proxy that reflects current filters
	globalRegistry.mu.RLock()
	defer globalRegistry.mu.RUnlock()

	filtersList := core.NewList()
	for _, filter := range globalRegistry.filters {
		// Create a tuple representing the filter: (action, message, category, module, lineno)
		filterTuple := core.TupleValue{
			core.StringValue(filter.Action),
			core.Nil, // message pattern (not implemented)
			filter.Category,
			core.StringValue(filter.Module),
			core.NumberValue(float64(filter.Lineno)),
		}
		filtersList.Append(filterTuple)
	}

	return filtersList
}
