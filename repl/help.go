package repl

import (
	"fmt"
	"io"
	"sort"
	"strings"
	
	"github.com/mmichie/m28/core"
)

// HelpSystem provides help and documentation for the REPL
type HelpSystem struct {
	ctx *core.Context
}

// NewHelpSystem creates a new help system
func NewHelpSystem(ctx *core.Context) *HelpSystem {
	return &HelpSystem{ctx: ctx}
}

// ShowHelp displays help for a specific topic or general help
func (h *HelpSystem) ShowHelp(topic string, w io.Writer) {
	if topic == "" {
		h.showGeneralHelp(w)
		return
	}
	
	// Try to find documentation for the topic
	if val, err := h.ctx.Lookup(topic); err == nil {
		h.showValueHelp(topic, val, w)
		return
	}
	
	// Check for special topics
	switch topic {
	case "syntax":
		h.showSyntaxHelp(w)
	case "types":
		h.showTypesHelp(w)
	case "special-forms":
		h.showSpecialFormsHelp(w)
	case "functions":
		h.showFunctionsHelp(w)
	case "modules":
		h.showModulesHelp(w)
	case "async":
		h.showAsyncHelp(w)
	default:
		fmt.Fprintf(w, "No help available for '%s'\n", topic)
		fmt.Fprintln(w, "Try 'help' for general help or 'help topics' for available topics.")
	}
}

// showGeneralHelp displays general help information
func (h *HelpSystem) showGeneralHelp(w io.Writer) {
	fmt.Fprintln(w, "M28 Language Help")
	fmt.Fprintln(w, "=================")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Basic Commands:")
	fmt.Fprintln(w, "  help          - Show this help")
	fmt.Fprintln(w, "  help <topic>  - Show help for specific topic")
	fmt.Fprintln(w, "  help <name>   - Show help for specific function/variable")
	fmt.Fprintln(w, "  exit, quit    - Exit the REPL")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Available Help Topics:")
	fmt.Fprintln(w, "  syntax        - Language syntax overview")
	fmt.Fprintln(w, "  types         - Data types")
	fmt.Fprintln(w, "  special-forms - Special forms (def, if, lambda, etc.)")
	fmt.Fprintln(w, "  functions     - Built-in functions")
	fmt.Fprintln(w, "  modules       - Module system")
	fmt.Fprintln(w, "  async         - Async/concurrent programming")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Examples:")
	fmt.Fprintln(w, "  (def x 42)           ; Define a variable")
	fmt.Fprintln(w, "  (def add (a b)       ; Define a function")
	fmt.Fprintln(w, "    (+ a b))")
	fmt.Fprintln(w, "  (add 1 2)            ; Call a function")
	fmt.Fprintln(w, "  [1, 2, 3]            ; Create a list")
	fmt.Fprintln(w, "  {\"a\": 1, \"b\": 2}    ; Create a dict")
	fmt.Fprintln(w)
}

// showValueHelp displays help for a specific value
func (h *HelpSystem) showValueHelp(name string, val core.Value, w io.Writer) {
	fmt.Fprintf(w, "Help for '%s':\n", name)
	fmt.Fprintf(w, "Type: %s\n", val.Type())
	
	// Show docstring if available
	if obj, ok := val.(core.Object); ok {
		if docVal, ok := obj.GetAttr("__doc__"); ok {
			if docStr, ok := docVal.(core.StringValue); ok {
				fmt.Fprintf(w, "\n%s\n", string(docStr))
			}
		}
	}
	
	// Show methods for objects
	if td := core.GetTypeDescriptorForValue(val); td != nil && len(td.Methods) > 0 {
		fmt.Fprintln(w, "\nMethods:")
		methods := make([]string, 0, len(td.Methods))
		for name := range td.Methods {
			methods = append(methods, name)
		}
		sort.Strings(methods)
		
		for _, name := range methods {
			method := td.Methods[name]
			fmt.Fprintf(w, "  %s(%s) - %s\n", name, h.formatArity(method.Arity), method.Doc)
		}
	}
	
	fmt.Fprintln(w)
}

// formatArity formats the arity for display
func (h *HelpSystem) formatArity(arity int) string {
	if arity < 0 {
		return "..."
	}
	params := make([]string, arity)
	for i := 0; i < arity; i++ {
		params[i] = fmt.Sprintf("arg%d", i+1)
	}
	return strings.Join(params, ", ")
}

// showSyntaxHelp displays syntax help
func (h *HelpSystem) showSyntaxHelp(w io.Writer) {
	fmt.Fprintln(w, "M28 Syntax")
	fmt.Fprintln(w, "==========")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Comments:")
	fmt.Fprintln(w, "  ; This is a comment")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Literals:")
	fmt.Fprintln(w, "  42               ; Number")
	fmt.Fprintln(w, "  3.14             ; Float")
	fmt.Fprintln(w, "  \"hello\"          ; String")
	fmt.Fprintln(w, "  true, false      ; Booleans")
	fmt.Fprintln(w, "  nil              ; Null value")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Collections:")
	fmt.Fprintln(w, "  [1, 2, 3]                ; List")
	fmt.Fprintln(w, "  (1, 2, 3)                ; Tuple")
	fmt.Fprintln(w, "  {\"a\": 1, \"b\": 2}        ; Dict")
	fmt.Fprintln(w, "  #{1, 2, 3}               ; Set")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Function Calls:")
	fmt.Fprintln(w, "  (function arg1 arg2)     ; Call with args")
	fmt.Fprintln(w, "  (obj.method arg)         ; Method call")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Variables:")
	fmt.Fprintln(w, "  (def name value)         ; Define variable")
	fmt.Fprintln(w, "  (set! name value)        ; Update variable")
	fmt.Fprintln(w)
}

// showTypesHelp displays types help
func (h *HelpSystem) showTypesHelp(w io.Writer) {
	fmt.Fprintln(w, "M28 Data Types")
	fmt.Fprintln(w, "==============")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Primitive Types:")
	fmt.Fprintln(w, "  number    - Numeric values (42, 3.14)")
	fmt.Fprintln(w, "  string    - Text values (\"hello\")")
	fmt.Fprintln(w, "  boolean   - true or false")
	fmt.Fprintln(w, "  nil       - Null value")
	fmt.Fprintln(w, "  symbol    - Identifiers (x, foo-bar)")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Collection Types:")
	fmt.Fprintln(w, "  list      - Mutable sequence [1, 2, 3]")
	fmt.Fprintln(w, "  tuple     - Immutable sequence (1, 2, 3)")
	fmt.Fprintln(w, "  dict      - Key-value mapping {\"a\": 1}")
	fmt.Fprintln(w, "  set       - Unique values #{1, 2, 3}")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Function Types:")
	fmt.Fprintln(w, "  function  - Regular functions")
	fmt.Fprintln(w, "  lambda    - Anonymous functions")
	fmt.Fprintln(w, "  builtin   - Built-in functions")
	fmt.Fprintln(w, "  method    - Bound methods")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Object Types:")
	fmt.Fprintln(w, "  class     - Class definitions")
	fmt.Fprintln(w, "  instance  - Class instances")
	fmt.Fprintln(w, "  module    - Imported modules")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Advanced Types:")
	fmt.Fprintln(w, "  generator - Generator objects")
	fmt.Fprintln(w, "  task      - Async tasks")
	fmt.Fprintln(w, "  channel   - Communication channels")
	fmt.Fprintln(w)
}

// showSpecialFormsHelp displays special forms help
func (h *HelpSystem) showSpecialFormsHelp(w io.Writer) {
	fmt.Fprintln(w, "M28 Special Forms")
	fmt.Fprintln(w, "=================")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Definition:")
	fmt.Fprintln(w, "  (def name value)         ; Define variable")
	fmt.Fprintln(w, "  (def name (args) body)   ; Define function")
	fmt.Fprintln(w, "  (set! name value)        ; Update variable")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Control Flow:")
	fmt.Fprintln(w, "  (if test then else)      ; Conditional")
	fmt.Fprintln(w, "  (cond (test1 expr1)      ; Multiple conditions")
	fmt.Fprintln(w, "        (test2 expr2))")
	fmt.Fprintln(w, "  (for x in seq body)      ; For loop")
	fmt.Fprintln(w, "  (while test body)        ; While loop")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Functions:")
	fmt.Fprintln(w, "  (lambda (args) body)     ; Anonymous function")
	fmt.Fprintln(w, "  (return value)           ; Return from function")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Exception Handling:")
	fmt.Fprintln(w, "  (try body")
	fmt.Fprintln(w, "    (except Error e body)")
	fmt.Fprintln(w, "    (finally body))")
	fmt.Fprintln(w, "  (raise exception)        ; Raise exception")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Object-Oriented:")
	fmt.Fprintln(w, "  (class Name Parent       ; Define class")
	fmt.Fprintln(w, "    (def method (self) body))")
	fmt.Fprintln(w, "  (super method args)      ; Call parent method")
	fmt.Fprintln(w)
}

// showFunctionsHelp displays built-in functions help
func (h *HelpSystem) showFunctionsHelp(w io.Writer) {
	fmt.Fprintln(w, "M28 Built-in Functions")
	fmt.Fprintln(w, "=====================")
	fmt.Fprintln(w)
	
	// Group functions by category
	categories := map[string][]string{
		"I/O": {"print", "input", "open", "read", "write"},
		"Type Conversion": {"str", "int", "float", "bool", "list", "dict", "tuple", "set"},
		"Type Checking": {"type", "isinstance", "issubclass"},
		"Math": {"abs", "min", "max", "sum", "round", "pow", "sqrt", "floor", "ceil"},
		"Sequences": {"len", "range", "enumerate", "zip", "reversed", "sorted"},
		"Functional": {"map", "filter", "reduce", "all", "any"},
		"String": {"upper", "lower", "strip", "split", "join", "replace", "find"},
		"Object": {"getattr", "setattr", "hasattr", "dir"},
	}
	
	for cat, funcs := range categories {
		fmt.Fprintf(w, "\n%s:\n", cat)
		for _, fn := range funcs {
			if val, err := h.ctx.Lookup(fn); err == nil {
				if td := core.GetTypeDescriptorForValue(val); td != nil {
					fmt.Fprintf(w, "  %-15s", fn)
					
					// Try to get brief description
					if bm, ok := val.(*core.BoundMethod); ok && bm.Method != nil {
						if bm.Method.Doc != "" {
							// Take first line of doc
							lines := strings.Split(bm.Method.Doc, "\n")
							fmt.Fprintf(w, " - %s", lines[0])
						}
					} else if bf, ok := val.(*core.BuiltinFunction); ok {
						_ = bf // Use brief description if available
						fmt.Fprint(w, " - Built-in function")
					}
					fmt.Fprintln(w)
				}
			}
		}
	}
	fmt.Fprintln(w)
}

// showModulesHelp displays module system help
func (h *HelpSystem) showModulesHelp(w io.Writer) {
	fmt.Fprintln(w, "M28 Module System")
	fmt.Fprintln(w, "=================")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Importing:")
	fmt.Fprintln(w, "  (import \"module\")              ; Import module")
	fmt.Fprintln(w, "  (import \"module\" as m)         ; Import with alias")
	fmt.Fprintln(w, "  (from \"module\" import x y)     ; Import specific items")
	fmt.Fprintln(w, "  (from \"module\" import *)       ; Import all exports")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Exporting:")
	fmt.Fprintln(w, "  (export name)                  ; Export a binding")
	fmt.Fprintln(w, "  (export (name1 name2))         ; Export multiple")
	fmt.Fprintln(w, "  (export *)                     ; Export all")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Module Access:")
	fmt.Fprintln(w, "  module.attr                    ; Access module attribute")
	fmt.Fprintln(w, "  (module.func args)             ; Call module function")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Module Paths:")
	fmt.Fprintln(w, "  ; Modules are searched in:")
	fmt.Fprintln(w, "  ; 1. Current directory")
	fmt.Fprintln(w, "  ; 2. M28_PATH directories")
	fmt.Fprintln(w, "  ; 3. Built-in modules")
	fmt.Fprintln(w)
}

// showAsyncHelp displays async/concurrent programming help
func (h *HelpSystem) showAsyncHelp(w io.Writer) {
	fmt.Fprintln(w, "M28 Async/Concurrent Programming")
	fmt.Fprintln(w, "================================")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Async Functions:")
	fmt.Fprintln(w, "  (async def foo (x)         ; Define async function")
	fmt.Fprintln(w, "    (await some-async-op x))")
	fmt.Fprintln(w, "  (async (lambda () body))   ; Async lambda")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Tasks:")
	fmt.Fprintln(w, "  (create_task func args)    ; Create a task")
	fmt.Fprintln(w, "  (run_async task)           ; Run task to completion")
	fmt.Fprintln(w, "  (gather task1 task2)       ; Run multiple tasks")
	fmt.Fprintln(w, "  (await task)               ; Wait for task result")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Channels:")
	fmt.Fprintln(w, "  (Channel)                  ; Create unbuffered channel")
	fmt.Fprintln(w, "  (Channel 10)               ; Create buffered channel")
	fmt.Fprintln(w, "  (send! ch value)           ; Send to channel")
	fmt.Fprintln(w, "  (recv! ch)                 ; Receive from channel")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Concurrency:")
	fmt.Fprintln(w, "  (go expr)                  ; Run expr in goroutine")
	fmt.Fprintln(w, "  (select                    ; Select from channels")
	fmt.Fprintln(w, "    ((recv! ch1) handler1)")
	fmt.Fprintln(w, "    ((send! ch2 val) handler2)")
	fmt.Fprintln(w, "    (default handler3))")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Utilities:")
	fmt.Fprintln(w, "  (sleep seconds)            ; Sleep for duration")
	fmt.Fprintln(w)
}