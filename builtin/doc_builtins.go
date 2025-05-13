package builtin

import (
	"github.com/mmichie/m28/core"
)

// init registers documentation for builtin functions
func init() {
	// Documentation for the print function
	core.RegisterDoc(core.DocEntry{
		Name:        "print",
		Type:        "builtin-function",
		Brief:       "Print objects to the console",
		Description: "Prints the given objects to the console, separated by spaces and followed by a newline.",
		Params: []core.ParamDoc{
			{
				Name:        "objects",
				Description: "One or more objects to print",
				Optional:    true,
			},
		},
		Returns:  "None",
		Examples: []string{`(print "Hello, world!")`, `(print "Value:", 42)`},
		Related:  []string{"str", "input"},
		Module:   "builtin",
	})

	// Documentation for the + function
	core.RegisterDoc(core.DocEntry{
		Name:        "+",
		Type:        "builtin-function",
		Brief:       "Add numbers or concatenate strings/sequences",
		Description: "Adds two or more numbers together, or concatenates strings, lists, or other sequences.",
		Params: []core.ParamDoc{
			{
				Name:        "x",
				Description: "The first operand",
			},
			{
				Name:        "y",
				Description: "The second operand",
			},
			{
				Name:        "additional operands",
				Description: "More operands to add",
				Optional:    true,
			},
		},
		Returns: "The sum of numbers or concatenated sequence",
		Examples: []string{
			`(+ 2 3)                 ; Returns 5`,
			`(+ 1 2 3 4)             ; Returns 10`,
			`(+ "Hello, " "world!")  ; Returns "Hello, world!"`,
			`(+ [1 2] [3 4])         ; Returns [1 2 3 4]`,
		},
		Related: []string{"-", "*", "/", "str"},
		Module:  "builtin",
	})

	// Documentation for the len function
	core.RegisterDoc(core.DocEntry{
		Name:        "len",
		Type:        "builtin-function",
		Brief:       "Return the length of an object",
		Description: "Returns the number of items in a sequence or mapping, such as a string, list, tuple, or dictionary.",
		Params: []core.ParamDoc{
			{
				Name:        "obj",
				Description: "The object to find the length of",
			},
		},
		Returns: "The length of the object as a number",
		Examples: []string{
			`(len "hello")     ; Returns 5`,
			`(len [1 2 3 4])   ; Returns 4`,
			`(len {})          ; Returns 0`,
		},
		Related: []string{"list", "dict", "str"},
		Module:  "builtin",
	})
}
