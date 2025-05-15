package special_forms

import (
	"github.com/mmichie/m28/core"
)

// init registers documentation for special forms
func init() {
	// Documentation for the def special form
	core.RegisterDoc(core.DocEntry{
		Name:        "def",
		Type:        "special-form",
		Brief:       "Define a function or variable",
		Description: "Creates a new binding in the current environment. Used to define functions or variables.",
		Params: []core.ParamDoc{
			{
				Name:        "name",
				Description: "The name to bind to",
			},
			{
				Name:        "value",
				Description: "The value to bind, or a parameter list and body if defining a function",
			},
		},
		Returns: "The defined value",
		Examples: []string{
			`(def x 10)                           ; Define a variable`,
			`(def (add a b) (+ a b))              ; Define a function`,
			`(def (greet name) (print "Hello" name))  ; Function with side effects`,
		},
		Related: []string{"lambda", "let", "class"},
	})

	// Documentation for the if special form
	core.RegisterDoc(core.DocEntry{
		Name:        "if",
		Type:        "special-form",
		Brief:       "Conditional expression",
		Description: "Evaluates a condition and, based on the result, evaluates either the true branch or the false branch.",
		Params: []core.ParamDoc{
			{
				Name:        "condition",
				Description: "The condition to test",
			},
			{
				Name:        "true-branch",
				Description: "The expression to evaluate if the condition is true",
			},
			{
				Name:        "false-branch",
				Description: "The expression to evaluate if the condition is false",
				Optional:    true,
			},
		},
		Returns: "The result of either the true branch or the false branch",
		Examples: []string{
			`(if (> x 0) "positive" "non-positive")`,
			`(if (empty? lst) (print "Empty list") (process lst))`,
		},
		Related: []string{"cond", "when", "and", "or"},
	})

	// Documentation for the for special form
	core.RegisterDoc(core.DocEntry{
		Name:        "for",
		Type:        "special-form",
		Brief:       "Iterate over a sequence",
		Description: "Iterates over the items in a sequence, binding each item to a variable and evaluating the body for each item.",
		Params: []core.ParamDoc{
			{
				Name:        "var",
				Description: "The variable to bind each item to",
			},
			{
				Name:        "sequence",
				Description: "The sequence to iterate over",
			},
			{
				Name:        "body",
				Description: "One or more expressions to evaluate for each item",
			},
		},
		Returns: "None",
		Examples: []string{
			`(for x [1 2 3] (print x))`,
			`(for item items (if (valid? item) (process item)))`,
		},
		Related: []string{"while", "map", "filter"},
	})

	// Documentation for the go special form
	core.RegisterDoc(core.DocEntry{
		Name:        "go",
		Type:        "special-form",
		Brief:       "Evaluate an expression in a new goroutine",
		Description: "Spawns a new goroutine and evaluates the given expression in that goroutine. Returns None immediately without waiting for the goroutine to complete.",
		Params: []core.ParamDoc{
			{
				Name:        "expr",
				Description: "The expression to evaluate in the new goroutine",
			},
		},
		Returns:  "None",
		Examples: []string{`(go (println "Hello from goroutine"))`, `(go (sleep 1) (println "Delayed message"))`},
		Related:  []string{"chan", "send", "recv"},
	})
}
