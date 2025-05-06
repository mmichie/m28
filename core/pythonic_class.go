package core

import (
	"fmt"
	"sync"
)

// PythonicClass represents a Python-style class
type PythonicClass struct {
	Name       string               // Class name
	Methods    map[string]*Lambda   // Methods defined on the class
	Attributes map[string]LispValue // Class-level attributes
	Parents    []*PythonicClass     // Parent classes (for inheritance)
	mu         sync.RWMutex         // Mutex for thread safety
}

// NewPythonicClass creates a new Python-style class
func NewPythonicClass(name string, parents []*PythonicClass) *PythonicClass {
	return &PythonicClass{
		Name:       name,
		Methods:    make(map[string]*Lambda),
		Attributes: make(map[string]LispValue),
		Parents:    parents,
	}
}

// AddMethod adds a method to the class
func (c *PythonicClass) AddMethod(name string, method *Lambda) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.Methods[name] = method
}

// GetMethod gets a method from the class, checking parent classes if needed
func (c *PythonicClass) GetMethod(name string) (*Lambda, bool) {
	c.mu.RLock()
	method, exists := c.Methods[name]
	c.mu.RUnlock()

	if exists {
		return method, true
	}

	// Check parent classes in order
	for _, parent := range c.Parents {
		if method, exists := parent.GetMethod(name); exists {
			return method, true
		}
	}

	return nil, false
}

// AddAttribute adds an attribute to the class
func (c *PythonicClass) AddAttribute(name string, value LispValue) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.Attributes[name] = value
}

// GetAttribute gets an attribute from the class, checking parent classes if needed
func (c *PythonicClass) GetAttribute(name string) (LispValue, bool) {
	c.mu.RLock()
	attr, exists := c.Attributes[name]
	c.mu.RUnlock()

	if exists {
		return attr, true
	}

	// Check parent classes in order
	for _, parent := range c.Parents {
		if attr, exists := parent.GetAttribute(name); exists {
			return attr, true
		}
	}

	return nil, false
}

// PythonicObject represents an instance of a PythonicClass
type PythonicObject struct {
	Class      *PythonicClass // The class this object is an instance of
	Attributes *PythonicDict  // Instance attributes
}

// NewPythonicObject creates a new instance of a class
func NewPythonicObject(class *PythonicClass) *PythonicObject {
	return &PythonicObject{
		Class:      class,
		Attributes: NewPythonicDict(),
	}
}

// GetAttribute gets an attribute from the object or its class
func (o *PythonicObject) GetAttribute(name string) (LispValue, bool) {
	// First check instance attributes
	if attr, exists := o.Attributes.Get(name); exists {
		return attr, true
	}

	// Then check class methods
	if method, exists := o.Class.GetMethod(name); exists {
		// Create a bound method that includes the instance as first argument
		boundMethod := &BoundMethod{
			Method:   method,
			Instance: o,
		}
		return boundMethod, true
	}

	// Then check class attributes
	return o.Class.GetAttribute(name)
}

// BoundMethod represents a method bound to a specific instance
type BoundMethod struct {
	Method   *Lambda
	Instance *PythonicObject
}

// Apply applies the bound method with the instance as first argument
func (bm *BoundMethod) Apply(e Evaluator, args []LispValue, env Environment) (LispValue, error) {
	// Prepend the instance as first argument
	newArgs := make([]LispValue, len(args)+1)
	newArgs[0] = bm.Instance
	copy(newArgs[1:], args)

	// Apply the method with the instance as first argument
	return e.Apply(bm.Method, newArgs, env)
}

// String returns a string representation of the bound method
func (bm *BoundMethod) String() string {
	return fmt.Sprintf("<bound method %v of %v>", bm.Method, bm.Instance)
}

// SuperObject represents a wrapper for accessing parent class methods
type SuperObject struct {
	Object *PythonicObject
}

// GetAttribute looks up attributes only in parent classes
func (s *SuperObject) GetAttribute(name string) (LispValue, bool) {
	// Skip checking instance attributes

	// Check parent class methods
	for _, parent := range s.Object.Class.Parents {
		if method, exists := parent.GetMethod(name); exists {
			// Create a bound method that includes the original instance as first argument
			boundMethod := &BoundMethod{
				Method:   method,
				Instance: s.Object,
			}
			return boundMethod, true
		}
	}

	// Check parent class attributes
	for _, parent := range s.Object.Class.Parents {
		if attr, exists := parent.GetAttribute(name); exists {
			return attr, true
		}
	}

	return nil, false
}

// String returns a string representation of the super object
func (s *SuperObject) String() string {
	return fmt.Sprintf("<super: %v>", s.Object.Class.Name)
}
