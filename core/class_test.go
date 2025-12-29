package core

import (
	"testing"
)

// TestNewClass tests basic class creation
func TestNewClass(t *testing.T) {
	// Create a class with no parent
	cls := NewClass("MyClass", nil)

	if cls.Name != "MyClass" {
		t.Errorf("Expected name 'MyClass', got '%s'", cls.Name)
	}

	if cls.Module != "__main__" {
		t.Errorf("Expected module '__main__', got '%s'", cls.Module)
	}

	if len(cls.Parents) != 0 {
		t.Errorf("Expected 0 parents, got %d", len(cls.Parents))
	}

	if cls.Methods == nil {
		t.Error("Methods map should not be nil")
	}

	if cls.Attributes == nil {
		t.Error("Attributes map should not be nil")
	}
}

// TestNewClassWithParent tests class creation with single parent
func TestNewClassWithParent(t *testing.T) {
	parent := NewClass("Parent", nil)
	child := NewClass("Child", parent)

	if len(child.Parents) != 1 {
		t.Errorf("Expected 1 parent, got %d", len(child.Parents))
	}

	if child.Parents[0] != parent {
		t.Error("Parent should be set correctly")
	}
}

// TestNewClassWithParents tests class creation with multiple parents
func TestNewClassWithParents(t *testing.T) {
	parent1 := NewClass("Parent1", nil)
	parent2 := NewClass("Parent2", nil)
	child := NewClassWithParents("Child", []*Class{parent1, parent2})

	if len(child.Parents) != 2 {
		t.Errorf("Expected 2 parents, got %d", len(child.Parents))
	}

	if child.Parents[0] != parent1 {
		t.Error("First parent should be Parent1")
	}

	if child.Parents[1] != parent2 {
		t.Error("Second parent should be Parent2")
	}
}

// TestIsSubclass tests the IsSubclass function
func TestIsSubclass(t *testing.T) {
	// Create class hierarchy: GrandParent -> Parent -> Child
	grandparent := NewClass("GrandParent", nil)
	parent := NewClass("Parent", grandparent)
	child := NewClass("Child", parent)

	// Same class
	if !IsSubclass(child, child) {
		t.Error("Class should be subclass of itself")
	}

	// Direct parent
	if !IsSubclass(child, parent) {
		t.Error("Child should be subclass of Parent")
	}

	// Grandparent
	if !IsSubclass(child, grandparent) {
		t.Error("Child should be subclass of GrandParent")
	}

	// Not subclass
	unrelated := NewClass("Unrelated", nil)
	if IsSubclass(child, unrelated) {
		t.Error("Child should not be subclass of Unrelated")
	}

	// Parent is not subclass of child
	if IsSubclass(parent, child) {
		t.Error("Parent should not be subclass of Child")
	}
}

// TestIsSubclassMultipleInheritance tests IsSubclass with multiple parents
func TestIsSubclassMultipleInheritance(t *testing.T) {
	// Create diamond inheritance: A -> B, C -> D
	//       A
	//      / \
	//     B   C
	//      \ /
	//       D
	a := NewClass("A", nil)
	b := NewClass("B", a)
	c := NewClass("C", a)
	d := NewClassWithParents("D", []*Class{b, c})

	// D is subclass of B
	if !IsSubclass(d, b) {
		t.Error("D should be subclass of B")
	}

	// D is subclass of C
	if !IsSubclass(d, c) {
		t.Error("D should be subclass of C")
	}

	// D is subclass of A (through both B and C)
	if !IsSubclass(d, a) {
		t.Error("D should be subclass of A")
	}
}

// TestIsInstanceOf tests the IsInstanceOf function
func TestIsInstanceOf(t *testing.T) {
	parent := NewClass("Parent", nil)
	child := NewClass("Child", parent)

	// Create instance of child
	instance := NewInstance(child)

	// Instance of its class
	if !IsInstanceOf(instance, child) {
		t.Error("Instance should be instance of Child")
	}

	// Instance of parent class
	if !IsInstanceOf(instance, parent) {
		t.Error("Instance should be instance of Parent")
	}

	// Not instance of unrelated class
	unrelated := NewClass("Unrelated", nil)
	if IsInstanceOf(instance, unrelated) {
		t.Error("Instance should not be instance of Unrelated")
	}
}

// TestClassSetMethod tests setting methods on a class
func TestClassSetMethod(t *testing.T) {
	cls := NewClass("MyClass", nil)

	// Create a simple builtin function as method
	method := NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
		return NumberValue(42), nil
	})

	cls.SetMethod("myMethod", method)

	// Get the method back
	got, ok := cls.GetMethod("myMethod")
	if !ok {
		t.Error("Method should exist")
	}

	if got != method {
		t.Error("Retrieved method should match set method")
	}

	// Non-existent method
	_, ok = cls.GetMethod("nonExistent")
	if ok {
		t.Error("Non-existent method should return false")
	}
}

// TestClassMethodInheritance tests method inheritance
func TestClassMethodInheritance(t *testing.T) {
	parent := NewClass("Parent", nil)
	child := NewClass("Child", parent)

	// Set method on parent
	parentMethod := NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
		return StringValue("parent"), nil
	})
	parent.SetMethod("inherited", parentMethod)

	// Child should inherit the method
	got, ok := child.GetMethod("inherited")
	if !ok {
		t.Error("Child should inherit method from parent")
	}

	if got != parentMethod {
		t.Error("Inherited method should be parent's method")
	}
}

// TestClassMethodOverride tests method overriding
func TestClassMethodOverride(t *testing.T) {
	parent := NewClass("Parent", nil)
	child := NewClass("Child", parent)

	// Set method on parent
	parentMethod := NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
		return StringValue("parent"), nil
	})
	parent.SetMethod("method", parentMethod)

	// Override in child
	childMethod := NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
		return StringValue("child"), nil
	})
	child.SetMethod("method", childMethod)

	// Child should return its own method
	got, ok := child.GetMethod("method")
	if !ok {
		t.Error("Method should exist")
	}

	if got != childMethod {
		t.Error("Child should return its own overridden method")
	}
}

// TestClassGetMethodWithClass tests GetMethodWithClass
func TestClassGetMethodWithClass(t *testing.T) {
	parent := NewClass("Parent", nil)
	child := NewClass("Child", parent)

	// Set method on parent
	parentMethod := NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
		return StringValue("parent"), nil
	})
	parent.SetMethod("inherited", parentMethod)

	// Get method with defining class
	method, defClass, ok := child.GetMethodWithClass("inherited")
	if !ok {
		t.Error("Method should be found")
	}

	if method != parentMethod {
		t.Error("Method should be parent's method")
	}

	if defClass != parent {
		t.Error("Defining class should be Parent")
	}
}

// TestClassAttributes tests class attribute access
func TestClassAttributes(t *testing.T) {
	cls := NewClass("MyClass", nil)

	// Set attribute
	cls.SetClassAttr("myAttr", NumberValue(42))

	// Get attribute
	val, ok := cls.GetClassAttr("myAttr")
	if !ok {
		t.Error("Attribute should exist")
	}

	if num, ok := val.(NumberValue); !ok || float64(num) != 42 {
		t.Errorf("Expected NumberValue(42), got %v", val)
	}
}

// TestClassSpecialAttrs tests special class attributes like __name__, __bases__
func TestClassSpecialAttrs(t *testing.T) {
	parent := NewClass("Parent", nil)
	child := NewClass("Child", parent)

	// __name__ - accessed via GetAttr, not GetClassAttr
	name, ok := child.GetAttr("__name__")
	if !ok {
		t.Error("__name__ should exist")
	}
	if str, ok := name.(StringValue); !ok || string(str) != "Child" {
		t.Errorf("Expected 'Child', got %v", name)
	}

	// __module__
	module, ok := child.GetAttr("__module__")
	if !ok {
		t.Error("__module__ should exist")
	}
	if str, ok := module.(StringValue); !ok || string(str) != "__main__" {
		t.Errorf("Expected '__main__', got %v", module)
	}

	// __bases__ - accessed via GetAttr
	bases, ok := child.GetAttr("__bases__")
	if !ok {
		t.Error("__bases__ should exist")
	}
	baseTuple, ok := bases.(TupleValue)
	if !ok {
		t.Errorf("__bases__ should be tuple, got %T", bases)
	}
	if len(baseTuple) != 1 {
		t.Errorf("Expected 1 base, got %d", len(baseTuple))
	}
	if baseTuple[0] != parent {
		t.Error("Base should be Parent")
	}
}

// TestClassMRO tests Method Resolution Order
func TestClassMRO(t *testing.T) {
	// Create diamond: A -> B, C -> D
	a := NewClass("A", nil)
	b := NewClass("B", a)
	c := NewClass("C", a)
	d := NewClassWithParents("D", []*Class{b, c})

	// __mro__ accessed via GetAttr
	mro, ok := d.GetAttr("__mro__")
	if !ok {
		t.Error("__mro__ should exist")
	}

	mroTuple, ok := mro.(TupleValue)
	if !ok {
		t.Errorf("__mro__ should be tuple, got %T", mro)
	}

	// MRO should start with D
	if len(mroTuple) < 1 {
		t.Fatal("MRO should have at least one element")
	}

	firstClass, ok := mroTuple[0].(*Class)
	if !ok || firstClass != d {
		t.Error("MRO should start with D")
	}

	// A should appear only once in MRO (deduplication)
	aCount := 0
	for _, v := range mroTuple {
		if cls, ok := v.(*Class); ok && cls == a {
			aCount++
		}
	}
	if aCount != 1 {
		t.Errorf("A should appear exactly once in MRO, got %d", aCount)
	}
}

// TestNewInstance tests instance creation
func TestNewInstance(t *testing.T) {
	cls := NewClass("MyClass", nil)
	instance := NewInstance(cls)

	if instance.Class != cls {
		t.Error("Instance should reference its class")
	}

	if instance.Attributes == nil {
		t.Error("Instance attributes should not be nil")
	}
}

// TestInstanceSetGetAttr tests instance attribute access
func TestInstanceSetGetAttr(t *testing.T) {
	cls := NewClass("MyClass", nil)
	instance := NewInstance(cls)

	// Set attribute
	err := instance.SetAttr("x", NumberValue(10))
	if err != nil {
		t.Errorf("SetAttr failed: %v", err)
	}

	// Get attribute
	val, ok := instance.GetAttr("x")
	if !ok {
		t.Error("Attribute should exist")
	}

	if num, ok := val.(NumberValue); !ok || float64(num) != 10 {
		t.Errorf("Expected NumberValue(10), got %v", val)
	}
}

// TestInstanceInheritsClassAttr tests that instances can access class attributes
func TestInstanceInheritsClassAttr(t *testing.T) {
	cls := NewClass("MyClass", nil)
	cls.SetClassAttr("classAttr", StringValue("class value"))

	instance := NewInstance(cls)

	// Instance should be able to access class attribute
	val, ok := instance.GetAttr("classAttr")
	if !ok {
		t.Error("Instance should access class attribute")
	}

	if str, ok := val.(StringValue); !ok || string(str) != "class value" {
		t.Errorf("Expected 'class value', got %v", val)
	}
}

// TestClassType tests the Type() method
func TestClassType(t *testing.T) {
	cls := NewClass("MyClass", nil)

	if cls.Type() != Type("class") {
		t.Errorf("Class Type should be 'class', got '%s'", cls.Type())
	}
}

// TestClassString tests the String() method
func TestClassString(t *testing.T) {
	cls := NewClass("MyClass", nil)

	// String() returns just the name for backward compatibility
	str := cls.String()
	if str != "MyClass" {
		t.Errorf("Expected \"MyClass\", got %q", str)
	}
}

// TestInstanceType tests instance Type() method
func TestInstanceType(t *testing.T) {
	cls := NewClass("MyClass", nil)
	instance := NewInstance(cls)

	if instance.Type() != Type("MyClass") {
		t.Errorf("Instance Type should be 'MyClass', got '%s'", instance.Type())
	}
}

// TestSuperCreation tests Super object creation
func TestSuperCreation(t *testing.T) {
	parent := NewClass("Parent", nil)
	child := NewClass("Child", parent)
	instance := NewInstance(child)

	super := NewSuper(child, instance)

	if super.Class != child {
		t.Error("Super should reference the class")
	}

	if super.Instance != instance {
		t.Error("Super should reference the instance")
	}
}

// TestNilClass tests handling of nil class in IsSubclass
func TestNilClass(t *testing.T) {
	cls := NewClass("MyClass", nil)

	// IsSubclass with nil child should return false
	if IsSubclass(nil, cls) {
		t.Error("IsSubclass(nil, cls) should return false")
	}

	// IsSubclass with nil parent should return false
	if IsSubclass(cls, nil) {
		t.Error("IsSubclass(cls, nil) should return false")
	}
}
