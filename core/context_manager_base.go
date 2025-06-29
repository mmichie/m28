package core

// BaseContextManager provides common functionality for context managers
type BaseContextManager struct {
	BaseObject
	registry *MethodRegistry
}

// NewBaseContextManager creates a new base context manager
func NewBaseContextManager(typeName Type) *BaseContextManager {
	cm := &BaseContextManager{
		BaseObject: *NewBaseObject(typeName),
	}
	return cm
}

// initRegistry initializes the registry with standard context manager methods
func (cm *BaseContextManager) initRegistry(enterHandler, exitHandler MethodHandler) {
	cm.registry = NewMethodRegistry()

	// Register standard context manager methods
	cm.registry.RegisterMethods(
		MakeMethod("__enter__", 0, "Enter the runtime context", enterHandler),
		MakeMethod("__exit__", 3, "Exit the runtime context", exitHandler),
	)
}

// GetRegistry implements AttributeProvider
func (cm *BaseContextManager) GetRegistry() *MethodRegistry {
	return cm.registry
}

// GetBaseObject implements AttributeProvider
func (cm *BaseContextManager) GetBaseObject() *BaseObject {
	return &cm.BaseObject
}

// GetAttr implements the new simplified GetAttr pattern
func (cm *BaseContextManager) GetAttr(name string) (Value, bool) {
	return GetAttrWithRegistry(cm, name)
}
