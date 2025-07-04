package core

// InitializeTypeRegistryRefactored sets up type descriptors for all built-in types
// This is the refactored version that splits the god function into smaller pieces
func InitializeTypeRegistryRefactored() {
	// I/O types
	registerFileType()

	// Concurrent types
	registerConcurrentTypes()

	// Object system types
	registerObjectTypes()

	// Primitive types
	registerPrimitiveTypes()

	// Collection types
	registerCollectionTypes()
}

// registerObjectTypes registers class and module types
func registerObjectTypes() {
	registerClassType()
	registerModuleType()
}

// registerPrimitiveTypes registers number, string, bool, nil, symbol, bytes, and bytearray types
func registerPrimitiveTypes() {
	registerNumberType()
	registerStringType()
	registerBoolType()
	registerNilType()
	registerSymbolType()
	registerBytesType()
	registerByteArrayType()
}

// registerCollectionTypes registers list, dict, tuple, and set types
func registerCollectionTypes() {
	registerListType()
	registerDictType()
	registerTupleType()
	registerSetType()
}
