package modules

import (
	"testing"

	"github.com/mmichie/m28/core"
)

func TestLazyLoading(t *testing.T) {
	// Clear the cache to test lazy loading
	moduleCache = make(map[string]*core.DictValue)

	// First call should initialize the module
	module1, ok := GetBuiltinModule("json")
	if !ok {
		t.Fatal("json module not found")
	}
	if module1 == nil {
		t.Fatal("json module is nil")
	}

	// Second call should return the cached module
	module2, ok := GetBuiltinModule("json")
	if !ok {
		t.Fatal("json module not found on second call")
	}

	// Should be the same instance
	if module1 != module2 {
		t.Error("GetBuiltinModule returned different instances for the same module")
	}
}

func TestNonExistentModule(t *testing.T) {
	module, ok := GetBuiltinModule("nonexistent")
	if ok {
		t.Error("GetBuiltinModule returned true for non-existent module")
	}
	if module != nil {
		t.Error("GetBuiltinModule returned non-nil for non-existent module")
	}
}

func BenchmarkGetBuiltinModule(b *testing.B) {
	// Pre-populate cache to test cached performance
	GetBuiltinModule("json")

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		GetBuiltinModule("json")
	}
}

func BenchmarkGetBuiltinModuleUncached(b *testing.B) {
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		// Clear cache to force re-initialization
		moduleCache = make(map[string]*core.DictValue)
		GetBuiltinModule("json")
	}
}
