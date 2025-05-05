package core

import (
	"sort"
)

// Additional methods for PythonicSet to provide Python-like functionality

// Union returns a new set containing all elements from both sets
func (s *PythonicSet) Union(other *PythonicSet) *PythonicSet {
	result := NewPythonicSet()
	s.mu.RLock()
	for elem := range s.data {
		result.Add(elem)
	}
	s.mu.RUnlock()

	other.mu.RLock()
	for elem := range other.data {
		result.Add(elem)
	}
	other.mu.RUnlock()

	return result
}

// Intersection returns a new set containing elements that are in both sets
func (s *PythonicSet) Intersection(other *PythonicSet) *PythonicSet {
	result := NewPythonicSet()
	s.mu.RLock()
	defer s.mu.RUnlock()

	other.mu.RLock()
	defer other.mu.RUnlock()

	// Iterate over the smaller set for efficiency
	if len(s.data) <= len(other.data) {
		for elem := range s.data {
			if _, ok := other.data[elem]; ok {
				result.Add(elem)
			}
		}
	} else {
		for elem := range other.data {
			if _, ok := s.data[elem]; ok {
				result.Add(elem)
			}
		}
	}

	return result
}

// Difference returns a new set with elements in this set but not in the other
func (s *PythonicSet) Difference(other *PythonicSet) *PythonicSet {
	result := NewPythonicSet()
	s.mu.RLock()
	defer s.mu.RUnlock()

	other.mu.RLock()
	defer other.mu.RUnlock()

	for elem := range s.data {
		if _, ok := other.data[elem]; !ok {
			result.Add(elem)
		}
	}

	return result
}

// SymmetricDifference returns a new set with elements in either set but not in both
func (s *PythonicSet) SymmetricDifference(other *PythonicSet) *PythonicSet {
	result := NewPythonicSet()
	s.mu.RLock()
	defer s.mu.RUnlock()

	other.mu.RLock()
	defer other.mu.RUnlock()

	// Add elements from s that are not in other
	for elem := range s.data {
		if _, ok := other.data[elem]; !ok {
			result.Add(elem)
		}
	}

	// Add elements from other that are not in s
	for elem := range other.data {
		if _, ok := s.data[elem]; !ok {
			result.Add(elem)
		}
	}

	return result
}

// IsSubset returns true if all elements in this set are in the other set
func (s *PythonicSet) IsSubset(other *PythonicSet) bool {
	s.mu.RLock()
	defer s.mu.RUnlock()

	other.mu.RLock()
	defer other.mu.RUnlock()

	// A set can't be a subset of a smaller set
	if len(s.data) > len(other.data) {
		return false
	}

	for elem := range s.data {
		if _, ok := other.data[elem]; !ok {
			return false
		}
	}

	return true
}

// IsSuperset returns true if all elements in the other set are in this set
func (s *PythonicSet) IsSuperset(other *PythonicSet) bool {
	return other.IsSubset(s)
}

// IsDisjoint returns true if the sets have no elements in common
func (s *PythonicSet) IsDisjoint(other *PythonicSet) bool {
	s.mu.RLock()
	defer s.mu.RUnlock()

	other.mu.RLock()
	defer other.mu.RUnlock()

	// For efficiency, iterate over the smaller set
	if len(s.data) <= len(other.data) {
		for elem := range s.data {
			if _, ok := other.data[elem]; ok {
				return false // Found a common element
			}
		}
	} else {
		for elem := range other.data {
			if _, ok := s.data[elem]; ok {
				return false // Found a common element
			}
		}
	}

	return true
}

// Update adds all elements from the other set to this set (in-place union)
func (s *PythonicSet) Update(other *PythonicSet) {
	other.mu.RLock()
	defer other.mu.RUnlock()

	for elem := range other.data {
		s.Add(elem)
	}
}

// IntersectionUpdate keeps only elements found in both sets (in-place intersection)
func (s *PythonicSet) IntersectionUpdate(other *PythonicSet) {
	s.mu.Lock()
	defer s.mu.Unlock()

	other.mu.RLock()
	defer other.mu.RUnlock()

	// Mark elements for deletion if they're not in other
	var toRemove []LispValue
	for elem := range s.data {
		if _, ok := other.data[elem]; !ok {
			toRemove = append(toRemove, elem)
		}
	}

	// Remove the marked elements
	for _, elem := range toRemove {
		delete(s.data, elem)
	}
}

// DifferenceUpdate removes elements found in the other set (in-place difference)
func (s *PythonicSet) DifferenceUpdate(other *PythonicSet) {
	s.mu.Lock()
	defer s.mu.Unlock()

	other.mu.RLock()
	defer other.mu.RUnlock()

	for elem := range other.data {
		delete(s.data, elem)
	}
}

// SymmetricDifferenceUpdate keeps elements in either set but not in both (in-place symmetric difference)
func (s *PythonicSet) SymmetricDifferenceUpdate(other *PythonicSet) {
	// This operation is more complex so we'll use the non-mutating version and then update
	result := s.SymmetricDifference(other)

	s.mu.Lock()
	defer s.mu.Unlock()

	// Replace the contents of s with the result
	s.data = make(map[LispValue]struct{}, len(result.data))
	for elem := range result.data {
		s.data[elem] = struct{}{}
	}
}

// ToList converts the set to a sorted list
func (s *PythonicSet) ToList() LispList {
	elements := s.sortedElements()
	result := make(LispList, len(elements))
	for i, elem := range elements {
		result[i] = elem
	}
	return result
}

// Clear removes all elements from the set
func (s *PythonicSet) Clear() {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.data = make(map[LispValue]struct{})
}

// Copy returns a new set with a copy of all elements
func (s *PythonicSet) Copy() *PythonicSet {
	result := NewPythonicSet()
	s.mu.RLock()
	defer s.mu.RUnlock()

	for elem := range s.data {
		result.Add(elem)
	}
	return result
}

// Pop removes and returns an arbitrary element from the set
// Returns the element and a bool indicating if an element was popped
func (s *PythonicSet) Pop() (LispValue, bool) {
	s.mu.Lock()
	defer s.mu.Unlock()

	if len(s.data) == 0 {
		return nil, false
	}

	// Get any element
	var elem LispValue
	for e := range s.data {
		elem = e
		break
	}

	// Remove it
	delete(s.data, elem)
	return elem, true
}

// Items returns all elements in the set as a slice
func (s *PythonicSet) Items() []LispValue {
	s.mu.RLock()
	defer s.mu.RUnlock()

	result := make([]LispValue, 0, len(s.data))
	for elem := range s.data {
		result = append(result, elem)
	}
	return result
}

// String returns a string representation of the set
func (s *PythonicSet) String() string {
	s.mu.RLock()
	defer s.mu.RUnlock()

	if len(s.data) == 0 {
		return "{}"
	}

	elements := make([]LispValue, 0, len(s.data))
	for elem := range s.data {
		elements = append(elements, elem)
	}

	// Sort for consistent output
	sort.Slice(elements, func(i, j int) bool {
		return Compare(elements[i], elements[j]) < 0
	})

	// Format as Python-like set
	result := "{"
	for i, elem := range elements {
		if i > 0 {
			result += ", "
		}
		result += PrintValue(elem)
	}
	result += "}"
	return result
}
