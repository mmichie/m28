package core

// Slice Normalization Helpers
// These helpers eliminate duplication in slice handling across list, tuple, and string types.

// NormalizeSliceIndices normalizes slice indices (start, stop, step) for a sequence of given length.
// Handles None/Nil values, negative indices, and clamping to valid ranges.
// Returns an error if step is zero.
func NormalizeSliceIndices(slice *SliceValue, length int) (start, stop, step int, err error) {
	// Default step
	step = 1
	if slice.Step != nil && slice.Step != Nil {
		if n, ok := slice.Step.(NumberValue); ok {
			step = int(n)
			if step == 0 {
				return 0, 0, 0, &ValueError{Message: "slice step cannot be zero"}
			}
		}
	}

	// Determine defaults for start and stop based on step direction
	if step > 0 {
		start = 0
		stop = length
	} else {
		start = length - 1
		stop = -length - 1
	}

	// Process start index
	if slice.Start != nil && slice.Start != Nil {
		if n, ok := slice.Start.(NumberValue); ok {
			start = int(n)
			if start < 0 {
				start = length + start
			}
			// Clamp to valid range
			if step > 0 {
				if start < 0 {
					start = 0
				}
				if start > length {
					start = length
				}
			} else {
				if start < -length-1 {
					start = -length - 1
				}
				if start >= length {
					start = length - 1
				}
			}
		}
	}

	// Process stop index
	if slice.Stop != nil && slice.Stop != Nil {
		if n, ok := slice.Stop.(NumberValue); ok {
			stop = int(n)
			if stop < 0 {
				stop = length + stop
			}
			// Clamp to valid range
			if step > 0 {
				if stop < 0 {
					stop = 0
				}
				if stop > length {
					stop = length
				}
			} else {
				if stop < -length-1 {
					stop = -length - 1
				}
				if stop >= length {
					stop = length - 1
				}
			}
		}
	}

	return start, stop, step, nil
}

// NormalizeIndex normalizes a single index for a sequence of given length.
// Handles negative indices (wraps around from the end).
// Returns an IndexError if the index is out of bounds.
func NormalizeIndex(index int, length int) (int, error) {
	if index < 0 {
		index = length + index
	}

	if index < 0 || index >= length {
		return 0, &IndexError{Index: index, Length: length}
	}

	return index, nil
}
