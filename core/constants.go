// Package core defines common constants used throughout M28
package core

// Character code constants
const (
	// ASCIIPrintableMin is the first printable ASCII character (space)
	ASCIIPrintableMin = 32

	// ASCIIPrintableMax is the last printable ASCII character including DEL
	ASCIIPrintableMax = 127

	// ASCIIPrintableMaxVisible is the last visible printable ASCII character (tilde)
	ASCIIPrintableMaxVisible = 126

	// ASCIIDel is the DEL control character
	ASCIIDel = 127

	// ASCIIMax is the maximum 7-bit ASCII value
	ASCIIMax = 127

	// MaxByteValue is the maximum value for a byte (8-bit unsigned integer)
	MaxByteValue = 255

	// UnicodeBMPLimit is the upper bound of the Unicode Basic Multilingual Plane
	// Characters with code points >= this value are in supplementary planes
	UnicodeBMPLimit = 65536
)

// Integer conversion constants
const (
	// IntBaseMin is the minimum base allowed for integer conversion
	IntBaseMin = 2

	// IntBaseMax is the maximum base allowed for integer conversion
	// Bases 2-36 use digits 0-9 and letters A-Z
	IntBaseMax = 36
)

// Optimization threshold constants
const (
	// PowerOptimizationThreshold is the maximum exponent value for which
	// integer power operations use optimized computation instead of
	// converting to floating point
	PowerOptimizationThreshold = 10000
)
