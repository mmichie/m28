// Package builtin provides built-in functions for the M28 language.
package builtin

// This file is now empty as all functions have been migrated to their respective files:
// - types.go: type, isinstance, issubclass, int, float, str, bool, is_none
// - attributes.go: dir, hasattr, getattr, setattr, delattr
// - collections.go: list, tuple, dict, set, frozenset, slice, len
// - iteration.go: range, enumerate, zip, iter, next
// - functional.go: map, filter, reduce, all, any, sum, min, max
// - numeric.go: abs, round, pow, divmod, complex
// - io.go: print, input, open
// - errors.go: Exception classes and error handling
// - misc.go: id, hash, help, repr, exec, eval, compile, globals, locals
// - operators/: arithmetic, comparison, logical operators
// - methods/: string, list, dict, set methods
// - modules/: math, random, json, os, time, datetime, pathlib, shutil, async
//
// Use registry.go's RegisterAllBuiltins to register all functions.
