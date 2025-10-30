package modules

import "github.com/mmichie/m28/core"

// InitOpcodeModule creates the _opcode module with bytecode opcodes
func InitOpcodeModule() *core.DictValue {
	module := core.NewDict()

	// Module docstring
	module.Set("__doc__", core.StringValue("Opcode support module"))

	// CPython 3.12 opcodes - minimal set needed by dis module
	// These are just the most common ones; add more as needed
	opcodes := map[string]int{
		"CACHE":                      0,
		"POP_TOP":                    1,
		"PUSH_NULL":                  2,
		"INTERPRETER_EXIT":           3,
		"END_FOR":                    4,
		"END_SEND":                   5,
		"NOP":                        9,
		"UNARY_NEGATIVE":             11,
		"UNARY_NOT":                  12,
		"BINARY_SUBSCR":              25,
		"BINARY_SLICE":               26,
		"STORE_SLICE":                27,
		"GET_LEN":                    30,
		"MATCH_MAPPING":              31,
		"MATCH_SEQUENCE":             32,
		"MATCH_KEYS":                 33,
		"PUSH_EXC_INFO":              35,
		"CHECK_EXC_MATCH":            36,
		"CHECK_EG_MATCH":             37,
		"WITH_EXCEPT_START":          49,
		"GET_AITER":                  50,
		"GET_ANEXT":                  51,
		"BEFORE_ASYNC_WITH":          52,
		"BEFORE_WITH":                53,
		"END_ASYNC_FOR":              54,
		"STORE_SUBSCR":               60,
		"DELETE_SUBSCR":              61,
		"GET_ITER":                   68,
		"GET_YIELD_FROM_ITER":        69,
		"LOAD_BUILD_CLASS":           71,
		"LOAD_ASSERTION_ERROR":       74,
		"RETURN_GENERATOR":           75,
		"RETURN_VALUE":               83,
		"SETUP_ANNOTATIONS":          85,
		"LOAD_LOCALS":                87,
		"POP_EXCEPT":                 89,
		"STORE_NAME":                 90,
		"DELETE_NAME":                91,
		"UNPACK_SEQUENCE":            92,
		"FOR_ITER":                   93,
		"UNPACK_EX":                  94,
		"STORE_ATTR":                 95,
		"DELETE_ATTR":                96,
		"STORE_GLOBAL":               97,
		"DELETE_GLOBAL":              98,
		"SWAP":                       99,
		"LOAD_CONST":                 100,
		"LOAD_NAME":                  101,
		"BUILD_TUPLE":                102,
		"BUILD_LIST":                 103,
		"BUILD_SET":                  104,
		"BUILD_MAP":                  105,
		"LOAD_ATTR":                  106,
		"COMPARE_OP":                 107,
		"IMPORT_NAME":                108,
		"IMPORT_FROM":                109,
		"JUMP_FORWARD":               110,
		"POP_JUMP_IF_FALSE":          114,
		"POP_JUMP_IF_TRUE":           115,
		"LOAD_GLOBAL":                116,
		"IS_OP":                      117,
		"CONTAINS_OP":                118,
		"RERAISE":                    119,
		"COPY":                       120,
		"BINARY_OP":                  122,
		"SEND":                       123,
		"LOAD_FAST":                  124,
		"STORE_FAST":                 125,
		"DELETE_FAST":                126,
		"LOAD_FAST_CHECK":            127,
		"POP_JUMP_IF_NOT_NONE":       128,
		"POP_JUMP_IF_NONE":           129,
		"RAISE_VARARGS":              130,
		"GET_AWAITABLE":              131,
		"BUILD_SLICE":                133,
		"JUMP_BACKWARD_NO_INTERRUPT": 134,
		"MAKE_CELL":                  135,
		"LOAD_CLOSURE":               136,
		"LOAD_DEREF":                 137,
		"STORE_DEREF":                138,
		"DELETE_DEREF":               139,
		"JUMP_BACKWARD":              140,
		"CALL_FUNCTION_EX":           142,
		"EXTENDED_ARG":               144,
		"LIST_APPEND":                145,
		"SET_ADD":                    146,
		"MAP_ADD":                    147,
		"LOAD_CLASSDEREF":            148,
		"COPY_FREE_VARS":             149,
		"RESUME":                     151,
		"MATCH_CLASS":                152,
		"FORMAT_VALUE":               155,
		"BUILD_CONST_KEY_MAP":        156,
		"BUILD_STRING":               157,
		"LOAD_METHOD":                160,
		"LIST_EXTEND":                162,
		"SET_UPDATE":                 163,
		"DICT_MERGE":                 164,
		"DICT_UPDATE":                165,
		"CALL":                       171,
		"KW_NAMES":                   172,
		"CALL_INTRINSIC_1":           173,
		"CALL_INTRINSIC_2":           174,
	}

	for name, value := range opcodes {
		module.Set(name, core.NumberValue(float64(value)))
	}

	// Add HAVE_ARGUMENT constant
	module.Set("HAVE_ARGUMENT", core.NumberValue(90))

	// stack_effect(opcode, oparg=None, *, jump=None) - compute stack effect of opcode
	module.Set("stack_effect", core.NewNamedBuiltinFunction("stack_effect", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, core.NewTypeError("stack_effect", nil, "stack_effect() takes at least 1 argument")
		}
		// Simplified implementation - just return 0 for now
		// A full implementation would calculate actual stack depth changes per opcode
		return core.NumberValue(0), nil
	}))

	return module
}
