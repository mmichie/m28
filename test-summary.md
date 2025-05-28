# M28 Testing Summary

## Week 3, Day 5 - Test and Fix Issues

### ✅ Completed
1. **Basic Examples (00_basics/)**
   - hello_world.m28 - Works perfectly
   - data_types.m28 - Fixed (removed scientific notation)
   - variables.m28 - Works with minor errors
   - operators.m28 - Works perfectly

2. **Function Examples (01_functions/)**
   - basic_functions.m28 - Fixed (removed unsupported features)
   - lambda_functions.m28 - Mostly works
   - higher_order.m28 - Mostly works

### ❌ Issues Found
1. **Unsupported Features**:
   - Scientific notation (1.23e-4)
   - Underscores in numbers (1_000_000)
   - Default parameters in functions
   - Multiple return values syntax
   - Multiple assignment
   - Variadic functions (*args)
   - Set literals {1, 2, 3}
   - Tuple literals (1, 2, 3)

2. **Control Flow Examples Need Fixes**:
   - conditionals.m28 - Has errors
   - loops.m28 - For loop syntax issue
   - exceptions.m28 - Parse error

### ✅ Working Features
- Basic data types (numbers, strings, lists, dicts)
- Functions (def, lambda, higher-order)
- List/dict dot notation for access
- List indexing with brackets
- Type checking (isinstance, type)
- All essential builtins (all, any, round, etc.)
- String, list, and dict methods
- Sets and tuples via functions

### 📋 Recommendations
1. Update examples to use only supported syntax
2. Add documentation about unsupported Python features
3. Consider implementing missing features in future releases