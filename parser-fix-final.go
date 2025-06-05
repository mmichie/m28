// Analysis of the bug:
// 
// When parsing (= self.x []), the parser should see three elements:
// 1. = (symbol)
// 2. self.x -> (. self "x") 
// 3. [] -> (quote [])
//
// But instead it's parsing elements 2 and 3 as a single indexed expression:
// self.x[] -> (get-item (. self "x") nil)
//
// The issue is in parsePostfix. Even though it checks for whitespace,
// something is causing it to continue parsing [] as an index.
//
// Potential fix locations:
// 1. In parsePostfix - be more strict about when to parse indexing
// 2. In parseList - handle expression boundaries differently  
// 3. In parseExpr - add context awareness

// The real issue might be that the whitespace is getting consumed
// somewhere before the check in parsePostfix.