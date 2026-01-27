## Lexer
- Mostly just error handling
- More tests

## Parser 
Expressions:
- typecasts (put this in the right precedence when done)
- assignment (done)
- logic and comparisons (done)
- bitwise (done)
- terms (done)
- factors (done)
- unary (done)
- postfix (done)
- struct constructor (done)
- primary (done)
- much, much more desugaring
	- array constructors into a set of array initializers
	- struct constructors
		- these two should make it so i dont even need to handle these in TAC
	- structs, members, and methods (done!) (except for the one found in TAC later on the doc)
	- for loops


### Declarations:
- global variables... (should be trivial) (done)
- types (mostly done)
	- array types are strange atm (fixed!)
	- a function pointer type!
		- *fn({type}) type
	- unless i want to support structs being declared at the bottom being able to be used anywhere (fixed in TAC)
	- would require another pass but should work out well if i decide to add a preprocessor too (yes, i did a pass over the global scope)
- variable declaration (done)
- function declaration (done)
	- function pointers!
	- lambdas
- struct declaration (done)
- enum declaration (done)

### Statements:
- if statements (done)
- while loops (done)
- for loops 
- blocks (done)
- expression statement (done)

### Error handling
- panic mode (done)
- i think there may be some strange edge case with self.unwrap_identifier(), but i think i want to test that later

### Semantic analysis
- may want to decouple the function / methods and members / variables in my enum

### TAC 
- tbh theres so much going on im a little bit lost
- get sizes of all identifiers
- a compiler built in `sizeof()` (should just use the lookup table...)
- the most basic of type casts (for arthemitic operations, as everything is i32 by default and will not compile if you try and add i32 + i8, e.g.)
- function stack sizes (should be able to reuse the same table);
- like, every statement
	- expressions are almost done though!
- ODR, undefined, out of scope (the foundations of these are built)
- auto pointer deref on binary dot operator would be really cool...
	- on bin expre
	- if lhs.get_type() is ptr type:
	- add a deref beforehand! (might be able to do this using the same recursive function?)
	- basically: `struct.method(*Struct)` -> `*struct.method(*Struct)`

### misc
- abstract out error printing logic asp
