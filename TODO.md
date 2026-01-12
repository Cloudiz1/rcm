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

### Declarations:
- global variables... (should be trivial) (done)
- types (mostly done)
	- array types are strange atm (fixed!)
	- a function pointer type!
		- *fn({type}) type
	- unless i want to support structs being declared at the bottom being able to be used anywhere
	- would require another pass but should work out well if i decide to add a preprocessor too
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

environments
deciding on a for loop syntax might also be good!
