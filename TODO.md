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
	- struct constructor (should be extrapolated to its own precedence)
- primary (done)

### Declarations:
- types (done)
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
- while statements (done)
- for statements 
- blocks (done)
- expression statement (done)

all of error handling
environments
deciding on a for loop syntax might also be good!
