# RCM

A statically typed compiler written in rust targeting x86 assembly. Heavily inspired by Zig, Rust, and LLVM.
You can run the up to the IR with `cargo run <path>` where path is an rcm file. Example rcm files can be found under `tests`.

In a previous iteration, this project compiled assembly for basic instructions and functions. Currently rewriting the IR to focus on parallelized optimizations, as outlined by Braun et al.

## Language features
- common expressions and statements
- type inference
- arrays and structures
- functions (including functions in structures, takes the zig approach to just implicitly pass in `&<struct>` on all functions that expect it).
- pointers and dot field access
	- like zig, the `->` operator is replaced with a `.` operator for implicit pointer dereferences.
	- also uses `.*` for dereferences, like zig

## Status
- [x] grammar
- [x] lexical analysis
- [x] recursive descent parser
- [x] static analysis
	- [x] type inference
	- [x] type checking
	- [x] checking return paths
	- [x] symbol tables
- [ ] SSA
	- [x] phi functions
	- [ ] CFG (constructed on the fly with braun et al )
		- [x] blocks
		- [ ] conditionals (blocked by short circuiting)
		- [ ] returns
	- [ ] expressions:
		- [x] basics
		- [ ] loads and stores (arrays, structs, pointers)
		- [x] function calls
	- [ ] parallelized optimizations
		- [x] value numbering
		- [x] copy propagation 
		- [ ] common subexpression elimination
		- [ ] peephole optimizations
- [ ] register allocation
	- [ ] liveness analysis
	- [ ] interference graph 
	- [ ] graph coloring
	- [ ] out of ssa translation
- [ ] instruction selection
- [ ] link time optimization

## References
- [Engineering a Compiler](https://gacbe.ac.in/images/E%20books/Engineering%20a%20Compiler%202nd%20ed%20-%20K.%20Cooper,%20L.%20Torczon%20(Morgan%20Kaufman,%202012)%20BBS%20bbbb.pdf)
- [Braun et al's simple and efficient construction of SSA form](https://c9x.me/compile/bib/braun13cc.pdf)
- [Boissinot's revisiting out of SSA translation for correctness](https://inria.hal.science/inria-00349925v1/document)
