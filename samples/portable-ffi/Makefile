all: bytecode

bytecode: compile.lisp
	ol compile.lisp

check: bytecode
	@../../vm bytecode
