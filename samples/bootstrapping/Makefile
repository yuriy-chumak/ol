all: repl

repl.sexp: ../../repl
	../../ol fasl-2-sexp.lisp $< >$@

repl: repl.sexp
	../../ol sexp-2-fasl.lisp <$< >$@
