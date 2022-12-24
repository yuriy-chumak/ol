all: repl

repl.sexp: ../../repl
	../../ol fasl-2-sexp.lisp $< >$@

TOOL?=awk
$(shell [ -s repl ] || rm -rf repl)

# -=( awk )=------------------------------------
ifeq ($(TOOL),awk)

repl.text: repl.sexp
	awk -f awk/sexp-2-fasl.awk --bignum  <$< >$@

# -=( ol )=-----------------------------
else ifeq ($(TOOL),ol)

repl.text: repl.sexp
	../../ol ol/sexp-2-fasl.lisp <$< >$@

else
$(error TOOL must be 'awk' or 'ol')
endif

repl: repl.text
	cat repl.text |LC_ALL=C awk -f txt2bin.awk >repl
