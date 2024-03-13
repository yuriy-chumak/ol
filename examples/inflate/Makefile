.PHONY: check

check: input.1.txt.gz input.9.txt.gz input.txt
	@for file in $^; do\
		./main.lisp $$file;\
	done

input.1.txt.gz: input.txt
	gzip -fk $< -c -1 >$@

input.9.txt.gz: input.txt
	gzip -fk $< -c -9 >$@
