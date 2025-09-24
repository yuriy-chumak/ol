all: src.inc check

.PHONY: check
check:
	./check

src.inc: src.inc.generator
	./$^ >$@
