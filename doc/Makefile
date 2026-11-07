all: R7RS-DIFFERENCES.md

RU=../doc-ru

R7RS-DIFFERENCES.md: $(RU)/R7RS-DIFFERENCES.md
	cat $< |tools/translate $(RU)/glossary.txt >$@
