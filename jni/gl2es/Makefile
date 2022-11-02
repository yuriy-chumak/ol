# cleanup while insuccessfull builds
# ----------------------------------
$(shell [ -s src/gles-2.0.c ] || rm -rf src/gles-2.0.c)

all: src/gles-2.0.c include/gles-2.0.h

src/gles-2.0.c: spec/gles-2.0.json
	ol gen.c.lisp <$^ >$@

include/gles-2.0.h: spec/gles-2.0.json
	ol gen.h.lisp <$^ >$@
