all: ol OpenGL.tar

CFLAGS += -DHAVE_DLOPEN=1
L := -ldl -lm -Xlinker --export-dynamic

CFLAGS_DEBUG   := -O0 -g2 -Wall
CFLAGS_RELEASE := -O2 -g0 -DNDEBUG

# inject our "open"
CFLAGS += -DOLVM_SETOPEN="int tar_open(const char *filename, int flags, int mode, void* userdata);OLVM_set_open(olvm, tar_open);"

ol: CFLAGS += $(CFLAGS_DEBUG)
ol: ../../src/olvm.c ../../extensions/ffi.c ../../extensions/tar.c ../../tmp/repl.c
	$(CC) $^ -o $@ -I../../includes \
	   $(CFLAGS) $(L) -DREPL=repl
	@echo Ok.

..tar: ../../libraries
	tar -cvf $@ --owner=OL/2.5 --group= --directory=$< --transform='s/^\.\///' . 

lib.tar: ../../libraries/lib
	tar -cvf $@ --owner=OL/2.5 --group= --directory=$< --transform='s/^\.\///' . 
OpenGL.tar: ../../libraries/OpenGL
	tar -cvf $@ --owner=OL/2.5 --group= --directory=$< --transform='s/^\.\///' . 
