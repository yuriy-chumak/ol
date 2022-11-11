# linux/*nix
vm: src/olvm.c
vm: extensions/ffi.c
ol: src/olvm.c
ol: extensions/ffi.c
ol: tmp/repl.c
libol.so: src/olvm.c
libol.so: extensions/ffi.c
libol.so: tmp/repl.c

# win/wine
ol%.exe: src/olvm.c
ol%.exe: extensions/ffi.c
ol%.exe: tmp/repl.c

# extensions
extensions/ffi.c: CFLAGS += -Iincludes
extensions/ffi.c: includes/ol/vm.h

