# gcc-wasm toolset used
# TODO: set OLVM_NOPINS, check running

ifndef MAKEFILE_MAIN
$(error Use toplevel Makefile, please.)
else

#config
GCC_WASM_ROOT?=/opt/gcc-wasm/build

# build commands
define build_wat
	$(GCC_WASM_ROOT)/gcc/xgcc -B$(GCC_WASM_ROOT)/gcc \
	   -ffreestanding -Iincludes/wasm -Iincludes -O3 \
	   -Wno-error=int-conversion -Wno-int-conversion \
	   -Wno-int-to-pointer-cast \
	   -DHAVE_DLOPEN=1 \
	   -S $< -o $@
endef

tmp/%.wat.o: tmp/%.wat
	/opt/wabt/build/wat2wasm --enable-annotations --relocatable $< -o $@

# rules

vpath %.c src extensions extensions/crt
tmp/%.wat: %.c
	$(call build_wat)

# build intermediate files
tmp/olvm.wat: src/olvm.c \
              $(shell find includes/wasm/ -type f -name '*.h') \
              extras/wasm.mk
tmp/olvm.wat.o: tmp/olvm.wat

tmp/ffi.wat: extensions/ffi.c \
              $(shell find includes/wasm/ -type f -name '*.h') \
              extras/wasm.mk
tmp/ffi.wat.o: tmp/ffi.wat

# build final binary
olvm.wasm: tmp/olvm.wat.o tmp/ffi.wat.o
	wasm-ld --no-entry --allow-undefined \
	   --export=__main_argc_argv \
	   --export=malloc --export=free --export=errno \
	   --export=OLVM_ffi --export=OLVM_mkcb --export OLVM_sizeof \
	   --export-memory \
	   --export-table --growable-table \
	   $^ -o $@

#wasm-opt --enable-nontrapping-float-to-int --enable-sign-ext olvm.wasm -O3 -o olvm.wasm

# #$(GCC_WASM_ROOT)/xgcc -B$(GCC_WASM_ROOT) -E -v -x c /dev/null
endif
