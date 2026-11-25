# gcc-wasm toolset used

ifndef MAKEFILE_MAIN
$(error Use toplevel Makefile, please.)
else

# TODO: set OLVM_NOPINS, check running

GCC_WASM_ROOT=/opt/gcc-wasm/build/gcc
tmp/olvm.wat: src/olvm.c $(shell find includes/wasm/ -type f -name '*.h')
	$(GCC_WASM_ROOT)/xgcc -B$(GCC_WASM_ROOT) \
	   -ffreestanding -Iincludes/wasm -O3 \
	   -Wno-error=int-conversion -Wno-int-conversion \
	   -S $< -o $@

tmp/olvm.wat.o: tmp/olvm.wat
	/opt/wabt/build/wat2wasm --enable-annotations --relocatable $< -o $@

olvm.wasm: tmp/olvm.wat.o
	wasm-ld --no-entry --allow-undefined \
	   --export=__main_argc_argv \
	   --export=malloc --export=free --export=errno \
	   --export-memory \
	   $^ -o $@
	#wasm-opt --enable-nontrapping-float-to-int --enable-sign-ext olvm.wasm -O3 -o olvm.wasm

#$(GCC_WASM_ROOT)/xgcc -B$(GCC_WASM_ROOT) -E -v -x c /dev/null
endif
