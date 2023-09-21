# additional features

ifndef MAKEFILE_MAIN
$(error Use toplevel Makefile, please.)
else

# emscripten version 1.37.0 required:
# (fastcomp for asm.js)
# $ apt install cmake node node-acorn
# $ git clone https://github.com/kripken/emscripten /opt/emscripten
# $ git clone https://github.com/kripken/emscripten-fastcomp /opt/emscripten/fastcomp
# $ git clone https://github.com/kripken/emscripten-fastcomp-clang /opt/emscripten/fastcomp/tools/clang
# $ mkdir -p /opt/emscripten/fastcomp/build; cd /opt/emscripten/fastcomp/build; \
    cmake .. -DCMAKE_BUILD_TYPE=Release -DLLVM_TARGETS_TO_BUILD="X86;JSBackend" \
             -DLLVM_INCLUDE_EXAMPLES=OFF -DLLVM_INCLUDE_TESTS=OFF -DCLANG_INCLUDE_EXAMPLES=OFF -DCLANG_INCLUDE_TESTS=OFF; \
    make -j2
# $ rm -f ~/.emscripten
# $ /opt/emscripten/emcc -v 2> /dev/null
# $ sed -i "/LLVM_ROOT/c\LLVM_ROOT = '/opt/emscripten/fastcomp/build/bin'" ~/.emscripten
olvm.js: src/olvm.c extensions/ffi.c
	emcc src/olvm.c extensions/ffi.c -Os \
	     -o $@ -DOLVM_NOMAIN=1 \
		 -DHAS_DLOPEN=1 -DOLVM_FFI=1 \
	     -s WASM=0  -s SIDE_MODULE=1 \
		 -s EXPORT_ALL=1 \
	     --memory-init-file 0
# DISABLE_EXCEPTION_CATCHING ?

# latest emscripten version required:
# $ source /opt/emsdk/emsdk_env.sh && make ol.wasm
ol.wasm: src/olvm.c tmp/repl.c
	emcc src/olvm.c \
	     tmp/repl.c -DREPL=repl \
	     extensions/ffi.c \
	     -O3 -o ol.html \
	         -Iincludes \
	   --use-preload-plugins \
	   -DHAS_DLOPEN=1 -DHAVE_SOCKETS=0 \
	   -s ASYNCIFY \
	   -s ASSERTIONS=0 \
	   -s ALLOW_MEMORY_GROWTH=1 \
	   -s FORCE_FILESYSTEM=0 \
	   -s WASM=1 && \
	# fix bugs in emscripten code \
	sed -i -r -e 's/(if\(result===undefined&&bytesRead===0\)\{)(throw)/\1bytesRead=-1;\2/g' \
	          -e 's/(Input: "\);if\(result!==null)/\1\&\&result!==undefined/' \
	          -e 's/(if\(!result\)\{return )null/\1result/' \
	    ol.js

#	          -e 's/(symbol=)(UTF8ToString\(symbol\))/\1"_"+\2/' ol.js
#	   extensions/ffi.c
#	   -s MAIN_MODULE \
#	   -s EXPORTED_FUNCTIONS=['_main','_OLVM_ffi','_OLVM_idf','_OLVM_sizeof','_OLVM_mkcb'] \

# important note: fix for emscripten to asyncify stdin:
# @@ -755,6 +755,7 @@ var TTY = {
#                                         throw new FS.ErrnoError(29)
#                                 }
#                                 if (result === undefined && bytesRead === 0) {
# +                                       bytesRead = -1;
#                                         throw new FS.ErrnoError(6)
#                                 }
#                                 if (result === null || result === undefined) break;
# @@ -804,7 +805,7 @@ var TTY = {
#                                         }
#                                 } else if (typeof window != "undefined" && typeof window.prompt == "function") {
#                                         result = window.prompt("Input: ");
# -                                       if (result !== null) {
# +                                       if (result !== null && result !== undefined) {
#                                                 result += "\n"
#                                         }
#                                 } else if (typeof readline == "function") {
# @@ -814,7 +815,7 @@ var TTY = {
#                                         }
#                                 }
#                                 if (!result) {
# -                                       return null
# +                                       return result
#                                 }
#                                 tty.input = intArrayFromString(result, true)
#                         }

endif
