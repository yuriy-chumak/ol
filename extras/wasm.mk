# WAJIC toolset used

ifndef MAKEFILE_MAIN
$(error Use toplevel Makefile, please.)
else

$(shell [ -d extras/wajic ] || mkdir extras/wajic)

extras/wajic/wajicup.js:
	curl -Lk https://github.com/yuriy-chumak/wajic/raw/refs/heads/master/wajicup.js -o $@

extras/wajic/tmp/wajic_system_2.0.34_llvm12_and_higher.zip:
	mkdir -p $(dir $@)
	curl -Lk https://github.com/schellingb/wajic/releases/download/bin/wajic_system_2.0.34_llvm12_and_higher.zip -o $@

extras/wajic/system: extras/wajic/tmp/wajic_system_2.0.34_llvm12_and_higher.zip
	unzip -o $< -d extras/wajic

# just an ol virtual machine, asynchronous!
olvm.wasm: src/olvm.c extras/wajic/wajicup.js extras/wajic/system
	node extras/wajic/wajicup.js -v \
	   $< \
	   $(basename $@).wasm $(basename $@).js $(basename $@).html \
	   -stacksize 1048576 \
	   -no_minify \
	   -cc "-I. -Iextras/wajic"

# full ol, asynchronous!
ol.wasm: src/olvm.c extras/wajic/wajicup.js extras/wajic/system
	node extras/wajic/wajicup.js -v \
	   $< \
	   $(basename $@).wasm $(basename $@).js $(basename $@).html \
	   -stacksize 67108864 \
	   -no_minify \
	   -cc "-I. -Iextras/wajic -DREPL=repl"

# ol compiler (ol without main), syncronous!
olc.wasm: extensions/wasm.c src/olvm.c tmp/repl.c
	node extras/wajic/wajicup.js -v \
	   -stacksize 1048576 \
	$< \
	   $(basename $@).wasm $(basename $@).js \
	   -no_minify \
	   -cc "-I. -Iextras/wajic -DREPL=repl -DOLVM_NOASYNC -DHAVE_DLOPEN=0"


# # full ol, but in embed form (is ready to evaluate commands)
# embed.wasm: src/olvm.c extras/wajic/wajicup.js extras/wajic/system
# 	node extras/wajic/wajicup.js -v \
# 	   $< \
# 	   $(basename $@).wasm $(basename $@).js $(basename $@).html \
# 	   -no_minify \
# 	   -cc "-I. -Iextras/wajic -DREPL=repl -DOLVM_NOASYNC=1"

# embed form with webgl and opengl libraries included
webgl.wasm: jni/embed.c extras/wajic/wajicup.js extras/wajic/system
	node extras/wajic/wajicup.js -v \
	   -stacksize 1048576 \
	$< \
	   $(basename $@).wasm $(basename $@).js \
	   -no_minify \
	   -embed otus/ffi.scm        libraries/otus/ffi.scm \
	   -embed otus/case-apply.scm libraries/otus/case-apply.scm \
	   -embed math/infix-notation.scm \
	                              libraries/math/infix-notation.scm \
	   \
	   -embed OpenGL/config.scm   libraries/OpenGL/config.scm \
	   -embed OpenGL/platform.scm libraries/OpenGL/platform.scm \
	   -embed OpenGL/1.scm        libraries/OpenGL/1.scm \
	   -embed OpenGL/1.1.scm      libraries/OpenGL/1.1.scm \
	   -embed OpenGL/1.2.scm      libraries/OpenGL/1.2.scm \
	   -embed OpenGL/1.3.scm      libraries/OpenGL/1.3.scm \
	   -embed OpenGL/1.4.scm      libraries/OpenGL/1.4.scm \
	   -embed OpenGL/1.5.scm      libraries/OpenGL/1.5.scm \
	   -embed OpenGL/2.scm        libraries/OpenGL/2.scm \
	   -embed OpenGL/2.1.scm      libraries/OpenGL/2.1.scm \
	   \
	   -embed lib/gl.scm          libraries/lib/gl.scm \
	   -embed lib/gl/config.scm   libraries/lib/gl/config.scm \
	   -embed lib/gl/WebGL.lisp   libraries/lib/gl/WebGL.lisp \
	   -cc "-I. -Iextras/wajic -DREPL=repl -DOLVM_NOASYNC=1"


#	   -stacksize 33554432 \

endif
