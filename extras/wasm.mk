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


ol.wasm: src/olvm.c extras/wajic/wajicup.js extras/wajic/system
	node extras/wajic/wajicup.js -v \
	   $< \
	   $(basename $@).wasm $(basename $@).js $(basename $@).html \
	   -stacksize 67108864 \
	   -no_minify \
	   -cc "-I. -Iextras/wajic"

endif
