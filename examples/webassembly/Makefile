all: olc.wasm olc.js
	@echo open http://127.0.0.1:4000/ol.html if you want Ol terminal
	@echo
	@echo ,l http/server |ol

# all: ol.js ol.wasm
# 	@echo open http://127.0.0.1:4000/ol.html if you want Ol terminal
# 	@echo
# 	@echo ,l http/server |ol

ol.wasm: ../../ol.wasm
	cp $< $@

ol.js: ../../ol.js
	cp $< $@


../../ol.wasm:
	make -C ../.. ol.wasm

# -=( olc )=-----------
olc.wasm: ../../olc.wasm 
	cp $< $@
olc.js:   ../../olc.js
	cp $< $@
../../olc.wasm: ../../extensions/wasm.c ../../extras/wasm.mk
	make -C ../.. olc.wasm

