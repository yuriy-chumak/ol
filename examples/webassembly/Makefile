all: ol.js ol.wasm
	echo open http://127.0.0.1:4000/ol.html with web browser
	echo ,l http/server |ol

ol.wasm: ../../ol.wasm
	cp $^ $@

ol.js: ../../ol.js
	cp $^ $@


../../ol.wasm:
	make -C ../.. ol.wasm
