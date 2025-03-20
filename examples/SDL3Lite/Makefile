all: libSDL3.so
update: SDL3Lite
	cd SDL3Lite; git pull origin

SDL3Lite:
	git clone https://github.com/JordanCpp/SDL3Lite SDL3Lite

SDL3Lite/build: SDL3Lite
	mkdir -p SDL3Lite/build

SDL3Lite/build/libSDL3.so: SDL3Lite/build
	cd SDL3Lite/build; cmake ..; make
	export LD_LIBRARY_PATH=`pwd`/SDL3Lite/build

libSDL3.so: SDL3Lite/build/libSDL3.so
	cp $< $@
