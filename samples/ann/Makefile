all: libann.so

libann.so: ann.c
	gcc $< -shared -fPIC -o libann.so \
	-I../../include -I../../src \
	-Xlinker --export-dynamic \
	-O2 -g2
