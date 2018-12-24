all: libNewton.so newton.lisp
	LD_LIBRARY_PATH=. ol newton.lisp

libNewton.so: newton-dynamics/coreLibrary_300/projects/posix64/libNewton.so
	cp $< $@

newton-dynamics:
	git clone --depth 1 https://github.com/yuriy-chumak/newton-dynamics.git
newton-dynamics/coreLibrary_300/projects/posix64/libNewton.so:
	make -C newton-dynamics/coreLibrary_300/projects/posix64
