export PATH := .:$(PATH)

.PHONY: all clean config install recompile tests

PREFIX := /usr
FAILED := $(shell mktemp -u)
CFLAGS += -std=c99 -O3 -DNDEBUG
boot.c := bootstrap~
repl.o := src/repl.o

all: ol

clean:
	rm -f boot.fasl
	rm -f $(repl.o)

install: ol repl
	install -d /usr/lib/ol
	install -d /usr/lib/ol/etc
	install -d /usr/lib/ol/lib
	install -d /usr/lib/ol/owl
	install -d /usr/lib/ol/r5rs
	install -d /usr/lib/ol/scheme
	install -d /usr/lib/ol/OpenGL
	install -D -m 644 etc/* /usr/lib/ol/etc
	install -D -m 644 lib/* /usr/lib/ol/lib
	install -D -m 644 owl/* /usr/lib/ol/owl
	install -D -m 644 r5rs/* /usr/lib/ol/r5rs
	install -D -m 644 scheme/* /usr/lib/ol/scheme
	install -D -m 644 OpenGL/* /usr/lib/ol/OpenGL
	# install executables:
	install -c -m 644 ol /usr/bin/ol
	install -c -m 644 repl /usr/lib/ol/repl
	
uninstall:
	-rm -rf /usr/lib/ol/
	

# config temporary disabled
config: config/HAS_DLOPEN\
        config/HAS_SOCKETS

config/HAS_DLOPEN:
	@printf "Checking for dlopen()... "
	@if echo "\
	    char dlopen();\
	    \
	    int main() {\
	    return dlopen();\
	    return 0;\
	    }" | gcc -xc - -ldl -o /dev/null 2>/dev/null; then\
		echo "Ok.";\
		printf 1 > $@;\
	else\
		echo "\033[0;31mNot found.\033[0m";\
		printf 0 > $@;\
	fi

config/HAS_SOCKETS:
	@printf "Checking for sockets support... "
	@if echo "\
	    char socket();\
	    \
	    int main() {\
	    return socket();\
	    return 0;\
	    }" | gcc -xc - -ldl -o /dev/null 2>/dev/null; then\
		echo "Ok.";\
		printf 1 > $@;\
	else\
		echo "\033[0;31mNot found.\033[0m";\
		printf 0 > $@;\
	fi


ol: src/olvm.c src/boot.c
	$(CC) $(CFLAGS) src/olvm.c src/boot.c -o $@ -s \
	-Xlinker --export-dynamic -ldl
	@echo Ok.


vm: src/olvm.c
	$(CC) $(CFLAGS) src/olvm.c -DNAKED_VM -o $@ -s \
	-Xlinker --export-dynamic -ldl
	@echo Ok.

	
#src/repl.o: repl
#	objcopy -B i386 -I binary -O default repl src/repl.o
src/boot.c: repl vm
	vm repl <src/to-c.scm >src/boot.c


recompile: boot.fasl
boot.fasl: vm repl src/ol.scm r5rs/*.scm lang/*.scm owl/*.scm
	@vm repl < src/ol.scm
	@if diff boot.fasl repl>/dev/null ;then\
	    echo '\033[1;32m  `___`  \033[0m' ;\
	    echo '\033[1;32m  (o,o)  \033[0m' ;\
	    echo '\033[1;32m  \)  )  \033[0m' ;\
	    echo '\033[1;32m___"_"___\033[0m' ;\
	    echo '\033[1;32mBuild Ok.\033[0m' ;\
	else \
	    cp -b $@ repl ;make $@ ;\
	fi


tests: \
    tests/apply.scm\
    tests/banana.scm\
    tests/callcc.scm\
    tests/case-lambda.scm\
    tests/echo.scm\
    tests/ellipsis.scm\
    tests/eval.scm\
    tests/factor-rand.scm\
    tests/factorial.scm\
    tests/fasl.scm\
    tests/ff-call.scm\
    tests/ff-del-rand.scm\
    tests/ff-rand.scm\
    tests/fib-rand.scm\
    tests/hashbang.scm\
    tests/iff-rand.scm\
    tests/library.scm\
    tests/macro-capture.scm\
    tests/macro-lambda.scm\
    tests/mail-order.scm\
    tests/math-rand.scm\
    tests/par-nested.scm\
    tests/par-nested-rand.scm\
    tests/par-rand.scm\
    tests/perm-rand.scm\
    tests/por-prime-rand.scm\
    tests/por-terminate.scm\
    tests/queue-rand.scm\
    tests/r5rs.scm\
    tests/r7rs.scm\
    tests/record.scm\
    tests/rlist-rand.scm\
    tests/seven.scm\
    tests/share.scm\
    tests/stable-rand.scm\
    tests/str-quote.scm\
    tests/string.scm\
    tests/suffix-rand.scm\
    tests/theorem-rand.scm\
    tests/toplevel-persist.scm\
    tests/utf-8-rand.scm\
    tests/vararg.scm\
    tests/vector-rand.scm\
    tests/numbers.scm
	@rm -f $(FAILED)
	@for F in $^ ;do \
	    echo -n "Testing $$F ... " ;\
	    if ./ol $$F | diff - $$F.ok >/dev/null ;then\
	        echo "Ok." ;\
	    else \
	        echo "\033[0;31mFailed!\033[0m" ;\
	        touch $(FAILED) ;\
	    fi ;\
	done
	@if [ -e $(FAILED) ] ;then rm -f $(FAILED); exit 1 ;fi
	@echo "passed!"

sample-embed:
	gcc src/sample-embed.c src/olvm.c src/boot.c -std=c99 -ldl -DEMBEDDED_VM -DHAS_DLOPEN=1 -DHAS_PINVOKE=1 -o sample-embed \
	-Xlinker --export-dynamic
