FAILED := $(shell mktemp -u)
CFLAGS := -std=c99 -O3
boot.c := bootstrap~

all: ol tests
boot: bootstrap

install:
	cp -b bootstrap src/boot.c
	
clean:
	rm -f bootstrap
	rm -f $(boot.c)


ol: src/olvm.c src/boot.c
	$(CC) $(CFLAGS) src/olvm.c src/boot.c -O3 -o ol
	
vm: src/olvm.c
	$(CC) $(CFLAGS) src/olvm.c -DNAKED_VM -O3 -o vm

$(boot.c): src/boot.c
	@cp src/boot.c $(boot.c)

boot.fasl: src/ol.scm ol $(boot.c) r5rs/*.scm lang/*.scm owl/*.scm
	@ol src/ol.scm

bootstrap: boot.fasl
	@ol src/to-c.scm > bootstrap
	@if diff bootstrap bootstrap~ >/dev/null ;then\
	    echo '\033[1;32m  `___`  \033[0m' ;\
	    echo '\033[1;32m  (o,o)  \033[0m' ;\
	    echo '\033[1;32m  \)  )  \033[0m' ;\
	    echo '\033[1;32m___"_"___\033[0m' ;\
	    echo '\033[1;32mBuild Ok.\033[0m' ;\
	else \
	    cp bootstrap $(boot.c) ;\
	    make boot ;\
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

.PHONY: tests
