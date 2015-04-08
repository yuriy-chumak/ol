
CFLAGS := -std=c99 -O3

all: ol

ol: src/olvm.c src/boot.c Makefile tests
	$(CC) $(CFLAGS) src/olvm.c src/boot.c -DSTANDALONE -ldl -o ol

#vm: src/olvm.c

tests: \
    tests/apply.scm\
    tests/banana.scm\
    tests/numbers.scm\
    tests/callcc.scm
#<------>bisect-rand.scm
#<------>callcc.scm
#<------>case-lambda.scm
#<------>echo.scm
#<------>ellipsis.scm
#<------>eval.scm
#<------>factor-rand.scm
#<------>factorial.scm
#<------>fasl.scm
#<------>ff-call.scm
#<------>ff-del-rand.scm
#<------>ff-rand.scm
#<------>fib-rand.scm
#<------>hashbang.scm
#<------>iff-rand.scm
#<------>library.scm
#<------>macro-capture.scm
#<------>macro-lambda.scm
#<------>mail-order.scm
#<------>math-rand.scm
#<------>par-nested.scm
#<------>par-nested-rand.scm
#<------>par-rand.scm
#<------>perm-rand.scm
#<------>por-prime-rand.scm
#<------>por-terminate.scm
#<------>queue-rand.scm
#<------>r5rs.scm
#<------>r7rs.scm
#<------>record.scm
#<------>rlist-rand.scm
#<------>seven.scm
#<------>share.scm
#<------>stable-rand.scm
#<------>str-quote.scm
#<------>string.scm
#<------>suffix-rand.scm
#<------>theorem-rand.scm
#<------>toplevel-persist.scm
#<------>utf-8-rand.scm
#<------>vararg.scm
#<------>vector-rand.scm
#<------>numbers.scm
	@rm -f /tmp/failed.$$$$
	@for F in $^ ; do \
	    echo -n "Testing $$F ... " ;\
	    if ol $$F | diff - $$F.ok >/dev/null ; then\
	        echo "Ok." ;\
	    else\
	        echo "\033[0;31mFailed!\033[0m" ;\
	        $(eval TMP := 1) \
	    fi;\
	done
	@if [ "$(TMP)" = "1" ]; then exit 1; fi
	echo "Tests passed!"

.PHONY: tests
