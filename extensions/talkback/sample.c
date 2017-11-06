/*
 * talkback.c
 *
 *  Talkback interface to the olvm
 *  Created on: Apr 13, 2017
 *      Author: uri
 */

#include <stdio.h>
#include <stdarg.h>
#include <string.h> // memcpy
#include <assert.h>
#include <stdint.h>

#if _WIN32
#define TALKBACK_API __declspec(dllexport)
#else
#define TALKBACK_API __attribute__((__visibility__("default")))
#endif

#include "talkback.h"
// temporary disable "<<" warning
#pragma GCC diagnostic ignored "-Wshift-count-overflow"


// -----------------------------------------------------------------------------
// helper functions to be able to process olvm data:


// ------------------------
/* IMPORT handler example */
static char* imports[] =
{ "./private/library1.scm",
    "(define-library (private library1)"
    "   (export function1)"
    "   (import (r5rs core) (owl math))"
    "(begin"
    "    (define (function1 x) (+ x 2))"
    "))",
  "./private/library2.scm",
    "(define-library (private library2)"
    "   (export function2)"
    "   (import (r5rs core))"
    "(begin"
    "    (define (function2 x) (- x 2))"
    "))",
  0
};

int do_load_library(const char* thename, char** output)
{
	char** imp = imports;
	while (*imp != 0) {
		if (strcmp(*imp++, thename) == 0) {
			*output = *imp;
			return 0;
		}
		imp++;
	};
	return 1;
}

// do publicly declared memcpy found
TALKBACK_API
void *MEMCPY(void *dest, const void *src, size_t n)
{
	return memcpy(dest, src, n);
}

///////////////////////////////////////////////////////////////////////////////
// main
/*

Talkback interface is the interface between olvm (Otus Lisp virtual machine) and
the external program from which olvm used.

OL_tb_start() and OL_tb_stop() starts new instance of olvm and kills it
respectively. This functions use pthread library to control of olvm execution.

To send some data to olvm (and force olvm to process it) whould be called
OL_tb_send() function. Format of this function very similar to printf(), so
it can be easy to send complex sentences including integer numbersm float, etc.
OL_tb_send() is non blocking function.

Please be aware, that all data sent by OL_tb_send function will be evaluated
as part of global olvm context. It means that any "(define ...)" caluses will
be evaluated and immediately be a part of all olvm functions.

To evaluate some data and receive the result of execution should be used
OL_tb_eval() function. Format of this function very similar to printf(), too.
This function evaluates data in LOCAL olvm context. This means that all
sentences will be forgot by olvm directly after producing result.

OL_tb_eval() is a blocking function. This means that you will be blocked until
olvm produces result or error. In case of error function will return 0.

Additionally you can check the error state using OL_tb_error() function.
Error should be dropped before continue execution using OL_tb_reset() function.

After OL_tb_eval() function olvm to prevent the returned data corruption by gc
switches to sleeping state. To continue execution the olvm (for example to
continue working the subprograms) you must call OL_tb_send or new OL_tb_eval().
In fact OL_tb_send(0) will be enought.


*/
int main(int argc, char** argv)
{
	void* oltb; // talkback handle
	oltb = OL_tb_start();

	void* got;

	// just simplification
	#define eval(format, ...) OL_tb_eval(oltb, format, ##__VA_ARGS__)
	#define send(format, ...) OL_tb_send(oltb, format, ##__VA_ARGS__)

	// let's define some demo functions
	printf("deploying our script functions...\n");
	send("(define (a n) (* n 17))");
	send("(define (f n) (fold * 1 (iota n 1 1)))");
	send("(define (s) \"this is string\")");

	// simply sure that functions was processed
	// this function not only sends some data to the vm,

	// safe variant of demo function
	int call(char* f, int x) {
		void* r = OL_tb_eval(oltb, "(%s %d)", f, x);
		if (r && is_number(r))
			return ol2int(r);
		else
			printf("/ got an error or not an integer result :( /");
		return 0;
	};


	// safe call of demo function a:
	printf("result of 'a(%d)' function: %d\n", 12, call("a", 12));

	// unsafe call (without result check) of same function:
	printf("result of 'a(%d)' function: %d\n", 12, ol2int(eval("(a %d)", 12)));

	printf("result of 'a(%d)' function: %d\n", 3, call("a", 3));
	printf("result of 'f(%d)' function: %d\n", 7, call("f", 7));

	// working with strings:
	got = eval("(s)");
	if (is_string(got))
		printf("got an string: %.*s\n", string_length(got), string_value(got));
	else
		printf("not a string result: %p", got);


	// let's import internal libraries:
	OL_tb_set_import_hook(oltb, do_load_library);
	send("(import (private library1))");
	eval("#t"); // simply wait for ol processing

	printf("result of 'function1': %d\n", ol2int(eval("(function1 4)")));

	// ------------------------------------------------------------------
	//! working with errors:
	printf("calling non existent function... ");
	got = eval("(non-existent-function 4)");
	if (got == 0) // it should be 0 :)
		printf("yupp, got an error.\n");
	else // should not happen
		printf("strange, no error:(\n");

	// so, we got an error. let's process it:
	// typically we got errors as list, so let's iterate over it:
	uintptr_t error = (uintptr_t)OL_tb_error(oltb);
	if (error && is_pair(error)) { // assume that pair is head of proper list
		printf("/ got an error: ");
		while (error != INULL) {
			uintptr_t part = car(error);
			if (is_string(part))
				printf("%.*s ", string_length(part), string_value(part));
			error = cdr(error);
		}
		printf("/ ");
	}


	// --------------------------------------------------------
	// let's check that we successfuly can continue evaluations
	OL_tb_reset(oltb); // clear error
	printf("result of 'a(%d)' function: %d\n", 77, call("a", 77));


	OL_tb_stop(oltb);
	return 0;
}


// -----------------------------------------------------------------------------
// sample public functions:
TALKBACK_API
int add_ii(int fa, int fb)
{
	// math
	int a = fa;    fprintf(stderr, "DEBUG: add_ii (a) = %d\n", a);
	int b = fb;    fprintf(stderr, "DEBUG: add_ii (b) = %d\n", b);
	int r = a + b; fprintf(stderr, "DEBUG: add_ii (r) = %d\n", r);

	// result
	return r;
}
