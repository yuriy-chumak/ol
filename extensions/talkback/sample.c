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

int main(int argc, char** argv)
{
	void* oltb; // talkback handle

	oltb = OL_tb_start();
	OL_tb_set_import_hook(oltb, do_load_library);

	// just simplification
	#define eval(format, ...) OL_tb_eval(oltb, format, ##__VA_ARGS__)
	#define send(format, ...) OL_tb_send(oltb, format, ##__VA_ARGS__)

	// let's define some demo functions
	char* a = "(define (a n) (* n 17))";
	printf("deploying function %s...\n", a);
	send(a);
	char* f = "(define (f n) (fold * 1 (iota n 1 1)))";
	printf("deploying function %s...\n", f);
	send(f);

	// simply sure that functions was processed
	// this function not only sends some data to the vm,

	// safe variant of demo function "a"
	int call(char* f, int x) {
		void* r = OL_tb_eval(oltb, "(%s %d)", f, x);
		if (is_number(r))
			return ol2int(r);
		else
			printf("/ got an error :( /");
		return 0;
	};


	// safe call of demo function a:
	printf("result of 'a(%d)' function: %d\n", 12, call("a", 12));

	// unsafe call (without result check) of same function:
	printf("result of 'a(%d)' function: %d\n", 12, ol2int(eval("(a %d)", 12)));


	printf("result of 'a(%d)' function: %d\n", 3, call("a", 3));
	printf("result of 'f(%d)' function: %d\n", 7, call("f", 7));

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
