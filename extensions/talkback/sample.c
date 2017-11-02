/*
 * talkback.c
 *
 *  Talkback interface to the olvm
 *  Created on: Apr 13, 2017
 *      Author: uri
 */

#ifdef EMBEDDED_VM
#include <stdio.h>
#include <stdarg.h>
#include <string.h> // memcpy

#if _WIN32
#define TALKBACK_API __declspec(dllexport)
#else
#define TALKBACK_API __attribute__((__visibility__("default")))
#endif

// -------------------------
// OL talkback interface:
void*OL_tb_start();
void OL_tb_stop(void* oltb);

void OL_tb_send(void* state, char* program);
void*OL_tb_eval(void* state, char* program);

int  OL_tb_get_error(void* state);

void OL_tb_set_import_hook(void* state, int (*do_load_library)(const char* thename, char** output));


void* oltb; // OL talkback handle


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
	oltb = OL_tb_start();
	//OL_tb_set_import_hook(oltb, do_load_library);

	// helper function to send formatted data to the olvm
	void send(char* format, ...) {
		char buff[256];
		va_list args;
		va_start(args, format);
		vsnprintf(buff, sizeof(buff), format, args);
		va_end(args);

		OL_tb_send(oltb, buff);
	}

	// this function returns the "integer" result of called function (if any)
	int eval(char* format, ...) {
		char buff[256] = "#t"; // default value if no request present
		if (format) {
			va_list args;
			va_start(args, format);
			vsnprintf(buff, sizeof(buff), format, args);
			va_end(args);
		}

		void* r = OL_tb_eval(oltb, buff);
		if (r != 0) {
			int out = (unsigned)r >> 8;
			if ((((unsigned)r >> 2) & 0x3F) == 32)
				out = -out;
			return out;
		}
		return 0;
	}


	// let's define some demo functions
	send("(define (a n) (* n 17))");
	send("(define (f n) (fold * 1 (iota n 1 1)))");

	// simply sure that functions was processed
	// this function not only sends some data to the vm,
	int got = eval("(a 15)");
	if (got == 0)
		printf("failed.\n");
	else {
		printf("ok.\n");
	}

	//printf("\nLoading 'main.scm' script if exist...");
	//OL_tb_send(oltb, ",load \"tutorial/sample-embed/main.scm\"\n"); <-- direct FS access
	//OL_tb_send(oltb, "(import (tutorial sample-embed main-lib))"); // can be overloaded

	// calling "a" function
	printf("result of 'a(12)' function: %d\n", eval("(a %d)", 12));

/*	printf("calling 'function1(42)' /imported/ function... ");
	got = OL_tb_eval(oltb, "(function1 42)", output, sizeof(output));
	if (OL_tb_get_failed(oltb))
		printf("failed. Error: %s\n", output);
	else
		printf("ok. Len: %d, value [%s]\n", got, output);

	printf("calling 'sum-factorsx(72)' /not existent/ function... ");
	got = OL_tb_eval(oltb, "(sum-factorsx 72)", output, sizeof(output));
	if (OL_tb_get_failed(oltb))
		printf("failed. Error: %s\n", output);
	else
		printf("ok. Len: %d, value [%s]\n", got, output);

	// calling "f" function
	printf("result of 'f(42)' function... ");
	got = OL_tb_eval(oltb, "(f 42)", output, sizeof(output));
	if (OL_tb_get_failed(oltb))
		printf("failed. Error: %s\n", output);
	else
		printf("ok. Len: %d, value [%s]\n", got, output);

	printf("result of 'a(12)' function: %d\n", a(12));
	printf("result of 'b(72)' function: %d\n", b(72));


	printf("\n\n\nbuffers example:\n");
	send(oltb, "(define memcpy-pi (dlsym $ type-int+ \"MEMCPY\" type-vector-raw type-int+ type-int+))");
	send(oltb, "(define memcpy-ip (dlsym $ type-int+ \"MEMCPY\" type-int+ type-vector-raw type-int+))");

	OL_tb_eval(oltb, "memcpy-pi", output, sizeof(output)); // wait for finish


	// So, Let's do the simple operation: copy LONG array to the OLVM and back increased one
	char input[] = { 1, 2, 3, 4, 5, 6, 7, 8 };
	printf("input: %d", input[0]);
	for (int i = 1; i < sizeof(input)/sizeof(*input); i++)
		printf(", %d", input[i]);
	printf("\n");

	printf("Let's apply 'map (lambda (x) (* x x))' to the vector: ");

	int len = sizeof(input);
	send(oltb, "(define buffer (vm:new-raw-object type-vector-raw %d)) (memcpy-pi buffer %d %d)", len, input, len);
	send(oltb, "(define new-buffer (list->vector (map (lambda (x) (* x x)) (vector->list buffer))))");
	send(oltb, "(memcpy-ip %d new-buffer %d)", input, len);

	got = OL_tb_eval(oltb, "memcpy-pi", output, sizeof(output)); // wait for finish
	if (OL_tb_get_failed(oltb))
		printf("failed. Error: %s\n", output);
	else {
		printf("ok.\n");

		printf("output: %d", input[0]);
		for (int i = 1; i < sizeof(input)/sizeof(*input); i++)
			printf(", %d", input[i]);
		printf("\n");
	}

*/
	OL_tb_stop(oltb);
	return 0;
}

#endif

// -----------------------------------------------------------------------------
// public functions:
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
