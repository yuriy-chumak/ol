/*
 * embed/sample.c
 *
 *  Embed sample to the olvm
 *  Created on: Apr 21, 2018
 *      Author: uri
 */

#include <stdio.h>
#include <stdarg.h>
#include <string.h> // memcpy
#include <assert.h>
#include <stdint.h>

#include "olvm.h"

/*#if _WIN32
#define TALKBACK_API __declspec(dllexport)
#else
#define TALKBACK_API __attribute__((__visibility__("default")))
#endif*/

/*#include "talkback.h"*/
// temporary disable "<<" warning
#pragma GCC diagnostic ignored "-Wshift-count-overflow"


// -----------------------------------------------------------------------------
// helper functions to be able to process olvm data:
typedef uintptr_t word;
typedef struct heap_t
{
	//  begin <= genstart <= end
	word *begin;     // begin of heap memory block
	word *end;       // end of heap
	word *genstart;  // new generation begin pointer
	// new (size) === *(size*)fp++
	word *fp;        // allocation pointer
} heap_t;

struct ol_t
{
	struct heap_t heap; // MUST be first member
};

#define I(val) (void*) (\
		((val) << 8) | 2)
#define R(v) ({\
		word reference = (word)(v);\
		(word*) reference; })
#define W sizeof(word)

#define ref(ob, n)                  ((R(ob))[n])
#define car(ob)                     ((R(ob))[1])
#define cdr(ob)                     ((R(ob))[2])

#define make_value(type, value)        (2 | ((word)(value) << 8) | ((type) << 2))
#define make_raw_header(type, size, p) (2 | ((word) (size) <<16) | ((type) << 2) | ((p) << 8) | (1 << 11))

static void* new_buffer(void* olvm, int type, char* data, int size)
{
	word* fp;
	fp = ((OL*)olvm)->heap.fp;

	int words = (size + W - 1) / W;
	int pads = (words * W - size);
	++words; // include header size

	word*p = fp;
	fp += words;
	*p = make_raw_header(type, words, pads); // type-string
	char* ptr = (char*)&p[1];
	while (size--)\
		*ptr++ = *data++;

	((OL*)olvm)->heap.fp = fp;
	return p;
}

static void* new_string(void* olvm, char* data)
{
	int size = strlen(data);
	return new_buffer(olvm, 3, data, size);
}

#define IFALSE                      make_value(13, 0) // #false





// ------------------------
/* IMPORT handler example */
/*static char* imports[] =
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
};*/

/*int do_load_library(const char* thename, char** output)
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
}*/

// do publicly declared memcpy found
/*TALKBACK_API
void *MEMCPY(void *dest, const void *src, size_t n)
{
	return memcpy(dest, src, n);
}*/

///////////////////////////////////////////////////////////////////////////////
// main
extern unsigned char _binary_repl_start[];

static char* bs_code =
		// todo: decode and evaluate the expression
		"(import (lang eval))"
		"(define (eval args) "
//		"	(print \"(cdr args): \" (cdr args))"
//		"	(print \"(length args): \" (length args))"
		"	(let ((expression "
		"		(apply"
		"			(lambda (env expression)"
//		"				(print \"expression: \" expression)"
		"				(tuple-case (eval-string env expression)"
		"					((ok value env)"
		"						(cons value (cons eval env)))" // and return pair (result . new-environment)
		"					(else"
		"						#false)))"	// currently after fail we broke the environment
		"			(take args 2))))"		// TODO: change this to recovering,
//		"	(print \"(car expression): \" (car expression))"
//		"	(print \"(cddr args): \" (cddr args))"
		"	(halt"
		"		(if (eq? (length args) 2)"	//  (just create some flag like 'ok and 'failed)
		"			expression"				// no additional steps required
		"			(cons (apply (car expression) (cddr args))"
		"				(cdr expression))))))"
		// return compiled "eval" function with current environment
		"(halt (cons eval *toplevel*))";



//"	)"

static
ssize_t read0(int fd, void *buf, size_t count, void* userdata)
{
	if (fd != 0) // not stdin
		return read(fd, buf, count);

	// let's read our bootsrap code via stdin
	static char* bs_pos = 0;
	if (bs_pos == 0)
		bs_pos = bs_code;
	if (!*bs_pos)
		return read(fd, buf, count); // let's read real stdin

	int written = 1;
	char* out = buf;
	while (count-- && (*out++ = *bs_pos++))
		++written;
	return written;
}

void* olvm; // otus lisp virtual machine instance
static
void *eval, *env;

void* embed_eval(char* format, ...)
{
	va_list vl;
	va_start(vl, format);

	int count = strlen(format);
	void** args = __builtin_alloca(count * sizeof(void*));

	args[0] = eval;
	args[1] = env;

	int i = 1;
	while (++i) // eternal loop
	switch (*format++)
	{
		case 's':
			args[i] = new_string(olvm, va_arg(vl, char*));
			continue;
		case 'i':
			args[i] = I(va_arg(vl, int));
			continue;
		case 0:
			goto end;
	}
end:
	va_end(vl);

	void* r = OL_continue(olvm, count+2, (void**)args);
	eval = car(cdr(r)); env = cdr(cdr(r));

	return (void*) car(r);
}

int main(int argc, char** argv)
{
	unsigned char* bootstrap = _binary_repl_start;
	olvm = OL_new(bootstrap);
	OL_set_read(olvm, read0);

	void *r; // just variables

	r = (void*) OL_run(olvm, 0, 0);
	// well, we have our "smart" script prepared,
	//  now save both of eval and env variables
	assert (r != IFALSE);
	eval = car(r);  env = cdr(r);

	// about our arguments
	//	if eval receives only one expression, then simply calculate and return it
	//	if expression with argments, the additionally apply this arguments to expression
	//	Note: if you want to call expression, but you have not arguments, just use
	//		parenthesis, like "(print)" instead of "print"

	// simple string evaluator example:
	{
		void* args[] = { eval, env,
				new_string(olvm, "+"), I(1), I(2), I(3)
		};
		r = OL_continue(olvm, 6, (void**)args);
		// better use sizeof(args)/sizeof(*args) instead of 6
		if (r != IFALSE) { // it's ok?
			assert (car(r) >> 8  ==  6);
		}
		//	additionally, save the new environment for feature
		//	calls. It's very important because old saved pointers
		//	can be invalidated during vm execution by GC.
		eval = car(cdr(r)); env = cdr(cdr(r));
	}

	// Well. Let's define new function
	{
		void *args[] = { eval, env,
				new_string(olvm, "(define (plus a b) (+ a b))")
		};
		r = OL_continue(olvm, 3, (void**)args);
		if (r == IFALSE) { // it's ok?
			assert (0 && "something wrong");
		}

		//	again, don;t forget to update eval and env1
		//	don't play games with GC
		eval = car(cdr(r)); env = cdr(cdr(r));
	}

	// Now, call our new function
	{
		void *args[] = { eval, env,
				new_string(olvm, "plus"), I(7), I(35)
		};
		r = OL_continue(olvm, 5, (void**)args);
		assert (r != IFALSE && "something wrong");

		assert (car(r) == I(42) && "7+35 must be equal to 42");
		printf("ok.\n");

		eval = car(cdr(r)); env = cdr(cdr(r));

	}

	embed_eval("sii", "plus", 12, 23);

	// done. free resources (and kill olvm)
	OL_free(olvm);
	return 0;
}


// -----------------------------------------------------------------------------
// sample public functions:
/*TALKBACK_API
int add_ii(int fa, int fb)
{
	// math
	int a = fa;    fprintf(stderr, "DEBUG: add_ii (a) = %d\n", a);
	int b = fb;    fprintf(stderr, "DEBUG: add_ii (b) = %d\n", b);
	int r = a + b; fprintf(stderr, "DEBUG: add_ii (r) = %d\n", r);

	// result
	return r;
}
*/
