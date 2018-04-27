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

#define make_raw_header(type, size, p) (2 | ((word) (size) << 16) | ((type) << 2) | ((p) << 8) | (1 << 11))

static void* new_string(void* olvm, char* data)
{
	word* fp;
	fp = ((OL*)olvm)->heap.fp;

	int size = strlen(data);
	int words = (size + W - 1) / W;
	int pads = (words * W - size);
	++words; // include header size

	word*p = fp;
	fp += words;
	*p = make_raw_header(3, words, pads); // type-string
	char* ptr = (char*)&p[1];
	while (size--)\
		*ptr++ = *data++;

	((OL*)olvm)->heap.fp = fp;
	return p;
}


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
		"(define eval (lambda (args) "
		""
		"(halt (apply"
		"			(lambda (expression env)"
		"				(tuple-case (repl-string env expression)"
		"					((ok value env)"
		"						(cons value env))" // and return pair (result . new-environment)
		"					(else"
		"						#false)))"
		"			args))))"	// no "else" clause for now
		"(halt (cons eval *toplevel*))";

//"		(type-bytecode"	// just execute bytecode
//"			(lambda (bytecode . args)"
//"				(apply bytecode args)))"

//		"	(case (type (car args))"
//		"		(type-string"	// if string evaluate string

//		"	(print \"args: \" (car args))"
//		"))"

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

int main(int argc, char** argv)
{
	void* olvm; // talkback handle
	unsigned char* bootstrap = _binary_repl_start;
	olvm = OL_new(bootstrap);
//	read_t old_reader =
	OL_set_read(olvm, read0);

	void* eval = 0;
	void* env = 0;

	void* r = (void*) OL_run(olvm, 0, 0);
	eval = car(r);
	env = cdr(r);

	// let's save eval for feature use
	void* x[] = { eval, new_string(olvm, "12"), env };
	OL_continue(olvm, 3, (void**)x);

//	void* y[] = { eval, I(22), env };
//	OL_continue(olvm, 3, (void**)y);

	// let's execute precompiled string


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
