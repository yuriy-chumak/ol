/*
 * embed.h
 *
 *  Otus Lisp extension
 *  that simplifies olvm integration into existing projects
 *
 *  Usage can be found in extensions/embed/sample and
 *                        samples/pacman
 *
 *  Created on: Apr 21, 2018
 *      Author: uri
 */
#pragma once

#include <stdio.h>
#include <stdarg.h>
#include <string.h> // memcpy
#include <assert.h>
#include <stdint.h>
#include <unistd.h>

//#ifndef OLVM_EMBED_PREFIX
//#define OLVM_EMBED_PREFIX embed_
//#endif

// basic ol data type ('all is the words')
#include "olvm.h"
typedef uintptr_t word;

// extension structure
#define olvm_t ol_t
typedef struct ol_t
{
	struct olvm_t* vm; // otus lisp virtual machine instance
	uintptr_t eval; // embed pinned 'eval' function id

	char* bs_pos; // bootstrap code position
} ol_t;


// otus lisp binary (please, build and link repl.o)
extern unsigned char _binary_repl_start[];

//#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wshift-count-overflow"

// -----------------------------------------------------------------------------
// helper functions to be able to process olvm data:
#define make_integer(val) (word) (\
		{ int v = (int)(val); \
		(v < 0) ? (-v << 8) | 0x82 : (v << 8) | 2; }) // integer value

// same as lisp ref, car and cdr functions
#define ref(ob, n) (uintptr_t)(((uintptr_t*)(ob))[n+1]) // +1 is for header skipping
#define car(ob) ref(ob, 0)
#define cdr(ob) ref(ob, 1)

#define caar(o) car(car(o))
#define cadr(o) car(cdr(o))
#define cdar(o) cdr(car(o))
#define cddr(o) cdr(cdr(o))

// commonly used constants
#define INULL  (uintptr_t)((2 << 8) | (13 << 2) | 2)
#define IFALSE (uintptr_t)((0 << 8) | (13 << 2) | 2)
#define ITRUE  (uintptr_t)((1 << 8) | (13 << 2) | 2)

// --------------------------------------------

static word new_buffer(ol_t* ol, int type, char* data, int size)
{
	int words = (size + sizeof(word) - 1) / sizeof(word);
	int pads = (words * sizeof(word) - size);

	word* p = (word*)OL_allocate(ol->vm, words);

	// #define make_raw_header(type, size, p) (2 | ((word) (size) << 16) | ((type) << 2) | ((p) << 8) | (1 << 11))
	// *p = make_raw_header(type, words, pads); // type-string
	++words; // include header size
	*p = (2 | ((word) (words) << 16) | ((type) << 2) | ((pads) << 8) | (1 << 11));
	char* ptr = (char*)&p[1];
	while (size--)\
		*ptr++ = *data++;

	return (word)p;
}

static word new_string(ol_t* ol, char* data)
{
	int size = strlen(data);
	return new_buffer(ol, 3, data, size);
}

static word new_bytevector(ol_t* ol, char* data, int size)
{
	return new_buffer(ol, 19, data, size);
}


static
ssize_t read0(int fd, void *buf, size_t count, void* userdata)
{
	if (fd != 0) // skip if not stdin
		return read(fd, buf, count);

	ol_t* ol = (ol_t*)userdata;

	// no more read
	if (!*ol->bs_pos)
		return read(fd, buf, count); // let's read real stdin

	// read stub code
	int written = 1;
	char* out = buf;
	while (count-- && (*out++ = *ol->bs_pos++))
		++written;
	return written;
}

//public
void embed_new(ol_t* embed)
{
	unsigned char* bootstrap = _binary_repl_start;

	embed->vm = OL_new(bootstrap);
	OL_set_read(embed->vm, read0);
	OL_userdata(embed->vm, embed);

	// embed boot code (prepares eval and env)
	static
	char* bs_code =
			"(define *interactive* #f)" // disable debugging
			"(import (lang eval)"
			"        (owl fasl))"
			"(halt (list (vm:pin"
			"(let*((this (cons (vm:pin *toplevel*) 0))" // internal function state (env . 0)
			"      (eval (lambda (exp args)"              // expression processor
			"               (tuple-case exp"
			"                  ((ok value env)"
			"                     (vm:unpin (car this))"     // release old env
			"                     (set-car! this (vm:pin env))"  // new env
			"                     (if (null? args)"
			"                        value"
			"                        (apply value args)))"
			"               (else is error"
			"                  (print-to stderr \"error: \" error)"
			"                  #false)))))"
			"   (lambda (expression) (halt"
			"      (let*((env (vm:deref (car this)))"
			"            (exp args (uncons expression #f)))"
			"      (case (type exp)"
			"         (type-string type-string-wide"
			"            (eval (eval-string env exp) args))"
			"         (type-fix+"
			"            (eval (eval-repl (vm:deref exp) env #f evaluate) args))"
			"         (type-bytevector"
			"            (eval (eval-repl (fasl-decode (vector->list exp) #f) (vm:deref (car this)) #f evaluate) args))"
			"         (else"
			"            (print \"Unprocessible expression type \" (type exp)))))))))))";
	embed->bs_pos = bs_code;

	word r; // execution result
	r = OL_run(embed->vm, 0, 0);
	// well, we have our "smart" script prepared,
	//  now save both eval and env variables
	assert (r != IFALSE);
	embed->eval = car(r);

}
///////////////////////////////////////////////////////////////////////////////
// main

word embed_eval(ol_t* ol, ...)
{
	va_list vl;
	va_start(vl, ol);
	int count = 0;
	while (va_arg(vl, void*) != 0)
		count++;
	va_end(vl);

	va_start(vl, ol);
	uintptr_t* args = __builtin_alloca((count+1) * sizeof(uintptr_t)); // just one for sanity zero

	args[0] = OL_deref(ol->vm, ol->eval); // deref right now, cause GC possibly moved the object
	int i = 0;
	while ((args[++i] = (uintptr_t)va_arg(vl, void*)) != 0) ;
	va_end(vl);

	// while (count--) {
	//     args[++i] = (uintptr_t)va_arg(vl, void*);
	// }

	word r = OL_continue(ol->vm, i, (void**)args);
	return r;
}

// ====================================================================

//! returns not 0 if argument is value
#define is_value(x) (((uintptr_t)(x)) & 2)

//! returns not 0 if argument is reference
#define is_reference(x) (!is_value(x))

//! returns the type of provided ol variable
#define thetype(x) (unsigned char)((((uintptr_t)(x)) >> 2) & 0x3F)

//! returns the type of provided ol value
#define valuetype(x) ({ uintptr_t p = (uintptr_t)(x);\
		assert (is_value(p) && "argument should be value");\
		thetype(p) & 0x1F; })

//! returns the type of provided ol reference
#define reftype(x) ({ uintptr_t p = (uintptr_t)(x);\
		assert (is_reference(p) && "argument should be reference");\
		thetype(*(uintptr_t*)(p)); })


//! returns not 0 if argument is a small signed number
#define is_small(x) ({ uintptr_t s = (uintptr_t)(x);\
		is_value(s) ?\
			valuetype(s) == 0 || valuetype(s) == 32\
		: 0; })

//! returns not 0 if argument is a number
#define is_number(x) ({ uintptr_t n = (uintptr_t)(x);\
		is_small(n) ? 1 \
		: is_reference(n) ? \
			reftype(n) == 40 || reftype(n) == 41\
			: 0; })

//! returns not 0 if argument is a string
#define is_string(x) ({ uintptr_t s = (uintptr_t)(x);\
		is_reference(s) ?\
			reftype(s) == 3 || reftype(s) == 22\
		: 0; })
//! returns length of ol string
#define string_length(x) ({ uintptr_t o = (uintptr_t)(x);\
		assert (is_string(o) && "argument should be a string");\
		(int)(((*(uintptr_t*)o >> 16) - 1) * sizeof(uintptr_t) -\
		      ((*(uintptr_t*)o >> 8) & 7)); })
//! returns address of ol string body, this is NOT null terminated string!
#define string_value(x) ({ uintptr_t o = (uintptr_t)(x);\
		assert (is_string(o) && "argument should be a string");\
		(const char*)(o+sizeof(uintptr_t)); })

#define is_bytevector(x) ({ uintptr_t s = (uintptr_t)(x);\
		is_reference(s) ?\
			reftype(s) == 19\
		: 0; })
#define bytevector_length(x) ({ uintptr_t o = (uintptr_t)(x);\
		assert (is_bytevector(o) && "argument should be a bytevector");\
		(int)(((*(uintptr_t*)o >> 16) - 1) * sizeof(uintptr_t) -\
		      ((*(uintptr_t*)o >> 8) & 7)); })
#define bytevector_value(x) ({ uintptr_t o = (uintptr_t)(x);\
		assert (is_bytevector(o) && "argument should be a bytevector");\
		(const char*)(o+sizeof(uintptr_t)); })

//! returns not 0 if argument is a cons (and maybe a list)
#define is_pair(x) ({ uintptr_t s = (uintptr_t)(x);\
		is_reference(s) ?\
			reftype(s) == 1\
		: 0; })

//! converts OL small into C signed integer
#define ol2small(x) ({ uintptr_t m = (uintptr_t)(x);\
		assert (is_small(m) && "argument should be a small number");\
		int v = m >> 8;\
		(m & 0x80) ? -v : v;})

//! converts OL number into C signed integer
#define ol2int(x) ({ uintptr_t u = (uintptr_t)(x);\
		assert (is_number(u) && "argument should be a number");\
		is_small(u) ? ol2small(u)\
			: ol2small(car(u)) | ol2small(cadr(u)) << ((sizeof (uintptr_t) * 8) - 8)/*FBITS*/;})

//#pragma GCC diagnostic pop
