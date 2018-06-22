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

#ifndef OLVM_EMBED_PREFIX
#define OLVM_EMBED_PREFIX // olvm_
#endif

// basic ol data type ('all is the words')
#include "olvm.h"
typedef uintptr_t word;

// extension structure
typedef struct embed_t
{
	struct ol_t* olvm; // otus lisp virtual machine instance
	// environment and eval function (can be changed after internal GC call)
	word eval, env;

	char* bs_pos; // bootstrap code position
} embed_t;


// otus lisp binary (please, build and link tmp/repl.o)
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

static word new_buffer(void* olvm, int type, char* data, int size)
{
	int words = (size + sizeof(word) - 1) / sizeof(word);
	int pads = (words * sizeof(word) - size);
	++words; // include header size

	word*p = (word*)OL_allocate(olvm, words);
	// #define make_raw_header(type, size, p) (2 | ((word) (size) << 16) | ((type) << 2) | ((p) << 8) | (1 << 11))
	// *p = make_raw_header(type, words, pads); // type-string
	*p = (2 | ((word) (words) << 16) | ((type) << 2) | ((pads) << 8) | (1 << 11));
	char* ptr = (char*)&p[1];
	while (size--)\
		*ptr++ = *data++;

	return (word)p;
}

static word new_string(void* olvm, char* data)
{
	int size = strlen(data);
	return new_buffer(olvm, 3, data, size);
}


static
ssize_t read0(int fd, void *buf, size_t count, void* userdata)
{
	if (fd != 0) // skip if not stdin
		return read(fd, buf, count);

	embed_t* embed = (embed_t*)userdata;

	// no more read
	if (!*embed->bs_pos)
		return read(fd, buf, count); // let's read real stdin

	// read stub code
	int written = 1;
	char* out = buf;
	while (count-- && (*out++ = *embed->bs_pos++))
		++written;
	return written;
}

//public
void embed_new(embed_t* embed)
{
	unsigned char* bootstrap = _binary_repl_start;

	embed->olvm = OL_new(bootstrap);
	OL_set_read(embed->olvm, read0);
	OL_userdata(embed->olvm, embed);

	// embed boot code (prepares eval and env)
	static
	char* bs_code =
			// todo: decode and evaluate the expression
			"(import (lang eval))"
			"(define *interactive* #f)" // disable debugging
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
	embed->bs_pos = bs_code;

	word r; // execution result
	r = OL_run(embed->olvm, 0, 0);
	// well, we have our "smart" script prepared,
	//  now save both of eval and env variables
	assert (r != IFALSE);
	embed->eval = car(r);  embed->env = cdr(r);
}
///////////////////////////////////////////////////////////////////////////////
// main

// C preprocessor trick, some kind of "map":
// https://github.com/swansontec/map-macro
// /*
#define EVAL0(...) __VA_ARGS__
#define EVAL1(...) EVAL0(EVAL0(EVAL0(__VA_ARGS__)))
#define EVAL2(...) EVAL1(EVAL1(EVAL1(__VA_ARGS__)))
#define EVAL3(...) EVAL2(EVAL2(EVAL2(__VA_ARGS__)))
#define EVAL4(...) EVAL3(EVAL3(EVAL3(__VA_ARGS__)))
#define EVAL(...)  EVAL4(EVAL4(EVAL4(__VA_ARGS__)))

#define MAP_END(...)
#define MAP_OUT
#define MAP_COMMA ,

#define MAP_GET_END2() 0, MAP_END
#define MAP_GET_END1(...) MAP_GET_END2
#define MAP_GET_END(...) MAP_GET_END1
#define MAP_NEXT0(test, next, ...) next MAP_OUT
#define MAP_NEXT1(test, next) MAP_NEXT0(test, next, 0)
#define MAP_NEXT(test, next)  MAP_NEXT1(MAP_GET_END test, next)

#define MAP0(f, x, peek, ...) f(x) MAP_NEXT(peek, MAP1)(f, peek, __VA_ARGS__)
#define MAP1(f, x, peek, ...) f(x) MAP_NEXT(peek, MAP0)(f, peek, __VA_ARGS__)

#define MAP_LIST_NEXT1(test, next) MAP_NEXT0(test, MAP_COMMA next, 0)
#define MAP_LIST_NEXT(test, next)  MAP_LIST_NEXT1(MAP_GET_END test, next)

#define MAP_LIST0(f, x, peek, ...) f(x) MAP_LIST_NEXT(peek, MAP_LIST1)(f, peek, __VA_ARGS__)
#define MAP_LIST1(f, x, peek, ...) f(x) MAP_LIST_NEXT(peek, MAP_LIST0)(f, peek, __VA_ARGS__)

#define MAP(f, ...) EVAL(MAP1(f, __VA_ARGS__, ()()(), ()()(), ()()(), 0))
#define MAP_LIST(f, ...) EVAL(MAP_LIST1(f, __VA_ARGS__, ()()(), ()()(), ()()(), 0))
// */  end of C preprocessor trick

#define _s(x) 's', x
#define _i(x) 'i', x

#define _q(x) \
	__builtin_choose_expr( __builtin_types_compatible_p (typeof(x), char[]), \
		's', \
	__builtin_choose_expr( __builtin_types_compatible_p (typeof(x), char*), \
		's', \
	__builtin_choose_expr( __builtin_types_compatible_p (typeof(x), int), \
		'i', \
	__builtin_choose_expr( __builtin_types_compatible_p (typeof(x), unsigned), \
		'i', \
	/*else*/ \
		'e')))), x

word embed_eval(embed_t* embed, ...)
{
	va_list vl;
	va_start(vl, embed);

	int count = 10; // probably, no more that 10 arguments will be?
	word* args = __builtin_alloca(count * sizeof(void*));

	args[0] = embed->eval;
	args[1] = embed->env;

	int i = 1;
	while (++i) {
		if (i > count) {
			word* temp = __builtin_alloca((count + 10) * sizeof(void*));
			memcpy(temp, args, count);
			args = temp;
		}
		switch (va_arg(vl, int))
		{
			case 's':
				args[i] = new_string(embed->olvm, va_arg(vl, char*));
				continue;
			case 'i':
				args[i] = make_integer(va_arg(vl, int));
				continue;
			case 'e':
				printf ("no automatic conversion for type");
				assert (0); break;
			case 0:
				goto end;
		}
	}
end:
	va_end(vl);

	word r = OL_continue(embed->olvm, i, (void**)args);
	embed->eval = car(cdr(r)); embed->env = cdr(cdr(r));

	return car(r);
}

#define _embed_eval embed_eval
#define embed_eval(f, ...) _embed_eval(f, MAP_LIST(_q, __VA_ARGS__), 0)

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

#define reftype(x) ({ uintptr_t p = (uintptr_t)(x);\
		assert (is_reference(p) && "argument should be reference");\
		thetype(*(uintptr_t*)(p)); })


//! returns not 0 if argument is a small signed number
#define is_small(x) ({ uintptr_t s = (uintptr_t)(x);\
		is_value(s) ?\
			valuetype(s) == 0 || valuetype(s) == 32\
		: 0; })

//! returns !0 if argument is a number
#define is_number(x) ({ uintptr_t n = (uintptr_t)(x);\
		is_small(n) ? 1 \
		: is_reference(n) ? \
			reftype(n) == 40 || reftype(n) == 41\
			: 0; })

//! returns !0 if argument is a string
#define is_string(x) ({ uintptr_t s = (uintptr_t)(x);\
		is_reference(s) ?\
			reftype(s) == 3 || reftype(s) == 22\
		: 0; })

//! returns !0 if argument is a cons (and maybe a list)
#define is_pair(x) ({ uintptr_t s = (uintptr_t)(x);\
		is_reference(s) ?\
			reftype(s) == 1\
		: 0; })


//! returns length of ol string
#define string_length(x) ({ uintptr_t o = (uintptr_t)(x);\
		assert (is_string(o) && "argument should be a small number");\
		(int)(((*(uintptr_t*)o >> 16) - 1) * sizeof(uintptr_t) -\
		      ((*(uintptr_t*)o >> 8) & 7)); })
//! returns address of ol string body, this is NOT null terminated string!
#define string_value(x) ({ uintptr_t o = (uintptr_t)(x);\
		assert (is_string(o) && "argument should be a small number");\
		(const char*)(o+sizeof(uintptr_t)); })

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
