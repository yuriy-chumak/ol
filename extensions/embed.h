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

#include <olvm.h>

// basic ol data type ('all is the words')
typedef uintptr_t word;

// extension structure
#define olvm_t ol_t
typedef struct ol_t
{
	struct olvm_t* vm;  // ol virtual machine instance
	word eval; // embed pinned 'eval' function id
} ol_t;


//#pragma GCC diagnostic push
//#pragma GCC diagnostic ignored "-Wshift-count-overflow"

// ====================================================================

//! returns not 0 if argument is value
#define is_value(x) (((word)(x)) & 2)

//! returns not 0 if argument is reference
#define is_reference(x) (!is_value(x))

//! returns the type of provided ol value
#define vtype(x) ({ word p = (word)(x);\
		assert (is_value(p) && "argument should be value");\
		(unsigned char)((( (word )(p)) >> 2) & 0x3F); })

//! returns the type of provided ol reference
#define reftype(x) ({ word p = (word)(x);\
		assert (is_reference(p) && "argument should be reference");\
		(unsigned char)(((*(word*)(p)) >> 2) & 0x3F); })

//! returns not 0 if argument is a small number (type-enum+ or type-enum-)
#define is_enum(x) ({ word s = (word)(x);\
		is_value(s) ?\
			vtype(s) == 0 || vtype(s) == 32\
		: 0; })

//! returns not 0 if argument is a number (type-int+ or type-int-)
#define is_number(x) ({ word n = (word)(x);\
		is_enum(n) ? 1 \
		: is_reference(n) ? \
			reftype(n) == 40 || reftype(n) == 41\
			: 0; })

//! returns not 0 if argument is a string (type-string or type-string-wide)
#define is_string(x) ({ word s = (word)(x);\
		is_reference(s) ?\
			reftype(s) == 3 || reftype(s) == 5\
		: 0; })
//! returns length of ol string
#define string_length(x) ({ word o = (word)(x);\
		assert (is_string(o) && "argument should be a string");\
		(int)(((*(word*)o >> 16) - 1) * sizeof(word) -\
		      ((*(word*)o >> 8) & 7)); })
//! returns address of ol string body, this is NOT null terminated string!
#define string_value(x) ({ word o = (word)(x);\
		assert (is_string(o) && "argument should be a string");\
		(const char*)(o+sizeof(word)); })

//! returns not 0 if argument is a bytevector (type-bytevector)
#define is_bytevector(x) ({ word s = (word)(x);\
		is_reference(s) ?\
			reftype(s) == 19 : 0; })
//! returns length of ol bytevector
#define bytevector_length(x) ({ word o = (word)(x);\
		assert (is_bytevector(o) && "argument should be a bytevector");\
		(int)(((*(word*)o >> 16) - 1) * sizeof(word) -\
		      ((*(word*)o >> 8) & 7)); })
//! return address of ol bytevector body
#define bytevector_value(x) ({ word o = (word)(x);\
		assert (is_bytevector(o) && "argument should be a bytevector");\
		(const char*)(o+sizeof(word)); })

//! returns not 0 if argument is a cons (and maybe a list)
#define is_pair(x) ({ word s = (word)(x);\
		is_reference(s) ?\
			reftype(s) == 1\
		: 0; })

//! converts ol small number into C signed integer
#define ol2small(x) ({ word m = (word)(x);\
		assert (is_enum(m) && "argument should be a small number");\
		int v = m >> 8;\
		(m & 0x80) ? -v : v;})

//! converts OL number into C signed integer
#define ol2int(x) ({ word u = (word)(x);\
		assert (is_number(u) && "argument should be a number");\
		is_enum(u) ? ol2small(u)\
			: (word)ol2small(car(u)) | (word)ol2small(cadr(u)) << ((sizeof (word) * 8) - 8)/*FBITS*/;})

//#pragma GCC diagnostic pop

// -----------------------------------------------------------------------------
// some magic functions to be able to process olvm data
// please, do not change

// function naming legend:
// "make_" prefix used for functions that do not allocate any memory
// "new_" prefix do the allocation (in olvm heap, sure).

// same as lisp (ref object n), (car object) and (cdr object) functions
#define ref(ob, n) (word)(((word*)(ob))[n+1]) // +1 is for header skipping
#define car(ob) ref(ob, 0)
#define cdr(ob) ref(ob, 1)

// caar, cadr, cdar, cddr - you can remove this
#define caar(o) car(car(o))
#define cadr(o) car(cdr(o))
#define cdar(o) cdr(car(o))
#define cddr(o) cdr(cdr(o))

// commonly used olvm constants, value type
#define INULL  (word)((2 << 8) | (13 << 2) | 2) // #null
#define IFALSE (word)((0 << 8) | (13 << 2) | 2) // #false
#define ITRUE  (word)((1 << 8) | (13 << 2) | 2) // #true

// --------------------------------------------

/* \brief create olvm integer from C integer
 *
 * Creates olvm integer from C integer, value type
 * This function does not allocate any memory
 *
 * \param val C integer value
 */
#define make_enum(val) (word) ({\
		int v = (int)(val); \
		(v < 0) ? (-v << 8) | 0x82 : (v << 8) | 2; })

#define make_integer(val) make_enum(val)

/* \brief allocate new olvm buffer
 *
 * Allocates new memory buffer in olvm heap, invokes GC
 * if required
 *
 * \param ol embed ol instance
 * \param type buffer type
 *        (this list of types you can find in "scheme/core.scm")
 * \param data source data pointer
 * \param size source data size (and buffer size, sure)
 */
static word new_buffer(ol_t* ol, int type, char* data, int size)
{
	int words = (size + sizeof(word) - 1) / sizeof(word);
	int pads = (words * sizeof(word) - size);

	word* p = (word*)OL_allocate(ol->vm, words);

	++words; // include header size
	*p = (2 | ((word) (words) << 16) | ((type) << 2) | ((pads) << 8) | (1 << 11));
	char* ptr = (char*)&p[1];
	while (size--) // can be change to memcpy(ptr, data, size)
		*ptr++ = *data++;

	return (word)p;
}

/* \brief allocate new olvm string
 *
 * Allocates new ANSI/UTF-8 string in olvm heap, invokes GC
 * if required
 *
 * \param ol embed ol instance
 * \param data zero-terminating source string
 */
static word new_string(ol_t* ol, char* data)
{
	int size = strlen(data);
	return new_buffer(ol, 3, data, size);
}

/* \brief allocate new olvm bytevector
 *
 * Allocates new bytevector in olvm heap, invokes GC
 * if required
 *
 * \param ol embed ol instance
 * \param data source data pointer
 * \param size source data size (and buffer size, sure)
 */
static word new_bytevector(ol_t* ol, char* data, int size)
{
	return new_buffer(ol, 19, data, size);
}

word d2ol(struct ol_t* ol, double v); // extern, olvm.c
static word new_rational(ol_t* ol, double data)
{
	return d2ol(ol->vm, data);
}

///////////////////////////////////////////////////////////////////////////////
// main functions

/* \brief create new embed ol instance
 *
 * Creates new embed ol instance
 *
 * \param embed pointer to store instance data
 */
void embed_new(ol_t* embed, unsigned char* bootstrap, int interactive)
{
	embed->vm = OL_new(bootstrap);
	OL_userdata(embed->vm, embed);

	word r; // execution result
	char* args[] = { "--embed", interactive ? "--interactive" : "--no-interactive" }; // ol execution arguments
	r = OL_run(embed->vm, sizeof(args) / sizeof(*args), args);
	// well, we have our "smart" script prepared,
	//  now save both eval and env variables
	assert (is_enum(r));
	embed->eval = ol2small(r);
}

/* \brief evaluate ol expression
 *
 * Evaluates expression by embed ol instance
 *
 * There are few different manners to call this function:
 * 1. if first argument is string and no more agruments, then evaluator
 *    simply returns value of argument. It means that for "1" it will return make_integer(1),
 *    for "\""hello\"" - pointer to "hello" ol string in memory, etc.
 * 2. if first argument is string and there are more arguments, then evaluator
 *    makes (apply this-string all-arguments). it can be used as quick way to evaluate
 *    function by name with parameters
 * 3. if first argument is numerical and maybe more than 0 arguments, then evaluator
 *    makes case 2, but before tries to deref first argument. in other works evaluator
 *    expects that numberical parameter is number of 'pinned' object.
 *    this case is very useful to store the functions between calls of embed ol because real
 *    address of function can be changed during the GC (garbage collection) and, maybe, fully
 *    removed as "not referenced by anyone" (gc does not knows anything about your native application)
 * 4. and at last if evaluator received bytevector it tries to decode it as fasl object
 *    and evaluate it like 2 or 3.
 *
 * \param ol embed ol instance
 */
word embed_eval(ol_t* embed, ...)
{
	va_list vl;
	va_start(vl, embed);
	int count = 0;
	while (va_arg(vl, void*) != 0)
		count++;
	va_end(vl);

	va_start(vl, embed);
	word* args = __builtin_alloca((count+1) * sizeof(word)); // just one for sanity zero

	args[0] = OL_deref(embed->vm, embed->eval); // deref right now, cause GC possibly moved the object
	int i = 0;
	while ((args[++i] = (word)va_arg(vl, void*)) != 0) ;
	va_end(vl);

	word r = OL_continue(embed->vm, i, (void**)args);
	return r;
}

// ====================================================================
