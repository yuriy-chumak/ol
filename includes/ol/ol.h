/*!# Otus Lisp
 * ```
 *                   small,
 *         `___`         embeddable
 *         (O,O)             and
 *         \)  )          purely
 *       ---"-"---     functional!
 * 
 * Copyright(c) 2014 - 2023 Yuriy Chumak
 * ```
 */
/**
 * This program is free software;  you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
**/
#pragma once

/**
 * Otus Lisp (Ol in short) is a purely functional dialect of Lisp.
 * 
 * Ol implements an extended subset of the R7RS Scheme, including
 * but not limited to some SRFIs. It's tiny (~ 64KB), embeddable,
 * and cross-platform;  provides a portable, high-level interface
 * to call code written in another language (c, python, lua, etc).
 * 
 * You can use Ol on Linux, Windows, macOS, Android, BSD (and its
 * descendants), webOS, Solaris and other operating systems based
 * on various hardware architectures (intel, arm, ppc, mips, etc).
**/

#ifndef __OL_H__65374DBFFB460CF7E3F765DF2A1F3A24__
#define __OL_H__65374DBFFB460CF7E3F765DF2A1F3A24__
#ifdef __cplusplus
    extern "C" {
#endif

#include <assert.h> // assert
#include <string.h> // strlen

#include <stdint.h> // uintptr_t
#include <unistd.h> // (s)size_t

#include <stdarg.h> // va_list

// notes: (игра слов)
// OL:
//	* сокращение от названия проекта - Otus Lisp (вырос из Owl Lisp'а),
//	* низкий порог вхождения (0L - число 0, L - level): Lisp - ОЧЕНЬ простой язык,
//	* тег нумерованного списка в html - (еще одна отсылка к lisp - языку обработки списков),
//	* ol' - сокращение от old (старый), отсылка к тому, что lisp - один из старейших языков.
//  * А еще, в корейском похожий иероглиф (이) переводится как "Это", "This".
// Otus (англ) - совка, подтип сов; создание (фин).
struct olvm_t;

// --------------------------------------------------------------------------
/**
 * Create new OL virtual machine (olvm)
 *
 * \param[in] Bootstrap binary code to be executed by olvm.
 *            If NULL then default REPL will be used.
 * \return Created olvm instance
 */
struct olvm_t*
OLVM_new(unsigned char* bootstrap);

/**
 * Destroy created OL virtual machine (olvm)
 *
 * \param[in] ol Valid olvm instance
 */
void
OLVM_delete(struct olvm_t* ol);

/**
 * Run the OL virtual machine (olvm)
 *
 * \param[in] ol Valid olvm instance
 * \param[in] argc Arguments count
 * \param[in] argv Arguments array
 *
 */
uintptr_t
OLVM_run(struct olvm_t* ol, int argc, char** argv);

/**
 * Continue OL vm after stop (if possible)
 *
 * \param[in] ol Valid olvm instance
 * \param[in] argc Arguments count
 * \param[in] argv Arguments array
 *
 * \note First argument must be valid olvm runnable object (function, bytecode, etc.)
 */
uintptr_t
OLVM_evaluate(struct olvm_t* ol, uintptr_t function, int argc, uintptr_t* argv);

uintptr_t
OLVM_apply(struct olvm_t* ol, uintptr_t function, uintptr_t args);

// --------------------------------------------------------------------------
// pinned objects support api
size_t
OLVM_pin(struct olvm_t* ol, uintptr_t ref);

uintptr_t
OLVM_deref(struct olvm_t* ol, uintptr_t id);

uintptr_t
OLVM_unpin(struct olvm_t* ol, size_t p);


// --------------------------------------------------------------------------
/**
 * Set and return userdata associated with olvm instance
 */
void*
OLVM_userdata(struct olvm_t* ol, void* userdata);

/**
 * Allocate new object in olvm heap
 */
void*
OLVM_allocate(struct olvm_t* ol, unsigned words);


// ---==( i/o hooks API )==--------------------------------------------------
// i/o hooks api

// open/close
typedef int (open_t) (const char *filename, int flags, int mode, void* userdata);
typedef int (close_t)(int fd, void* userdata);
open_t*  OLVM_set_open(struct olvm_t* ol, open_t open);
close_t* OLVM_set_close(struct olvm_t* ol, close_t close);

// read/write
typedef ssize_t (read_t) (int fd, void *buf, size_t count, void* userdata);
typedef ssize_t (write_t)(int fd, void *buf, size_t count, void* userdata);
read_t*  OLVM_set_read(struct olvm_t* ol, read_t read);
write_t* OLVM_set_write(struct olvm_t* ol, write_t write);

// stat
struct stat;
typedef int (stat_t) (const char *filename, struct stat *st, void* userdata);
stat_t*  OLVM_set_stat(struct olvm_t* ol, stat_t stat);

// idle
typedef void (idle_t) (void* userdata);
idle_t*  OLVM_set_idle(struct olvm_t* ol, idle_t idle);

// ==========================================================================
struct olvm_t {
	// new (size) === { *(size*)fp; fp += size; }
	uintptr_t *fp;        // allocation pointer

	// always: begin <= genstart <= end
	uintptr_t *begin;     // begin of heap
	uintptr_t *end;       // end of heap
	uintptr_t *genstart;  // young generation begin pointer

	// call the GC,
	// returns 1 if does something
	int (*gc)(struct olvm_t* ol, unsigned words);
};

// =====================================================================
// helpers

//! returns not 0 if argument is value
#define is_value(x) (((uintptr_t)(x)) & 2)

//! returns not 0 if argument is reference
#define is_reference(x) (!is_value(x))

//! returns the type of provided ol value
#define vtype(x) ({ uintptr_t p = (uintptr_t)(x);\
		assert (is_value(p) && "argument should be value");\
		(unsigned char)((( (uintptr_t )(p)) >> 2) & 0x3F); })

//! returns the type of provided ol reference
#define reftype(x) ({ uintptr_t p = (uintptr_t)(x);\
		assert (is_reference(p) && "argument should be reference");\
		(unsigned char)(((*(uintptr_t*)(p)) >> 2) & 0x3F); })

//! returns not 0 if argument is a small number (type-enum+ or type-enum-)
#define is_enum(x) ({ uintptr_t s = (uintptr_t)(x);\
		is_value(s) ?\
			vtype(s) == 0 || vtype(s) == 32\
		: 0; })

//! returns not 0 if argument is a number (type-int+ or type-int-)
#define is_number(x) ({ uintptr_t n = (uintptr_t)(x);\
		is_enum(n) ? 1 \
		: is_reference(n) ? \
			reftype(n) == 40 || reftype(n) == 41\
			: 0; })

//! returns not 0 if argument is a string (type-string or type-string-wide)
#define is_string(x) ({ uintptr_t s = (uintptr_t)(x);\
		is_reference(s) ?\
			reftype(s) == 3 || reftype(s) == 5\
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

//! returns not 0 if argument is a bytevector (type-bytevector)
#define is_bytevector(x) ({ uintptr_t s = (uintptr_t)(x);\
		is_reference(s) ?\
			reftype(s) == 19 : 0; })
//! returns length of ol bytevector
#define bytevector_length(x) ({ uintptr_t o = (uintptr_t)(x);\
		assert (is_bytevector(o) && "argument should be a bytevector");\
		(int)(((*(uintptr_t*)o >> 16) - 1) * sizeof(uintptr_t) -\
		      ((*(uintptr_t*)o >> 8) & 7)); })
//! return address of ol bytevector body
#define bytevector_value(x) ({ uintptr_t o = (uintptr_t)(x);\
		assert (is_bytevector(o) && "argument should be a bytevector");\
		(const char*)(o+sizeof(uintptr_t)); })

//! returns not 0 if argument is a cons (and maybe a list)
#define is_pair(x) ({ uintptr_t s = (uintptr_t)(x);\
		is_reference(s) ?\
			reftype(s) == 1\
		: 0; })

//! converts ol small number into C signed integer
#define ol2small(x) ({ uintptr_t m = (uintptr_t)(x);\
		assert (is_enum(m) && "argument should be a small number");\
		int v = m >> 8;\
		(m & 0x80) ? -v : v;})

//! converts OL number into C signed integer
#define ol2int(x) ({ uintptr_t u = (uintptr_t)(x);\
		assert (is_number(u) && "argument should be a number");\
		is_enum(u) ? ol2small(u)\
			: (uintptr_t)ol2small(car(u)) | (uintptr_t)ol2small(cadr(u)) << ((sizeof (uintptr_t) * 8) - 8)/*FBITS*/;})

#define is_vector(x) ({ uintptr_t s = (uintptr_t)(x);\
		is_reference(s) ?\
			reftype(s) == 2 : 0; })


// internal structure that helps working with olvm
typedef struct ol_t
{
	struct olvm_t* vm;  // ol virtual machine instance
	size_t eval; // embed pinned 'eval' function id
} ol_t;

static
uintptr_t
OL_new(ol_t* embed, unsigned char* bootstrap)
{
	embed->vm = OLVM_new(bootstrap);
	OLVM_userdata(embed->vm, embed);

	uintptr_t r; // execution result
	char* args[] = { "--embed", "--non-interactive" }; // ol execution arguments
	r = OLVM_run(embed->vm, sizeof(args) / sizeof(*args), args);
	// well, we have our "smart" script prepared,
	//  now save both eval and env variables
	assert (is_enum(r));
	embed->eval = ol2small(r);

	return r;
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
static
uintptr_t OL_evalv(ol_t* embed, va_list* vp)
{
	va_list vl;

	int count = 0;
	va_copy(vl, *vp);
	while (va_arg(vl, void*) != 0)
		count++;
	uintptr_t* args = __builtin_alloca((count+1) * sizeof(uintptr_t)); // just one for sanity zero

	va_copy(vl, *vp);
	for (int i = 0; i < count; i++)
		args[i] = va_arg(vl, uintptr_t);

	// embed->eval is a (lambda (args) ...),
	// so we need to prepare a list of arguments
	uintptr_t userdata = 0x236; // #null
	{
		uintptr_t* fp = embed->vm->fp;
		for (int i = count-1; i >= 0; i--, fp += 3) {
			fp[0] = 0x30006; // TPAIR
			fp[1] = args[i];
			fp[2] = userdata;
			userdata = (uintptr_t) fp;
		}
		embed->vm->fp = fp;
	}

	return OLVM_evaluate(embed->vm,
		OLVM_deref(embed->vm, embed->eval),
		1, &userdata);
}

static
uintptr_t OL_eval(ol_t* embed, ...)
{
	va_list vl;
	va_start(vl, embed);
	return OL_evalv(embed, &vl);
}

static
void OL_delete(ol_t* embed)
{
	OLVM_delete(embed->vm);
}

// c++ interface:
#ifdef __cplusplus
class OL
{
private:
	ol_t ol;

public:
	OL(unsigned char* bootstrap) { OLVM_new(&ol, bootstrap); }
	virtual ~OL() { OL_delete(ol); }

	uintptr_t run(int argc, char** argv) {
		return OLVM_run(vm, argc, argv);
	}

	void* userdata(void* userdata)
	{
	return OLVM_userdata(vm, userdata);
	}
};

// ansi c interface:
#else
typedef struct ol_t OL;

#endif


//#pragma GCC diagnostic pop

// -----------------------------------------------------------------------------
// some magic functions to be able to process olvm data
// please, do not change

// function naming legend:
// "make_" prefix used for functions that do not allocate any memory
// "new_" prefix do the allocation (in olvm heap, sure).

// same as lisp (ref object n), (car object) and (cdr object) functions
#define ref(ob, n) (uintptr_t)(((uintptr_t*)(ob))[n]) // +1 is for header skipping
#define car(ob) ref(ob, 1)
#define cdr(ob) ref(ob, 2)

// caar, cadr, cdar, cddr - you can remove this
#define caar(o) car(car(o))
#define cadr(o) car(cdr(o))
#define cdar(o) cdr(car(o))
#define cddr(o) cdr(cdr(o))

// commonly used olvm constants, value type
#define INULL  (uintptr_t)((2 << 8) | (13 << 2) | 2) // #null
#define IFALSE (uintptr_t)((0 << 8) | (13 << 2) | 2) // #false
#define ITRUE  (uintptr_t)((1 << 8) | (13 << 2) | 2) // #true

// --------------------------------------------

/* \brief create olvm integer from C integer
 *
 * Creates olvm integer from C integer, value type
 * This function does not allocate any memory
 *
 * \param val C integer value
 */
#define make_enum(val) (uintptr_t) ({\
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
static uintptr_t new_buffer(ol_t* ol, int type, char* data, int size)
{
	int words = (size + sizeof(uintptr_t) - 1) / sizeof(uintptr_t);
	int pads = (words * sizeof(uintptr_t) - size);

	uintptr_t* p = (uintptr_t*)OLVM_allocate(ol->vm, words);

	++words; // include header size
	*p = (2 | ((uintptr_t) (words) << 16) | ((type) << 2) | ((pads) << 8) | (1 << 11));
	char* ptr = (char*)&p[1];
	while (size--) // can be changed to memcpy(ptr, data, size)
		*ptr++ = *data++;

	return (uintptr_t)p;
}

/* \brief allocate new olvm string
 *
 * Allocates new ANSI/UTF-8 string in olvm heap, invokes GC
 * if required
 *
 * \param ol embed ol instance
 * \param data zero-terminating source string
 */
static uintptr_t new_string(ol_t* ol, char* data)
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
static uintptr_t new_bytevector(ol_t* ol, char* data, int size)
{
	return new_buffer(ol, 19, data, size);
}

uintptr_t d2ol(struct olvm_t* ol, double v); // extern, olvm.c
static uintptr_t new_rational(ol_t* ol, double data)
{
	return d2ol(ol->vm, data);
}

static uintptr_t new_vptr(ol_t* ol, void* ptr)
{
	return new_buffer(ol, 49, (char*)&ptr, sizeof(ptr));
}

//-- end of header
#ifdef __cplusplus
    }
#endif
#endif//__OL_H__
