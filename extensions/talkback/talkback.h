/*
 * talkback.c
 *
 *  Talkback extension to the olvm
 *  Created on: Apr 13, 2017
 *      Author: Yuriy Chumak
 *
 * --------------------------------------------------------------
 * This program is free software;  you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * --------------------------------------------------------------
 */
#pragma once

#ifdef __cplusplus
extern "C" {
#endif

/*!
 * \brief OL_tb_start() -> olvm
 *
 * Attempts to start new instance of ol virtual machine
 *
 * \return olvm instance
 */
void*OL_tb_start();

/*!
 * \brief OL_tb_stop(olvm) -> none
 *
 * Attempts to stop existing instance of ol virtual machine
 *
 * \param olvm olvm instance
 */
void OL_tb_stop(void* olvm);

/*!
 * \brief OL_tb_send(olvm, program) -> none
 *
 * Forces olvm to execute provided text program in global context
 *
 * This function forces the olvm continue working and process the
 * provided program. All changes made by program will be fixed in
 * the current (global) context. So you can do "define" statement
 * and expect to be able use it in feature calls.
 *
 * Any result of provided program will be ignored.
 *
 * \param olvm olvm instance
 * \param format,... formatted program, terminated by '\0' string
 */
void OL_tb_send(void* olvm, char* format, ...);

/*!
 * \brief OL_tb_eval(olvm, program) -> result
 *
 * Forces olvm to execute provided text program in local context
 * and returns the value as result of execution.
 *
 * This function forces the olvm continue working and process the
 * provided program. All changes made by program will be lost!
 * after executing.
 *
 * Result of execution can be processed using provided in this
 * header macroses like 'is_value', 'ol2int', etc.
 *
 * \param olvm olvm instance
 * \param format,... formatted program, terminated by '\0' string
 *
 * \return pointer to result of provided program or 0 if error was detected
 */
void* OL_tb_eval(void* state, char* format, ...);

/*!
 * \brief OL_tb_get_error(olvm) -> error
 *
 * Returns the error of olvm execution, if any
 *
 * Returned error can be processed using provided in this header
 * macroses like 'is_value', 'to_number', etc.
 *
 * \return execution error or 0 if no error was detected
 */
void*OL_tb_get_error(void* olvm);

/*!
 * \brief OL_tb_set_import_hook(olvm, hook) -> none
 */
void OL_tb_set_import_hook(void* olvm, int (*hook)(const char* thename, char** output));


// -----------------------------------------------------------------------------
// helper functions to work with olvm values:

// internal ol memory object manipulations:
#define ref(ob, n) (void*)(((uintptr_t*)(ob))[n])
#define car(ob) ref(ob, 1)
#define cdr(ob) ref(ob, 2)

#define caar(o) car(car(o))
#define cadr(o) car(cdr(o))
#define cdar(o) cdr(car(o))
#define cddr(o) cdr(cdr(o))

//! size of biggest small number in bits
#define FBITS ((sizeof (uintptr_t) * 8) - 8)

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

//! converts OL number into C signed integer
#define ol2small(x) ({ uintptr_t m = (uintptr_t)(x);\
		assert (is_small(m) && "argument should be a small number");\
		int v = m >> 8;\
		(m & 0x80) ? -v : v;})

#define ol2int(x) ({ uintptr_t u = (uintptr_t)(x);\
		assert (is_number(u) && "argument should be a number");\
		is_small(u) ? ol2small(u)\
			: ol2small(car(u)) | ol2small(cadr(u)) << FBITS;})


#ifdef __cplusplus
}
#endif
