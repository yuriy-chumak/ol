/*
 * embed/sample.c
 *
 *  Embed sample to the olvm
 *  Created on: Apr 21, 2018
 *      Author: uri
 */

#include "embed.h"

// olvm:
ol_t ol;

// Well, let's embed ol into our project and do some staff
int main(int argc, char** argv)
{
	intptr_t r; // result of ol functions, will use frequently

	// 1. create new olvm
	embed_new(&ol);

	// our embed extension can work in different manners:
	// 1. if eval got only one string parameter it returns the value of parameter
	//    this can be used to evaluate variables, constants
	//    or any expressions from the string (i.e. you can read whole lisp file
	//    info string and send it to execute by olvm)
	//
	// 2. if eval got one string parameter and more than 0 arguments it makes
	//    'apply' to them - (apply arg0 args)
	//    can be used as quick way to evaluate function by name with parameters
	//
	// 3. if eval got one numerical parameter and maybe more than 0 arguments
	//    this case means case 2, but with function referenced not by name but by
	//    'pin' id. So function must be pinned object.
	//
	// 4. and at last if eval received bytevector it tries to decode it as fasl
	//    object and evaluate like 2 or 3.

	// few words about 3 and 4 will be above with samples.

	// Note: if you want to call expression, but you have not arguments, just use
	//       parenthesis, like "(print)" instead of "print"


	// Well, let's try few simple tests:
	//   we will check results by assert. it's easy and demonstrates embed macro usages

	// * simple number evaluation (number avaluates to itself)
	r = embed_eval(&ol, new_string(&ol, "1234567"), 0);
	assert (r == make_integer(1234567));

	// * ok, how about functions? let's sum three numbers
	r = embed_eval(&ol, new_string(&ol, "+"), make_integer(1), make_integer(2), make_integer(103), 0);
	assert (r == make_integer(106));

	// * maybe custom function?
	r = embed_eval(&ol, new_string(&ol, "(define (f x) (if (eq? x 0) 1 (* x (f (- x 1)))))"), 0);
	assert (r != IFALSE);

	r = embed_eval(&ol, new_string(&ol, "f"), make_integer(7), 0);
	assert (r == make_integer(5040));

	
	// Huh. It's too much syntactic constructions, let's simplify, ok?
	// We will use macro trick that gives as ability to automatic cast
	// regular C variables to olvm instances.
	// Please, don't affraid

// C preprocessor trick, some kind of "map":
// https://github.com/swansontec/map-macro
///*
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
//*/  end of C preprocessor trick

// just code simplification (magic):
// we automatically will call new_string and make_integer depends on argument
// but in general case you should manually do this. or not.
#define _Q(x) \
	__builtin_choose_expr( __builtin_types_compatible_p (__typeof__(x), char[]),   new_string(&ol, (char*)(uintptr_t)x), \
	__builtin_choose_expr( __builtin_types_compatible_p (__typeof__(x), char*),    new_string(&ol, (char*)(uintptr_t)x), \
	__builtin_choose_expr( __builtin_types_compatible_p (__typeof__(x), uintptr_t),(uintptr_t)x, \
	__builtin_choose_expr( __builtin_types_compatible_p (__typeof__(x), int),      make_integer((int)(uintptr_t)x), \
	__builtin_choose_expr( __builtin_types_compatible_p (__typeof__(x), unsigned), make_integer((int)(uintptr_t)x), \
	__builtin_choose_expr( __builtin_types_compatible_p (__typeof__(x), long),     make_integer((long)(uintptr_t)x), \
	IFALSE))))))
#define eval(...) embed_eval(&ol, MAP_LIST(_Q, __VA_ARGS__), 0)

	// * ok, how about functions? let's sum three numbers
	r = eval("+", 1, 2, 103);
	assert (r == make_integer(106));

	// * maybe custom function?
	r = eval("(define (f x) (if (eq? x 0) 1 (* x (f (- x 1)))))");
	assert (r != IFALSE);

	r = eval("f", 7);
	assert (r == make_integer(5040));

	// Much better, huh?

	// And now we will try to compile some function and execute it without using string names!
	// first of all we need function compiler. let's compile declared above 'f'
	r = eval("(vm:pin f)");
	assert (is_value(r));

	// very simple, yes? Please, don't forget to delete this function when you will no need it enymore
	uintptr_t f = r;
	r = eval(f, 4);
	assert (r = make_integer(24));
	r = eval(f, 7);
	assert (r = make_integer(5040));

	// r = eval("vm:unpin", f); // free
	// assert (r == ITRUE);


	// Ok. What about precompiled core? For example ..... some naive cryptography?
	// assume, that I have a function, named caesar, that decripts some text.
	// and this is compiled function:
	char causar[] = {2, 16, 26, 11, 1, 0, 21, 1, 1, 3, 4, 1, 1, 2, 5, 5, 5, 8, 4, 5, 5, 3, 4, 8, 3, 2, 5, 3, 17, 2, 16,
	                 16, 11, 1, 0, 11, 1, 1, 3, 4, 1, 1, 2, 3, 2, 4, 1, 17, 2, 16, 35, 11, 1, 0, 30, 1, 1, 2, 4, 1, 1,
					 3, 5, 1, 1, 4, 6, 4, 4, 2, 2, 5, 6, 7, 9, 4, 5, 5, 3, 4, 7, 3, 2, 5, 3, 17, 2, 16, 30, 11, 2, 0,
					 10, 14, 19, 5, 19, 2, 5, 4, 6, 24, 6, 11, 3, 0, 11, 14, 19, 6, 19, 3, 6, 4, 5, 7, 24, 7, 17, 2, 16,
					 12, 11, 3, 0, 7, 40, 4, 5, 6, 7, 24, 6, 17, 1, 17, 2, 3, 4, 2, 16, 61, 11, 1, 0, 56, 1, 1, 2, 4, 1,
					 1, 3, 5, 10, 4, 5, 3, 6, 16, 5, 30, 0, 1, 2, 2, 7, 1, 1, 4, 8, 1, 1, 5, 9, 4, 4, 2, 3, 8, 9, 3, 14,
					 1, 11, 5, 5, 4, 11, 5, 2, 7, 3, 1, 1, 4, 7, 205, 3, 2, 7, 1, 17, 1, 17, 3, 1, 3, 7, 2, 16, 40, 11,
					 3, 0, 35, 1, 1, 2, 6, 47, 6, 4, 7, 1, 2, 2, 8, 1, 1, 4, 9, 3, 6, 2, 3, 9, 4, 3, 5, 3, 1, 1, 3, 5,
					 9, 7, 4, 2, 8, 3, 17, 1, 17, 3, 1, 5, 2, 2, 16, 46, 11, 1, 0, 41, 1, 1, 3, 4, 1, 1, 4, 5, 3, 5, 2,
					 4, 4, 5, 3, 6, 36, 4, 4, 1, 2, 2, 8, 1, 1, 2, 9, 3, 5, 2, 3, 6, 3, 9, 3, 14, 1, 5, 2, 8, 3, 17, 1,
					 17, 4, 1, 7, 6, 2, 2, 16, 25, 11, 3, 0, 20, 36, 4, 6, 1, 1, 2, 7, 6, 5, 3, 3, 4, 5, 3, 9, 6, 4, 2,
					 7, 2, 17, 1, 17, 3, 1, 10, 2, 0};
	// and we have a message
	char message[] = {68, 111, 108, 102, 104, 35, 100, 113, 103, 35, 69, 114, 101, 35, 122, 100, 113, 119, 35, 119, 114,
	                  35, 105, 111, 108, 115, 35, 100, 35, 102, 114, 108, 113, 35, 101, 124, 35, 119, 104, 111, 104, 115,
					  107, 114, 113, 104, 49};

	// let's execute one
	r = embed_eval(&ol, new_bytevector(&ol, causar, sizeof(causar)/sizeof(causar[0])),
						new_bytevector(&ol, message, sizeof(message)/sizeof(message[0])),
						make_integer(3), 0);
	assert (is_bytevector(r));
	assert (bytevector_length(r) == 47);
	assert (strncmp(bytevector_value(r), "Alice and Bob want to flip a coin by telephone.", 47) == 0);

	// wow, it works! But if we want frequently use causar function, let's decode, compile and 'pin' it into olvm memory
	r = eval("(define (compile bytecode) (vm:pin (fasl-decode (bytevector->list bytecode) #f)))");
	assert (r != IFALSE);
	r = eval("compile", new_bytevector(&ol, causar, sizeof(causar)/sizeof(causar[0])));
	assert (r != IFALSE);

	// now we have a pinned 'causar' function and can call it everywhere and everytime
	uintptr_t causar_pinned = r;

	r = eval(causar_pinned, new_bytevector(&ol, message, sizeof(message)/sizeof(message[0])), 3);
	assert (is_bytevector(r));
	assert (bytevector_length(r) == 47);
	assert (strncmp(bytevector_value(r), "Alice and Bob want to flip a coin by telephone.", 47) == 0);

	printf("ok.\n");
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


