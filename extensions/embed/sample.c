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

// just code simplification (magic):
// we automatically will call new_string and make_integer depends on argument
// but in general case you should manually do this. or not.
#define _Q(x) \
	__builtin_choose_expr( __builtin_types_compatible_p (typeof(x), char[]), \
		new_string(&ol, (char*)(uintptr_t)x), \
	__builtin_choose_expr( __builtin_types_compatible_p (typeof(x), char*), \
		new_string(&ol, (char*)(uintptr_t)x), \
	__builtin_choose_expr( __builtin_types_compatible_p (typeof(x), int), \
		make_integer((int)(uintptr_t)x), \
	__builtin_choose_expr( __builtin_types_compatible_p (typeof(x), unsigned), \
		make_integer((int)(uintptr_t)x), \
	/*else*/ \
		IFALSE))))

#define eval(...) embed_eval(&ol, MAP_LIST(_Q, __VA_ARGS__), 0)


int main(int argc, char** argv)
{
	word r; // result of ol functions

	// let's create new olvm
	embed_new(&ol);

	// our embed extension can work in two manner:
	// if eval got only one parameter it returns the value of parameter
	// in other case it makes apply the arguments to first parameter
	// Note: if you want to call expression, but you have not arguments, just use
	//       parenthesis, like "(print)" instead of "print"
	// let's demonstrate it:

	// new function declaration:
	r = eval("(define (plus a b) (+ a b))");

	// call the function with arguments
	r = eval("plus", 7, 11);

	assert (r == make_integer(18)); // and check the result

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
