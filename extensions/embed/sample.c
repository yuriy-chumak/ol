/*
 * embed/sample.c
 *
 *  Embed sample to the olvm
 *  Created on: Apr 21, 2018
 *      Author: uri
 */

#include "embed.h"

int main(int argc, char** argv)
{
	embed_t ol; // otus lisp instance
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
	r = embed_eval(&ol, "(define (plus a b) (+ a b))");

	// call the function with arguments
	r = embed_eval(&ol, "plus", 7, 11);
	assert (r == I(18)); // and check the result

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
