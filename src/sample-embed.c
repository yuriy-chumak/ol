/*
 * sample-embed.c
 *
 *  Created on: Jun 10, 2015
 *      Author: uri
 */
#if EMBEDDED_VM

#include "pinvoke.h"

#include <stdio.h>

// embedded example
__attribute__((__visibility__("default")))
word* sample_add(struct OL* ol, word arguments) {
	word* fp; // memory pointer
	fp = ol->heap.fp;

	word* fa = (word*)car(arguments); arguments = cdr(arguments);
	word* fb = (word*)car(arguments); arguments = cdr(arguments);

	// math
	int a = sftoi(fa); fprintf(stderr, "DEBUG: a = %d\n", a);
	int b = sftoi(fb); fprintf(stderr, "DEBUG: b = %d\n", b);
	int r = a + b;     fprintf(stderr, "DEBUG: r = %d\n", r);
	// result
	word* result = F(r);

	ol->heap.fp = fp;
	return result;
}


int main(int argc, char** argv)
{
	if (argc != 1) {
		printf("no options required\n");
	}

	struct OL* ol = OL_new(
			"(import (owl pinvoke) (owl io))"
			"(define % (dlopen '() RTLD_LAZY))" // get own handle
			"(define sample_add (dlsym+ % \"sample_add\"))"
			"(print \"sample_add: \""
			"   (sample_add 1 2))", 0);
	OL_eval(ol, 0, 0);
	OL_free(ol);
}

#endif
