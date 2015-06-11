/*
 * sample-embed.c
 *
 *  Created on: Jun 10, 2015
 *      Author: uri
 */
#if EMBEDDED_VM

#include "olvm_xtra.h"

#include <stdio.h>

// embedded example
__attribute__((__visibility__("default")))
word* sample_add(OL* ol, word arguments) {
	word* fp; // memory pointer
	fp = ol->fp;

	word* fa = (word*)car(arguments); arguments = cdr(arguments);
	word* fb = (word*)car(arguments); arguments = cdr(arguments);

	// math
	int a = sftoi(fa);
	int b = sftoi(fb);
	int r = a + b;
	// result
	word* result = F(r);

	ol->fp = fp;//
	return result;
}


int main(int argc, char** argv)
{
	if (argc != 1) {
		printf("no options required\n");
	}

	return vm_new(
			"(import (owl pinvoke) (owl io))"
			"(define % (dlopen '() RTLD_LAZY))" // get own handle
			"(define sample_add (dlsym+ % \"sample_add\"))"
			"(print \"sample_add: \""
			"   (sample_add 1 2))"
			"(halt 1)", 0);
}

#endif
