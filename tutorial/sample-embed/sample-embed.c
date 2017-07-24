/*
 * sample-embed.c
 *
 *  Created on: Jun 10, 2015
 *  Changed at: Apr 12, 2017
 *      Author: uri
 */

#include "olvm.h"

#if EMBEDDED_VM
#include <stdio.h>

// embedded example
#if _WIN32
__declspec(dllexport)
#else
__attribute__((__visibility__("default")))
#endif

int sample_add(int fa, int fb)
{
	// math
	int a = fa;    fprintf(stderr, "DEBUG: a = %d\n", a);
	int b = fb;    fprintf(stderr, "DEBUG: b = %d\n", b);
	int r = a + b; fprintf(stderr, "DEBUG: r = %d\n", r);

	// result
	return r;
}


int main(int argc, char** argv)
{
	if (argc != 1) {
		printf("no options required\n");
	}

	OL* ol = OL_new((unsigned char*)
			// preparation steps:
			"(import (otus pinvoke) (owl io))"
			"(define $ (dlopen))" // get own handle
			"(define sample_add (dlsym $ type-int+ \"sample_add\" type-int+ type-int+))"

			// main program body
			"(print \"sample_add: \""
			"   (sample_add 1 2))", 0);
	OL_run(ol, 0, 0);

	OL_free(ol);
}

#endif
