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
word* sample_add(OL* ol, word* fa, word* fb) {
	word* fp; // memory pointer
	fp = ol->fp;

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
//			"(define THIS 44)"
			"(define RAWP 45)"
			"(define % (dlopen '() RTLD_LAZY))" // get own handle
			// 44 - memory top pointer (fp) //todo: change to OL*
			// 45 - direct mem pointer
			"(define sample_add (dlsym+ % (__cdecl RAWP) \"sample_add\"  RAWP RAWP))"
			"(print \"sample_add: \""
			"   (sample_add 1 2))"
			"(halt 1)", 0);
}

#endif
