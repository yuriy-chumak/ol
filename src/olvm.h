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

#ifndef __OLVM_H__0F78631C_47C6_11E4_BBBE_64241D5D46B0__
#define	__OLVM_H__0F78631C_47C6_11E4_BBBE_64241D5D46B0__
#ifdef __cplusplus
	extern "C" {
#endif

// тут игра слов OL:
//	сокращение от Owl-Lisp,
//	нулевой порог вхождения (0L - число 0) (Lisp - ОЧЕНЬ простой язык),
//	тег нумерованного списка в html - (еще одна отсылка к lisp - языку обработки списков),
//	ol' - сокращение от old (старый), отсылка к тому, что lisp - один из старейших языков.
struct OLvm;

// internal option
#define NO_SECCOMP
//efine STANDALONE // самостоятельный бинарник без потоков

// set for 1 to disable overflow check for binary images in release
#define NO_NAT_OVERFLOW_CHECK 0

struct OLvm*
OL_new(unsigned char* bootstrap, void (*release)(void*));
void OL_free(struct OLvm* ol);

void* OL_eval(struct OLvm* handle, int argc, char** argv);

//int olvm(unsigned char* bootstrap, void (*release)(void*));

#if 0 //EMBEDDED_VM
//int vm_alive(struct OL* vm); // (возможно не нужна) проверяет, что vm еще работает

int vm_puts(struct OL* vm, char *message, int n);
int vm_gets(struct OL* vm, char *message, int n);
int vm_feof(struct OL* vm);  // все ли забрали из входящего буфера
#endif


#ifdef __cplusplus
class OL
{
private:
	OLvm* vm;
public:
	OL(unsigned char* language) { vm = ol_new(language, 0); }
	virtual ~OLvm() { free(vm); }


	int eval() {
		return 0;
	}

//	int stop() { puts(vm, ",quit", 5); }

//	int puts(char *message, int n) { vm_puts(vm, message, n);
//	int gets(char *message, int n) { vm_gets(vm, message, n);
};
#else
#define OL struct OLvm
//typedef struct OL OL;
#endif

//-- end of header
#ifdef __cplusplus
	}
#endif
#endif//__OLVM_H__0F78631C_47C6_11E4_BBBE_64241D5D46B0__
