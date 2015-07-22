#pragma once
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
struct OL;

// defaults. please don't change. use -DOPTIONSYMBOL commandline option instead
#ifndef HAS_SOCKETS
#define HAS_SOCKETS 1 // system sockets support
#endif

#ifndef HAS_DLOPEN
#define HAS_DLOPEN 1  // dlopen/dlsym support
#endif

#ifndef HAS_PINVOKE
#define HAS_PINVOKE 1 // pinvoke (for dlopen/dlsym) support
#endif

#ifndef EMBEDDED_VM   // use as embedded vm in project
#define EMBEDDED_VM 0
#endif

// internal option
#define NO_SECCOMP
//efine STANDALONE // самостоятельный бинарник без потоков

//-- end of options


int olvm(unsigned char* bootstrap, void (*release)(void*));

#if 0 //EMBEDDED_VM
//int vm_alive(struct OL* vm); // (возможно не нужна) проверяет, что vm еще работает

int vm_puts(struct OL* vm, char *message, int n);
int vm_gets(struct OL* vm, char *message, int n);
int vm_feof(struct OL* vm);  // все ли забрали из входящего буфера
#endif


#ifdef __cplusplus
/*class OL
{
private:
	OL* vm;
public:
	OLvm(unsigned char* language) { vm = vm_new(language); }
	virtual ~OLvm() { free(vm); }

	int stop() { puts(vm, ",quit", 5); }

	int puts(char *message, int n) { vm_puts(vm, message, n);
	int gets(char *message, int n) { vm_gets(vm, message, n);
};*/
#else
typedef struct OL OL;
#endif

//-- end of header
#ifdef __cplusplus
	}
#endif
#endif//__OLVM_H__0F78631C_47C6_11E4_BBBE_64241D5D46B0__
