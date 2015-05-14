#pragma once
#ifndef __OLVM_H__0F78631C_47C6_11E4_BBBE_64241D5D46B0__
#define	__OLVM_H__0F78631C_47C6_11E4_BBBE_64241D5D46B0__

// тут игра слов OL:
//	сокращение от Owl-Lisp
//	нулевой порог вхождения (0L - число 0) (Lisp - очень простой язык)
//	тег нумерованного списка в html - (еще одна отсылка к lisp - языку обработки списков)
//  ol' - сокращение от old (старый), отсылка к тому, что lisp - один из старейших языков
struct OL;

// defaults. please don't change. use -DOPTIONSYMBOL commandline option instead
#ifndef HAS_SOCKETS
#define HAS_SOCKETS 0 // system sockets support
#endif

#ifndef HAS_DLOPEN
#define HAS_DLOPEN 0  // dlopen/dlsym support
#endif

#ifndef HAS_PINVOKE
#define HAS_PINVOKE 0 // pinvoke (for dlopen/dlsym) support
#endif

#ifndef EMBEDDED_VM   // use as embedded vm in project
#define EMBEDDED_VM 0
#endif

// internal option
#define NO_SECCOMP
//efine STANDALONE // самостоятельный бинарник без потоков

//-- end of options

//-- common header
#ifdef __cplusplus
	extern "C" {
#endif

// todo: add vm_free or vm_delete or vm_destroy or something

#ifdef EMBEDDED_VM
struct OL*
vm_new(unsigned char* language, void (*release)(void*));

//int vm_alive(struct OL* vm); // (возможно не нужна) проверяет, что vm еще работает

int vm_puts(struct OL* vm, char *message, int n);
int vm_gets(struct OL* vm, char *message, int n);
int vm_feof(struct OL* vm);  // все ли забрали из входящего буфера

#ifdef __cplusplus
struct OL
{
private:
	OL* vm;
public:
	OLvm(unsigned char* language) { vm = vm_new(language); }
	virtual ~OLvm() { free(vm); }

	int stop() { puts(vm, ",quit", 5); }

	int puts(char *message, int n) { vm_puts(vm, message, n);
	int gets(char *message, int n) { vm_gets(vm, message, n);
};
#else
typedef struct OL OL;
#endif

#endif

//-- end of header
#ifdef __cplusplus
	}
#endif

#endif//__OLVM_H__0F78631C_47C6_11E4_BBBE_64241D5D46B0__
