#pragma once
#ifndef __OLVM_H__0F78631C_47C6_11E4_BBBE_64241D5D46B0__
#define	__OLVM_H__0F78631C_47C6_11E4_BBBE_64241D5D46B0__

// common options, can change
#define HAS_SOCKETS 1


// тут игра слов OL <> 0L
//	нулевой порог вхождения
//	сокращение от OwlLisp
//	а еще в html - тег нумерованного СПИСКА (еще одна отсылка к lisp)
struct OL;


#ifdef __cplusplus
	extern "C" {
#endif

// todo: change to vm_new and vm_free or vm_delete or vm_destroy or something

#ifndef STANDALONE
struct OL*
vm_new(unsigned char* language, void (*release)(void*));

//int vm_alive(struct OL* vm); // (возможно не нужна) проверяет, что vm еще работает

int vm_puts(struct OL* vm, char *message, int n);
int vm_gets(struct OL* vm, char *message, int n);
int vm_feof(struct OL* vm);  // все ли забрали из входящего буфера

#endif

#ifdef __cplusplus
struct OL
{
private:
	OL* vm;
public:
	OLvm(unsigned char* language) { vm = vm_start(language); }
	virtual ~OLvm() { free(vm); }

	int stop() { puts(vm, ",quit", 5); }

	int puts(char *message, int n) { vm_puts(vm, message, n);
	int gets(char *message, int n) { vm_gets(vm, message, n);
};
#else
typedef struct OL OL;
#endif

#ifdef __cplusplus
	}
#endif


// defaults. please don't change.
#ifndef HAS_SOCKETS
#define HAS_SOCKETS 0
#endif


#endif//__OLVM_H__0F78631C_47C6_11E4_BBBE_64241D5D46B0__
