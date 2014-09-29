#pragma once
#ifndef __OLVM_H__0F78631C_47C6_11E4_BBBE_64241D5D46B0__
#define	__OLVM_H__0F78631C_47C6_11E4_BBBE_64241D5D46B0__

// тут игра слов OL <> 0L
//	нулевой порог вхождения
//	сокращение от OwlLisp
//	а еще в html - тег нумерованного СПИСКА (еще одна отсылка к lisp)

#ifdef __cplusplus
#	define EXTERN extern "C"

	class OL;
#else
#	define EXTERN

	struct OL;
#endif


EXTERN struct
OL* vm_start(unsigned char* language); // после того, как машина закончит работу, можно просто сделать free()
EXTERN int vm_alive(struct OL* vm); // (возможно не нужна) проверяет, что vm еще работает

EXTERN int vm_puts(struct OL* vm, char *message, int n);
EXTERN int vm_stop(struct OL* vm);
EXTERN int vm_gets(struct OL* vm, char *message, int n);
EXTERN int vm_feof(struct OL* vm);  // все ли забрали из входящего буфера

#ifdef __cplusplus
class OL
{
private:
	struct OL* vm;
public:
	OLvm(unsigned char* language) { vm = vm_start(language); }
	int stop() { vm_stop(vm); }
	bool alive() { return vm_alive(vm); }

	int puts(char *message, int n) { vm_puts(vm, message, n);
	int gets(char *message, int n) { vm_gets(vm, message, n);

};
#else
	typedef struct OL OL;
#endif

#endif//__OLVM_H__0F78631C_47C6_11E4_BBBE_64241D5D46B0__
