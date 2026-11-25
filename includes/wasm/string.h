#pragma once

void* wasm_memmove(void *dest, void* src, size_t n)
{
	D("wasm_memmove(%p, %p, %d)", dest, src, n);
	unsigned char *d = (unsigned char *)dest;
	unsigned char *s = (unsigned char *)src;
	if (d < s)
		while (n--)
			*d++ = *s++;
	else {
		d += n;
		s += n;
		while (n--)
			*--d = *--s;
	}
	return dest;
}
#define memmove wasm_memmove

// __attribute__((optimize("no-tree-loop-distribute-patterns")))
void* wasm_memcpy(void *dest, const void *src, size_t n)
{
	unsigned char *d = (unsigned char *)dest;
	unsigned char *s = (unsigned char *)src;
	while (n--)
		*d++ = *s++;
	return dest;
}
#define memcpy wasm_memcpy


size_t strlen(char *str)
{
	unsigned int l = 0;
	while (*str++) l++;

	return l;
}

int strcmp(unsigned char *str1, unsigned char *str2)
{
	unsigned char a, b;
	do {
		a = *str1++, b = *str2++;
		if (a < b) return -1;
		if (a > b) return +1;
	}
	while (a && b);
	return 0;
}

int strncmp(unsigned char *str1, unsigned char *str2, size_t n)
{
	int i = 0;
	unsigned char a, b;
	do {
		a = *str1++, b = *str2++;
		if (a < b) return -1;
		if (a > b) return +1;
	}
	while (a && b && i++ < n);
	return 0;
}

void* memset (void *dest, unsigned char ch, size_t count)
{
	// D("memset(%p, %d, %d)", dest, ch, count);
	unsigned char *p = (unsigned char *)dest;
	while (count--)
		*p++ = ch;

	return dest;
}

/*void* memcpy(void *dest, void* src, size_t n)
{
	//D("memcpy(%p, %p, %d)", dest, src, n);
	unsigned char *d = (unsigned char *)dest;
	unsigned char *s = (unsigned char *)src;
	while (n--)
		*d++ = *s++;
	return dest;
}*/


// int strcmp(const char *str1, const char *str2);


	// #include <stddef.h>
	// #include <stdint.h>

	// extern unsigned char __heap_base[];

	// typedef struct Block {
	// 	size_t size;          /* размер полезной нагрузки (без заголовка) */
	// 	int free;             /* 1 — свободен, 0 — занят */
	// 	struct Block *next;   /* следующий блок в списке */
	// } Block;

	// #define ALIGN8(x) (((x) + 7u) & ~(size_t)7u)
	// #define HDR_SIZE  ALIGN8(sizeof(Block))

	// static Block *head = NULL;
	// static uintptr_t heap_ptr = 0;

	// static void *request_space(size_t size) {
	// 	if (heap_ptr == 0) {
	// 		heap_ptr = ALIGN8((uintptr_t)__heap_base);
	// 	}

	// 	size_t total = HDR_SIZE + size;
	// 	uintptr_t cur_end = (uintptr_t)__builtin_wasm_memory_size(0) * 65536u;

	// 	if (heap_ptr + total > cur_end) {
	// 		size_t need_bytes = (heap_ptr + total) - cur_end;
	// 		size_t need_pages = (need_bytes + 65535u) / 65536u;
	// 		if (__builtin_wasm_memory_grow(0, need_pages) == (size_t)-1) {
	// 			return NULL;
	// 		}
	// 	}

	// 	Block *blk = (Block *)heap_ptr;
	// 	heap_ptr += total;
	// 	blk->size = size;
	// 	blk->free = 0;
	// 	blk->next = NULL;

	// 	if (!head) {
	// 		head = blk;
	// 	} else {
	// 		Block *p = head;
	// 		while (p->next) p = p->next;
	// 		p->next = blk;
	// 	}

	// 	return (unsigned char *)blk + HDR_SIZE;
	// }

	// static void split_block(Block *blk, size_t size) {
	// 	/* Делим, только если хватит места под новый заголовок + минимум 8 байт данных */
	// 	if (blk->size >= size + HDR_SIZE + 8u) {
	// 		unsigned char *base = (unsigned char *)blk;
	// 		Block *rem = (Block *)(base + HDR_SIZE + size);
	// 		rem->size = blk->size - size - HDR_SIZE;
	// 		rem->free = 1;
	// 		rem->next = blk->next;

	// 		blk->size = size;
	// 		blk->next = rem;
	// 	}
	// }

	// static void coalesce(void) {
	// 	Block *p = head;
	// 	while (p && p->next) {
	// 		if (p->free && p->next->free) {
	// 			p->size += HDR_SIZE + p->next->size;
	// 			p->next = p->next->next;
	// 		} else {
	// 			p = p->next;
	// 		}
	// 	}
	// }

	// void *malloc(size_t size) {
	// 	if (size == 0) return NULL;
	// 	size = ALIGN8(size);

	// 	Block *p = head;
	// 	while (p) {
	// 		if (p->free && p->size >= size) {
	// 			p->free = 0;
	// 			split_block(p, size);
	// 			return (unsigned char *)p + HDR_SIZE;
	// 		}
	// 		p = p->next;
	// 	}

	// 	return request_space(size);
	// }

	// void free(void *ptr) {
	// 	if (!ptr) return;
	// 	Block *blk = (Block *)((unsigned char *)ptr - HDR_SIZE);
	// 	blk->free = 1;
	// 	coalesce();
	// }
