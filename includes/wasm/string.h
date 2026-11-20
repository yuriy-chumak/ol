#pragma once
// #include <stddef.h>
// #include <stdint.h>

#define PAGE_SIZE 65536u
#define ALIGN8(x) (((x) + 7u) & ~(uintptr_t)7u)

extern unsigned char __heap_base;
extern size_t __wasm_grow(size_t pages);

static struct {
	uintptr_t ptr;
	uintptr_t end;
} heap = { 0 };

void* malloc(size_t n) {
	// D("malloc(%d)", n);
	if (n == 0) return NULL;

	if (heap.ptr == 0) {
		heap.ptr = ALIGN8((uintptr_t)&__heap_base);
		heap.end = __wasm_grow(0);
	}	

	uintptr_t start = ALIGN8(heap.ptr);
	uintptr_t next = start + n;
	if (next < start) return NULL;

	if (next > heap.end) {
		size_t need_bytes = (size_t)(next - heap.end);
		size_t pages = (need_bytes + PAGE_SIZE - 1u) / PAGE_SIZE;
		size_t new_end = __wasm_grow(pages);
		if (new_end == 0 || new_end < next) {
			return NULL;
		}
		heap.end = new_end;
	}

	heap.ptr = next;
	return (void *)start;
}

void free(void *p) {
	// do nothing for now
	(void)p;
}

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
