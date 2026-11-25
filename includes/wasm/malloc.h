#pragma once
#include <stdint.h>
#include <stddef.h>

void *malloc(size_t n_);
void free(void *p);
void *realloc(void *p, size_t n);

// ----------------------------------------------------------------------------
#ifdef __OLVM_NAME__

#include <string.h>
#include <assert.h>

// Заголовок блока. Младший бит size = флаг занятости (1 = занят, 0 = свободен).
#define ALIGN8(x) (((x) + 7u) & ~7u)
#define PAGE_SIZE 65536

// logging
#define R_ "\033[31m"
#define G_ "\033[32m"
#define B_ "\033[34m"
#define Y_ "\033[33m"
#define _EL "\033[0m"

#define DR(format, ...) D(R_ format _EL, ##__VA_ARGS__)
#define DG(format, ...) D(G_ format _EL, ##__VA_ARGS__)
#define DB(format, ...) D(B_ format _EL, ##__VA_ARGS__)
#define DY(format, ...) D(Y_ format _EL, ##__VA_ARGS__)

// heap
__attribute__((__aligned__(8)))
typedef struct block {
    size_t size;   // размер данных, младший бит = занят
    struct block *next;
	
	__attribute__((__aligned__(8)))
	unsigned char data[];
} block_t;

struct {
	// char* bottom; // heap bottom
    char *begin; // floating begin (assume all previous blocks are used)
    char* end;
} heap = { 0 };

extern unsigned char __heap_base;
extern size_t __wasm_grow(size_t pages);

static_assert (sizeof(block_t) % 8 == 0, "block_t size must be aligned by 8");

#define HDR           ALIGN8(sizeof(block_t))

void *malloc(size_t n_) {
	DG("malloc(%d):", n_);
	if (n_ == 0) return NULL;

	bool USED(block_t *b) {
		return b->size & 1; }
	bool FREE(block_t *b) {
		return !USED(b); }
	
	size_t SIZE(block_t *b) {
		return b->size & ~(size_t)1;
	}

    if (heap.begin == 0) {
        heap.begin = ALIGN8((uintptr_t)&__heap_base);
		DY("  malloc setup on %p", heap.begin);
        heap.end = __wasm_grow(0);
		DY("           end is %p (%d bytes in heap)", heap.end, heap.end - heap.begin);

        block_t *b = (block_t *)heap.begin;
        b->size = heap.end - heap.begin - sizeof(*b);   // mark all heap as free
        b->next = 0;
    }

    size_t n = ALIGN8(n_);

    for (int attempt = 1; attempt <= 2; attempt++) {
		DY("  attempt %d", attempt);
        block_t *last = NULL;
        for (block_t *b = (block_t *)heap.begin; b; last = b, b = b->next) {
			DY("    %p(%d) is %s", b, SIZE(b), FREE(b) ? "free" : "used");
            // ленивое слияние свободных соседей
            while (FREE(b) && b->next && FREE(b->next)) {
				DY("    merging %p(%d) and %p(%d) into %p(%d)",
					b, SIZE(b),  b->next, SIZE(b->next),  b, SIZE(b) + HDR + SIZE(b->next));
                block_t *nx = b->next;
                b->size = SIZE(b) + HDR + SIZE(nx);
                b->next = nx->next;
            }
            if (USED(b)) {
				DY("      skip used block");
				continue;
			}
            if (SIZE(b) < n) {
				DY("      too small, skip");
				continue;
			}

			DY("      this block is ok");
            if (SIZE(b) >= n + HDR) {   /* делим */
                block_t *nx = (block_t *)((char*)&b->data + n);
				DY("      split %p(%d) to %p(%d) and %p(%d)",
					b, SIZE(b), b, n, nx, SIZE(b) - n - HDR);

                nx->size = SIZE(b) - n - HDR;
                nx->next = b->next;
                b->size = n | 1;
                b->next = nx;
            } else {
                b->size |= 1;
            }
			DG("malloc(%d) -> %p(%d)", n_, &b->data, SIZE(b));

			// if (heap->begin < b) ; TODO: ?
			// 	heap->begin = b;
            return &b->data;
        }

        /* места нет — растим кучу и добавляем свободный блок в конец */
		DY("    enlarge heap");
        size_t need = n + HDR;
        size_t pages = (need + PAGE_SIZE - 1) / PAGE_SIZE;
        size_t old_end = heap.end;
        size_t new_end = __wasm_grow(pages);
        if (new_end == 0 || new_end < old_end + need) {
			DR("malloc(%d) -> NULL", n_);
			return NULL;
		}
        heap.end = new_end;

        block_t *b = (block_t *)old_end;
        b->size = new_end - old_end - HDR;
        b->next = 0;
		DY("      allocated %p(%d)", b, SIZE(b));
        if (last) last->next = b;
    }
	DR("malloc(%d) -> NULL", n_);
    return NULL;
}

#define BLK(p)        ((block_t *)((uintptr_t)(p) - HDR))

void free(void *p) {
	DR("free(%p)", p);
	if (!p) return;

	bool USED(block_t *b) {
		return b->size & 1; }

	for (block_t *it = (block_t*)heap.begin; it; it = it->next) {
		if (&it->data == p) {
			assert(USED(it));
			it->size &= ~1;

			// if (heap.begin > it)
			// 	heap.begin = it;
			return;
		}
	}
}

void *realloc(void *p, size_t n) {
	DR("realloc(%p, %d)", p, n);
    if (!p) return malloc(n);
    if (n == 0) { free(p); return NULL; }

	// TODO: check if memory block is last.
    block_t *b = BLK(p);
    size_t old = b->size & ~(size_t)1;
    n = ALIGN8(n);
    if (old >= n) return p;

    void *q = malloc(n);
    if (!q) return NULL;
    memcpy(q, p, old);
    free(p);
    return q;
}
// --------------------------------------------------------------------------------------------------
#endif