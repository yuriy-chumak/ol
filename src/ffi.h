#pragma once

/*
 * ffi.h
 *
 *  Created on: Sep 8, 2015
 *  Copyright (c) 2015-2017
 *      Author: uri
 */

#include <stdint.h>

// unsigned int that is capable of storing a pointer
// основной тип даных, зависит от разрядности машины
// based on C99 standard, <stdint.h>
typedef uintptr_t word;

// память машины, управляемая сборщиком мусора
typedef struct heap_t
{
	//  begin <= genstart <= end
	word *begin;     // begin of heap memory block
	word *end;       // end of heap

	word *genstart;  // new generation begin pointer

	word *fp;        // allocation pointer
} heap_t;

struct OL
{
	struct heap_t heap; // must be first member
	word max_heap_size; // max heap size in MB

	// вызвать GC если в памяти мало места в КБ
	// для безусловного вызова передать -1
	void (*gc)(int kb);
};

// парочка полезных макросов
#define IPOS                        8  // offset of immediate payload
#define SPOS                        16 // offset of size bits in header
#define TPOS                        2  // offset of type bits in header
#define RPOS                        11 // offset of RAW bit in header (IPOS+3)

#define TPAIR                       (1)
#define TTUPLE                      (2)
#define TSTRING                     (3)

#define TPORT                       (12)
#define TCONST                      (13)

#define make_immediate(value, type) ((((word)value) << IPOS) | ((type) << TPOS)                         | 2)
#define make_header(size, type)     (( (word)(size) << SPOS) | ((type) << TPOS)                         | 2)
#define F(val)                      (((word)(val) << IPOS) | 2)

#define RAWBIT                      ((1 << RPOS))
#define RAWH(t)                     (t | (RAWBIT >> TPOS))

#define FBITS                       ((__SIZEOF_LONG__ * 8) - 8) // bits in fixnum
#define HIGHBIT                     ((unsigned long)1 << FBITS) // high long bit set
#define FMAX                        (((long)1 << FBITS)-1) // maximum fixnum (and most negative fixnum)
#define MAXOBJ                      0xffff         // max words in tuple including header


#define IFALSE                      make_immediate(0, TCONST)
#define ITRUE                       make_immediate(1, TCONST)
#define INULL                       make_immediate(2, TCONST)
#define IEMPTY                      make_immediate(3, TCONST) /* empty ff */


#define NEW(size) ({\
	word* addr = self->heap.fp;\
	self->heap.fp += size;\
	/*return*/ addr;\
})

// аллоцировать новый объект (указанного типа)
#define NEW_OBJECT(size, type) ({\
word*p = NEW (size);\
	*p = make_header(size, type);\
	/*return*/ p;\
})

#define NEW_PAIR(a1, a2) ({\
	word data1 = (word) a1;\
	word data2 = (word) a2;\
	/* точка следования */ \
word*p = NEW_OBJECT (3, TPAIR);\
	p[1] = data1;\
	p[2] = data2;\
	/*return*/ p;\
})

// создать новый порт
#define new_port(a) ({\
word value = (word) a;\
	word* me = NEW_OBJECT (2, RAWH(TPORT));\
	me[1] = value;\
	/*return*/ me;\
})

#define uftoi(fix)  ({ ((word)fix >> IPOS); })
#define sftoi(fix)  ({ ((word)fix & 0x80) ? -uftoi (fix) : uftoi (fix); })

#define car(ob)                     (((word*)(ob))[1])
#define cdr(ob)                     (((word*)(ob))[2])

#define hdrsize(x)  ((((word)x) >> SPOS) & MAXOBJ)
#define padsize(x)  ((((word)x) >> IPOS) & 7)
#define hdrtype(x)  ((((word)x) >> TPOS) & 0x3F) // 0xFF from (p) << 8) in make_raw_header

#define typeof(x) hdrtype(x)
