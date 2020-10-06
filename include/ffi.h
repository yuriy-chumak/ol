/*
 * ffi.h
 *
 *  Created on: Sep 8, 2015
 *  Copyright (c) 2015-2020
 *      Author: uri
 */
#pragma once

#define USE_OLVM_DECLARATION
#include "olvm.c"

#if 0

// #include <stdint.h>
// #include <setjmp.h>

// память машины, управляемая сборщиком мусора
typedef struct heap_t
{
	//  begin <= genstart <= end
	word *begin;     // begin of heap memory block
	word *end;       // end of heap
	word *genstart;  // new generation begin pointer

	word *fp;        // allocation pointer

	jmp_buf fail;
} heap_t;

struct OL
{
	struct heap_t heap; // must be first member
	word max_heap_size; // max heap size in MiB

	int (*gc)(struct OL* ol, int ws);
};

// константы виртуальной машины и парочка макросов
#define IPOS                         8 // offset of immediate payload
#define SPOS                        16 // offset of size bits in header
#define TPOS                         2 // offset of type bits in header
#define RPOS                        11 // offset of RAW bit in header (IPOS+3)
#define PPOS                         8 // offset of padding bits in header

#define RAWBIT                      (1 << RPOS) // todo: rename to BSBIT (binstream bit)
#define BINARY                      (RAWBIT >> TPOS)

#define TPAIR                        1
#define TVECTOR                      2
#define TSTRING                      3

#define TPORT                       12
#define TCONST                      13

#define TINEXACT                    44

#define make_immediate(value, type) (((word)(value) << IPOS) | ((type) << TPOS) | 2)

#define header3(type, size, padding)(2 | ((word)(size) << SPOS) | ((type) << TPOS) | ((padding) << PPOS))
#define header2(type, size)         header3(type, size, 0)
#define HEADER_MACRO(_1, _2, _3, NAME, ...) NAME
#define make_header(...)            HEADER_MACRO(__VA_ARGS__, header3, header2, NOTHING, NOTHING)(__VA_ARGS__)

#define F(value)                    (((word)(value) << IPOS) | 2)

#define RAWBIT                      (1 << RPOS)
#define RAWH(t)                     (t | (RAWBIT >> TPOS))

#define FBITS                       ((__SIZEOF_LONG__ * 8) - 8) // bits in fixnum
#define HIGHBIT                     ((unsigned long)1 << FBITS) // high long bit set
#define FMAX                        (((long)1 << FBITS) - 1) // maximum fixnum (and most negative fixnum)
#define MAXOBJ                      0xFFFF // max words in vector including header

#define R(v) ({\
		word reference = (word)(v);\
		assert (!(reference & (W-1)) && "olvm references must be aligned to word boundary");\
		(word*) reference; })

#define ref(ob, n)                  ((word*)((R(ob))[n]))
// #define car(ob)                     ref(ob, 1)
// #define cdr(ob)                     ref(ob, 2)


#define IFALSE                      make_immediate(0, TCONST)
#define ITRUE                       make_immediate(1, TCONST)
#define INULL                       make_immediate(2, TCONST)
#define IEMPTY                      make_immediate(3, TCONST) /* empty ff */


#define NEW(_size) ({ \
	word* _addr = fp; \
	fp += (_size) + 1;\
	/*return*/ _addr; \
})

// аллоцировать новый объект (указанного типа)
#define NEW_OBJECT(_type, _size) ({\
word*p = NEW (_size);\
	*p = make_header(_type, _size+1, 0);\
	/*return*/ p;\
})

#define NEW_BLOB(_type, _size, _pads) ({\
word*p = NEW (_size);\
	*p = make_header(BINARY|_type, _size+1, _pads);\
	/*return*/ p;\
})

#define NEW_TYPED_PAIR(_type, _a1, _a2) ({\
	word _data1 = (word) _a1;\
	word _data2 = (word) _a2;\
	/* точка следования */ \
word*p = NEW_OBJECT (_type, 2);\
	p[1] = _data1;\
	p[2] = _data2;\
	/*return*/ p;\
})

#define NEW_PAIR(a1, a2) NEW_TYPED_PAIR(TPAIR, a1, a2)


#define uftoi(fix)  ({ ((word)fix >> IPOS); }) // todo: rename to uvtoi
#define sftoi(fix)  ({ ((word)fix & 0x80) ? -uftoi (fix) : uftoi (fix); })

#define car(ob)                     (((word*)(ob))[1])
#define cdr(ob)                     (((word*)(ob))[2])

#define hdrsize(x)  ((((word)x) >> SPOS) & MAXOBJ)
#define padsize(x)  ((((word)x) >> IPOS) & 7)
#define hdrtype(x)  ((((word)x) >> TPOS) & 0x3F) // 0xFF from (p) << 8) in make_raw_header

#define typeof(x) hdrtype(x)

// есть в оригинале
#define header_size(x)              (((word)(x)) >> SPOS) // header_t(x).size // todo: rename to object_size
#define object_size(x)              (((word)(x)) >> SPOS)
#define header_pads(x)              (unsigned char) ((((word)(x)) >> IPOS) & 7) // header_t(x).padding

#define value_type(x)               (unsigned char) ((((word)(x)) >> TPOS) & 0x3F)
#define reference_type(x)           (value_type (*R(x)))

#define reference_size(x)           ((header_size(*R(x)) - 1))
#define binstream_size(x)           ((header_size(*R(x)) - 1) * sizeof(word) - header_pads(*R(x)))

#endif