/*
 * olvm_xtra.h
 *
 *  Created on: Jun 10, 2015
 *      Author: uri
 */

#ifndef SRC_OLVM_XTRA_H_
#define SRC_OLVM_XTRA_H_

#include "olvm.h"

#include <stdint.h>
// unsigned int that is capable of storing a pointer
// основной тип даных, зависит от разрядности машины
// based on C99 standard, <stdint.h>
typedef uintptr_t word;

struct OL
{
	word *fp; // allocation pointer (top of allocated heap)
};

// парочка полезных макросов
#define IPOS                        8  // offset of immediate payload
#define SPOS                        16 // offset of size bits in header
#define TPOS                        2  // offset of type bits in header
#define RPOS                        11 // offset of RAW bit in header (IPOS+3)

#define TPAIR                       (1)
#define TTUPLE                      (2)
#define TSTRING                     (3)

#define make_immediate(value, type) ((((word)value) << IPOS) | ((type) << TPOS)                         | 2)
#define make_header(size, type)     (( (word)(size) << SPOS) | ((type) << TPOS)                         | 2)
#define F(val)                      (((word)(val) << IPOS) | 2)


#define NEW(size) ({\
	word* addr = fp;\
	fp += size;\
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


#define uftoi(fix)  ({ ((word)fix >> IPOS); })
#define sftoi(fix)  ({ ((word)fix & 0x80) ? -uftoi (fix) : uftoi (fix); })



#endif /* SRC_OLVM_XTRA_H_ */
