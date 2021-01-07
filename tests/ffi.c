#include <stdio.h>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif


#ifdef __unix__
#	define PUBLIC __attribute__ ((__visibility__("default"))) __attribute__((used))
#endif

#ifdef _WIN32
#	define PUBLIC __declspec(dllexport)
#endif

#ifdef __APPLE__
#	define PUBLIC __attribute__ ((__visibility__("default"))) __attribute__((used))
#endif

#if 1
#	define LOG(...)
#else
#	include <stdio.h>
#	include <inttypes.h>
#	define LOG printf
#endif

#define DONE(f)  LOG("<=" f "\n", r); return (r);

#include <stdint.h>

// new pack of ffi test functions
// c: char
// C: signed char
// s: short
// S: signed short
// i: int
// I: signed int
// l: long
// L: signed long
// q: long long (quadword)
// Q: signed long long (quadword)
// f: float
// d: double

// simple type->type mirroring functions
#define v2v(index, p, type) PUBLIC type index##2##index(type x) { type y = x; printf(" [" p " => " p "] ", x, y); fflush(stdout); return y; }

v2v(c, "%u", unsigned char)
v2v(C, "%d", signed char)
v2v(s, "%u", unsigned short)
v2v(S, "%d", signed short)
v2v(i, "%u", unsigned int)
v2v(I, "%d", signed int)
v2v(l, "%lu", unsigned long)
v2v(L, "%ld", signed long)
#ifndef __ARM_EABI__
v2v(q, "%llu", unsigned long long)
v2v(Q, "%lld", signed long long)
#endif
v2v(f, "%f", float)
v2v(d, "%f", double)

#ifdef __ARM_EABI__
static inline unsigned long ABS(signed long long x)
{
	signed long y = (signed long) x;
	return y < 0 ? -y : y;
}

signed long long Q2Q(signed long long x) {
	signed long long y = x;

	char text[32];
	text[31] = 0;
	char*p = &text[30];
	char sign = x < 0;
	if (x < 0) x = -x;

	do {
		*p-- = (x % 10) + '0'; x /= 10;
	}
	while (x > 0);

	if (sign)
		*p = '-';
	else
		p++;

	printf(" [%s =>", p);

	x = y;

	p = &text[30];
	sign = x < 0;
	if (x < 0) x = -x;

	do {
		*p-- = (x % 10) + '0'; x /= 10;
	}
	while (x > 0);

	if (sign)
		*p = '-';
	else
		p++;

	printf(" %s] ", p);

	fflush(stdout);
	return y;
}

unsigned long long q2q(unsigned long long x) {
	unsigned long long y = x;

	char text[32];
	text[31] = 0;
	char*p = &text[30];

	do {
		*p-- = (x % 10) + '0'; x /= 10;
	}
	while (x > 0);
	p++;

	printf(" [%s =>", p);

	x = y;
	p = &text[30];

	do {
		*p-- = (x % 10) + '0'; x /= 10;
	}
	while (x > 0);
	p++;

	printf(" %s] ", p);

	fflush(stdout);
	return y;
}

#endif//__ARM_EABI__

#define vvvvvvvvvvvvvvvv2v(index, P, type) PUBLIC type \
index##index##index##index##index##index##index##index##index##index##index##index##index##index##index##index##2##index \
		(type a, type b, type c, type d, type e, type f, type g, type h, type i, type j, type k, type l, type m, type n, type o, type p) \
{ \
	type y = a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p; \
	printf(" [" P ", " P ", " P ", " P \
	       ", " P ", " P ", " P ", " P \
	       ", " P ", " P ", " P ", " P \
	       ", " P ", " P ", " P ", " P " => " P "] ", \
	       a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, y); \
	fflush(stdout); \
	return y; }

vvvvvvvvvvvvvvvv2v(c, "%u", unsigned char)
vvvvvvvvvvvvvvvv2v(s, "%u", unsigned short)
vvvvvvvvvvvvvvvv2v(i, "%u", unsigned int)
vvvvvvvvvvvvvvvv2v(l, "%lu", unsigned long)
vvvvvvvvvvvvvvvv2v(q, "%llu", unsigned long long)

vvvvvvvvvvvvvvvv2v(C, "%d", signed char)
vvvvvvvvvvvvvvvv2v(S, "%d", signed short)
vvvvvvvvvvvvvvvv2v(I, "%d", signed int)
vvvvvvvvvvvvvvvv2v(L, "%ld", signed long)
vvvvvvvvvvvvvvvv2v(Q, "%lld", signed long long)

vvvvvvvvvvvvvvvv2v(f, "%f", float)
vvvvvvvvvvvvvvvv2v(d, "%f", double)


PUBLIC
double cCsSiIlLqQfd2d(unsigned char c, signed char C,
                      unsigned short s, signed short S,
				      unsigned int i, signed int I,
				      unsigned long l, signed long L,
				      unsigned long long q, signed long long Q,
				      float f, double d)
{
	double y = c+C+s+S+i+I+l+L+q+Q+f+d;
	printf(" [%u, %d, %u, %d, %u, %d, %lu, %ld, %llu, %lld, %f, %f => %f] ",
			   c,  C,  s,  S,  i,  I,  l,   L,   q,    Q,    f,  d,    y);
	fflush(stdout);
	return y;
}


// new pack of referencing ffi test functions
// simple (fft* type)->type mirroring functions
#define r2v(index, type) PUBLIC type r##index##2##index(type* x) { return index##2##index(*x); }

r2v(c, unsigned char)
r2v(C, signed char)
r2v(s, unsigned short)
r2v(S, signed short)
r2v(i, unsigned int)
r2v(I, signed int)
r2v(l, unsigned long)
r2v(L, signed long)
r2v(q, unsigned long long)
r2v(Q, signed long long)
r2v(f, float)
r2v(d, double)

// simple (fft* type)->type mirroring functions
#define p2v(index, p, type) PUBLIC type rp##index##2##index(type* x) { printf(" [" p " => ", *x); *x = *x - 1; printf(p "] ", *x); fflush(stdout); return *x; }

p2v(c, "%u", unsigned char)
p2v(C, "%d", signed char)
p2v(s, "%u", unsigned short)
p2v(S, "%d", signed short)
p2v(i, "%u", unsigned int)
p2v(I, "%d", signed int)
p2v(l, "%lu", unsigned long)
p2v(L, "%ld", signed long)
#ifndef __ARM_EABI__
p2v(q, "%llu", unsigned long long)
p2v(Q, "%lld", signed long long)
#endif
p2v(f, "%f", float)
p2v(d, "%f", double)


#ifdef SELFTEST
int main() {
	printf("i2i: %d\n", i2i(1));
	printf("I2I: %d\n", I2I(1));
	printf("s2s: %d\n", s2s(1));
	printf("S2S: %d\n", S2S(1));
	printf("l2l: %d\n", l2l(1));
	printf("L2L: %d\n", L2L(1));
	printf("q2q: %lld\n", q2q(1));
	printf("Q2Q: %lld\n", Q2Q(1));
	printf("f2f: %f\n", f2f(1.1));
	printf("d2d: %f\n", d2d(1.1));

	return 0;
}
#endif


PUBLIC
int i_i(int a)
{
	LOG("a=%d\n", a);

	return a;
}

PUBLIC
float f_f(float a)
{
	LOG("a=%f\n", a);

	return a;
}

PUBLIC
double d_d(double a)
{
	LOG("a=%f\n", a);

	return a;
}





PUBLIC
float fiiii(float f, int a, int b, int c, int d)
{
	LOG("f=%f\n", f);
	LOG("a=%d\n", a);
	LOG("b=%d\n", b);
	LOG("c=%d\n", c);
	LOG("d=%d\n", d);

	return (f + a+b+c+d);
}

PUBLIC
float iffiiiifi(int i, float f, float g, int j, int k, int l, int m, float h, int n)
{
	LOG("i=%d\n", i);
	LOG("f=%f\n", f);
	LOG("g=%f\n", g);
	LOG("j=%d\n", j);
	LOG("k=%d\n", k);
	LOG("l=%d\n", l);
	LOG("m=%d\n", m);
	LOG("h=%f\n", h);
	LOG("n=%d\n", n);

	return (f+g+h + i+j+k+l+m+n);
}

PUBLIC
double ddddddddd(double d1, double d2, double d3, double d4, double d5, double d6, double d7, double d8, double d9)
{
	LOG("d1=%f\n", d1);
	LOG("d2=%f\n", d2);
	LOG("d3=%f\n", d3);
	LOG("d4=%f\n", d4);
	LOG("d5=%f\n", d5);
	LOG("d6=%f\n", d6);
	LOG("d7=%f\n", d7);
	LOG("d8=%f\n", d8);
	LOG("d9=%f\n", d9);
	return (d1+d2+d3+d4+d5+d6+d7+d8+d9);
}

PUBLIC
float iffiiiifiiffffff(int i, float f, float g, int j, int k, int l, int m, float h, int n, int o, float f1, float f2, float f3, float f4, float f5, float f6)
{
	LOG("i=%d\n", i);
	LOG("f=%f\n", f);
	LOG("g=%f\n", g);
	LOG("j=%d\n", j);
	LOG("k=%d\n", k);
	LOG("l=%d\n", l);
	LOG("m=%d\n", m);
	LOG("h=%f\n", h);
	LOG("n=%d\n", n);
	LOG("o=%d\n", o);
	LOG("%f, %f, %f, %f, %f, %f\n", f1, f2, f3, f4, f5, f6);

	return (f+g+h + i+j+k+l+m+n);
}

PUBLIC
float fii(float f, int a, int b)
{
	LOG("f=%f\n", f);
	LOG("a=%d\n", a);
	LOG("b=%d\n", b);

	return (f + a+b);
}

PUBLIC
float fi(float f, int a)
{
	LOG("f=%f\n", f);
	LOG("a=%d\n", a);

	return (f + a);
}

PUBLIC
float ifiii(int a, float f, int b, int c, int d)
{
	LOG("a=%d\n", a);
	LOG("f=%f\n", f);
	LOG("b=%d\n", b);
	LOG("c=%d\n", c);
	LOG("d=%d\n", d);

	return (f + a+b+c+d);
}

PUBLIC
float iiiif(int a, int b, int c, int d, float f)
{
	LOG("a=%d\n", a);
	LOG("b=%d\n", b);
	LOG("c=%d\n", c);
	LOG("d=%d\n", d);
	LOG("f=%f\n", f);

	return (f + a+b+c+d);
}

PUBLIC
float fiiif(float f, int a, int b, int c, float g)
{
	LOG("f=%f\n", f);
	LOG("a=%d\n", a);
	LOG("b=%d\n", b);
	LOG("c=%d\n", c);
	LOG("g=%f\n", g);

	return (f+g + a+b+c);
}

PUBLIC
int test4(int a, int b, int c, int d)
{
	LOG("a=%d\n", a);
	LOG("b=%d\n", b);
	LOG("c=%d\n", c);
	LOG("d=%d\n", d);

	return (a+b+c+d);
}

PUBLIC
int test5(int a, int b, int c, int d, int e)
{
	LOG("a=%d\n", a);
	LOG("b=%d\n", b);
	LOG("c=%d\n", c);
	LOG("d=%d\n", d);
	LOG("e=%d\n", e);

	return (a+b+c+d+e);
}

PUBLIC
int test6(int a, int b, int c, int d, int e, int f)
{
	LOG("a=%d\n", a);
	LOG("b=%d\n", b);
	LOG("c=%d\n", c);
	LOG("d=%d\n", d);
	LOG("e=%d\n", e);
	LOG("f=%d\n", f);

	return (a+b+c+d+e+f);
}


PUBLIC
unsigned short fft_16u(unsigned short i)
{
	LOG("%u\n", i);
	return i;
}

PUBLIC
int fft_32i(int i)
{
	LOG("%d\n", i);
	return i;
}

PUBLIC
unsigned int fft_32u(unsigned int i)
{
	LOG("%u\n", i);
	return i;
}

typedef signed sint64 __attribute__ ((mode (DI))); // signed 64-bit
typedef unsigned uint64 __attribute__ ((mode (DI))); // unsigned 64-bit

PUBLIC
sint64 fft_64i(sint64 i)
{
	LOG("%"PRId64"\n", i);
	return i;
}

PUBLIC
uint64 fft_64u(uint64 i)
{
	LOG("%"PRIu64"\n", i);
	return i;
}

// fft-int32& test
PUBLIC
int fft_16pointers(int length, short* shorts)
{
	if (length < 6)
		return 1;
	LOG("0=%d\n", shorts[0]);
	LOG("1=%d\n", shorts[1]);
	LOG("2=%d\n", shorts[2]);
	LOG("3=%d\n", shorts[3]);
	LOG("4=%d\n", shorts[4]);
	LOG("5=%d\n", shorts[5]);


	shorts[0] = 0;
	shorts[1] = 1;
	shorts[2] = INT16_MAX;
	shorts[3] = UINT16_MAX;
	shorts[4] = -1;
	shorts[5] = -INT16_MAX;

	return -1;
}

PUBLIC
int fft_16pointersu(int length, unsigned short* shorts)
{
	if (length < 6)
		return 1;
	LOG("0=%u\n", shorts[0]);
	LOG("1=%u\n", shorts[1]);
	LOG("2=%u\n", shorts[2]);
	LOG("3=%u\n", shorts[3]);
	LOG("4=%u\n", shorts[4]);
	LOG("5=%u\n", shorts[5]);


	shorts[0] = 0;
	shorts[1] = 1;
	shorts[2] = INT16_MAX;
	shorts[3] = UINT16_MAX;
	shorts[4] = -1;
	shorts[5] = -INT16_MAX;

	return -1;
}

PUBLIC
int fft_32pointers(int length, int* ints)
{
	if (length < 6)
		return 1;

	LOG("0=%d\n", ints[0]);
	LOG("1=%d\n", ints[1]);
	LOG("2=%d\n", ints[2]);
	LOG("3=%d\n", ints[3]);
	LOG("4=%d\n", ints[4]);
	LOG("5=%d\n", ints[5]);

	ints[0] = 0;
	ints[1] = 1;
	ints[2] = INT32_MAX;
	ints[3] = UINT32_MAX;
	ints[4] = -1;
	ints[5] = -INT32_MAX;

	return -1;
}

PUBLIC
int fft_32pointersu(int length, unsigned int* ints)
{
	if (length < 6)
		return 1;

	LOG("0=%u\n", ints[0]);
	LOG("1=%u\n", ints[1]);
	LOG("2=%u\n", ints[2]);
	LOG("3=%u\n", ints[3]);
	LOG("4=%u\n", ints[4]);
	LOG("5=%u\n", ints[5]);

	ints[0] = 0;
	ints[1] = 1;
	ints[2] = INT32_MAX;
	ints[3] = UINT32_MAX;
	ints[4] = -1;
	ints[5] = -INT32_MAX;

	return -1;
}

PUBLIC
int fft_64pointers(int length, long long* longs)
{
   if (length < 6)
      return 1;
   longs[0] = 0;
   longs[1] = 1;
   longs[2] = INT64_MAX;
   longs[3] = UINT64_MAX;
   longs[4] = -1;
   longs[5] = -INT64_MAX;

   return -1;
}

// callbacks
PUBLIC
void callback_call_i(int (*callback) (int))
{
	callback(0);
	callback(1);
	callback(-1);
	callback(999999);
	callback(-999999);
}

PUBLIC
void callback_call_ii(int (*callback) (int, int))
{
	callback(0, 0);
	callback(1, -1);
	callback(-1, 1);
	callback(999999, -999999);
	callback(-999999, 999999);
}

PUBLIC
void callback_call_iii(int (*callback) (int, int, int))
{
	callback(0, 0, 0);
	callback(1, -1, 2);
	callback(-1, 1, -2);
	callback(999999, -999999, 7777777);
	callback(-999999, 999999, -7777777);
}

PUBLIC
void callback_call_iiii(int (*callback) (int, int, int, int))
{
	callback(0, 0, 0, 0);
	callback(1, -1, 2, -2);
	callback(-1, 1, -2, 2);
	callback(999999, -999999, 7777777, -7777777);
	callback(-999999, 999999, -7777777, 7777777);
}

PUBLIC
void callback_call_iiiii(int (*callback) (int, int, int, int, int))
{
	callback(0, 0, 0, 0, 0);
	callback(1, -1, 2, -2, 3);
	callback(-1, 1, -2, 2, -3);
	callback(999999, -999999, 7777777, -7777777, 11111111);
	callback(-999999, 999999, -7777777, 7777777, -11111111);
}

PUBLIC
void callback_call_iiiiii(int (*callback) (int, int, int, int, int, int))
{
	callback(0, 0, 0, 0, 0, 0);
	callback(1, -1, 2, -2, 3, -3);
	callback(-1, 1, -2, 2, -3, 3);
	callback(999999, -999999, 7777777, -7777777, 11111111, -11111111);
	callback(-999999, 999999, -7777777, 7777777, -11111111, 11111111);
}

PUBLIC
void callback_call_iiiiiii(int (*callback) (int, int, int, int, int, int, int))
{
	callback(0, 0, 0, 0, 0, 0, 0);
	callback(1, -1, 2, -2, 3, -3, 4);
	callback(-1, 1, -2, 2, -3, 3, -4);
	callback(999999, -999999, 7777777, -7777777, 11111111, -11111111, 444444444);
	callback(-999999, 999999, -7777777, 7777777, -11111111, 11111111, -444444444);
}

PUBLIC
void callback_call_iiiiiiii(int (*callback) (int, int, int, int, int, int, int, int))
{
	callback(0, 0, 0, 0, 0, 0, 0, 0);
	callback(1, -1, 2, -2, 3, -3, 4, -4);
	callback(-1, 1, -2, 2, -3, 3, -4, 4);
	callback(999999, -999999, 7777777, -7777777, 11111111, -11111111, 444444444, -444444444);
	callback(-999999, 999999, -7777777, 7777777, -11111111, 11111111, -444444444, 444444444);
}

// wchar
#ifdef _WIN32
	typedef WCHAR widechar;
#else
	#include <wchar.h>
	typedef wchar_t widechar;
#endif
#include <string.h>
#include <stdlib.h>

// -----------------------------------------------------------------------------------
// This is part of libutf-8 with minimal changes, Copyright (c) 1999 G. Adam Stanislav
// I need a thirdparty check for my code, so will use this proofed in time c portion
#define	INVALID	0x80000000

#define	get(c)	c = *strptr++; \
	if (chars) (*chars)++; \
	if ((c) == 0) return (unsigned int)EOF

unsigned int sgetu8(unsigned char *strptr, int *chars) {
	unsigned int c;
	int i, iterations;
	unsigned char ch;

	if (chars) *chars = 0;

	if (strptr == NULL)
		return (unsigned int)EOF;

	get(c);

	if ((c & 0xFE) == 0xFC) {
		c &= 0x01;
		iterations = 5;
	}
	else if ((c & 0xFC) == 0xF8) {
		c &= 0x03;
		iterations = 4;
	}
	else if ((c & 0xF8) == 0xF0) {
		c &= 0x07;
		iterations = 3;
	}
	else if ((c & 0xF0) == 0xE0) {
		c &= 0x0F;
		iterations = 2;
	}
	else if ((c & 0xE0) == 0xC0) {
		c &= 0x1F;
		iterations = 1;
	}
	else if ((c & 0x80) == 0x80)
		return INVALID;
	else return c;

	for (i = 0; i < iterations; i++) {
		get(ch);
		if ((ch & 0xC0) != 0x80)
			return INVALID;
		c <<= 6;
		c |= ch & 0x3F;
	}

	return c;
}

#define	bits(c)	(0x80 | ((c) & 0x3F))
#define	put(c)	*strptr++ = (c);
#define	putbits(c)	put(bits(c))
#define	finish()	*strptr = '\0'

char * sputu8(unsigned int c, char *strptr) {
	if (strptr != NULL) {
		if (c < 0x80) {
			put(c);
			finish();
		}
		else if (c < 0x800) {
			put(0xC0 | (c >>  6));
			putbits(c);
			finish();
		}
		else if (c < 0x10000) {
			put(0xE0 | (c >> 12));
			putbits(c >>  6);
			putbits(c);
			finish();
		}
		else if (c < 0x200000) {
			put(0xF0 | (c >> 18));
			putbits(c >> 12);
			putbits(c >>  6);
			putbits(c);
			finish();
		}
		else if (c < 0x400000) {
			put(0xF8 | (c >> 24));
			putbits(c >> 18);
			putbits(c >> 12);
			putbits(c >>  6);
			putbits(c);
			finish();
		}
		else if (c < 0x80000000) {
			put(0xFC | (c >> 30));
			putbits(c >> 24);
			putbits(c >> 18);
			putbits(c >> 12);
			putbits(c >>  6);
			putbits(c);
			finish();
		}
		else {	/* Not a valid Unicode "character" */
			finish();
		}
	}

	return strptr;
}
// end of part of libutf-8
// -----------------------

PUBLIC
char* reverse_string(char* str)
{
	// strdup+reverse
	int len = strlen(str);
	int* u32 = (int*)malloc((len+1)* sizeof(int));
	
	int rlen = 0;
	while (*str) {
		int x = 0;
		u32[rlen++] = sgetu8(str, &x);
		str += x;
	}

	char* out = (char*)malloc((len+1)* sizeof(char));
	char* tmp = out;
	for (int i = 0; i < rlen; i++)
		tmp = sputu8(u32[rlen - i - 1], tmp);

	out[len] = 0;

	free(u32);
	// memory leak - this is by design, do not fix
	return out;
}

PUBLIC
widechar* reverse_string_wide(widechar* str)
{
	// wcsdup+reverse
	int len = wcslen(str);
	widechar* out = (widechar*)malloc((len+1) * sizeof(widechar));
	for (int i = 0; i < len; i++)
		out[i] = str[len - i - 1];
	out[len] = 0;

	// wprintf output very dependent on host configuration, so can break the tests
	// uncomment only for some visual testing
	// wprintf(L"[%ls]", str);
	// fflush(stdout);

	// memory leak - this is by design, do not fix
	return out;
}
