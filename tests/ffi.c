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
#define v2v(index, p, type) PUBLIC \
type index##2##index(type x) \
{ \
	type y = x; \
	printf(" [" p " => " p "] ", x, y); fflush(stdout); \
	return y; \
}

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

PUBLIC
double cCfdsSfdiIfdlLfdqQfdfd2d(unsigned char c, signed char C,
                                float fc, double dc,
                                unsigned short s, signed short S,
                                float fs, double ds,
                                unsigned int i, signed int I,
                                float fi, double di,
                                unsigned long l, signed long L,
                                float fl, double dl,
                                unsigned long long q, signed long long Q,
                                float fq, double dq,
                                float f, double d)
{
	double y = c+C+fc+dc+
	           s+S+fs+ds+
	           i+I+fi+di+
	           l+L+fl+dl+
	           q+Q+fq+dq+
	           f+d;
	printf("%s", " [");
	printf("%u, %d, %f, %f, ", c, C, fc, dc);
	printf("%u, %d, %f, %f, ", s, S, fs, ds);
	printf("%u, %d, %f, %f, ", i, I, fi, di);
	printf("%lu, %ld, %f, %f, ", l, L, fl, dl);
	printf("%llu, %lld, %f, %f, ", q, Q, fq, dq);
	printf("%f, %f => %f] ", f, d, y);
	fflush(stdout);
	return y;
}


// new pack of referencing ffi test functions
// simple (fft* type)->type mirroring functions
#define pv2v(index, type) PUBLIC \
type p##index##2##index(type* x) \
{ \
	return index##2##index(*x); \
}

pv2v(c, unsigned char)
pv2v(C, signed char)
pv2v(s, unsigned short)
pv2v(S, signed short)
pv2v(i, unsigned int)
pv2v(I, signed int)
pv2v(l, unsigned long)
pv2v(L, signed long)
pv2v(q, unsigned long long)
pv2v(Q, signed long long)
pv2v(f, float)
pv2v(d, double)

// simple (fft* type)->type mirroring functions
#define rv2v(index, p, type) PUBLIC \
type r##index##2##index(type* x) \
{ \
	printf(" [" p " => ", *x); *x -= 1; \
	printf(p "] ", *x); fflush(stdout); \
	return *x + 2; \
}

rv2v(c, "%u", unsigned char)
rv2v(C, "%d", signed char)
rv2v(s, "%u", unsigned short)
rv2v(S, "%d", signed short)
rv2v(i, "%u", unsigned int)
rv2v(I, "%d", signed int)
rv2v(l, "%lu", unsigned long)
rv2v(L, "%ld", signed long)
#ifndef __ARM_EABI__
rv2v(q, "%llu", unsigned long long)
rv2v(Q, "%lld", signed long long)
#endif
rv2v(f, "%f", float)
rv2v(d, "%f", double)

// extended (fft* type)->type mirroring functions
#define p2v3(index, p, type) PUBLIC \
type rp##index##2##index##3(type* x) { \
	printf(" [" p ", " p ", " p " => ", x[0], x[1], x[2]); \
	x[2] = x[0] + x[1]; \
	printf(p "] ", x[2]); \
	fflush(stdout); \
	return x[2]; \
}

p2v3(c, "%u", unsigned char)
p2v3(C, "%d", signed char)
p2v3(s, "%u", unsigned short)
p2v3(S, "%d", signed short)
p2v3(i, "%u", unsigned int)
p2v3(I, "%d", signed int)
p2v3(l, "%lu", unsigned long)
p2v3(L, "%ld", signed long)
#ifndef __ARM_EABI__
p2v3(q, "%llu", unsigned long long)
p2v3(Q, "%lld", signed long long)
#endif
p2v3(f, "%f", float)
p2v3(d, "%f", double)


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



// callbacks
PUBLIC
void callback_call_i(int (*callback) (int))
{
	printf(" = %d\n", callback(0)); fflush(stdout);
	printf(" = %d\n", callback(1)); fflush(stdout);
	printf(" = %d\n", callback(-1)); fflush(stdout);
	printf(" = %d\n", callback(2147483647)); fflush(stdout);
	printf(" = %d\n", callback(-2147483648)); fflush(stdout);
}

PUBLIC
void callback_call_iiiiiiiiii(int (*callback) (int, int, int, int, int, int, int, int, int, int))
{
	printf(" = %d\n", callback(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)); fflush(stdout);
	printf(" = %d\n", callback(1, -1, 2, -2, 3, -3, 4, -4, 5, -6)); fflush(stdout);
	printf(" = %d\n", callback(-1, 1, -2, 2, -3, 3, -4, 4, -5, 6)); fflush(stdout);
	printf(" = %d\n", callback(999999, -999999, 7777777, -7777777, 11111111, -11111111, 444444444, -444444444, 33333, 22)); fflush(stdout);
	printf(" = %d\n", callback(-999999, 999999, -7777777, 7777777, -11111111, 11111111, -444444444, 444444444, -33333, -22)); fflush(stdout);
}

PUBLIC
void callback_call_f(float (*callback) (float))
{
	printf(" = %f\n", callback(0.0f)); fflush(stdout);
	printf(" = %f\n", callback(1.1f)); fflush(stdout);
	printf(" = %f\n", callback(-1.1f)); fflush(stdout);
	printf(" = %f\n", callback(129.984375f)); fflush(stdout);
	printf(" = %f\n", callback(-129.984375f)); fflush(stdout);
}

PUBLIC
void callback_call_d(double (*callback) (double))
{
	printf(" = %f\n", callback(0.0)); fflush(stdout);
	printf(" = %f\n", callback(1.1)); fflush(stdout);
	printf(" = %f\n", callback(-1.1)); fflush(stdout);
	printf(" = %f\n", callback(129.984375)); fflush(stdout);
	printf(" = %f\n", callback(-129.984375)); fflush(stdout);
}

PUBLIC
void callback_call_ifif(float (*callback) (int, float, int, float))
{
	printf(" = %f\n", callback(0, 0.0f, 0, 0)); fflush(stdout);
	printf(" = %f\n", callback(1, 1.1f, -2, -2.2f)); fflush(stdout);
	printf(" = %f\n", callback(-1, -1.1f, 2, 2.2f)); fflush(stdout);
	printf(" = %f\n", callback(999999, -129.984375f, 111111, -16.201171875)); fflush(stdout);
	printf(" = %f\n", callback(-999999, 129.984375, -111111, 16.201171875)); fflush(stdout);
}

PUBLIC
void callback_call_ifid(double (*callback) (int, float, int, double))
{
	printf(" = %f\n", callback(0, 0.0f, 0, 0)); fflush(stdout);
	printf(" = %f\n", callback(1, 1.1f, -2, -2.2f)); fflush(stdout);
	printf(" = %f\n", callback(-1, -1.1f, 2, 2.2f)); fflush(stdout);
	printf(" = %f\n", callback(999999, -129.984375f, 111111, -16.201171875)); fflush(stdout);
	printf(" = %f\n", callback(-999999, 129.984375, -111111, 16.201171875)); fflush(stdout);
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
		u32[rlen++] = sgetu8((unsigned char*)str, &x);
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

// structores

typedef struct item12
{
	int x;
	int y;
	int z;
} item12;

PUBLIC
item12 iii2struct12(int x, int y, int z)
{
	item12 i;
	i.x = x;
	i.y = y;
	i.z = z;

	printf(" = { %d, %d, %d }\n", i.x, i.y, i.z); fflush(stdout);
	return i;
}

typedef struct item20
{
	int x;
	int y;
	int z;
	int w1,w2;
} item20;

PUBLIC
item20 iii2struct20(int x, int y, int z)
{
	item20 i;
	i.x = x;
	i.y = y;
	i.z = z;
	i.w1 = 0; i.w2 = 0;

	printf(" = { %d, %d, %d }\n", i.x, i.y, i.z); fflush(stdout);
	return i;
}

PUBLIC
item12 iistruct122struct12(int x, int y, item12 z)
{
	item12 i = z;
	i.x = x;
	i.y = y;

	printf(" = { %d, %d, %d, %d, %d}\n", i.x, i.y, z.x, z.y, z.z); fflush(stdout);
	return i;
}

PUBLIC
item20 iistruct202struct20(int x, int y, item20 z)
{
	item20 i = z;
	i.x = x;
	i.y = y;

	printf(" = { %d, %d, %d, %d, %d, %d, %d}\n", i.x, i.y, z.x, z.y, z.z, z.w1, z.w2); fflush(stdout);
	return i;
}


#define STRUCT2(name, type1, type2, format) \
struct name \
{\
	type1 x;\
	type2 y;\
};\
PUBLIC \
int name ## 2i(struct name a)\
{\
	printf(" = { " format " } -> ", a.x, a.y); fflush(stdout);\
	return a.x + a.y;\
}

#define STRUCT2DECL(name, type, format) \
STRUCT2(_ ## name ## c_, type, char, format " " "%d")\
STRUCT2(_ ## name ## s_, type, short, format " " "%d")\
STRUCT2(_ ## name ## i_, type, int, format " " "%d")\
STRUCT2(_ ## name ## q_, type, long long, format " " "%lld")\
STRUCT2(_ ## name ## f_, type, float, format " " "%f")\
STRUCT2(_ ## name ## d_, type, double, format " " "%f")

STRUCT2DECL(c, char, "%d")
STRUCT2DECL(s, short, "%d")
STRUCT2DECL(i, int, "%d")
STRUCT2DECL(q, long long, "%lld")
STRUCT2DECL(f, float, "%f")
STRUCT2DECL(d, double, "%f")


// -----------------------------------------------------------------------------------
#include <stdarg.h>

int format(const char *format, ...)
{
    va_list ap;
    va_start(ap, format);

	size_t len = strlen(format);
    for (int i = 0; i < len; i++) {
		if (format[i] == '%') {
			char ch = format[++i];
			switch (ch) {
				case 'i': {
					int x = va_arg(ap, int);
					printf("%d", x);
					break;
				}
				case 'f': {
					double x = va_arg(ap, double);
					printf("%f", x);
					break;
				}
				default:
					printf("%c", ch);
					break;
			}

		}
		else
			printf("%c", format[i]);
	}
    va_end(ap);
	fflush(stdout);
    return 0;
}
