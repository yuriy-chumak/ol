#ifdef __unix__
#	define PUBLIC __attribute__ ((__visibility__("default"))) __attribute__((used))
#endif

#ifdef _WIN32
#	define PUBLIC __declspec(dllexport)
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

PUBLIC
int i_i(int a)
{
	LOG("a=%d\n", a);

	int r = (int)a;
	DONE("%d")
}

PUBLIC
float f_f(float a)
{
	LOG("a=%f\n", a);

	float r = (float)a;
	DONE("%f")
}

PUBLIC
double d_d(double a)
{
	LOG("a=%f\n", a);

	double r = (double)a;
	DONE("%f")
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
short fft_16i(short i)
{
	LOG("%d\n", i);
	return i;
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
