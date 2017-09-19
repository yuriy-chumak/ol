#ifdef __unix__
#	define PUBLIC __attribute__ ((__visibility__("default")))
#endif

#ifdef _WIN32
#	define PUBLIC __declspec(dllexport)
#endif

#if 1
#	define LOG(...)
#else
#	include <stdio.h>
#	define LOG printf
#endif

#define DONE(f)  LOG("<=" f "\n", r); return (r);

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

	return (a+b+c+d + f);
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

	return (i+j+k+l+m+n + f+g+h);
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

	return (i+j+k+l+m+n + f+g+h);
}

PUBLIC
float fii(float f, int a, int b)
{
	LOG("f=%f\n", f);
	LOG("a=%d\n", a);
	LOG("b=%d\n", b);

	return (a+b + f);
}

PUBLIC
float fi(float f, int a)
{
	LOG("f=%f\n", f);
	LOG("a=%d\n", a);

	return (a + f);
}

PUBLIC
float ifiii(int a, float f, int b, int c, int d)
{
	LOG("a=%d\n", a);
	LOG("f=%f\n", f);
	LOG("b=%d\n", b);
	LOG("c=%d\n", c);
	LOG("d=%d\n", d);

	return (a+b+c+d + f);
}

PUBLIC
float iiiif(int a, int b, int c, int d, float f)
{
	LOG("a=%d\n", a);
	LOG("b=%d\n", b);
	LOG("c=%d\n", c);
	LOG("d=%d\n", d);
	LOG("f=%f\n", f);

	return (a+b+c+d + f);
}

PUBLIC
float fiiif(float f, int a, int b, int c, float g)
{
	LOG("f=%f\n", f);
	LOG("a=%d\n", a);
	LOG("b=%d\n", b);
	LOG("c=%d\n", c);
	LOG("g=%f\n", g);

	return (a+b+c + f+g);
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
