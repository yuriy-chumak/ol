/* Notes:
 * no tests for type 'long' because 'long' is either 'int' or 'long long' depends on OS
   and is completely covered by other tests ('int' and 'long long').
 * type legend: 'c' for char, 's' for short, 'i' for int, 'q' for long long,
   capital chars for unsigned prefixes;  'f' for float, 'd' for double.
 * output inside {{ .. }} are native code output
 */

#include <stdio.h>

// function returning numeric type by value (neutral values, type limits)
// (such calls are optimized for fast execution)
/*
type cN_()
{
   type y = valueSUFFIX;
   printf('{{ () => format }}', y); fflush(stdout);
   return y;
}*/

unsigned char C0_()
{
   unsigned char y = 0;
   printf("{{ () => %u }}", y); fflush(stdout);
   return y;
}

unsigned char C1_()
{
   unsigned char y = 1;
   printf("{{ () => %u }}", y); fflush(stdout);
   return y;
}

unsigned char C255_()
{
   unsigned char y = 255;
   printf("{{ () => %u }}", y); fflush(stdout);
   return y;
}

unsigned short S0_()
{
   unsigned short y = 0;
   printf("{{ () => %u }}", y); fflush(stdout);
   return y;
}

unsigned short S1_()
{
   unsigned short y = 1;
   printf("{{ () => %u }}", y); fflush(stdout);
   return y;
}

unsigned short S65535_()
{
   unsigned short y = 65535;
   printf("{{ () => %u }}", y); fflush(stdout);
   return y;
}

unsigned int I0_()
{
   unsigned int y = 0;
   printf("{{ () => %u }}", y); fflush(stdout);
   return y;
}

unsigned int I1_()
{
   unsigned int y = 1;
   printf("{{ () => %u }}", y); fflush(stdout);
   return y;
}

unsigned int I4294967295_()
{
   unsigned int y = 4294967295;
   printf("{{ () => %u }}", y); fflush(stdout);
   return y;
}

unsigned long long L0_()
{
   unsigned long long y = 0ULL;
   printf("{{ () => %llu }}", y); fflush(stdout);
   return y;
}

unsigned long long L1_()
{
   unsigned long long y = 1ULL;
   printf("{{ () => %llu }}", y); fflush(stdout);
   return y;
}

unsigned long long L18446744073709551615_()
{
   unsigned long long y = 18446744073709551615ULL;
   printf("{{ () => %llu }}", y); fflush(stdout);
   return y;
}

signed char cm128_()
{
   signed char y = -128;
   printf("{{ () => %d }}", y); fflush(stdout);
   return y;
}

signed char cm1_()
{
   signed char y = -1;
   printf("{{ () => %d }}", y); fflush(stdout);
   return y;
}

signed char c0_()
{
   signed char y = 0;
   printf("{{ () => %d }}", y); fflush(stdout);
   return y;
}

signed char c1_()
{
   signed char y = 1;
   printf("{{ () => %d }}", y); fflush(stdout);
   return y;
}

signed char c127_()
{
   signed char y = 127;
   printf("{{ () => %d }}", y); fflush(stdout);
   return y;
}

signed short sm32768_()
{
   signed short y = -32768;
   printf("{{ () => %d }}", y); fflush(stdout);
   return y;
}

signed short sm1_()
{
   signed short y = -1;
   printf("{{ () => %d }}", y); fflush(stdout);
   return y;
}

signed short s0_()
{
   signed short y = 0;
   printf("{{ () => %d }}", y); fflush(stdout);
   return y;
}

signed short s1_()
{
   signed short y = 1;
   printf("{{ () => %d }}", y); fflush(stdout);
   return y;
}

signed short s32767_()
{
   signed short y = 32767;
   printf("{{ () => %d }}", y); fflush(stdout);
   return y;
}

signed int im2147483648_()
{
   signed int y = -2147483648;
   printf("{{ () => %d }}", y); fflush(stdout);
   return y;
}

signed int im1_()
{
   signed int y = -1;
   printf("{{ () => %d }}", y); fflush(stdout);
   return y;
}

signed int i0_()
{
   signed int y = 0;
   printf("{{ () => %d }}", y); fflush(stdout);
   return y;
}

signed int i1_()
{
   signed int y = 1;
   printf("{{ () => %d }}", y); fflush(stdout);
   return y;
}

signed int i2147483647_()
{
   signed int y = 2147483647;
   printf("{{ () => %d }}", y); fflush(stdout);
   return y;
}

signed long long lm9223372036854775807_()
{
   signed long long y = -9223372036854775807LL;
   printf("{{ () => %lld }}", y); fflush(stdout);
   return y;
}

signed long long lm1_()
{
   signed long long y = -1LL;
   printf("{{ () => %lld }}", y); fflush(stdout);
   return y;
}

signed long long l0_()
{
   signed long long y = 0LL;
   printf("{{ () => %lld }}", y); fflush(stdout);
   return y;
}

signed long long l1_()
{
   signed long long y = 1LL;
   printf("{{ () => %lld }}", y); fflush(stdout);
   return y;
}

signed long long l9223372036854775807_()
{
   signed long long y = 9223372036854775807LL;
   printf("{{ () => %lld }}", y); fflush(stdout);
   return y;
}

float fm10000000000_()
{
   float y = -10000000000.0f;
   printf("{{ () => %f }}", y); fflush(stdout);
   return y;
}

float fm1_()
{
   float y = -1.0f;
   printf("{{ () => %f }}", y); fflush(stdout);
   return y;
}

float f0_()
{
   float y = 0.0f;
   printf("{{ () => %f }}", y); fflush(stdout);
   return y;
}

float f1_()
{
   float y = 1.0f;
   printf("{{ () => %f }}", y); fflush(stdout);
   return y;
}

float f10000000000_()
{
   float y = 10000000000.0f;
   printf("{{ () => %f }}", y); fflush(stdout);
   return y;
}

double dm1000000000000000000000000000000000000000000000000_()
{
   double y = -1000000000000000000000000000000000000000000000000.0;
   printf("{{ () => %f }}", y); fflush(stdout);
   return y;
}

double dm1_()
{
   double y = -1.0;
   printf("{{ () => %f }}", y); fflush(stdout);
   return y;
}

double d0_()
{
   double y = 0.0;
   printf("{{ () => %f }}", y); fflush(stdout);
   return y;
}

double d1_()
{
   double y = 1.0;
   printf("{{ () => %f }}", y); fflush(stdout);
   return y;
}

double d1000000000000000000000000000000000000000000000000_()
{
   double y = 1000000000000000000000000000000000000000000000000.0;
   printf("{{ () => %f }}", y); fflush(stdout);
   return y;
}

// simple type(n) functions
/*
void v_cc..c(n)(type a1, type a2, .., type an)
{
   printf('{{ %u %u .. %u(n) }}', a1, a2, .., an); fflush(stdout);
}*/

void void_C(unsigned char a1)
{
   printf("{{ %u }}", a1); fflush(stdout);
}

void void_CC(unsigned char a1, unsigned char a2)
{
   printf("{{ %u %u }}", a1, a2); fflush(stdout);
}

void void_CCC(unsigned char a1, unsigned char a2, unsigned char a3)
{
   printf("{{ %u %u %u }}", a1, a2, a3); fflush(stdout);
}

void void_CCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4)
{
   printf("{{ %u %u %u %u }}", a1, a2, a3, a4); fflush(stdout);
}

void void_CCCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4, unsigned char a5)
{
   printf("{{ %u %u %u %u %u }}", a1, a2, a3, a4, a5); fflush(stdout);
}

void void_CCCCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4, unsigned char a5, unsigned char a6)
{
   printf("{{ %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6); fflush(stdout);
}

void void_CCCCCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4, unsigned char a5, unsigned char a6, unsigned char a7)
{
   printf("{{ %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7); fflush(stdout);
}

void void_CCCCCCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4, unsigned char a5, unsigned char a6, unsigned char a7, unsigned char a8)
{
   printf("{{ %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8); fflush(stdout);
}

void void_CCCCCCCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4, unsigned char a5, unsigned char a6, unsigned char a7, unsigned char a8, unsigned char a9)
{
   printf("{{ %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9); fflush(stdout);
}

void void_CCCCCCCCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4, unsigned char a5, unsigned char a6, unsigned char a7, unsigned char a8, unsigned char a9, unsigned char a10)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10); fflush(stdout);
}

void void_CCCCCCCCCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4, unsigned char a5, unsigned char a6, unsigned char a7, unsigned char a8, unsigned char a9, unsigned char a10, unsigned char a11)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11); fflush(stdout);
}

void void_CCCCCCCCCCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4, unsigned char a5, unsigned char a6, unsigned char a7, unsigned char a8, unsigned char a9, unsigned char a10, unsigned char a11, unsigned char a12)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12); fflush(stdout);
}

void void_CCCCCCCCCCCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4, unsigned char a5, unsigned char a6, unsigned char a7, unsigned char a8, unsigned char a9, unsigned char a10, unsigned char a11, unsigned char a12, unsigned char a13)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13); fflush(stdout);
}

void void_CCCCCCCCCCCCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4, unsigned char a5, unsigned char a6, unsigned char a7, unsigned char a8, unsigned char a9, unsigned char a10, unsigned char a11, unsigned char a12, unsigned char a13, unsigned char a14)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14); fflush(stdout);
}

void void_CCCCCCCCCCCCCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4, unsigned char a5, unsigned char a6, unsigned char a7, unsigned char a8, unsigned char a9, unsigned char a10, unsigned char a11, unsigned char a12, unsigned char a13, unsigned char a14, unsigned char a15)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15); fflush(stdout);
}

void void_CCCCCCCCCCCCCCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4, unsigned char a5, unsigned char a6, unsigned char a7, unsigned char a8, unsigned char a9, unsigned char a10, unsigned char a11, unsigned char a12, unsigned char a13, unsigned char a14, unsigned char a15, unsigned char a16)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16); fflush(stdout);
}

void void_CCCCCCCCCCCCCCCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4, unsigned char a5, unsigned char a6, unsigned char a7, unsigned char a8, unsigned char a9, unsigned char a10, unsigned char a11, unsigned char a12, unsigned char a13, unsigned char a14, unsigned char a15, unsigned char a16, unsigned char a17)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17); fflush(stdout);
}

void void_CCCCCCCCCCCCCCCCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4, unsigned char a5, unsigned char a6, unsigned char a7, unsigned char a8, unsigned char a9, unsigned char a10, unsigned char a11, unsigned char a12, unsigned char a13, unsigned char a14, unsigned char a15, unsigned char a16, unsigned char a17, unsigned char a18)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18); fflush(stdout);
}

void void_CCCCCCCCCCCCCCCCCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4, unsigned char a5, unsigned char a6, unsigned char a7, unsigned char a8, unsigned char a9, unsigned char a10, unsigned char a11, unsigned char a12, unsigned char a13, unsigned char a14, unsigned char a15, unsigned char a16, unsigned char a17, unsigned char a18, unsigned char a19)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19); fflush(stdout);
}

void void_CCCCCCCCCCCCCCCCCCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4, unsigned char a5, unsigned char a6, unsigned char a7, unsigned char a8, unsigned char a9, unsigned char a10, unsigned char a11, unsigned char a12, unsigned char a13, unsigned char a14, unsigned char a15, unsigned char a16, unsigned char a17, unsigned char a18, unsigned char a19, unsigned char a20)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20); fflush(stdout);
}

void void_CCCCCCCCCCCCCCCCCCCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4, unsigned char a5, unsigned char a6, unsigned char a7, unsigned char a8, unsigned char a9, unsigned char a10, unsigned char a11, unsigned char a12, unsigned char a13, unsigned char a14, unsigned char a15, unsigned char a16, unsigned char a17, unsigned char a18, unsigned char a19, unsigned char a20, unsigned char a21)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21); fflush(stdout);
}

void void_CCCCCCCCCCCCCCCCCCCCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4, unsigned char a5, unsigned char a6, unsigned char a7, unsigned char a8, unsigned char a9, unsigned char a10, unsigned char a11, unsigned char a12, unsigned char a13, unsigned char a14, unsigned char a15, unsigned char a16, unsigned char a17, unsigned char a18, unsigned char a19, unsigned char a20, unsigned char a21, unsigned char a22)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22); fflush(stdout);
}

void void_CCCCCCCCCCCCCCCCCCCCCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4, unsigned char a5, unsigned char a6, unsigned char a7, unsigned char a8, unsigned char a9, unsigned char a10, unsigned char a11, unsigned char a12, unsigned char a13, unsigned char a14, unsigned char a15, unsigned char a16, unsigned char a17, unsigned char a18, unsigned char a19, unsigned char a20, unsigned char a21, unsigned char a22, unsigned char a23)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23); fflush(stdout);
}

void void_CCCCCCCCCCCCCCCCCCCCCCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4, unsigned char a5, unsigned char a6, unsigned char a7, unsigned char a8, unsigned char a9, unsigned char a10, unsigned char a11, unsigned char a12, unsigned char a13, unsigned char a14, unsigned char a15, unsigned char a16, unsigned char a17, unsigned char a18, unsigned char a19, unsigned char a20, unsigned char a21, unsigned char a22, unsigned char a23, unsigned char a24)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24); fflush(stdout);
}

void void_S(unsigned short a1)
{
   printf("{{ %u }}", a1); fflush(stdout);
}

void void_SS(unsigned short a1, unsigned short a2)
{
   printf("{{ %u %u }}", a1, a2); fflush(stdout);
}

void void_SSS(unsigned short a1, unsigned short a2, unsigned short a3)
{
   printf("{{ %u %u %u }}", a1, a2, a3); fflush(stdout);
}

void void_SSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4)
{
   printf("{{ %u %u %u %u }}", a1, a2, a3, a4); fflush(stdout);
}

void void_SSSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4, unsigned short a5)
{
   printf("{{ %u %u %u %u %u }}", a1, a2, a3, a4, a5); fflush(stdout);
}

void void_SSSSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4, unsigned short a5, unsigned short a6)
{
   printf("{{ %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6); fflush(stdout);
}

void void_SSSSSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4, unsigned short a5, unsigned short a6, unsigned short a7)
{
   printf("{{ %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7); fflush(stdout);
}

void void_SSSSSSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4, unsigned short a5, unsigned short a6, unsigned short a7, unsigned short a8)
{
   printf("{{ %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8); fflush(stdout);
}

void void_SSSSSSSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4, unsigned short a5, unsigned short a6, unsigned short a7, unsigned short a8, unsigned short a9)
{
   printf("{{ %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9); fflush(stdout);
}

void void_SSSSSSSSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4, unsigned short a5, unsigned short a6, unsigned short a7, unsigned short a8, unsigned short a9, unsigned short a10)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10); fflush(stdout);
}

void void_SSSSSSSSSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4, unsigned short a5, unsigned short a6, unsigned short a7, unsigned short a8, unsigned short a9, unsigned short a10, unsigned short a11)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11); fflush(stdout);
}

void void_SSSSSSSSSSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4, unsigned short a5, unsigned short a6, unsigned short a7, unsigned short a8, unsigned short a9, unsigned short a10, unsigned short a11, unsigned short a12)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12); fflush(stdout);
}

void void_SSSSSSSSSSSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4, unsigned short a5, unsigned short a6, unsigned short a7, unsigned short a8, unsigned short a9, unsigned short a10, unsigned short a11, unsigned short a12, unsigned short a13)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13); fflush(stdout);
}

void void_SSSSSSSSSSSSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4, unsigned short a5, unsigned short a6, unsigned short a7, unsigned short a8, unsigned short a9, unsigned short a10, unsigned short a11, unsigned short a12, unsigned short a13, unsigned short a14)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14); fflush(stdout);
}

void void_SSSSSSSSSSSSSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4, unsigned short a5, unsigned short a6, unsigned short a7, unsigned short a8, unsigned short a9, unsigned short a10, unsigned short a11, unsigned short a12, unsigned short a13, unsigned short a14, unsigned short a15)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15); fflush(stdout);
}

void void_SSSSSSSSSSSSSSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4, unsigned short a5, unsigned short a6, unsigned short a7, unsigned short a8, unsigned short a9, unsigned short a10, unsigned short a11, unsigned short a12, unsigned short a13, unsigned short a14, unsigned short a15, unsigned short a16)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16); fflush(stdout);
}

void void_SSSSSSSSSSSSSSSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4, unsigned short a5, unsigned short a6, unsigned short a7, unsigned short a8, unsigned short a9, unsigned short a10, unsigned short a11, unsigned short a12, unsigned short a13, unsigned short a14, unsigned short a15, unsigned short a16, unsigned short a17)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17); fflush(stdout);
}

void void_SSSSSSSSSSSSSSSSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4, unsigned short a5, unsigned short a6, unsigned short a7, unsigned short a8, unsigned short a9, unsigned short a10, unsigned short a11, unsigned short a12, unsigned short a13, unsigned short a14, unsigned short a15, unsigned short a16, unsigned short a17, unsigned short a18)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18); fflush(stdout);
}

void void_SSSSSSSSSSSSSSSSSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4, unsigned short a5, unsigned short a6, unsigned short a7, unsigned short a8, unsigned short a9, unsigned short a10, unsigned short a11, unsigned short a12, unsigned short a13, unsigned short a14, unsigned short a15, unsigned short a16, unsigned short a17, unsigned short a18, unsigned short a19)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19); fflush(stdout);
}

void void_SSSSSSSSSSSSSSSSSSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4, unsigned short a5, unsigned short a6, unsigned short a7, unsigned short a8, unsigned short a9, unsigned short a10, unsigned short a11, unsigned short a12, unsigned short a13, unsigned short a14, unsigned short a15, unsigned short a16, unsigned short a17, unsigned short a18, unsigned short a19, unsigned short a20)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20); fflush(stdout);
}

void void_SSSSSSSSSSSSSSSSSSSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4, unsigned short a5, unsigned short a6, unsigned short a7, unsigned short a8, unsigned short a9, unsigned short a10, unsigned short a11, unsigned short a12, unsigned short a13, unsigned short a14, unsigned short a15, unsigned short a16, unsigned short a17, unsigned short a18, unsigned short a19, unsigned short a20, unsigned short a21)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21); fflush(stdout);
}

void void_SSSSSSSSSSSSSSSSSSSSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4, unsigned short a5, unsigned short a6, unsigned short a7, unsigned short a8, unsigned short a9, unsigned short a10, unsigned short a11, unsigned short a12, unsigned short a13, unsigned short a14, unsigned short a15, unsigned short a16, unsigned short a17, unsigned short a18, unsigned short a19, unsigned short a20, unsigned short a21, unsigned short a22)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22); fflush(stdout);
}

void void_SSSSSSSSSSSSSSSSSSSSSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4, unsigned short a5, unsigned short a6, unsigned short a7, unsigned short a8, unsigned short a9, unsigned short a10, unsigned short a11, unsigned short a12, unsigned short a13, unsigned short a14, unsigned short a15, unsigned short a16, unsigned short a17, unsigned short a18, unsigned short a19, unsigned short a20, unsigned short a21, unsigned short a22, unsigned short a23)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23); fflush(stdout);
}

void void_SSSSSSSSSSSSSSSSSSSSSSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4, unsigned short a5, unsigned short a6, unsigned short a7, unsigned short a8, unsigned short a9, unsigned short a10, unsigned short a11, unsigned short a12, unsigned short a13, unsigned short a14, unsigned short a15, unsigned short a16, unsigned short a17, unsigned short a18, unsigned short a19, unsigned short a20, unsigned short a21, unsigned short a22, unsigned short a23, unsigned short a24)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24); fflush(stdout);
}

void void_I(unsigned int a1)
{
   printf("{{ %u }}", a1); fflush(stdout);
}

void void_II(unsigned int a1, unsigned int a2)
{
   printf("{{ %u %u }}", a1, a2); fflush(stdout);
}

void void_III(unsigned int a1, unsigned int a2, unsigned int a3)
{
   printf("{{ %u %u %u }}", a1, a2, a3); fflush(stdout);
}

void void_IIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4)
{
   printf("{{ %u %u %u %u }}", a1, a2, a3, a4); fflush(stdout);
}

void void_IIIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4, unsigned int a5)
{
   printf("{{ %u %u %u %u %u }}", a1, a2, a3, a4, a5); fflush(stdout);
}

void void_IIIIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6)
{
   printf("{{ %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6); fflush(stdout);
}

void void_IIIIIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6, unsigned int a7)
{
   printf("{{ %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7); fflush(stdout);
}

void void_IIIIIIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6, unsigned int a7, unsigned int a8)
{
   printf("{{ %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8); fflush(stdout);
}

void void_IIIIIIIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6, unsigned int a7, unsigned int a8, unsigned int a9)
{
   printf("{{ %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9); fflush(stdout);
}

void void_IIIIIIIIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6, unsigned int a7, unsigned int a8, unsigned int a9, unsigned int a10)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10); fflush(stdout);
}

void void_IIIIIIIIIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6, unsigned int a7, unsigned int a8, unsigned int a9, unsigned int a10, unsigned int a11)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11); fflush(stdout);
}

void void_IIIIIIIIIIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6, unsigned int a7, unsigned int a8, unsigned int a9, unsigned int a10, unsigned int a11, unsigned int a12)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12); fflush(stdout);
}

void void_IIIIIIIIIIIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6, unsigned int a7, unsigned int a8, unsigned int a9, unsigned int a10, unsigned int a11, unsigned int a12, unsigned int a13)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13); fflush(stdout);
}

void void_IIIIIIIIIIIIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6, unsigned int a7, unsigned int a8, unsigned int a9, unsigned int a10, unsigned int a11, unsigned int a12, unsigned int a13, unsigned int a14)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14); fflush(stdout);
}

void void_IIIIIIIIIIIIIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6, unsigned int a7, unsigned int a8, unsigned int a9, unsigned int a10, unsigned int a11, unsigned int a12, unsigned int a13, unsigned int a14, unsigned int a15)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15); fflush(stdout);
}

void void_IIIIIIIIIIIIIIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6, unsigned int a7, unsigned int a8, unsigned int a9, unsigned int a10, unsigned int a11, unsigned int a12, unsigned int a13, unsigned int a14, unsigned int a15, unsigned int a16)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16); fflush(stdout);
}

void void_IIIIIIIIIIIIIIIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6, unsigned int a7, unsigned int a8, unsigned int a9, unsigned int a10, unsigned int a11, unsigned int a12, unsigned int a13, unsigned int a14, unsigned int a15, unsigned int a16, unsigned int a17)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17); fflush(stdout);
}

void void_IIIIIIIIIIIIIIIIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6, unsigned int a7, unsigned int a8, unsigned int a9, unsigned int a10, unsigned int a11, unsigned int a12, unsigned int a13, unsigned int a14, unsigned int a15, unsigned int a16, unsigned int a17, unsigned int a18)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18); fflush(stdout);
}

void void_IIIIIIIIIIIIIIIIIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6, unsigned int a7, unsigned int a8, unsigned int a9, unsigned int a10, unsigned int a11, unsigned int a12, unsigned int a13, unsigned int a14, unsigned int a15, unsigned int a16, unsigned int a17, unsigned int a18, unsigned int a19)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19); fflush(stdout);
}

void void_IIIIIIIIIIIIIIIIIIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6, unsigned int a7, unsigned int a8, unsigned int a9, unsigned int a10, unsigned int a11, unsigned int a12, unsigned int a13, unsigned int a14, unsigned int a15, unsigned int a16, unsigned int a17, unsigned int a18, unsigned int a19, unsigned int a20)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20); fflush(stdout);
}

void void_IIIIIIIIIIIIIIIIIIIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6, unsigned int a7, unsigned int a8, unsigned int a9, unsigned int a10, unsigned int a11, unsigned int a12, unsigned int a13, unsigned int a14, unsigned int a15, unsigned int a16, unsigned int a17, unsigned int a18, unsigned int a19, unsigned int a20, unsigned int a21)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21); fflush(stdout);
}

void void_IIIIIIIIIIIIIIIIIIIIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6, unsigned int a7, unsigned int a8, unsigned int a9, unsigned int a10, unsigned int a11, unsigned int a12, unsigned int a13, unsigned int a14, unsigned int a15, unsigned int a16, unsigned int a17, unsigned int a18, unsigned int a19, unsigned int a20, unsigned int a21, unsigned int a22)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22); fflush(stdout);
}

void void_IIIIIIIIIIIIIIIIIIIIIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6, unsigned int a7, unsigned int a8, unsigned int a9, unsigned int a10, unsigned int a11, unsigned int a12, unsigned int a13, unsigned int a14, unsigned int a15, unsigned int a16, unsigned int a17, unsigned int a18, unsigned int a19, unsigned int a20, unsigned int a21, unsigned int a22, unsigned int a23)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23); fflush(stdout);
}

void void_IIIIIIIIIIIIIIIIIIIIIIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6, unsigned int a7, unsigned int a8, unsigned int a9, unsigned int a10, unsigned int a11, unsigned int a12, unsigned int a13, unsigned int a14, unsigned int a15, unsigned int a16, unsigned int a17, unsigned int a18, unsigned int a19, unsigned int a20, unsigned int a21, unsigned int a22, unsigned int a23, unsigned int a24)
{
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24); fflush(stdout);
}

void void_L(unsigned long long a1)
{
   printf("{{ %llu }}", a1); fflush(stdout);
}

void void_LL(unsigned long long a1, unsigned long long a2)
{
   printf("{{ %llu %llu }}", a1, a2); fflush(stdout);
}

void void_LLL(unsigned long long a1, unsigned long long a2, unsigned long long a3)
{
   printf("{{ %llu %llu %llu }}", a1, a2, a3); fflush(stdout);
}

void void_LLLL(unsigned long long a1, unsigned long long a2, unsigned long long a3, unsigned long long a4)
{
   printf("{{ %llu %llu %llu %llu }}", a1, a2, a3, a4); fflush(stdout);
}

void void_LLLLL(unsigned long long a1, unsigned long long a2, unsigned long long a3, unsigned long long a4, unsigned long long a5)
{
   printf("{{ %llu %llu %llu %llu %llu }}", a1, a2, a3, a4, a5); fflush(stdout);
}

void void_LLLLLL(unsigned long long a1, unsigned long long a2, unsigned long long a3, unsigned long long a4, unsigned long long a5, unsigned long long a6)
{
   printf("{{ %llu %llu %llu %llu %llu %llu }}", a1, a2, a3, a4, a5, a6); fflush(stdout);
}

void void_LLLLLLL(unsigned long long a1, unsigned long long a2, unsigned long long a3, unsigned long long a4, unsigned long long a5, unsigned long long a6, unsigned long long a7)
{
   printf("{{ %llu %llu %llu %llu %llu %llu %llu }}", a1, a2, a3, a4, a5, a6, a7); fflush(stdout);
}

void void_LLLLLLLL(unsigned long long a1, unsigned long long a2, unsigned long long a3, unsigned long long a4, unsigned long long a5, unsigned long long a6, unsigned long long a7, unsigned long long a8)
{
   printf("{{ %llu %llu %llu %llu %llu %llu %llu %llu }}", a1, a2, a3, a4, a5, a6, a7, a8); fflush(stdout);
}

void void_LLLLLLLLL(unsigned long long a1, unsigned long long a2, unsigned long long a3, unsigned long long a4, unsigned long long a5, unsigned long long a6, unsigned long long a7, unsigned long long a8, unsigned long long a9)
{
   printf("{{ %llu %llu %llu %llu %llu %llu %llu %llu %llu }}", a1, a2, a3, a4, a5, a6, a7, a8, a9); fflush(stdout);
}

void void_LLLLLLLLLL(unsigned long long a1, unsigned long long a2, unsigned long long a3, unsigned long long a4, unsigned long long a5, unsigned long long a6, unsigned long long a7, unsigned long long a8, unsigned long long a9, unsigned long long a10)
{
   printf("{{ %llu %llu %llu %llu %llu %llu %llu %llu %llu %llu }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10); fflush(stdout);
}

void void_LLLLLLLLLLL(unsigned long long a1, unsigned long long a2, unsigned long long a3, unsigned long long a4, unsigned long long a5, unsigned long long a6, unsigned long long a7, unsigned long long a8, unsigned long long a9, unsigned long long a10, unsigned long long a11)
{
   printf("{{ %llu %llu %llu %llu %llu %llu %llu %llu %llu %llu %llu }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11); fflush(stdout);
}

void void_LLLLLLLLLLLL(unsigned long long a1, unsigned long long a2, unsigned long long a3, unsigned long long a4, unsigned long long a5, unsigned long long a6, unsigned long long a7, unsigned long long a8, unsigned long long a9, unsigned long long a10, unsigned long long a11, unsigned long long a12)
{
   printf("{{ %llu %llu %llu %llu %llu %llu %llu %llu %llu %llu %llu %llu }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12); fflush(stdout);
}

void void_c(signed char a1)
{
   printf("{{ %d }}", a1); fflush(stdout);
}

void void_cc(signed char a1, signed char a2)
{
   printf("{{ %d %d }}", a1, a2); fflush(stdout);
}

void void_ccc(signed char a1, signed char a2, signed char a3)
{
   printf("{{ %d %d %d }}", a1, a2, a3); fflush(stdout);
}

void void_cccc(signed char a1, signed char a2, signed char a3, signed char a4)
{
   printf("{{ %d %d %d %d }}", a1, a2, a3, a4); fflush(stdout);
}

void void_ccccc(signed char a1, signed char a2, signed char a3, signed char a4, signed char a5)
{
   printf("{{ %d %d %d %d %d }}", a1, a2, a3, a4, a5); fflush(stdout);
}

void void_cccccc(signed char a1, signed char a2, signed char a3, signed char a4, signed char a5, signed char a6)
{
   printf("{{ %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6); fflush(stdout);
}

void void_ccccccc(signed char a1, signed char a2, signed char a3, signed char a4, signed char a5, signed char a6, signed char a7)
{
   printf("{{ %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7); fflush(stdout);
}

void void_cccccccc(signed char a1, signed char a2, signed char a3, signed char a4, signed char a5, signed char a6, signed char a7, signed char a8)
{
   printf("{{ %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8); fflush(stdout);
}

void void_ccccccccc(signed char a1, signed char a2, signed char a3, signed char a4, signed char a5, signed char a6, signed char a7, signed char a8, signed char a9)
{
   printf("{{ %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9); fflush(stdout);
}

void void_cccccccccc(signed char a1, signed char a2, signed char a3, signed char a4, signed char a5, signed char a6, signed char a7, signed char a8, signed char a9, signed char a10)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10); fflush(stdout);
}

void void_ccccccccccc(signed char a1, signed char a2, signed char a3, signed char a4, signed char a5, signed char a6, signed char a7, signed char a8, signed char a9, signed char a10, signed char a11)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11); fflush(stdout);
}

void void_cccccccccccc(signed char a1, signed char a2, signed char a3, signed char a4, signed char a5, signed char a6, signed char a7, signed char a8, signed char a9, signed char a10, signed char a11, signed char a12)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12); fflush(stdout);
}

void void_ccccccccccccc(signed char a1, signed char a2, signed char a3, signed char a4, signed char a5, signed char a6, signed char a7, signed char a8, signed char a9, signed char a10, signed char a11, signed char a12, signed char a13)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13); fflush(stdout);
}

void void_cccccccccccccc(signed char a1, signed char a2, signed char a3, signed char a4, signed char a5, signed char a6, signed char a7, signed char a8, signed char a9, signed char a10, signed char a11, signed char a12, signed char a13, signed char a14)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14); fflush(stdout);
}

void void_ccccccccccccccc(signed char a1, signed char a2, signed char a3, signed char a4, signed char a5, signed char a6, signed char a7, signed char a8, signed char a9, signed char a10, signed char a11, signed char a12, signed char a13, signed char a14, signed char a15)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15); fflush(stdout);
}

void void_cccccccccccccccc(signed char a1, signed char a2, signed char a3, signed char a4, signed char a5, signed char a6, signed char a7, signed char a8, signed char a9, signed char a10, signed char a11, signed char a12, signed char a13, signed char a14, signed char a15, signed char a16)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16); fflush(stdout);
}

void void_ccccccccccccccccc(signed char a1, signed char a2, signed char a3, signed char a4, signed char a5, signed char a6, signed char a7, signed char a8, signed char a9, signed char a10, signed char a11, signed char a12, signed char a13, signed char a14, signed char a15, signed char a16, signed char a17)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17); fflush(stdout);
}

void void_cccccccccccccccccc(signed char a1, signed char a2, signed char a3, signed char a4, signed char a5, signed char a6, signed char a7, signed char a8, signed char a9, signed char a10, signed char a11, signed char a12, signed char a13, signed char a14, signed char a15, signed char a16, signed char a17, signed char a18)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18); fflush(stdout);
}

void void_ccccccccccccccccccc(signed char a1, signed char a2, signed char a3, signed char a4, signed char a5, signed char a6, signed char a7, signed char a8, signed char a9, signed char a10, signed char a11, signed char a12, signed char a13, signed char a14, signed char a15, signed char a16, signed char a17, signed char a18, signed char a19)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19); fflush(stdout);
}

void void_cccccccccccccccccccc(signed char a1, signed char a2, signed char a3, signed char a4, signed char a5, signed char a6, signed char a7, signed char a8, signed char a9, signed char a10, signed char a11, signed char a12, signed char a13, signed char a14, signed char a15, signed char a16, signed char a17, signed char a18, signed char a19, signed char a20)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20); fflush(stdout);
}

void void_ccccccccccccccccccccc(signed char a1, signed char a2, signed char a3, signed char a4, signed char a5, signed char a6, signed char a7, signed char a8, signed char a9, signed char a10, signed char a11, signed char a12, signed char a13, signed char a14, signed char a15, signed char a16, signed char a17, signed char a18, signed char a19, signed char a20, signed char a21)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21); fflush(stdout);
}

void void_cccccccccccccccccccccc(signed char a1, signed char a2, signed char a3, signed char a4, signed char a5, signed char a6, signed char a7, signed char a8, signed char a9, signed char a10, signed char a11, signed char a12, signed char a13, signed char a14, signed char a15, signed char a16, signed char a17, signed char a18, signed char a19, signed char a20, signed char a21, signed char a22)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22); fflush(stdout);
}

void void_ccccccccccccccccccccccc(signed char a1, signed char a2, signed char a3, signed char a4, signed char a5, signed char a6, signed char a7, signed char a8, signed char a9, signed char a10, signed char a11, signed char a12, signed char a13, signed char a14, signed char a15, signed char a16, signed char a17, signed char a18, signed char a19, signed char a20, signed char a21, signed char a22, signed char a23)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23); fflush(stdout);
}

void void_cccccccccccccccccccccccc(signed char a1, signed char a2, signed char a3, signed char a4, signed char a5, signed char a6, signed char a7, signed char a8, signed char a9, signed char a10, signed char a11, signed char a12, signed char a13, signed char a14, signed char a15, signed char a16, signed char a17, signed char a18, signed char a19, signed char a20, signed char a21, signed char a22, signed char a23, signed char a24)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24); fflush(stdout);
}

void void_s(signed short a1)
{
   printf("{{ %d }}", a1); fflush(stdout);
}

void void_ss(signed short a1, signed short a2)
{
   printf("{{ %d %d }}", a1, a2); fflush(stdout);
}

void void_sss(signed short a1, signed short a2, signed short a3)
{
   printf("{{ %d %d %d }}", a1, a2, a3); fflush(stdout);
}

void void_ssss(signed short a1, signed short a2, signed short a3, signed short a4)
{
   printf("{{ %d %d %d %d }}", a1, a2, a3, a4); fflush(stdout);
}

void void_sssss(signed short a1, signed short a2, signed short a3, signed short a4, signed short a5)
{
   printf("{{ %d %d %d %d %d }}", a1, a2, a3, a4, a5); fflush(stdout);
}

void void_ssssss(signed short a1, signed short a2, signed short a3, signed short a4, signed short a5, signed short a6)
{
   printf("{{ %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6); fflush(stdout);
}

void void_sssssss(signed short a1, signed short a2, signed short a3, signed short a4, signed short a5, signed short a6, signed short a7)
{
   printf("{{ %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7); fflush(stdout);
}

void void_ssssssss(signed short a1, signed short a2, signed short a3, signed short a4, signed short a5, signed short a6, signed short a7, signed short a8)
{
   printf("{{ %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8); fflush(stdout);
}

void void_sssssssss(signed short a1, signed short a2, signed short a3, signed short a4, signed short a5, signed short a6, signed short a7, signed short a8, signed short a9)
{
   printf("{{ %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9); fflush(stdout);
}

void void_ssssssssss(signed short a1, signed short a2, signed short a3, signed short a4, signed short a5, signed short a6, signed short a7, signed short a8, signed short a9, signed short a10)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10); fflush(stdout);
}

void void_sssssssssss(signed short a1, signed short a2, signed short a3, signed short a4, signed short a5, signed short a6, signed short a7, signed short a8, signed short a9, signed short a10, signed short a11)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11); fflush(stdout);
}

void void_ssssssssssss(signed short a1, signed short a2, signed short a3, signed short a4, signed short a5, signed short a6, signed short a7, signed short a8, signed short a9, signed short a10, signed short a11, signed short a12)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12); fflush(stdout);
}

void void_sssssssssssss(signed short a1, signed short a2, signed short a3, signed short a4, signed short a5, signed short a6, signed short a7, signed short a8, signed short a9, signed short a10, signed short a11, signed short a12, signed short a13)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13); fflush(stdout);
}

void void_ssssssssssssss(signed short a1, signed short a2, signed short a3, signed short a4, signed short a5, signed short a6, signed short a7, signed short a8, signed short a9, signed short a10, signed short a11, signed short a12, signed short a13, signed short a14)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14); fflush(stdout);
}

void void_sssssssssssssss(signed short a1, signed short a2, signed short a3, signed short a4, signed short a5, signed short a6, signed short a7, signed short a8, signed short a9, signed short a10, signed short a11, signed short a12, signed short a13, signed short a14, signed short a15)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15); fflush(stdout);
}

void void_ssssssssssssssss(signed short a1, signed short a2, signed short a3, signed short a4, signed short a5, signed short a6, signed short a7, signed short a8, signed short a9, signed short a10, signed short a11, signed short a12, signed short a13, signed short a14, signed short a15, signed short a16)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16); fflush(stdout);
}

void void_sssssssssssssssss(signed short a1, signed short a2, signed short a3, signed short a4, signed short a5, signed short a6, signed short a7, signed short a8, signed short a9, signed short a10, signed short a11, signed short a12, signed short a13, signed short a14, signed short a15, signed short a16, signed short a17)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17); fflush(stdout);
}

void void_ssssssssssssssssss(signed short a1, signed short a2, signed short a3, signed short a4, signed short a5, signed short a6, signed short a7, signed short a8, signed short a9, signed short a10, signed short a11, signed short a12, signed short a13, signed short a14, signed short a15, signed short a16, signed short a17, signed short a18)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18); fflush(stdout);
}

void void_sssssssssssssssssss(signed short a1, signed short a2, signed short a3, signed short a4, signed short a5, signed short a6, signed short a7, signed short a8, signed short a9, signed short a10, signed short a11, signed short a12, signed short a13, signed short a14, signed short a15, signed short a16, signed short a17, signed short a18, signed short a19)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19); fflush(stdout);
}

void void_ssssssssssssssssssss(signed short a1, signed short a2, signed short a3, signed short a4, signed short a5, signed short a6, signed short a7, signed short a8, signed short a9, signed short a10, signed short a11, signed short a12, signed short a13, signed short a14, signed short a15, signed short a16, signed short a17, signed short a18, signed short a19, signed short a20)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20); fflush(stdout);
}

void void_sssssssssssssssssssss(signed short a1, signed short a2, signed short a3, signed short a4, signed short a5, signed short a6, signed short a7, signed short a8, signed short a9, signed short a10, signed short a11, signed short a12, signed short a13, signed short a14, signed short a15, signed short a16, signed short a17, signed short a18, signed short a19, signed short a20, signed short a21)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21); fflush(stdout);
}

void void_ssssssssssssssssssssss(signed short a1, signed short a2, signed short a3, signed short a4, signed short a5, signed short a6, signed short a7, signed short a8, signed short a9, signed short a10, signed short a11, signed short a12, signed short a13, signed short a14, signed short a15, signed short a16, signed short a17, signed short a18, signed short a19, signed short a20, signed short a21, signed short a22)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22); fflush(stdout);
}

void void_sssssssssssssssssssssss(signed short a1, signed short a2, signed short a3, signed short a4, signed short a5, signed short a6, signed short a7, signed short a8, signed short a9, signed short a10, signed short a11, signed short a12, signed short a13, signed short a14, signed short a15, signed short a16, signed short a17, signed short a18, signed short a19, signed short a20, signed short a21, signed short a22, signed short a23)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23); fflush(stdout);
}

void void_ssssssssssssssssssssssss(signed short a1, signed short a2, signed short a3, signed short a4, signed short a5, signed short a6, signed short a7, signed short a8, signed short a9, signed short a10, signed short a11, signed short a12, signed short a13, signed short a14, signed short a15, signed short a16, signed short a17, signed short a18, signed short a19, signed short a20, signed short a21, signed short a22, signed short a23, signed short a24)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24); fflush(stdout);
}

void void_i(signed int a1)
{
   printf("{{ %d }}", a1); fflush(stdout);
}

void void_ii(signed int a1, signed int a2)
{
   printf("{{ %d %d }}", a1, a2); fflush(stdout);
}

void void_iii(signed int a1, signed int a2, signed int a3)
{
   printf("{{ %d %d %d }}", a1, a2, a3); fflush(stdout);
}

void void_iiii(signed int a1, signed int a2, signed int a3, signed int a4)
{
   printf("{{ %d %d %d %d }}", a1, a2, a3, a4); fflush(stdout);
}

void void_iiiii(signed int a1, signed int a2, signed int a3, signed int a4, signed int a5)
{
   printf("{{ %d %d %d %d %d }}", a1, a2, a3, a4, a5); fflush(stdout);
}

void void_iiiiii(signed int a1, signed int a2, signed int a3, signed int a4, signed int a5, signed int a6)
{
   printf("{{ %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6); fflush(stdout);
}

void void_iiiiiii(signed int a1, signed int a2, signed int a3, signed int a4, signed int a5, signed int a6, signed int a7)
{
   printf("{{ %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7); fflush(stdout);
}

void void_iiiiiiii(signed int a1, signed int a2, signed int a3, signed int a4, signed int a5, signed int a6, signed int a7, signed int a8)
{
   printf("{{ %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8); fflush(stdout);
}

void void_iiiiiiiii(signed int a1, signed int a2, signed int a3, signed int a4, signed int a5, signed int a6, signed int a7, signed int a8, signed int a9)
{
   printf("{{ %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9); fflush(stdout);
}

void void_iiiiiiiiii(signed int a1, signed int a2, signed int a3, signed int a4, signed int a5, signed int a6, signed int a7, signed int a8, signed int a9, signed int a10)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10); fflush(stdout);
}

void void_iiiiiiiiiii(signed int a1, signed int a2, signed int a3, signed int a4, signed int a5, signed int a6, signed int a7, signed int a8, signed int a9, signed int a10, signed int a11)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11); fflush(stdout);
}

void void_iiiiiiiiiiii(signed int a1, signed int a2, signed int a3, signed int a4, signed int a5, signed int a6, signed int a7, signed int a8, signed int a9, signed int a10, signed int a11, signed int a12)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12); fflush(stdout);
}

void void_iiiiiiiiiiiii(signed int a1, signed int a2, signed int a3, signed int a4, signed int a5, signed int a6, signed int a7, signed int a8, signed int a9, signed int a10, signed int a11, signed int a12, signed int a13)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13); fflush(stdout);
}

void void_iiiiiiiiiiiiii(signed int a1, signed int a2, signed int a3, signed int a4, signed int a5, signed int a6, signed int a7, signed int a8, signed int a9, signed int a10, signed int a11, signed int a12, signed int a13, signed int a14)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14); fflush(stdout);
}

void void_iiiiiiiiiiiiiii(signed int a1, signed int a2, signed int a3, signed int a4, signed int a5, signed int a6, signed int a7, signed int a8, signed int a9, signed int a10, signed int a11, signed int a12, signed int a13, signed int a14, signed int a15)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15); fflush(stdout);
}

void void_iiiiiiiiiiiiiiii(signed int a1, signed int a2, signed int a3, signed int a4, signed int a5, signed int a6, signed int a7, signed int a8, signed int a9, signed int a10, signed int a11, signed int a12, signed int a13, signed int a14, signed int a15, signed int a16)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16); fflush(stdout);
}

void void_iiiiiiiiiiiiiiiii(signed int a1, signed int a2, signed int a3, signed int a4, signed int a5, signed int a6, signed int a7, signed int a8, signed int a9, signed int a10, signed int a11, signed int a12, signed int a13, signed int a14, signed int a15, signed int a16, signed int a17)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17); fflush(stdout);
}

void void_iiiiiiiiiiiiiiiiii(signed int a1, signed int a2, signed int a3, signed int a4, signed int a5, signed int a6, signed int a7, signed int a8, signed int a9, signed int a10, signed int a11, signed int a12, signed int a13, signed int a14, signed int a15, signed int a16, signed int a17, signed int a18)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18); fflush(stdout);
}

void void_iiiiiiiiiiiiiiiiiii(signed int a1, signed int a2, signed int a3, signed int a4, signed int a5, signed int a6, signed int a7, signed int a8, signed int a9, signed int a10, signed int a11, signed int a12, signed int a13, signed int a14, signed int a15, signed int a16, signed int a17, signed int a18, signed int a19)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19); fflush(stdout);
}

void void_iiiiiiiiiiiiiiiiiiii(signed int a1, signed int a2, signed int a3, signed int a4, signed int a5, signed int a6, signed int a7, signed int a8, signed int a9, signed int a10, signed int a11, signed int a12, signed int a13, signed int a14, signed int a15, signed int a16, signed int a17, signed int a18, signed int a19, signed int a20)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20); fflush(stdout);
}

void void_iiiiiiiiiiiiiiiiiiiii(signed int a1, signed int a2, signed int a3, signed int a4, signed int a5, signed int a6, signed int a7, signed int a8, signed int a9, signed int a10, signed int a11, signed int a12, signed int a13, signed int a14, signed int a15, signed int a16, signed int a17, signed int a18, signed int a19, signed int a20, signed int a21)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21); fflush(stdout);
}

void void_iiiiiiiiiiiiiiiiiiiiii(signed int a1, signed int a2, signed int a3, signed int a4, signed int a5, signed int a6, signed int a7, signed int a8, signed int a9, signed int a10, signed int a11, signed int a12, signed int a13, signed int a14, signed int a15, signed int a16, signed int a17, signed int a18, signed int a19, signed int a20, signed int a21, signed int a22)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22); fflush(stdout);
}

void void_iiiiiiiiiiiiiiiiiiiiiii(signed int a1, signed int a2, signed int a3, signed int a4, signed int a5, signed int a6, signed int a7, signed int a8, signed int a9, signed int a10, signed int a11, signed int a12, signed int a13, signed int a14, signed int a15, signed int a16, signed int a17, signed int a18, signed int a19, signed int a20, signed int a21, signed int a22, signed int a23)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23); fflush(stdout);
}

void void_iiiiiiiiiiiiiiiiiiiiiiii(signed int a1, signed int a2, signed int a3, signed int a4, signed int a5, signed int a6, signed int a7, signed int a8, signed int a9, signed int a10, signed int a11, signed int a12, signed int a13, signed int a14, signed int a15, signed int a16, signed int a17, signed int a18, signed int a19, signed int a20, signed int a21, signed int a22, signed int a23, signed int a24)
{
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24); fflush(stdout);
}

void void_l(long long a1)
{
   printf("{{ %lld }}", a1); fflush(stdout);
}

void void_ll(long long a1, long long a2)
{
   printf("{{ %lld %lld }}", a1, a2); fflush(stdout);
}

void void_lll(long long a1, long long a2, long long a3)
{
   printf("{{ %lld %lld %lld }}", a1, a2, a3); fflush(stdout);
}

void void_llll(long long a1, long long a2, long long a3, long long a4)
{
   printf("{{ %lld %lld %lld %lld }}", a1, a2, a3, a4); fflush(stdout);
}

void void_lllll(long long a1, long long a2, long long a3, long long a4, long long a5)
{
   printf("{{ %lld %lld %lld %lld %lld }}", a1, a2, a3, a4, a5); fflush(stdout);
}

void void_llllll(long long a1, long long a2, long long a3, long long a4, long long a5, long long a6)
{
   printf("{{ %lld %lld %lld %lld %lld %lld }}", a1, a2, a3, a4, a5, a6); fflush(stdout);
}

void void_lllllll(long long a1, long long a2, long long a3, long long a4, long long a5, long long a6, long long a7)
{
   printf("{{ %lld %lld %lld %lld %lld %lld %lld }}", a1, a2, a3, a4, a5, a6, a7); fflush(stdout);
}

void void_llllllll(long long a1, long long a2, long long a3, long long a4, long long a5, long long a6, long long a7, long long a8)
{
   printf("{{ %lld %lld %lld %lld %lld %lld %lld %lld }}", a1, a2, a3, a4, a5, a6, a7, a8); fflush(stdout);
}

void void_lllllllll(long long a1, long long a2, long long a3, long long a4, long long a5, long long a6, long long a7, long long a8, long long a9)
{
   printf("{{ %lld %lld %lld %lld %lld %lld %lld %lld %lld }}", a1, a2, a3, a4, a5, a6, a7, a8, a9); fflush(stdout);
}

void void_llllllllll(long long a1, long long a2, long long a3, long long a4, long long a5, long long a6, long long a7, long long a8, long long a9, long long a10)
{
   printf("{{ %lld %lld %lld %lld %lld %lld %lld %lld %lld %lld }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10); fflush(stdout);
}

void void_lllllllllll(long long a1, long long a2, long long a3, long long a4, long long a5, long long a6, long long a7, long long a8, long long a9, long long a10, long long a11)
{
   printf("{{ %lld %lld %lld %lld %lld %lld %lld %lld %lld %lld %lld }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11); fflush(stdout);
}

void void_llllllllllll(long long a1, long long a2, long long a3, long long a4, long long a5, long long a6, long long a7, long long a8, long long a9, long long a10, long long a11, long long a12)
{
   printf("{{ %lld %lld %lld %lld %lld %lld %lld %lld %lld %lld %lld %lld }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12); fflush(stdout);
}

void void_f(float a1)
{
   printf("{{ %f }}", a1); fflush(stdout);
}

void void_ff(float a1, float a2)
{
   printf("{{ %f %f }}", a1, a2); fflush(stdout);
}

void void_fff(float a1, float a2, float a3)
{
   printf("{{ %f %f %f }}", a1, a2, a3); fflush(stdout);
}

void void_ffff(float a1, float a2, float a3, float a4)
{
   printf("{{ %f %f %f %f }}", a1, a2, a3, a4); fflush(stdout);
}

void void_fffff(float a1, float a2, float a3, float a4, float a5)
{
   printf("{{ %f %f %f %f %f }}", a1, a2, a3, a4, a5); fflush(stdout);
}

void void_ffffff(float a1, float a2, float a3, float a4, float a5, float a6)
{
   printf("{{ %f %f %f %f %f %f }}", a1, a2, a3, a4, a5, a6); fflush(stdout);
}

void void_fffffff(float a1, float a2, float a3, float a4, float a5, float a6, float a7)
{
   printf("{{ %f %f %f %f %f %f %f }}", a1, a2, a3, a4, a5, a6, a7); fflush(stdout);
}

void void_ffffffff(float a1, float a2, float a3, float a4, float a5, float a6, float a7, float a8)
{
   printf("{{ %f %f %f %f %f %f %f %f }}", a1, a2, a3, a4, a5, a6, a7, a8); fflush(stdout);
}

void void_fffffffff(float a1, float a2, float a3, float a4, float a5, float a6, float a7, float a8, float a9)
{
   printf("{{ %f %f %f %f %f %f %f %f %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9); fflush(stdout);
}

void void_ffffffffff(float a1, float a2, float a3, float a4, float a5, float a6, float a7, float a8, float a9, float a10)
{
   printf("{{ %f %f %f %f %f %f %f %f %f %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10); fflush(stdout);
}

void void_fffffffffff(float a1, float a2, float a3, float a4, float a5, float a6, float a7, float a8, float a9, float a10, float a11)
{
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11); fflush(stdout);
}

void void_ffffffffffff(float a1, float a2, float a3, float a4, float a5, float a6, float a7, float a8, float a9, float a10, float a11, float a12)
{
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12); fflush(stdout);
}

void void_fffffffffffff(float a1, float a2, float a3, float a4, float a5, float a6, float a7, float a8, float a9, float a10, float a11, float a12, float a13)
{
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13); fflush(stdout);
}

void void_ffffffffffffff(float a1, float a2, float a3, float a4, float a5, float a6, float a7, float a8, float a9, float a10, float a11, float a12, float a13, float a14)
{
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14); fflush(stdout);
}

void void_fffffffffffffff(float a1, float a2, float a3, float a4, float a5, float a6, float a7, float a8, float a9, float a10, float a11, float a12, float a13, float a14, float a15)
{
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15); fflush(stdout);
}

void void_ffffffffffffffff(float a1, float a2, float a3, float a4, float a5, float a6, float a7, float a8, float a9, float a10, float a11, float a12, float a13, float a14, float a15, float a16)
{
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16); fflush(stdout);
}

void void_fffffffffffffffff(float a1, float a2, float a3, float a4, float a5, float a6, float a7, float a8, float a9, float a10, float a11, float a12, float a13, float a14, float a15, float a16, float a17)
{
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17); fflush(stdout);
}

void void_ffffffffffffffffff(float a1, float a2, float a3, float a4, float a5, float a6, float a7, float a8, float a9, float a10, float a11, float a12, float a13, float a14, float a15, float a16, float a17, float a18)
{
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18); fflush(stdout);
}

void void_fffffffffffffffffff(float a1, float a2, float a3, float a4, float a5, float a6, float a7, float a8, float a9, float a10, float a11, float a12, float a13, float a14, float a15, float a16, float a17, float a18, float a19)
{
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19); fflush(stdout);
}

void void_ffffffffffffffffffff(float a1, float a2, float a3, float a4, float a5, float a6, float a7, float a8, float a9, float a10, float a11, float a12, float a13, float a14, float a15, float a16, float a17, float a18, float a19, float a20)
{
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20); fflush(stdout);
}

void void_fffffffffffffffffffff(float a1, float a2, float a3, float a4, float a5, float a6, float a7, float a8, float a9, float a10, float a11, float a12, float a13, float a14, float a15, float a16, float a17, float a18, float a19, float a20, float a21)
{
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21); fflush(stdout);
}

void void_ffffffffffffffffffffff(float a1, float a2, float a3, float a4, float a5, float a6, float a7, float a8, float a9, float a10, float a11, float a12, float a13, float a14, float a15, float a16, float a17, float a18, float a19, float a20, float a21, float a22)
{
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22); fflush(stdout);
}

void void_fffffffffffffffffffffff(float a1, float a2, float a3, float a4, float a5, float a6, float a7, float a8, float a9, float a10, float a11, float a12, float a13, float a14, float a15, float a16, float a17, float a18, float a19, float a20, float a21, float a22, float a23)
{
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23); fflush(stdout);
}

void void_ffffffffffffffffffffffff(float a1, float a2, float a3, float a4, float a5, float a6, float a7, float a8, float a9, float a10, float a11, float a12, float a13, float a14, float a15, float a16, float a17, float a18, float a19, float a20, float a21, float a22, float a23, float a24)
{
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24); fflush(stdout);
}

void void_d(double a1)
{
   printf("{{ %f }}", a1); fflush(stdout);
}

void void_dd(double a1, double a2)
{
   printf("{{ %f %f }}", a1, a2); fflush(stdout);
}

void void_ddd(double a1, double a2, double a3)
{
   printf("{{ %f %f %f }}", a1, a2, a3); fflush(stdout);
}

void void_dddd(double a1, double a2, double a3, double a4)
{
   printf("{{ %f %f %f %f }}", a1, a2, a3, a4); fflush(stdout);
}

void void_ddddd(double a1, double a2, double a3, double a4, double a5)
{
   printf("{{ %f %f %f %f %f }}", a1, a2, a3, a4, a5); fflush(stdout);
}

void void_dddddd(double a1, double a2, double a3, double a4, double a5, double a6)
{
   printf("{{ %f %f %f %f %f %f }}", a1, a2, a3, a4, a5, a6); fflush(stdout);
}

void void_ddddddd(double a1, double a2, double a3, double a4, double a5, double a6, double a7)
{
   printf("{{ %f %f %f %f %f %f %f }}", a1, a2, a3, a4, a5, a6, a7); fflush(stdout);
}

void void_dddddddd(double a1, double a2, double a3, double a4, double a5, double a6, double a7, double a8)
{
   printf("{{ %f %f %f %f %f %f %f %f }}", a1, a2, a3, a4, a5, a6, a7, a8); fflush(stdout);
}

void void_ddddddddd(double a1, double a2, double a3, double a4, double a5, double a6, double a7, double a8, double a9)
{
   printf("{{ %f %f %f %f %f %f %f %f %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9); fflush(stdout);
}

void void_dddddddddd(double a1, double a2, double a3, double a4, double a5, double a6, double a7, double a8, double a9, double a10)
{
   printf("{{ %f %f %f %f %f %f %f %f %f %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10); fflush(stdout);
}

void void_ddddddddddd(double a1, double a2, double a3, double a4, double a5, double a6, double a7, double a8, double a9, double a10, double a11)
{
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11); fflush(stdout);
}

void void_dddddddddddd(double a1, double a2, double a3, double a4, double a5, double a6, double a7, double a8, double a9, double a10, double a11, double a12)
{
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12); fflush(stdout);
}

void void_ddddddddddddd(double a1, double a2, double a3, double a4, double a5, double a6, double a7, double a8, double a9, double a10, double a11, double a12, double a13)
{
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13); fflush(stdout);
}

void void_dddddddddddddd(double a1, double a2, double a3, double a4, double a5, double a6, double a7, double a8, double a9, double a10, double a11, double a12, double a13, double a14)
{
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14); fflush(stdout);
}

void void_ddddddddddddddd(double a1, double a2, double a3, double a4, double a5, double a6, double a7, double a8, double a9, double a10, double a11, double a12, double a13, double a14, double a15)
{
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15); fflush(stdout);
}

void void_dddddddddddddddd(double a1, double a2, double a3, double a4, double a5, double a6, double a7, double a8, double a9, double a10, double a11, double a12, double a13, double a14, double a15, double a16)
{
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16); fflush(stdout);
}

void void_ddddddddddddddddd(double a1, double a2, double a3, double a4, double a5, double a6, double a7, double a8, double a9, double a10, double a11, double a12, double a13, double a14, double a15, double a16, double a17)
{
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17); fflush(stdout);
}

void void_dddddddddddddddddd(double a1, double a2, double a3, double a4, double a5, double a6, double a7, double a8, double a9, double a10, double a11, double a12, double a13, double a14, double a15, double a16, double a17, double a18)
{
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18); fflush(stdout);
}

void void_ddddddddddddddddddd(double a1, double a2, double a3, double a4, double a5, double a6, double a7, double a8, double a9, double a10, double a11, double a12, double a13, double a14, double a15, double a16, double a17, double a18, double a19)
{
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19); fflush(stdout);
}

void void_dddddddddddddddddddd(double a1, double a2, double a3, double a4, double a5, double a6, double a7, double a8, double a9, double a10, double a11, double a12, double a13, double a14, double a15, double a16, double a17, double a18, double a19, double a20)
{
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20); fflush(stdout);
}

void void_ddddddddddddddddddddd(double a1, double a2, double a3, double a4, double a5, double a6, double a7, double a8, double a9, double a10, double a11, double a12, double a13, double a14, double a15, double a16, double a17, double a18, double a19, double a20, double a21)
{
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21); fflush(stdout);
}

void void_dddddddddddddddddddddd(double a1, double a2, double a3, double a4, double a5, double a6, double a7, double a8, double a9, double a10, double a11, double a12, double a13, double a14, double a15, double a16, double a17, double a18, double a19, double a20, double a21, double a22)
{
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22); fflush(stdout);
}

void void_ddddddddddddddddddddddd(double a1, double a2, double a3, double a4, double a5, double a6, double a7, double a8, double a9, double a10, double a11, double a12, double a13, double a14, double a15, double a16, double a17, double a18, double a19, double a20, double a21, double a22, double a23)
{
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23); fflush(stdout);
}

void void_dddddddddddddddddddddddd(double a1, double a2, double a3, double a4, double a5, double a6, double a7, double a8, double a9, double a10, double a11, double a12, double a13, double a14, double a15, double a16, double a17, double a18, double a19, double a20, double a21, double a22, double a23, double a24)
{
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24); fflush(stdout);
}

// all types together
void v_CSILcsilfd(unsigned char a1, unsigned short a2, unsigned int a3, unsigned long long a4, signed char a5, signed short a6, signed int a7, long long a8, float a9, double a10)
{
   printf("{{ %u %u %u %llu %d %d %d %lld %f %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10); fflush(stdout);
}

// mixed type1(n-1), type2 functions
/*
void v_cc..d[i]..c(N)(typeA a1, typeA a2, .., typeB b, .., type aN)
{
   printf('{{ %u %u .. %u .. %u(N) }}', a1, a2, .., b , .., aN); fflush(stdout);
}*/
