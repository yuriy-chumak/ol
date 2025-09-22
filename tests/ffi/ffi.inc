// function returning numeric types by value (neutral values, type limits)
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
// simple type(n)->type functions
/*
type c_cc..c(n)(type a1, type a2, .., type aN)
{
   type y = a1 + a2 + .. + aN;
   printf('{{ %u %u .. %u(n) => format }}', a1, a2, .., aN, y); fflush(stdout);
   return y;
}*/

unsigned char C_C(unsigned char a1)
{
   unsigned char y = a1;
   printf("{{ %u => %u }}", a1, y); fflush(stdout);
   return y;
}

unsigned char C_CC(unsigned char a1, unsigned char a2)
{
   unsigned char y = a1 + a2;
   printf("{{ %u %u => %u }}", a1, a2, y); fflush(stdout);
   return y;
}

unsigned char C_CCC(unsigned char a1, unsigned char a2, unsigned char a3)
{
   unsigned char y = a1 + a2 + a3;
   printf("{{ %u %u %u => %u }}", a1, a2, a3, y); fflush(stdout);
   return y;
}

unsigned char C_CCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4)
{
   unsigned char y = a1 + a2 + a3 + a4;
   printf("{{ %u %u %u %u => %u }}", a1, a2, a3, a4, y); fflush(stdout);
   return y;
}

unsigned char C_CCCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4, unsigned char a5)
{
   unsigned char y = a1 + a2 + a3 + a4 + a5;
   printf("{{ %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, y); fflush(stdout);
   return y;
}

unsigned char C_CCCCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4, unsigned char a5, unsigned char a6)
{
   unsigned char y = a1 + a2 + a3 + a4 + a5 + a6;
   printf("{{ %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, y); fflush(stdout);
   return y;
}

unsigned char C_CCCCCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4, unsigned char a5, unsigned char a6, unsigned char a7)
{
   unsigned char y = a1 + a2 + a3 + a4 + a5 + a6 + a7;
   printf("{{ %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, y); fflush(stdout);
   return y;
}

unsigned char C_CCCCCCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4, unsigned char a5, unsigned char a6, unsigned char a7, unsigned char a8)
{
   unsigned char y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8;
   printf("{{ %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, y); fflush(stdout);
   return y;
}

unsigned char C_CCCCCCCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4, unsigned char a5, unsigned char a6, unsigned char a7, unsigned char a8, unsigned char a9)
{
   unsigned char y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9;
   printf("{{ %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, y); fflush(stdout);
   return y;
}

unsigned char C_CCCCCCCCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4, unsigned char a5, unsigned char a6, unsigned char a7, unsigned char a8, unsigned char a9, unsigned char a10)
{
   unsigned char y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10;
   printf("{{ %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, y); fflush(stdout);
   return y;
}

unsigned char C_CCCCCCCCCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4, unsigned char a5, unsigned char a6, unsigned char a7, unsigned char a8, unsigned char a9, unsigned char a10, unsigned char a11)
{
   unsigned char y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, y); fflush(stdout);
   return y;
}

unsigned char C_CCCCCCCCCCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4, unsigned char a5, unsigned char a6, unsigned char a7, unsigned char a8, unsigned char a9, unsigned char a10, unsigned char a11, unsigned char a12)
{
   unsigned char y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, y); fflush(stdout);
   return y;
}

unsigned char C_CCCCCCCCCCCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4, unsigned char a5, unsigned char a6, unsigned char a7, unsigned char a8, unsigned char a9, unsigned char a10, unsigned char a11, unsigned char a12, unsigned char a13)
{
   unsigned char y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, y); fflush(stdout);
   return y;
}

unsigned char C_CCCCCCCCCCCCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4, unsigned char a5, unsigned char a6, unsigned char a7, unsigned char a8, unsigned char a9, unsigned char a10, unsigned char a11, unsigned char a12, unsigned char a13, unsigned char a14)
{
   unsigned char y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, y); fflush(stdout);
   return y;
}

unsigned char C_CCCCCCCCCCCCCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4, unsigned char a5, unsigned char a6, unsigned char a7, unsigned char a8, unsigned char a9, unsigned char a10, unsigned char a11, unsigned char a12, unsigned char a13, unsigned char a14, unsigned char a15)
{
   unsigned char y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, y); fflush(stdout);
   return y;
}

unsigned char C_CCCCCCCCCCCCCCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4, unsigned char a5, unsigned char a6, unsigned char a7, unsigned char a8, unsigned char a9, unsigned char a10, unsigned char a11, unsigned char a12, unsigned char a13, unsigned char a14, unsigned char a15, unsigned char a16)
{
   unsigned char y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, y); fflush(stdout);
   return y;
}

unsigned char C_CCCCCCCCCCCCCCCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4, unsigned char a5, unsigned char a6, unsigned char a7, unsigned char a8, unsigned char a9, unsigned char a10, unsigned char a11, unsigned char a12, unsigned char a13, unsigned char a14, unsigned char a15, unsigned char a16, unsigned char a17)
{
   unsigned char y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, y); fflush(stdout);
   return y;
}

unsigned char C_CCCCCCCCCCCCCCCCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4, unsigned char a5, unsigned char a6, unsigned char a7, unsigned char a8, unsigned char a9, unsigned char a10, unsigned char a11, unsigned char a12, unsigned char a13, unsigned char a14, unsigned char a15, unsigned char a16, unsigned char a17, unsigned char a18)
{
   unsigned char y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, y); fflush(stdout);
   return y;
}

unsigned char C_CCCCCCCCCCCCCCCCCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4, unsigned char a5, unsigned char a6, unsigned char a7, unsigned char a8, unsigned char a9, unsigned char a10, unsigned char a11, unsigned char a12, unsigned char a13, unsigned char a14, unsigned char a15, unsigned char a16, unsigned char a17, unsigned char a18, unsigned char a19)
{
   unsigned char y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, y); fflush(stdout);
   return y;
}

unsigned char C_CCCCCCCCCCCCCCCCCCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4, unsigned char a5, unsigned char a6, unsigned char a7, unsigned char a8, unsigned char a9, unsigned char a10, unsigned char a11, unsigned char a12, unsigned char a13, unsigned char a14, unsigned char a15, unsigned char a16, unsigned char a17, unsigned char a18, unsigned char a19, unsigned char a20)
{
   unsigned char y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, y); fflush(stdout);
   return y;
}

unsigned char C_CCCCCCCCCCCCCCCCCCCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4, unsigned char a5, unsigned char a6, unsigned char a7, unsigned char a8, unsigned char a9, unsigned char a10, unsigned char a11, unsigned char a12, unsigned char a13, unsigned char a14, unsigned char a15, unsigned char a16, unsigned char a17, unsigned char a18, unsigned char a19, unsigned char a20, unsigned char a21)
{
   unsigned char y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, y); fflush(stdout);
   return y;
}

unsigned char C_CCCCCCCCCCCCCCCCCCCCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4, unsigned char a5, unsigned char a6, unsigned char a7, unsigned char a8, unsigned char a9, unsigned char a10, unsigned char a11, unsigned char a12, unsigned char a13, unsigned char a14, unsigned char a15, unsigned char a16, unsigned char a17, unsigned char a18, unsigned char a19, unsigned char a20, unsigned char a21, unsigned char a22)
{
   unsigned char y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, y); fflush(stdout);
   return y;
}

unsigned char C_CCCCCCCCCCCCCCCCCCCCCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4, unsigned char a5, unsigned char a6, unsigned char a7, unsigned char a8, unsigned char a9, unsigned char a10, unsigned char a11, unsigned char a12, unsigned char a13, unsigned char a14, unsigned char a15, unsigned char a16, unsigned char a17, unsigned char a18, unsigned char a19, unsigned char a20, unsigned char a21, unsigned char a22, unsigned char a23)
{
   unsigned char y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22 + a23;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, y); fflush(stdout);
   return y;
}

unsigned char C_CCCCCCCCCCCCCCCCCCCCCCCC(unsigned char a1, unsigned char a2, unsigned char a3, unsigned char a4, unsigned char a5, unsigned char a6, unsigned char a7, unsigned char a8, unsigned char a9, unsigned char a10, unsigned char a11, unsigned char a12, unsigned char a13, unsigned char a14, unsigned char a15, unsigned char a16, unsigned char a17, unsigned char a18, unsigned char a19, unsigned char a20, unsigned char a21, unsigned char a22, unsigned char a23, unsigned char a24)
{
   unsigned char y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22 + a23 + a24;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, y); fflush(stdout);
   return y;
}

unsigned short S_S(unsigned short a1)
{
   unsigned short y = a1;
   printf("{{ %u => %u }}", a1, y); fflush(stdout);
   return y;
}

unsigned short S_SS(unsigned short a1, unsigned short a2)
{
   unsigned short y = a1 + a2;
   printf("{{ %u %u => %u }}", a1, a2, y); fflush(stdout);
   return y;
}

unsigned short S_SSS(unsigned short a1, unsigned short a2, unsigned short a3)
{
   unsigned short y = a1 + a2 + a3;
   printf("{{ %u %u %u => %u }}", a1, a2, a3, y); fflush(stdout);
   return y;
}

unsigned short S_SSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4)
{
   unsigned short y = a1 + a2 + a3 + a4;
   printf("{{ %u %u %u %u => %u }}", a1, a2, a3, a4, y); fflush(stdout);
   return y;
}

unsigned short S_SSSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4, unsigned short a5)
{
   unsigned short y = a1 + a2 + a3 + a4 + a5;
   printf("{{ %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, y); fflush(stdout);
   return y;
}

unsigned short S_SSSSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4, unsigned short a5, unsigned short a6)
{
   unsigned short y = a1 + a2 + a3 + a4 + a5 + a6;
   printf("{{ %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, y); fflush(stdout);
   return y;
}

unsigned short S_SSSSSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4, unsigned short a5, unsigned short a6, unsigned short a7)
{
   unsigned short y = a1 + a2 + a3 + a4 + a5 + a6 + a7;
   printf("{{ %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, y); fflush(stdout);
   return y;
}

unsigned short S_SSSSSSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4, unsigned short a5, unsigned short a6, unsigned short a7, unsigned short a8)
{
   unsigned short y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8;
   printf("{{ %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, y); fflush(stdout);
   return y;
}

unsigned short S_SSSSSSSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4, unsigned short a5, unsigned short a6, unsigned short a7, unsigned short a8, unsigned short a9)
{
   unsigned short y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9;
   printf("{{ %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, y); fflush(stdout);
   return y;
}

unsigned short S_SSSSSSSSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4, unsigned short a5, unsigned short a6, unsigned short a7, unsigned short a8, unsigned short a9, unsigned short a10)
{
   unsigned short y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10;
   printf("{{ %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, y); fflush(stdout);
   return y;
}

unsigned short S_SSSSSSSSSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4, unsigned short a5, unsigned short a6, unsigned short a7, unsigned short a8, unsigned short a9, unsigned short a10, unsigned short a11)
{
   unsigned short y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, y); fflush(stdout);
   return y;
}

unsigned short S_SSSSSSSSSSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4, unsigned short a5, unsigned short a6, unsigned short a7, unsigned short a8, unsigned short a9, unsigned short a10, unsigned short a11, unsigned short a12)
{
   unsigned short y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, y); fflush(stdout);
   return y;
}

unsigned short S_SSSSSSSSSSSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4, unsigned short a5, unsigned short a6, unsigned short a7, unsigned short a8, unsigned short a9, unsigned short a10, unsigned short a11, unsigned short a12, unsigned short a13)
{
   unsigned short y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, y); fflush(stdout);
   return y;
}

unsigned short S_SSSSSSSSSSSSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4, unsigned short a5, unsigned short a6, unsigned short a7, unsigned short a8, unsigned short a9, unsigned short a10, unsigned short a11, unsigned short a12, unsigned short a13, unsigned short a14)
{
   unsigned short y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, y); fflush(stdout);
   return y;
}

unsigned short S_SSSSSSSSSSSSSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4, unsigned short a5, unsigned short a6, unsigned short a7, unsigned short a8, unsigned short a9, unsigned short a10, unsigned short a11, unsigned short a12, unsigned short a13, unsigned short a14, unsigned short a15)
{
   unsigned short y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, y); fflush(stdout);
   return y;
}

unsigned short S_SSSSSSSSSSSSSSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4, unsigned short a5, unsigned short a6, unsigned short a7, unsigned short a8, unsigned short a9, unsigned short a10, unsigned short a11, unsigned short a12, unsigned short a13, unsigned short a14, unsigned short a15, unsigned short a16)
{
   unsigned short y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, y); fflush(stdout);
   return y;
}

unsigned short S_SSSSSSSSSSSSSSSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4, unsigned short a5, unsigned short a6, unsigned short a7, unsigned short a8, unsigned short a9, unsigned short a10, unsigned short a11, unsigned short a12, unsigned short a13, unsigned short a14, unsigned short a15, unsigned short a16, unsigned short a17)
{
   unsigned short y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, y); fflush(stdout);
   return y;
}

unsigned short S_SSSSSSSSSSSSSSSSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4, unsigned short a5, unsigned short a6, unsigned short a7, unsigned short a8, unsigned short a9, unsigned short a10, unsigned short a11, unsigned short a12, unsigned short a13, unsigned short a14, unsigned short a15, unsigned short a16, unsigned short a17, unsigned short a18)
{
   unsigned short y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, y); fflush(stdout);
   return y;
}

unsigned short S_SSSSSSSSSSSSSSSSSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4, unsigned short a5, unsigned short a6, unsigned short a7, unsigned short a8, unsigned short a9, unsigned short a10, unsigned short a11, unsigned short a12, unsigned short a13, unsigned short a14, unsigned short a15, unsigned short a16, unsigned short a17, unsigned short a18, unsigned short a19)
{
   unsigned short y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, y); fflush(stdout);
   return y;
}

unsigned short S_SSSSSSSSSSSSSSSSSSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4, unsigned short a5, unsigned short a6, unsigned short a7, unsigned short a8, unsigned short a9, unsigned short a10, unsigned short a11, unsigned short a12, unsigned short a13, unsigned short a14, unsigned short a15, unsigned short a16, unsigned short a17, unsigned short a18, unsigned short a19, unsigned short a20)
{
   unsigned short y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, y); fflush(stdout);
   return y;
}

unsigned short S_SSSSSSSSSSSSSSSSSSSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4, unsigned short a5, unsigned short a6, unsigned short a7, unsigned short a8, unsigned short a9, unsigned short a10, unsigned short a11, unsigned short a12, unsigned short a13, unsigned short a14, unsigned short a15, unsigned short a16, unsigned short a17, unsigned short a18, unsigned short a19, unsigned short a20, unsigned short a21)
{
   unsigned short y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, y); fflush(stdout);
   return y;
}

unsigned short S_SSSSSSSSSSSSSSSSSSSSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4, unsigned short a5, unsigned short a6, unsigned short a7, unsigned short a8, unsigned short a9, unsigned short a10, unsigned short a11, unsigned short a12, unsigned short a13, unsigned short a14, unsigned short a15, unsigned short a16, unsigned short a17, unsigned short a18, unsigned short a19, unsigned short a20, unsigned short a21, unsigned short a22)
{
   unsigned short y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, y); fflush(stdout);
   return y;
}

unsigned short S_SSSSSSSSSSSSSSSSSSSSSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4, unsigned short a5, unsigned short a6, unsigned short a7, unsigned short a8, unsigned short a9, unsigned short a10, unsigned short a11, unsigned short a12, unsigned short a13, unsigned short a14, unsigned short a15, unsigned short a16, unsigned short a17, unsigned short a18, unsigned short a19, unsigned short a20, unsigned short a21, unsigned short a22, unsigned short a23)
{
   unsigned short y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22 + a23;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, y); fflush(stdout);
   return y;
}

unsigned short S_SSSSSSSSSSSSSSSSSSSSSSSS(unsigned short a1, unsigned short a2, unsigned short a3, unsigned short a4, unsigned short a5, unsigned short a6, unsigned short a7, unsigned short a8, unsigned short a9, unsigned short a10, unsigned short a11, unsigned short a12, unsigned short a13, unsigned short a14, unsigned short a15, unsigned short a16, unsigned short a17, unsigned short a18, unsigned short a19, unsigned short a20, unsigned short a21, unsigned short a22, unsigned short a23, unsigned short a24)
{
   unsigned short y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22 + a23 + a24;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, y); fflush(stdout);
   return y;
}

unsigned int I_I(unsigned int a1)
{
   unsigned int y = a1;
   printf("{{ %u => %u }}", a1, y); fflush(stdout);
   return y;
}

unsigned int I_II(unsigned int a1, unsigned int a2)
{
   unsigned int y = a1 + a2;
   printf("{{ %u %u => %u }}", a1, a2, y); fflush(stdout);
   return y;
}

unsigned int I_III(unsigned int a1, unsigned int a2, unsigned int a3)
{
   unsigned int y = a1 + a2 + a3;
   printf("{{ %u %u %u => %u }}", a1, a2, a3, y); fflush(stdout);
   return y;
}

unsigned int I_IIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4)
{
   unsigned int y = a1 + a2 + a3 + a4;
   printf("{{ %u %u %u %u => %u }}", a1, a2, a3, a4, y); fflush(stdout);
   return y;
}

unsigned int I_IIIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4, unsigned int a5)
{
   unsigned int y = a1 + a2 + a3 + a4 + a5;
   printf("{{ %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, y); fflush(stdout);
   return y;
}

unsigned int I_IIIIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6)
{
   unsigned int y = a1 + a2 + a3 + a4 + a5 + a6;
   printf("{{ %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, y); fflush(stdout);
   return y;
}

unsigned int I_IIIIIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6, unsigned int a7)
{
   unsigned int y = a1 + a2 + a3 + a4 + a5 + a6 + a7;
   printf("{{ %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, y); fflush(stdout);
   return y;
}

unsigned int I_IIIIIIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6, unsigned int a7, unsigned int a8)
{
   unsigned int y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8;
   printf("{{ %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, y); fflush(stdout);
   return y;
}

unsigned int I_IIIIIIIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6, unsigned int a7, unsigned int a8, unsigned int a9)
{
   unsigned int y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9;
   printf("{{ %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, y); fflush(stdout);
   return y;
}

unsigned int I_IIIIIIIIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6, unsigned int a7, unsigned int a8, unsigned int a9, unsigned int a10)
{
   unsigned int y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10;
   printf("{{ %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, y); fflush(stdout);
   return y;
}

unsigned int I_IIIIIIIIIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6, unsigned int a7, unsigned int a8, unsigned int a9, unsigned int a10, unsigned int a11)
{
   unsigned int y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, y); fflush(stdout);
   return y;
}

unsigned int I_IIIIIIIIIIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6, unsigned int a7, unsigned int a8, unsigned int a9, unsigned int a10, unsigned int a11, unsigned int a12)
{
   unsigned int y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, y); fflush(stdout);
   return y;
}

unsigned int I_IIIIIIIIIIIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6, unsigned int a7, unsigned int a8, unsigned int a9, unsigned int a10, unsigned int a11, unsigned int a12, unsigned int a13)
{
   unsigned int y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, y); fflush(stdout);
   return y;
}

unsigned int I_IIIIIIIIIIIIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6, unsigned int a7, unsigned int a8, unsigned int a9, unsigned int a10, unsigned int a11, unsigned int a12, unsigned int a13, unsigned int a14)
{
   unsigned int y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, y); fflush(stdout);
   return y;
}

unsigned int I_IIIIIIIIIIIIIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6, unsigned int a7, unsigned int a8, unsigned int a9, unsigned int a10, unsigned int a11, unsigned int a12, unsigned int a13, unsigned int a14, unsigned int a15)
{
   unsigned int y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, y); fflush(stdout);
   return y;
}

unsigned int I_IIIIIIIIIIIIIIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6, unsigned int a7, unsigned int a8, unsigned int a9, unsigned int a10, unsigned int a11, unsigned int a12, unsigned int a13, unsigned int a14, unsigned int a15, unsigned int a16)
{
   unsigned int y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, y); fflush(stdout);
   return y;
}

unsigned int I_IIIIIIIIIIIIIIIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6, unsigned int a7, unsigned int a8, unsigned int a9, unsigned int a10, unsigned int a11, unsigned int a12, unsigned int a13, unsigned int a14, unsigned int a15, unsigned int a16, unsigned int a17)
{
   unsigned int y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, y); fflush(stdout);
   return y;
}

unsigned int I_IIIIIIIIIIIIIIIIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6, unsigned int a7, unsigned int a8, unsigned int a9, unsigned int a10, unsigned int a11, unsigned int a12, unsigned int a13, unsigned int a14, unsigned int a15, unsigned int a16, unsigned int a17, unsigned int a18)
{
   unsigned int y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, y); fflush(stdout);
   return y;
}

unsigned int I_IIIIIIIIIIIIIIIIIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6, unsigned int a7, unsigned int a8, unsigned int a9, unsigned int a10, unsigned int a11, unsigned int a12, unsigned int a13, unsigned int a14, unsigned int a15, unsigned int a16, unsigned int a17, unsigned int a18, unsigned int a19)
{
   unsigned int y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, y); fflush(stdout);
   return y;
}

unsigned int I_IIIIIIIIIIIIIIIIIIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6, unsigned int a7, unsigned int a8, unsigned int a9, unsigned int a10, unsigned int a11, unsigned int a12, unsigned int a13, unsigned int a14, unsigned int a15, unsigned int a16, unsigned int a17, unsigned int a18, unsigned int a19, unsigned int a20)
{
   unsigned int y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, y); fflush(stdout);
   return y;
}

unsigned int I_IIIIIIIIIIIIIIIIIIIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6, unsigned int a7, unsigned int a8, unsigned int a9, unsigned int a10, unsigned int a11, unsigned int a12, unsigned int a13, unsigned int a14, unsigned int a15, unsigned int a16, unsigned int a17, unsigned int a18, unsigned int a19, unsigned int a20, unsigned int a21)
{
   unsigned int y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, y); fflush(stdout);
   return y;
}

unsigned int I_IIIIIIIIIIIIIIIIIIIIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6, unsigned int a7, unsigned int a8, unsigned int a9, unsigned int a10, unsigned int a11, unsigned int a12, unsigned int a13, unsigned int a14, unsigned int a15, unsigned int a16, unsigned int a17, unsigned int a18, unsigned int a19, unsigned int a20, unsigned int a21, unsigned int a22)
{
   unsigned int y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, y); fflush(stdout);
   return y;
}

unsigned int I_IIIIIIIIIIIIIIIIIIIIIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6, unsigned int a7, unsigned int a8, unsigned int a9, unsigned int a10, unsigned int a11, unsigned int a12, unsigned int a13, unsigned int a14, unsigned int a15, unsigned int a16, unsigned int a17, unsigned int a18, unsigned int a19, unsigned int a20, unsigned int a21, unsigned int a22, unsigned int a23)
{
   unsigned int y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22 + a23;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, y); fflush(stdout);
   return y;
}

unsigned int I_IIIIIIIIIIIIIIIIIIIIIIII(unsigned int a1, unsigned int a2, unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6, unsigned int a7, unsigned int a8, unsigned int a9, unsigned int a10, unsigned int a11, unsigned int a12, unsigned int a13, unsigned int a14, unsigned int a15, unsigned int a16, unsigned int a17, unsigned int a18, unsigned int a19, unsigned int a20, unsigned int a21, unsigned int a22, unsigned int a23, unsigned int a24)
{
   unsigned int y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22 + a23 + a24;
   printf("{{ %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u => %u }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, y); fflush(stdout);
   return y;
}

signed char c_c(signed char a1)
{
   signed char y = a1;
   printf("{{ %d => %d }}", a1, y); fflush(stdout);
   return y;
}

signed char c_cc(signed char a1, signed char a2)
{
   signed char y = a1 + a2;
   printf("{{ %d %d => %d }}", a1, a2, y); fflush(stdout);
   return y;
}

signed char c_ccc(signed char a1, signed char a2, signed char a3)
{
   signed char y = a1 + a2 + a3;
   printf("{{ %d %d %d => %d }}", a1, a2, a3, y); fflush(stdout);
   return y;
}

signed char c_cccc(signed char a1, signed char a2, signed char a3, signed char a4)
{
   signed char y = a1 + a2 + a3 + a4;
   printf("{{ %d %d %d %d => %d }}", a1, a2, a3, a4, y); fflush(stdout);
   return y;
}

signed char c_ccccc(signed char a1, signed char a2, signed char a3, signed char a4, signed char a5)
{
   signed char y = a1 + a2 + a3 + a4 + a5;
   printf("{{ %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, y); fflush(stdout);
   return y;
}

signed char c_cccccc(signed char a1, signed char a2, signed char a3, signed char a4, signed char a5, signed char a6)
{
   signed char y = a1 + a2 + a3 + a4 + a5 + a6;
   printf("{{ %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, y); fflush(stdout);
   return y;
}

signed char c_ccccccc(signed char a1, signed char a2, signed char a3, signed char a4, signed char a5, signed char a6, signed char a7)
{
   signed char y = a1 + a2 + a3 + a4 + a5 + a6 + a7;
   printf("{{ %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, y); fflush(stdout);
   return y;
}

signed char c_cccccccc(signed char a1, signed char a2, signed char a3, signed char a4, signed char a5, signed char a6, signed char a7, signed char a8)
{
   signed char y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8;
   printf("{{ %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, y); fflush(stdout);
   return y;
}

signed char c_ccccccccc(signed char a1, signed char a2, signed char a3, signed char a4, signed char a5, signed char a6, signed char a7, signed char a8, signed char a9)
{
   signed char y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9;
   printf("{{ %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, y); fflush(stdout);
   return y;
}

signed char c_cccccccccc(signed char a1, signed char a2, signed char a3, signed char a4, signed char a5, signed char a6, signed char a7, signed char a8, signed char a9, signed char a10)
{
   signed char y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10;
   printf("{{ %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, y); fflush(stdout);
   return y;
}

signed char c_ccccccccccc(signed char a1, signed char a2, signed char a3, signed char a4, signed char a5, signed char a6, signed char a7, signed char a8, signed char a9, signed char a10, signed char a11)
{
   signed char y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, y); fflush(stdout);
   return y;
}

signed char c_cccccccccccc(signed char a1, signed char a2, signed char a3, signed char a4, signed char a5, signed char a6, signed char a7, signed char a8, signed char a9, signed char a10, signed char a11, signed char a12)
{
   signed char y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, y); fflush(stdout);
   return y;
}

signed char c_ccccccccccccc(signed char a1, signed char a2, signed char a3, signed char a4, signed char a5, signed char a6, signed char a7, signed char a8, signed char a9, signed char a10, signed char a11, signed char a12, signed char a13)
{
   signed char y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, y); fflush(stdout);
   return y;
}

signed char c_cccccccccccccc(signed char a1, signed char a2, signed char a3, signed char a4, signed char a5, signed char a6, signed char a7, signed char a8, signed char a9, signed char a10, signed char a11, signed char a12, signed char a13, signed char a14)
{
   signed char y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, y); fflush(stdout);
   return y;
}

signed char c_ccccccccccccccc(signed char a1, signed char a2, signed char a3, signed char a4, signed char a5, signed char a6, signed char a7, signed char a8, signed char a9, signed char a10, signed char a11, signed char a12, signed char a13, signed char a14, signed char a15)
{
   signed char y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, y); fflush(stdout);
   return y;
}

signed char c_cccccccccccccccc(signed char a1, signed char a2, signed char a3, signed char a4, signed char a5, signed char a6, signed char a7, signed char a8, signed char a9, signed char a10, signed char a11, signed char a12, signed char a13, signed char a14, signed char a15, signed char a16)
{
   signed char y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, y); fflush(stdout);
   return y;
}

signed char c_ccccccccccccccccc(signed char a1, signed char a2, signed char a3, signed char a4, signed char a5, signed char a6, signed char a7, signed char a8, signed char a9, signed char a10, signed char a11, signed char a12, signed char a13, signed char a14, signed char a15, signed char a16, signed char a17)
{
   signed char y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, y); fflush(stdout);
   return y;
}

signed char c_cccccccccccccccccc(signed char a1, signed char a2, signed char a3, signed char a4, signed char a5, signed char a6, signed char a7, signed char a8, signed char a9, signed char a10, signed char a11, signed char a12, signed char a13, signed char a14, signed char a15, signed char a16, signed char a17, signed char a18)
{
   signed char y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, y); fflush(stdout);
   return y;
}

signed char c_ccccccccccccccccccc(signed char a1, signed char a2, signed char a3, signed char a4, signed char a5, signed char a6, signed char a7, signed char a8, signed char a9, signed char a10, signed char a11, signed char a12, signed char a13, signed char a14, signed char a15, signed char a16, signed char a17, signed char a18, signed char a19)
{
   signed char y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, y); fflush(stdout);
   return y;
}

signed char c_cccccccccccccccccccc(signed char a1, signed char a2, signed char a3, signed char a4, signed char a5, signed char a6, signed char a7, signed char a8, signed char a9, signed char a10, signed char a11, signed char a12, signed char a13, signed char a14, signed char a15, signed char a16, signed char a17, signed char a18, signed char a19, signed char a20)
{
   signed char y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, y); fflush(stdout);
   return y;
}

signed char c_ccccccccccccccccccccc(signed char a1, signed char a2, signed char a3, signed char a4, signed char a5, signed char a6, signed char a7, signed char a8, signed char a9, signed char a10, signed char a11, signed char a12, signed char a13, signed char a14, signed char a15, signed char a16, signed char a17, signed char a18, signed char a19, signed char a20, signed char a21)
{
   signed char y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, y); fflush(stdout);
   return y;
}

signed char c_cccccccccccccccccccccc(signed char a1, signed char a2, signed char a3, signed char a4, signed char a5, signed char a6, signed char a7, signed char a8, signed char a9, signed char a10, signed char a11, signed char a12, signed char a13, signed char a14, signed char a15, signed char a16, signed char a17, signed char a18, signed char a19, signed char a20, signed char a21, signed char a22)
{
   signed char y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, y); fflush(stdout);
   return y;
}

signed char c_ccccccccccccccccccccccc(signed char a1, signed char a2, signed char a3, signed char a4, signed char a5, signed char a6, signed char a7, signed char a8, signed char a9, signed char a10, signed char a11, signed char a12, signed char a13, signed char a14, signed char a15, signed char a16, signed char a17, signed char a18, signed char a19, signed char a20, signed char a21, signed char a22, signed char a23)
{
   signed char y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22 + a23;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, y); fflush(stdout);
   return y;
}

signed char c_cccccccccccccccccccccccc(signed char a1, signed char a2, signed char a3, signed char a4, signed char a5, signed char a6, signed char a7, signed char a8, signed char a9, signed char a10, signed char a11, signed char a12, signed char a13, signed char a14, signed char a15, signed char a16, signed char a17, signed char a18, signed char a19, signed char a20, signed char a21, signed char a22, signed char a23, signed char a24)
{
   signed char y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22 + a23 + a24;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, y); fflush(stdout);
   return y;
}

signed short s_s(signed short a1)
{
   signed short y = a1;
   printf("{{ %d => %d }}", a1, y); fflush(stdout);
   return y;
}

signed short s_ss(signed short a1, signed short a2)
{
   signed short y = a1 + a2;
   printf("{{ %d %d => %d }}", a1, a2, y); fflush(stdout);
   return y;
}

signed short s_sss(signed short a1, signed short a2, signed short a3)
{
   signed short y = a1 + a2 + a3;
   printf("{{ %d %d %d => %d }}", a1, a2, a3, y); fflush(stdout);
   return y;
}

signed short s_ssss(signed short a1, signed short a2, signed short a3, signed short a4)
{
   signed short y = a1 + a2 + a3 + a4;
   printf("{{ %d %d %d %d => %d }}", a1, a2, a3, a4, y); fflush(stdout);
   return y;
}

signed short s_sssss(signed short a1, signed short a2, signed short a3, signed short a4, signed short a5)
{
   signed short y = a1 + a2 + a3 + a4 + a5;
   printf("{{ %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, y); fflush(stdout);
   return y;
}

signed short s_ssssss(signed short a1, signed short a2, signed short a3, signed short a4, signed short a5, signed short a6)
{
   signed short y = a1 + a2 + a3 + a4 + a5 + a6;
   printf("{{ %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, y); fflush(stdout);
   return y;
}

signed short s_sssssss(signed short a1, signed short a2, signed short a3, signed short a4, signed short a5, signed short a6, signed short a7)
{
   signed short y = a1 + a2 + a3 + a4 + a5 + a6 + a7;
   printf("{{ %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, y); fflush(stdout);
   return y;
}

signed short s_ssssssss(signed short a1, signed short a2, signed short a3, signed short a4, signed short a5, signed short a6, signed short a7, signed short a8)
{
   signed short y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8;
   printf("{{ %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, y); fflush(stdout);
   return y;
}

signed short s_sssssssss(signed short a1, signed short a2, signed short a3, signed short a4, signed short a5, signed short a6, signed short a7, signed short a8, signed short a9)
{
   signed short y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9;
   printf("{{ %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, y); fflush(stdout);
   return y;
}

signed short s_ssssssssss(signed short a1, signed short a2, signed short a3, signed short a4, signed short a5, signed short a6, signed short a7, signed short a8, signed short a9, signed short a10)
{
   signed short y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10;
   printf("{{ %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, y); fflush(stdout);
   return y;
}

signed short s_sssssssssss(signed short a1, signed short a2, signed short a3, signed short a4, signed short a5, signed short a6, signed short a7, signed short a8, signed short a9, signed short a10, signed short a11)
{
   signed short y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, y); fflush(stdout);
   return y;
}

signed short s_ssssssssssss(signed short a1, signed short a2, signed short a3, signed short a4, signed short a5, signed short a6, signed short a7, signed short a8, signed short a9, signed short a10, signed short a11, signed short a12)
{
   signed short y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, y); fflush(stdout);
   return y;
}

signed short s_sssssssssssss(signed short a1, signed short a2, signed short a3, signed short a4, signed short a5, signed short a6, signed short a7, signed short a8, signed short a9, signed short a10, signed short a11, signed short a12, signed short a13)
{
   signed short y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, y); fflush(stdout);
   return y;
}

signed short s_ssssssssssssss(signed short a1, signed short a2, signed short a3, signed short a4, signed short a5, signed short a6, signed short a7, signed short a8, signed short a9, signed short a10, signed short a11, signed short a12, signed short a13, signed short a14)
{
   signed short y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, y); fflush(stdout);
   return y;
}

signed short s_sssssssssssssss(signed short a1, signed short a2, signed short a3, signed short a4, signed short a5, signed short a6, signed short a7, signed short a8, signed short a9, signed short a10, signed short a11, signed short a12, signed short a13, signed short a14, signed short a15)
{
   signed short y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, y); fflush(stdout);
   return y;
}

signed short s_ssssssssssssssss(signed short a1, signed short a2, signed short a3, signed short a4, signed short a5, signed short a6, signed short a7, signed short a8, signed short a9, signed short a10, signed short a11, signed short a12, signed short a13, signed short a14, signed short a15, signed short a16)
{
   signed short y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, y); fflush(stdout);
   return y;
}

signed short s_sssssssssssssssss(signed short a1, signed short a2, signed short a3, signed short a4, signed short a5, signed short a6, signed short a7, signed short a8, signed short a9, signed short a10, signed short a11, signed short a12, signed short a13, signed short a14, signed short a15, signed short a16, signed short a17)
{
   signed short y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, y); fflush(stdout);
   return y;
}

signed short s_ssssssssssssssssss(signed short a1, signed short a2, signed short a3, signed short a4, signed short a5, signed short a6, signed short a7, signed short a8, signed short a9, signed short a10, signed short a11, signed short a12, signed short a13, signed short a14, signed short a15, signed short a16, signed short a17, signed short a18)
{
   signed short y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, y); fflush(stdout);
   return y;
}

signed short s_sssssssssssssssssss(signed short a1, signed short a2, signed short a3, signed short a4, signed short a5, signed short a6, signed short a7, signed short a8, signed short a9, signed short a10, signed short a11, signed short a12, signed short a13, signed short a14, signed short a15, signed short a16, signed short a17, signed short a18, signed short a19)
{
   signed short y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, y); fflush(stdout);
   return y;
}

signed short s_ssssssssssssssssssss(signed short a1, signed short a2, signed short a3, signed short a4, signed short a5, signed short a6, signed short a7, signed short a8, signed short a9, signed short a10, signed short a11, signed short a12, signed short a13, signed short a14, signed short a15, signed short a16, signed short a17, signed short a18, signed short a19, signed short a20)
{
   signed short y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, y); fflush(stdout);
   return y;
}

signed short s_sssssssssssssssssssss(signed short a1, signed short a2, signed short a3, signed short a4, signed short a5, signed short a6, signed short a7, signed short a8, signed short a9, signed short a10, signed short a11, signed short a12, signed short a13, signed short a14, signed short a15, signed short a16, signed short a17, signed short a18, signed short a19, signed short a20, signed short a21)
{
   signed short y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, y); fflush(stdout);
   return y;
}

signed short s_ssssssssssssssssssssss(signed short a1, signed short a2, signed short a3, signed short a4, signed short a5, signed short a6, signed short a7, signed short a8, signed short a9, signed short a10, signed short a11, signed short a12, signed short a13, signed short a14, signed short a15, signed short a16, signed short a17, signed short a18, signed short a19, signed short a20, signed short a21, signed short a22)
{
   signed short y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, y); fflush(stdout);
   return y;
}

signed short s_sssssssssssssssssssssss(signed short a1, signed short a2, signed short a3, signed short a4, signed short a5, signed short a6, signed short a7, signed short a8, signed short a9, signed short a10, signed short a11, signed short a12, signed short a13, signed short a14, signed short a15, signed short a16, signed short a17, signed short a18, signed short a19, signed short a20, signed short a21, signed short a22, signed short a23)
{
   signed short y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22 + a23;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, y); fflush(stdout);
   return y;
}

signed short s_ssssssssssssssssssssssss(signed short a1, signed short a2, signed short a3, signed short a4, signed short a5, signed short a6, signed short a7, signed short a8, signed short a9, signed short a10, signed short a11, signed short a12, signed short a13, signed short a14, signed short a15, signed short a16, signed short a17, signed short a18, signed short a19, signed short a20, signed short a21, signed short a22, signed short a23, signed short a24)
{
   signed short y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22 + a23 + a24;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, y); fflush(stdout);
   return y;
}

signed int i_i(signed int a1)
{
   signed int y = a1;
   printf("{{ %d => %d }}", a1, y); fflush(stdout);
   return y;
}

signed int i_ii(signed int a1, signed int a2)
{
   signed int y = a1 + a2;
   printf("{{ %d %d => %d }}", a1, a2, y); fflush(stdout);
   return y;
}

signed int i_iii(signed int a1, signed int a2, signed int a3)
{
   signed int y = a1 + a2 + a3;
   printf("{{ %d %d %d => %d }}", a1, a2, a3, y); fflush(stdout);
   return y;
}

signed int i_iiii(signed int a1, signed int a2, signed int a3, signed int a4)
{
   signed int y = a1 + a2 + a3 + a4;
   printf("{{ %d %d %d %d => %d }}", a1, a2, a3, a4, y); fflush(stdout);
   return y;
}

signed int i_iiiii(signed int a1, signed int a2, signed int a3, signed int a4, signed int a5)
{
   signed int y = a1 + a2 + a3 + a4 + a5;
   printf("{{ %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, y); fflush(stdout);
   return y;
}

signed int i_iiiiii(signed int a1, signed int a2, signed int a3, signed int a4, signed int a5, signed int a6)
{
   signed int y = a1 + a2 + a3 + a4 + a5 + a6;
   printf("{{ %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, y); fflush(stdout);
   return y;
}

signed int i_iiiiiii(signed int a1, signed int a2, signed int a3, signed int a4, signed int a5, signed int a6, signed int a7)
{
   signed int y = a1 + a2 + a3 + a4 + a5 + a6 + a7;
   printf("{{ %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, y); fflush(stdout);
   return y;
}

signed int i_iiiiiiii(signed int a1, signed int a2, signed int a3, signed int a4, signed int a5, signed int a6, signed int a7, signed int a8)
{
   signed int y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8;
   printf("{{ %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, y); fflush(stdout);
   return y;
}

signed int i_iiiiiiiii(signed int a1, signed int a2, signed int a3, signed int a4, signed int a5, signed int a6, signed int a7, signed int a8, signed int a9)
{
   signed int y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9;
   printf("{{ %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, y); fflush(stdout);
   return y;
}

signed int i_iiiiiiiiii(signed int a1, signed int a2, signed int a3, signed int a4, signed int a5, signed int a6, signed int a7, signed int a8, signed int a9, signed int a10)
{
   signed int y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10;
   printf("{{ %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, y); fflush(stdout);
   return y;
}

signed int i_iiiiiiiiiii(signed int a1, signed int a2, signed int a3, signed int a4, signed int a5, signed int a6, signed int a7, signed int a8, signed int a9, signed int a10, signed int a11)
{
   signed int y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, y); fflush(stdout);
   return y;
}

signed int i_iiiiiiiiiiii(signed int a1, signed int a2, signed int a3, signed int a4, signed int a5, signed int a6, signed int a7, signed int a8, signed int a9, signed int a10, signed int a11, signed int a12)
{
   signed int y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, y); fflush(stdout);
   return y;
}

signed int i_iiiiiiiiiiiii(signed int a1, signed int a2, signed int a3, signed int a4, signed int a5, signed int a6, signed int a7, signed int a8, signed int a9, signed int a10, signed int a11, signed int a12, signed int a13)
{
   signed int y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, y); fflush(stdout);
   return y;
}

signed int i_iiiiiiiiiiiiii(signed int a1, signed int a2, signed int a3, signed int a4, signed int a5, signed int a6, signed int a7, signed int a8, signed int a9, signed int a10, signed int a11, signed int a12, signed int a13, signed int a14)
{
   signed int y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, y); fflush(stdout);
   return y;
}

signed int i_iiiiiiiiiiiiiii(signed int a1, signed int a2, signed int a3, signed int a4, signed int a5, signed int a6, signed int a7, signed int a8, signed int a9, signed int a10, signed int a11, signed int a12, signed int a13, signed int a14, signed int a15)
{
   signed int y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, y); fflush(stdout);
   return y;
}

signed int i_iiiiiiiiiiiiiiii(signed int a1, signed int a2, signed int a3, signed int a4, signed int a5, signed int a6, signed int a7, signed int a8, signed int a9, signed int a10, signed int a11, signed int a12, signed int a13, signed int a14, signed int a15, signed int a16)
{
   signed int y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, y); fflush(stdout);
   return y;
}

signed int i_iiiiiiiiiiiiiiiii(signed int a1, signed int a2, signed int a3, signed int a4, signed int a5, signed int a6, signed int a7, signed int a8, signed int a9, signed int a10, signed int a11, signed int a12, signed int a13, signed int a14, signed int a15, signed int a16, signed int a17)
{
   signed int y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, y); fflush(stdout);
   return y;
}

signed int i_iiiiiiiiiiiiiiiiii(signed int a1, signed int a2, signed int a3, signed int a4, signed int a5, signed int a6, signed int a7, signed int a8, signed int a9, signed int a10, signed int a11, signed int a12, signed int a13, signed int a14, signed int a15, signed int a16, signed int a17, signed int a18)
{
   signed int y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, y); fflush(stdout);
   return y;
}

signed int i_iiiiiiiiiiiiiiiiiii(signed int a1, signed int a2, signed int a3, signed int a4, signed int a5, signed int a6, signed int a7, signed int a8, signed int a9, signed int a10, signed int a11, signed int a12, signed int a13, signed int a14, signed int a15, signed int a16, signed int a17, signed int a18, signed int a19)
{
   signed int y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, y); fflush(stdout);
   return y;
}

signed int i_iiiiiiiiiiiiiiiiiiii(signed int a1, signed int a2, signed int a3, signed int a4, signed int a5, signed int a6, signed int a7, signed int a8, signed int a9, signed int a10, signed int a11, signed int a12, signed int a13, signed int a14, signed int a15, signed int a16, signed int a17, signed int a18, signed int a19, signed int a20)
{
   signed int y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, y); fflush(stdout);
   return y;
}

signed int i_iiiiiiiiiiiiiiiiiiiii(signed int a1, signed int a2, signed int a3, signed int a4, signed int a5, signed int a6, signed int a7, signed int a8, signed int a9, signed int a10, signed int a11, signed int a12, signed int a13, signed int a14, signed int a15, signed int a16, signed int a17, signed int a18, signed int a19, signed int a20, signed int a21)
{
   signed int y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, y); fflush(stdout);
   return y;
}

signed int i_iiiiiiiiiiiiiiiiiiiiii(signed int a1, signed int a2, signed int a3, signed int a4, signed int a5, signed int a6, signed int a7, signed int a8, signed int a9, signed int a10, signed int a11, signed int a12, signed int a13, signed int a14, signed int a15, signed int a16, signed int a17, signed int a18, signed int a19, signed int a20, signed int a21, signed int a22)
{
   signed int y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, y); fflush(stdout);
   return y;
}

signed int i_iiiiiiiiiiiiiiiiiiiiiii(signed int a1, signed int a2, signed int a3, signed int a4, signed int a5, signed int a6, signed int a7, signed int a8, signed int a9, signed int a10, signed int a11, signed int a12, signed int a13, signed int a14, signed int a15, signed int a16, signed int a17, signed int a18, signed int a19, signed int a20, signed int a21, signed int a22, signed int a23)
{
   signed int y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22 + a23;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, y); fflush(stdout);
   return y;
}

signed int i_iiiiiiiiiiiiiiiiiiiiiiii(signed int a1, signed int a2, signed int a3, signed int a4, signed int a5, signed int a6, signed int a7, signed int a8, signed int a9, signed int a10, signed int a11, signed int a12, signed int a13, signed int a14, signed int a15, signed int a16, signed int a17, signed int a18, signed int a19, signed int a20, signed int a21, signed int a22, signed int a23, signed int a24)
{
   signed int y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22 + a23 + a24;
   printf("{{ %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d => %d }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, y); fflush(stdout);
   return y;
}

float f_f(float a1)
{
   float y = a1;
   printf("{{ %f => %f }}", a1, y); fflush(stdout);
   return y;
}

float f_ff(float a1, float a2)
{
   float y = a1 + a2;
   printf("{{ %f %f => %f }}", a1, a2, y); fflush(stdout);
   return y;
}

float f_fff(float a1, float a2, float a3)
{
   float y = a1 + a2 + a3;
   printf("{{ %f %f %f => %f }}", a1, a2, a3, y); fflush(stdout);
   return y;
}

float f_ffff(float a1, float a2, float a3, float a4)
{
   float y = a1 + a2 + a3 + a4;
   printf("{{ %f %f %f %f => %f }}", a1, a2, a3, a4, y); fflush(stdout);
   return y;
}

float f_fffff(float a1, float a2, float a3, float a4, float a5)
{
   float y = a1 + a2 + a3 + a4 + a5;
   printf("{{ %f %f %f %f %f => %f }}", a1, a2, a3, a4, a5, y); fflush(stdout);
   return y;
}

float f_ffffff(float a1, float a2, float a3, float a4, float a5, float a6)
{
   float y = a1 + a2 + a3 + a4 + a5 + a6;
   printf("{{ %f %f %f %f %f %f => %f }}", a1, a2, a3, a4, a5, a6, y); fflush(stdout);
   return y;
}

float f_fffffff(float a1, float a2, float a3, float a4, float a5, float a6, float a7)
{
   float y = a1 + a2 + a3 + a4 + a5 + a6 + a7;
   printf("{{ %f %f %f %f %f %f %f => %f }}", a1, a2, a3, a4, a5, a6, a7, y); fflush(stdout);
   return y;
}

float f_ffffffff(float a1, float a2, float a3, float a4, float a5, float a6, float a7, float a8)
{
   float y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8;
   printf("{{ %f %f %f %f %f %f %f %f => %f }}", a1, a2, a3, a4, a5, a6, a7, a8, y); fflush(stdout);
   return y;
}

float f_fffffffff(float a1, float a2, float a3, float a4, float a5, float a6, float a7, float a8, float a9)
{
   float y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9;
   printf("{{ %f %f %f %f %f %f %f %f %f => %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, y); fflush(stdout);
   return y;
}

float f_ffffffffff(float a1, float a2, float a3, float a4, float a5, float a6, float a7, float a8, float a9, float a10)
{
   float y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10;
   printf("{{ %f %f %f %f %f %f %f %f %f %f => %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, y); fflush(stdout);
   return y;
}

float f_fffffffffff(float a1, float a2, float a3, float a4, float a5, float a6, float a7, float a8, float a9, float a10, float a11)
{
   float y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11;
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f => %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, y); fflush(stdout);
   return y;
}

float f_ffffffffffff(float a1, float a2, float a3, float a4, float a5, float a6, float a7, float a8, float a9, float a10, float a11, float a12)
{
   float y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12;
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f => %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, y); fflush(stdout);
   return y;
}

float f_fffffffffffff(float a1, float a2, float a3, float a4, float a5, float a6, float a7, float a8, float a9, float a10, float a11, float a12, float a13)
{
   float y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13;
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f => %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, y); fflush(stdout);
   return y;
}

float f_ffffffffffffff(float a1, float a2, float a3, float a4, float a5, float a6, float a7, float a8, float a9, float a10, float a11, float a12, float a13, float a14)
{
   float y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14;
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f => %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, y); fflush(stdout);
   return y;
}

float f_fffffffffffffff(float a1, float a2, float a3, float a4, float a5, float a6, float a7, float a8, float a9, float a10, float a11, float a12, float a13, float a14, float a15)
{
   float y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15;
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f => %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, y); fflush(stdout);
   return y;
}

float f_ffffffffffffffff(float a1, float a2, float a3, float a4, float a5, float a6, float a7, float a8, float a9, float a10, float a11, float a12, float a13, float a14, float a15, float a16)
{
   float y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16;
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f => %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, y); fflush(stdout);
   return y;
}

float f_fffffffffffffffff(float a1, float a2, float a3, float a4, float a5, float a6, float a7, float a8, float a9, float a10, float a11, float a12, float a13, float a14, float a15, float a16, float a17)
{
   float y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17;
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f => %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, y); fflush(stdout);
   return y;
}

float f_ffffffffffffffffff(float a1, float a2, float a3, float a4, float a5, float a6, float a7, float a8, float a9, float a10, float a11, float a12, float a13, float a14, float a15, float a16, float a17, float a18)
{
   float y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18;
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f => %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, y); fflush(stdout);
   return y;
}

float f_fffffffffffffffffff(float a1, float a2, float a3, float a4, float a5, float a6, float a7, float a8, float a9, float a10, float a11, float a12, float a13, float a14, float a15, float a16, float a17, float a18, float a19)
{
   float y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19;
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f => %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, y); fflush(stdout);
   return y;
}

float f_ffffffffffffffffffff(float a1, float a2, float a3, float a4, float a5, float a6, float a7, float a8, float a9, float a10, float a11, float a12, float a13, float a14, float a15, float a16, float a17, float a18, float a19, float a20)
{
   float y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20;
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f => %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, y); fflush(stdout);
   return y;
}

float f_fffffffffffffffffffff(float a1, float a2, float a3, float a4, float a5, float a6, float a7, float a8, float a9, float a10, float a11, float a12, float a13, float a14, float a15, float a16, float a17, float a18, float a19, float a20, float a21)
{
   float y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21;
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f => %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, y); fflush(stdout);
   return y;
}

float f_ffffffffffffffffffffff(float a1, float a2, float a3, float a4, float a5, float a6, float a7, float a8, float a9, float a10, float a11, float a12, float a13, float a14, float a15, float a16, float a17, float a18, float a19, float a20, float a21, float a22)
{
   float y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22;
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f => %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, y); fflush(stdout);
   return y;
}

float f_fffffffffffffffffffffff(float a1, float a2, float a3, float a4, float a5, float a6, float a7, float a8, float a9, float a10, float a11, float a12, float a13, float a14, float a15, float a16, float a17, float a18, float a19, float a20, float a21, float a22, float a23)
{
   float y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22 + a23;
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f => %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, y); fflush(stdout);
   return y;
}

float f_ffffffffffffffffffffffff(float a1, float a2, float a3, float a4, float a5, float a6, float a7, float a8, float a9, float a10, float a11, float a12, float a13, float a14, float a15, float a16, float a17, float a18, float a19, float a20, float a21, float a22, float a23, float a24)
{
   float y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22 + a23 + a24;
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f => %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, y); fflush(stdout);
   return y;
}

double d_d(double a1)
{
   double y = a1;
   printf("{{ %f => %f }}", a1, y); fflush(stdout);
   return y;
}

double d_dd(double a1, double a2)
{
   double y = a1 + a2;
   printf("{{ %f %f => %f }}", a1, a2, y); fflush(stdout);
   return y;
}

double d_ddd(double a1, double a2, double a3)
{
   double y = a1 + a2 + a3;
   printf("{{ %f %f %f => %f }}", a1, a2, a3, y); fflush(stdout);
   return y;
}

double d_dddd(double a1, double a2, double a3, double a4)
{
   double y = a1 + a2 + a3 + a4;
   printf("{{ %f %f %f %f => %f }}", a1, a2, a3, a4, y); fflush(stdout);
   return y;
}

double d_ddddd(double a1, double a2, double a3, double a4, double a5)
{
   double y = a1 + a2 + a3 + a4 + a5;
   printf("{{ %f %f %f %f %f => %f }}", a1, a2, a3, a4, a5, y); fflush(stdout);
   return y;
}

double d_dddddd(double a1, double a2, double a3, double a4, double a5, double a6)
{
   double y = a1 + a2 + a3 + a4 + a5 + a6;
   printf("{{ %f %f %f %f %f %f => %f }}", a1, a2, a3, a4, a5, a6, y); fflush(stdout);
   return y;
}

double d_ddddddd(double a1, double a2, double a3, double a4, double a5, double a6, double a7)
{
   double y = a1 + a2 + a3 + a4 + a5 + a6 + a7;
   printf("{{ %f %f %f %f %f %f %f => %f }}", a1, a2, a3, a4, a5, a6, a7, y); fflush(stdout);
   return y;
}

double d_dddddddd(double a1, double a2, double a3, double a4, double a5, double a6, double a7, double a8)
{
   double y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8;
   printf("{{ %f %f %f %f %f %f %f %f => %f }}", a1, a2, a3, a4, a5, a6, a7, a8, y); fflush(stdout);
   return y;
}

double d_ddddddddd(double a1, double a2, double a3, double a4, double a5, double a6, double a7, double a8, double a9)
{
   double y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9;
   printf("{{ %f %f %f %f %f %f %f %f %f => %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, y); fflush(stdout);
   return y;
}

double d_dddddddddd(double a1, double a2, double a3, double a4, double a5, double a6, double a7, double a8, double a9, double a10)
{
   double y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10;
   printf("{{ %f %f %f %f %f %f %f %f %f %f => %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, y); fflush(stdout);
   return y;
}

double d_ddddddddddd(double a1, double a2, double a3, double a4, double a5, double a6, double a7, double a8, double a9, double a10, double a11)
{
   double y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11;
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f => %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, y); fflush(stdout);
   return y;
}

double d_dddddddddddd(double a1, double a2, double a3, double a4, double a5, double a6, double a7, double a8, double a9, double a10, double a11, double a12)
{
   double y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12;
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f => %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, y); fflush(stdout);
   return y;
}

double d_ddddddddddddd(double a1, double a2, double a3, double a4, double a5, double a6, double a7, double a8, double a9, double a10, double a11, double a12, double a13)
{
   double y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13;
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f => %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, y); fflush(stdout);
   return y;
}

double d_dddddddddddddd(double a1, double a2, double a3, double a4, double a5, double a6, double a7, double a8, double a9, double a10, double a11, double a12, double a13, double a14)
{
   double y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14;
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f => %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, y); fflush(stdout);
   return y;
}

double d_ddddddddddddddd(double a1, double a2, double a3, double a4, double a5, double a6, double a7, double a8, double a9, double a10, double a11, double a12, double a13, double a14, double a15)
{
   double y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15;
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f => %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, y); fflush(stdout);
   return y;
}

double d_dddddddddddddddd(double a1, double a2, double a3, double a4, double a5, double a6, double a7, double a8, double a9, double a10, double a11, double a12, double a13, double a14, double a15, double a16)
{
   double y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16;
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f => %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, y); fflush(stdout);
   return y;
}

double d_ddddddddddddddddd(double a1, double a2, double a3, double a4, double a5, double a6, double a7, double a8, double a9, double a10, double a11, double a12, double a13, double a14, double a15, double a16, double a17)
{
   double y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17;
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f => %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, y); fflush(stdout);
   return y;
}

double d_dddddddddddddddddd(double a1, double a2, double a3, double a4, double a5, double a6, double a7, double a8, double a9, double a10, double a11, double a12, double a13, double a14, double a15, double a16, double a17, double a18)
{
   double y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18;
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f => %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, y); fflush(stdout);
   return y;
}

double d_ddddddddddddddddddd(double a1, double a2, double a3, double a4, double a5, double a6, double a7, double a8, double a9, double a10, double a11, double a12, double a13, double a14, double a15, double a16, double a17, double a18, double a19)
{
   double y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19;
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f => %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, y); fflush(stdout);
   return y;
}

double d_dddddddddddddddddddd(double a1, double a2, double a3, double a4, double a5, double a6, double a7, double a8, double a9, double a10, double a11, double a12, double a13, double a14, double a15, double a16, double a17, double a18, double a19, double a20)
{
   double y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20;
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f => %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, y); fflush(stdout);
   return y;
}

double d_ddddddddddddddddddddd(double a1, double a2, double a3, double a4, double a5, double a6, double a7, double a8, double a9, double a10, double a11, double a12, double a13, double a14, double a15, double a16, double a17, double a18, double a19, double a20, double a21)
{
   double y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21;
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f => %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, y); fflush(stdout);
   return y;
}

double d_dddddddddddddddddddddd(double a1, double a2, double a3, double a4, double a5, double a6, double a7, double a8, double a9, double a10, double a11, double a12, double a13, double a14, double a15, double a16, double a17, double a18, double a19, double a20, double a21, double a22)
{
   double y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22;
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f => %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, y); fflush(stdout);
   return y;
}

double d_ddddddddddddddddddddddd(double a1, double a2, double a3, double a4, double a5, double a6, double a7, double a8, double a9, double a10, double a11, double a12, double a13, double a14, double a15, double a16, double a17, double a18, double a19, double a20, double a21, double a22, double a23)
{
   double y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22 + a23;
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f => %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, y); fflush(stdout);
   return y;
}

double d_dddddddddddddddddddddddd(double a1, double a2, double a3, double a4, double a5, double a6, double a7, double a8, double a9, double a10, double a11, double a12, double a13, double a14, double a15, double a16, double a17, double a18, double a19, double a20, double a21, double a22, double a23, double a24)
{
   double y = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22 + a23 + a24;
   printf("{{ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f => %f }}", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, y); fflush(stdout);
   return y;
}
