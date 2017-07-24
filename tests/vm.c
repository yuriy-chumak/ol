#include <olvm.h>

#include <stdio.h>
#include <unistd.h>
typedef uintptr_t word;

#define BEGIN(x) \
{ \
	printf("Testing " x " instruction... ");
#define END() \
	OL_free(olvm); \
}

#define EVAL(...) \
	unsigned char bootstrap[] = __VA_ARGS__; \
	OL* olvm = OL_new(bootstrap, 0); \
	word output = (word) OL_run(olvm, 0, 0);

#define ASSERT(...) \
	if (__VA_ARGS__) { \
		succeeded++; \
		printf("Ok.\n"); \
	} \
	else { \
		failed++; \
		printf("Failed.\n"); \
	} \
	total++;

#define TESTCASE(name, assertion, ...) \
	BEGIN(name) \
		EVAL(__VA_ARGS__) \
		ASSERT(output assertion) \
	END()

#if EMBEDDED_VM
int main(int argc, char** argv)
{
	int failed = 0;
	int succeeded = 0;
	int total = 0;

	/// LDI
	TESTCASE("LDE",  == 0x336, // #empty
		//lambda(x)  LDE[6]  RET[6]
		{ 2, 16, 4,  13, 6,  24, 6,  0 }
	);
	TESTCASE("LDN",  == 0x236, // #null
		//lambda(x)  LDN[6]  RET[6]
		{ 2, 16, 4,  77, 6,  24, 6,  0 }
	);
	TESTCASE("LDT",  == 0x136, // #true
		//lambda(x)  LDN[6]  RET[6]
		{ 2, 16, 4, 141, 6,  24, 6,  0 }
	);
	TESTCASE("LDF",  == 0x036, // #false
		//lambda(x)  LDN[6]  RET[6]
		{ 2, 16, 4, 205, 6,  24, 6,  0 }
	);

	/// LD
	TESTCASE("LD",   == 0x202,
		//lambda(x)  LD  2>[6]  RET[6]
		{ 2, 16, 5,  14, 2, 6,  24, 6,  0 }
	);
	/// MOVE
	TESTCASE("MOVE", == 0x202,
		//lambda(x)  LD  2>[6]  MOVE [6]>[7]  RET[7]
		{ 2, 16, 8,  14, 2, 6,  9, 6, 7,      24, 7,  0 }
	);


	/// GOTO, APPLY, RET, RUN, ARITY_ERROR, SYS, LDI, LD, REFI, MOVE, MOV2
	/// JEQ, JP, JAF, VMNEW, VMRAW, RAWQ, CONS, TYPE, SIZE, CAST, CAR, CDR
	/// REF, SETREF, SETREFE, EQ, LESS, CLOCK
	/// SYSCALL: ...
	/// UNREEL, TUPLEAPPLY, FFAPPLY, MKRED, MKBLACK, FFTOGGLE, FFREDQ, FFRIGHTQ
	/// ADDITION, DIVISION, MULTIPLICATION, SUBTRACTION
	/// BINARY_AND, BINARY_OR, BINARY_XOR, SHIFT_RIGHT, SHIFT_LEFT

	///
	return (failed != 0);
}
#endif
