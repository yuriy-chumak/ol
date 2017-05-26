#include <olvm.h>

#include <stdio.h>
#include <unistd.h>
typedef uintptr_t word;

#define BEGIN(x) \
{ \
	printf("Testing " x " instructions... ");
#define END() \
	OL_free(olvm); \
}

#define EVAL(...) \
	unsigned char bootstrap[] = __VA_ARGS__; \
	OL* olvm = OL_new(bootstrap, 0); \
	word output = (word)OL_eval(olvm, 0, 0);

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


int main(int argc, char** argv)
{
	int failed = 0;
	int succeeded = 0;
	int total = 0;

	/// LD+RET
	TESTCASE("LD+RET", == 2,
		//lambda(x)  LD  2>[6]  RET[6]
		{ 2, 16, 5,  14, 2, 6,  24, 6,  0 }
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
