#include <stdio.h>
#include <unistd.h>

#include <ol/vm.h>

#define BEGIN(x) \
{ \
	printf("Testing " x " instruction... ");
#define END() \
	OLVM_delete(olvm); \
}

#define EVAL(...) \
	unsigned char bootstrap[] = __VA_ARGS__; \
	olvm_t* olvm = OLVM_new(bootstrap); \
	word output = (word) OLVM_run(olvm, 0, 0);

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

#	define LDI   13      // LDE (13), LDN (77), LDT (141), LDF (205)
#	  define LDE (LDI + 0*64)
#	  define LDN (LDI + 1*64)
#	  define LDT (LDI + 2*64)
#	  define LDF (LDI + 3*64)
#	define LD    14
#	define RET   24

#	define MOVE   9

int main(int argc, char** argv)
{
	int failed = 0;
	int succeeded = 0;
	int total = 0;

	/// LDI
	TESTCASE("LDE",  == 0x336, // #empty
		//lambda(x)  LDE[6]  RET[6]
		{ 2, TBYTECODE, 4,
			LDE, 6,
			RET, 6,
		  0 }
	);
	TESTCASE("LDN",  == 0x236, // #null
		//lambda(x)  LDN[6]  RET[6]
		{ 2, TBYTECODE, 4,
			LDN, 6,
			RET, 6,
		  0 }
	);
	TESTCASE("LDT",  == 0x136, // #true
		//lambda(x)  LDT[6]  RET[6]
		{ 2, TBYTECODE, 4,
			LDT, 6,
			RET, 6,
		  0 }
	);
	TESTCASE("LDF",  == 0x036, // #false
		//lambda(x)  LDF[6]  RET[6]
		{ 2, TBYTECODE, 4,
			LDF, 6,
			RET, 6,
		  0 }
	);

	/// LD
	TESTCASE("LD",   == 0x202,
		//lambda(x)  LD  2>[6]  RET[6]
		{ 2, TBYTECODE, 5,
			LD, 2, 6,
			RET, 6,
		  0 }
	);
	/// MOVE
	TESTCASE("MOVE", == 0x202,
		//lambda(x)  LD  2>[6]  MOVE [6]>[7]  RET[7]
		{ 2, TBYTECODE, 8,
			LD, 2, 6,
			MOVE, 6, 7,
			RET, 7,
		  0 }
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
