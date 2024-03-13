#include <ol/ol.h>

// olvm:
ol_t ol;

// just code simplification, some kind of magic to not manually write 'new_string' and 'make_integer':
// we automatically call new_string or make_integer dependly on argument type
// but in general case you should do this manually. or not.
// C preprocessor trick, some kind of "map":
// https://github.com/swansontec/map-macro
///*
#define EVAL0(...) __VA_ARGS__
#define EVAL1(...) EVAL0(EVAL0(EVAL0(__VA_ARGS__)))
#define EVAL2(...) EVAL1(EVAL1(EVAL1(__VA_ARGS__)))
#define EVAL3(...) EVAL2(EVAL2(EVAL2(__VA_ARGS__)))
#define EVAL4(...) EVAL3(EVAL3(EVAL3(__VA_ARGS__)))
#define EVAL(...)  EVAL4(EVAL4(EVAL4(__VA_ARGS__)))

#define MAP_END(...)
#define MAP_OUT
#define MAP_COMMA ,

#define MAP_GET_END2() 0, MAP_END
#define MAP_GET_END1(...) MAP_GET_END2
#define MAP_GET_END(...) MAP_GET_END1
#define MAP_NEXT0(test, next, ...) next MAP_OUT
#define MAP_NEXT1(test, next) MAP_NEXT0(test, next, 0)
#define MAP_NEXT(test, next)  MAP_NEXT1(MAP_GET_END test, next)

#define MAP0(f, x, peek, ...) f(x) MAP_NEXT(peek, MAP1)(f, peek, __VA_ARGS__)
#define MAP1(f, x, peek, ...) f(x) MAP_NEXT(peek, MAP0)(f, peek, __VA_ARGS__)

#define MAP_LIST_NEXT1(test, next) MAP_NEXT0(test, MAP_COMMA next, 0)
#define MAP_LIST_NEXT(test, next)  MAP_LIST_NEXT1(MAP_GET_END test, next)

#define MAP_LIST0(f, x, peek, ...) f(x) MAP_LIST_NEXT(peek, MAP_LIST1)(f, peek, __VA_ARGS__)
#define MAP_LIST1(f, x, peek, ...) f(x) MAP_LIST_NEXT(peek, MAP_LIST0)(f, peek, __VA_ARGS__)

#define MAP(f, ...) EVAL(MAP1(f, __VA_ARGS__, ()()(), ()()(), ()()(), 0))
#define MAP_LIST(f, ...) EVAL(MAP_LIST1(f, __VA_ARGS__, ()()(), ()()(), ()()(), 0))
//*/  end of C preprocessor trick

#define _Q(x) \
	__builtin_choose_expr( __builtin_types_compatible_p (__typeof__(x), char[]),   new_string(&ol, (char*)(uintptr_t)x), \
	__builtin_choose_expr( __builtin_types_compatible_p (__typeof__(x), char*),    new_string(&ol, (char*)(uintptr_t)x), \
	__builtin_choose_expr( __builtin_types_compatible_p (__typeof__(x), signed char),    make_integer((signed)(uintptr_t)x), \
	__builtin_choose_expr( __builtin_types_compatible_p (__typeof__(x), unsigned char),  make_integer((unsigned)(uintptr_t)x), \
	__builtin_choose_expr( __builtin_types_compatible_p (__typeof__(x), signed short),   make_integer((signed)(uintptr_t)x), \
	__builtin_choose_expr( __builtin_types_compatible_p (__typeof__(x), unsigned short), make_integer((unsigned)(uintptr_t)x), \
	__builtin_choose_expr( __builtin_types_compatible_p (__typeof__(x), signed int),     make_integer((signed)(uintptr_t)x), \
	__builtin_choose_expr( __builtin_types_compatible_p (__typeof__(x), unsigned int),   make_integer((unsigned)(uintptr_t)x), \
	__builtin_choose_expr( __builtin_types_compatible_p (__typeof__(x), long),     make_integer((long)(uintptr_t)x), \
	__builtin_choose_expr( __builtin_types_compatible_p (__typeof__(x), uintptr_t),(uintptr_t)x, \
	IFALSE))))))))))

#define eval(...) OL_eval(&ol, MAP_LIST(_Q, __VA_ARGS__), 0)

extern unsigned char REPL[];
void ol_new_ol()
{
	OL_new(&ol, REPL);
	eval("(import (main))");
        eval("(born-blinky)");
}

void ol_delete_ol()
{
	OL_delete(&ol);
}


uintptr_t ol_points()
{
	return eval("points");
}

void ol_get_blinky(int* x, int* y)
{
	uintptr_t xy = eval("(get-blinky)");
	assert (is_pair(xy));

	*x = ol2int(car(xy));
	*y = ol2int(cdr(xy));
}

size_t ol_get_used_memory()
{
	uintptr_t m = eval("(ref (syscall 1117) 1)");
	assert(is_number(m));

	return (size_t) ol2int(m);
}

size_t ol_get_heap_memory()
{
	uintptr_t m = eval("(ref (syscall 1117) 3)");
	assert(is_number(m));

	return (size_t) ol2int(m);
}

void ol_eat_the_point(int x, int y)
{
	eval("eat-the-point", x, y);
}

void ol_blinky_move(int x, int y)
{
	eval("blinky-move", x, y);
}

int ol_get_level(int x, int y)
{
	uintptr_t m = eval("get-level", x, y);
	assert(is_number(m));

	return ol2int(m);
}
