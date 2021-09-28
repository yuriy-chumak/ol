#include <ol/ol.h>

// olvm:
ol_t ol;

extern unsigned char REPL[];
void ol_new_ol()
{
	OL_new(&ol, REPL);
	
	OL_eval(&ol, new_string(&ol, "(import (main))"), 0);
	OL_eval(&ol, new_string(&ol, "(born-blinky)"), 0);
}

void ol_delete_ol()
{
	OL_delete(&ol);
}


uintptr_t ol_points()
{
	return OL_eval(&ol,
		new_string(&ol, "points"),
		0);
}

void ol_get_blinky(int* x, int* y)
{
	uintptr_t xy = OL_eval(&ol,
		new_string(&ol, "(get-blinky)"),
		0);
	assert (is_pair(xy));

	*x = ol2int(car(xy));
	*y = ol2int(cdr(xy));
}

uintptr_t ol_get_used_memory()
{
	uintptr_t m = OL_eval(&ol,
		new_string(&ol, "(ref (syscall 1117) 1)"),
		0);
	assert(is_number(m));

	return (size_t) ol2int(m);
}

size_t ol_get_heap_memory()
{
	uintptr_t m = OL_eval(&ol,
		new_string(&ol, "(ref (syscall 1117) 3)"),
		0);
	assert (is_number(m));

	return (size_t) ol2int(m);
}

void ol_eat_the_point(int x, int y)
{
	OL_eval(&ol,
		new_string(&ol, "eat-the-point"),
		make_integer(x),
		make_integer(y),
		0);
}

void ol_blinky_move(int x, int y)
{
	OL_eval(&ol,
		new_string(&ol, "blinky-move"),
		make_integer(x),
		make_integer(y),
		0);
}

int ol_get_level(int x, int y)
{
	uintptr_t m = OL_eval(&ol,
		new_string(&ol, "get-level"),
		make_integer(x),
		make_integer(y),
		0);
	assert (is_number(m));

	return ol2int(m);
}
