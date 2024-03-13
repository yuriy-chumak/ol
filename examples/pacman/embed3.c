#include <ol/ol.h>

// olvm:
struct olvm_t* vm;

int points;
int get_blinky;
int get_heap_memory;
int get_used_memory;
int eat_the_point;
int blinky_move;
int get_level;

extern unsigned char tmp_bin[];
void ol_new_ol()
{
	vm = OLVM_new(tmp_bin);
	OLVM_userdata(vm, &vm);

	uintptr_t
	r = OLVM_run(vm, 0, 0);
	// well, we have our "smart" script prepared,
	//  now save functions for feature use
	assert (is_vector(r));

	points = ol2int(ref(r, 1));
	get_blinky = ol2int(ref(r, 2));
	get_used_memory = ol2int(ref(r, 3));
	get_heap_memory = ol2int(ref(r, 4));
	eat_the_point = ol2int(ref(r, 5));
	blinky_move = ol2int(ref(r, 6));
	get_level = ol2int(ref(r, 7));
}

void ol_delete_ol()
{
	OLVM_delete(vm);
}


uintptr_t ol_points()
{
	return OLVM_evaluate(vm,
		OLVM_deref(vm, points),
		0, NULL);
}

void ol_get_blinky(int* x, int* y)
{
	uintptr_t xy = OLVM_evaluate(vm,
		OLVM_deref(vm, get_blinky),
		0, NULL);
	assert (is_pair(xy));

	*x = ol2int(car(xy));
	*y = ol2int(cdr(xy));
}

size_t ol_get_used_memory()
{
	uintptr_t m = OLVM_evaluate(vm,
		OLVM_deref(vm, get_used_memory),
		0, NULL);
	assert(is_number(m));

	return (size_t) ol2int(m);
}

size_t ol_get_heap_memory()
{
	uintptr_t m = OLVM_evaluate(vm,
		OLVM_deref(vm, get_heap_memory),
		0, NULL);
	assert(is_number(m));

	return (size_t) ol2int(m);
}

void ol_eat_the_point(int x, int y)
{
	uintptr_t args[] = {
		make_integer(x),
		make_integer(y)
	};

	OLVM_evaluate(vm,
		OLVM_deref(vm, eat_the_point),
		2, args);
}

void ol_blinky_move(int x, int y)
{
	uintptr_t args[] = {
		make_integer(x),
		make_integer(y)
	};

	OLVM_evaluate(vm,
		OLVM_deref(vm, blinky_move),
		2, args);
}

int ol_get_level(int x, int y)
{
	uintptr_t args[] = {
		make_integer(x),
		make_integer(y)
	};

	uintptr_t m = OLVM_evaluate(vm,
		OLVM_deref(vm, get_level),
		2, args);
	assert(is_number(m));

	return ol2int(m);
}
