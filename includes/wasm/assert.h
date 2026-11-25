#pragma once

#ifndef static_assert
# if GCC_VERSION < 40600
#	define CONCATENATE_DETAIL(x, y) x##y
#	define CONCATENATE(x, y) CONCATENATE_DETAIL(x, y)
#	define MAKE_UNIQUE(x) CONCATENATE(x, __COUNTER__)
#	define static_assert(condition, comment) \
		typedef char MAKE_UNIQUE(static_assertion_)[2*(!!(condition)) - 1];
# else
#	define static_assert _Static_assert
# endif
#endif

extern void abort();
#define assert(expr) \
	do { \
		if (!(expr)) { \
			E("%s:%d: assertion failed: %s\n", __FILE__, __LINE__, #expr); \
			abort(); \
		} \
	} while (0)
