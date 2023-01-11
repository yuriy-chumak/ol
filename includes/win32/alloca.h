#pragma once
#ifdef _WIN32

#include <malloc.h>

// a version of _alloca with security enhancements
#ifdef alloca
#undef alloca
#endif

#define alloca _alloca

#endif
