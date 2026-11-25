#pragma once

#pragma GCC push_options
#pragma GCC optimize ("no-math-errno")

#define sqrt __builtin_sqrt

#define sqrtf __builtin_sqrtf

#pragma GCC pop_options