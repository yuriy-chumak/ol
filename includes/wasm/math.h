#pragma once

#pragma GCC push_options
#pragma GCC optimize ("no-math-errno")

#define sqrt __builtin_sqrt
#define sqrtf __builtin_sqrtf

#define sin __builtin_sin
#define sinf __builtin_sinf
#define asin __builtin_asin
#define asinf __builtin_asinf

#define cos __builtin_cos
#define cosf __builtin_cosf
#define acos __builtin_acos
#define acosf __builtin_acosf

#define tan __builtin_tan
#define tanf __builtin_tanf
#define atan __builtin_atan
#define atanf __builtin_atanf
#define atan2 __builtin_atan2
#define atan2f __builtin_atan2f

#define log __builtin_log
#define logf __builtin_logf

#define exp __builtin_exp
#define expf __builtin_expf

#define pow __builtin_pow
#define powf __builtin_powf

#define floor __builtin_floor
#define floorf __builtin_floorf

#pragma GCC pop_options