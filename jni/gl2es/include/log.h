#include <android/log.h>
#define APP_NAME "gl2es"
#define DLOG(...) __android_log_print(ANDROID_LOG_DEBUG,   APP_NAME, __VA_ARGS__)
#define ILOG(...) __android_log_print(ANDROID_LOG_INFO,    APP_NAME, __VA_ARGS__)
#define ELOG(...) __android_log_print(ANDROID_LOG_ERROR,   APP_NAME, __VA_ARGS__)
#define VLOG(...) __android_log_print(ANDROID_LOG_VERBOSE, APP_NAME, __VA_ARGS__)
#define FLOG(...) __android_log_print(ANDROID_LOG_FATAL,   APP_NAME, __VA_ARGS__)

