// Otus Lisp Java (Android) Interface
#include <jni.h>
#include <android/log.h>
#include <android/asset_manager_jni.h>

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include <strings.h>

#include <../extensions/embed.h>

#include <fcntl.h>
#include <unistd.h>

// #include <zip.h>
extern unsigned char repl[]; // otus lisp binary

#define LOGD(...) __android_log_print(ANDROID_LOG_DEBUG, "ol", __VA_ARGS__)
#define LOGI(...) __android_log_print(ANDROID_LOG_INFO, "ol", __VA_ARGS__)
#define LOGE(...) __android_log_print(ANDROID_LOG_ERROR, "ol", __VA_ARGS__)

#define OL_HOME "/sdcard/ol"
// ------------------------------------------------------------------------
// olvm: todo: make dynamic
ol_t ol;

// just code simplification, some kind of magic to not manually write 'new_string' and 'make_integer':
// we automatically call new_string or make_integer dependly on argument type
// but in general case you should do this manually. or not.
// C preprocessor trick, some kind of "map":
// https://github.com/swansontec/map-macro

#define EVAL0(...) __VA_ARGS__
#define EVAL1(...) EVAL0(EVAL0(EVAL0(__VA_ARGS__)))
#define EVAL2(...) EVAL1(EVAL1(EVAL1(__VA_ARGS__)))
#define EVAL3(...) EVAL2(EVAL2(EVAL2(__VA_ARGS__)))
#define EVAL4(...) EVAL3(EVAL3(EVAL3(__VA_ARGS__)))
#define EVAL(...)    EVAL4(EVAL4(EVAL4(__VA_ARGS__)))

#define MAP_END(...)
#define MAP_OUT
#define MAP_COMMA ,

#define MAP_GET_END2() 0, MAP_END
#define MAP_GET_END1(...) MAP_GET_END2
#define MAP_GET_END(...) MAP_GET_END1
#define MAP_NEXT0(test, next, ...) next MAP_OUT
#define MAP_NEXT1(test, next) MAP_NEXT0(test, next, 0)
#define MAP_NEXT(test, next)    MAP_NEXT1(MAP_GET_END test, next)

#define MAP0(f, x, peek, ...) f(x) MAP_NEXT(peek, MAP1)(f, peek, __VA_ARGS__)
#define MAP1(f, x, peek, ...) f(x) MAP_NEXT(peek, MAP0)(f, peek, __VA_ARGS__)

#define MAP_LIST_NEXT1(test, next) MAP_NEXT0(test, MAP_COMMA next, 0)
#define MAP_LIST_NEXT(test, next)    MAP_LIST_NEXT1(MAP_GET_END test, next)

#define MAP_LIST0(f, x, peek, ...) f(x) MAP_LIST_NEXT(peek, MAP_LIST1)(f, peek, __VA_ARGS__)
#define MAP_LIST1(f, x, peek, ...) f(x) MAP_LIST_NEXT(peek, MAP_LIST0)(f, peek, __VA_ARGS__)

#define MAP(f, ...) EVAL(MAP1(f, __VA_ARGS__, ()()(), ()()(), ()()(), 0))
#define MAP_LIST(f, ...) EVAL(MAP_LIST1(f, __VA_ARGS__, ()()(), ()()(), ()()(), 0))
//*/    end of C preprocessor trick

#define _Q(x) \
    __builtin_choose_expr( __builtin_types_compatible_p (__typeof__(x), char[]),         new_string(&ol, (char*)(uintptr_t)x), \
    __builtin_choose_expr( __builtin_types_compatible_p (__typeof__(x), char*),          new_string(&ol, (char*)(uintptr_t)x), \
    __builtin_choose_expr( __builtin_types_compatible_p (__typeof__(x), signed char),    make_integer((signed)(uintptr_t)x), \
    __builtin_choose_expr( __builtin_types_compatible_p (__typeof__(x), unsigned char),  make_integer((unsigned)(uintptr_t)x), \
    __builtin_choose_expr( __builtin_types_compatible_p (__typeof__(x), signed short),   make_integer((signed)(uintptr_t)x), \
    __builtin_choose_expr( __builtin_types_compatible_p (__typeof__(x), unsigned short), make_integer((unsigned)(uintptr_t)x), \
    __builtin_choose_expr( __builtin_types_compatible_p (__typeof__(x), signed int),     make_integer((signed)(uintptr_t)x), \
    __builtin_choose_expr( __builtin_types_compatible_p (__typeof__(x), unsigned int),   make_integer((unsigned)(uintptr_t)x), \
    __builtin_choose_expr( __builtin_types_compatible_p (__typeof__(x), long),           make_integer((long)(uintptr_t)x), \
    __builtin_choose_expr( __builtin_types_compatible_p (__typeof__(x), uintptr_t),(uintptr_t)x, IFALSE))))))))))

#define eval(...) embed_eval(&ol, MAP_LIST(_Q, __VA_ARGS__), 0)
// ------------------------------------------------------------------------

static jobject java_asset_manager = NULL;
static AAssetManager* asset_manager = NULL;
static AAsset** fds; // file descriptors
static int fds_size;

int assets_open(const char *filename, int flags, int mode, void* userdata)
{
    int file = open(filename, flags, mode);
    LOGI("open file: %s(%d)", filename, file);
    if (file != -1)
        return file;

    // no file, try open /sdcard/ol/file

    // "./" workaround:
    if (strncmp(filename, "./", 2) == 0)
        filename += 2;

    // new filename:
    static char* sdcardol = OL_HOME;
    char* filename2 = (char*) __builtin_alloca(strlen(filename) + strlen(sdcardol) + 2);
    snprintf(filename2, strlen(filename) + strlen(sdcardol) + 2, "%s/%s", sdcardol, filename);

    file = open(filename2, flags, mode);
    LOGI("open file: %s(%d)", filename2, file);
    if (file != -1)
        return file;

    // at least try to read assets
    if (strlen(filename) > 2 && filename[0] == '.' && filename[1] == '/')
        filename += 2;
            AAsset* asset = AAssetManager_open(asset_manager, filename, 0);
            LOGI("open asset: %s(%p)", filename, asset);
            if (asset) {
                int i = 3; // 0, 1, 2 - reserved
                for (; i < fds_size; i++) {
            if (fds[i] == 0) {
                fds[i] = asset;
                file = -i;
                break;
            }
        }
        // no available descriptors?
        if (i == fds_size) {
            int fds_size_new = fds_size * 5 / 8; // 1.6, ~1.618
            AAsset** fds_new = realloc(fds, fds_size_new);
            if (fds_new) {
                fds = fds_new;
                fds_size = fds_size_new;

                fds[i] = asset;
                file = -i;
            }
        }
    }

    return file;
}

int assets_close(int fd, void* userdata)
{
    LOGI("close file: %d", fd);
    if (fd < 0) { // assets
        fd = -fd;
        if (fd < fds_size) {
            AAsset* aa = fds[fd];
            if (aa != 0) {
                AAsset_close(aa);
                fds[fd] = 0;
                return 0;
            }
        }
        // error:
        return -1;
    }
    else
    return close(fd);
}

ssize_t assets_read(int fd, void *buf, size_t count, void* userdata)
{
    if (fd < 0) { // assets
        fd = -fd;
        if (fd < fds_size) {
            AAsset* aa = fds[fd];
            if (aa != 0) {
                return AAsset_read(aa, buf, count);
            }
        }
        // error:
        return -1;
    }
    else
    return read(fd, buf, count);
}

// redirect stdout/stderr to logcat
ssize_t assets_write(int fd, void *buf, size_t count, void* userdata)
{
    if (fd == 0) { // stdin means "debug"
        LOGD("%.*s", (int)count, (char*)buf);
        return count;
    }
    if (fd == 1) { // stdout
        LOGI("%.*s", (int)count, (char*)buf);
        return count;
    }
    if (fd == 2) { // stderr
        LOGE("%.*s", (int)count, (char*)buf);
        return count;
    }
    return write(fd, buf, count);
}

// hmmmmm, is it required?
// #include <ft2build.h>
// #include <freetype/freetype.h>
// ---------------------------------------

JNIEXPORT void JNICALL Java_name_yuriy_1chumak_ol_Olvm_nativeNew(JNIEnv* jenv, jobject class)
{
	// init:
	fds_size = 16;
	fds = (AAsset**) malloc(sizeof(*fds) * fds_size);

	// gl4es init
	// setenv("LIBGL_NOBANNER", "0", 1);

	// let's start our application
	ol.vm = OL_new(repl);
	OL_userdata(ol.vm, &ol);

	char* args[] = { "--embed", "--no-interactive" }; //, "--home=/sdcard/ol" }; // ol execution arguments
	word r = OL_run(ol.vm, sizeof(args) / sizeof(*args), args);

    // well, we have our "smart" script prepared,
    //  now save both eval and env variables
    assert (is_enum(r)); // todo: change to is_enump
    ol.eval = r >> 8;

    OL_set_open(ol.vm, assets_open);
    OL_set_close(ol.vm, assets_close);
    OL_set_read(ol.vm, assets_read);
    OL_set_write(ol.vm, assets_write); // redirects stdout/atderr to logcat

    return;
}

JNIEXPORT void JNICALL Java_name_yuriy_1chumak_ol_Olvm_nativeDelete(JNIEnv* jenv, jobject class)
{
    // embed_delete(&ol);
    if (java_asset_manager) {
        (*jenv)->DeleteGlobalRef(jenv, java_asset_manager);
        java_asset_manager = NULL;
    }
}

JNIEXPORT void JNICALL Java_name_yuriy_1chumak_ol_Olvm_nativeSetAssetManager(JNIEnv* jenv, jobject jobj, jobject assetManager)
{
    java_asset_manager = (*jenv)->NewGlobalRef(jenv, assetManager);
    asset_manager = AAssetManager_fromJava(jenv, java_asset_manager);
}

JNIEXPORT jobject JNICALL Java_name_yuriy_1chumak_ol_Olvm_eval(JNIEnv* jenv, jobject jobj, jarray args)
{
    jint argc = (*jenv)->GetArrayLength(jenv, args);
//    LOGI("argumets count: %d", argc);

    jclass String = (*jenv)->FindClass(jenv, "java/lang/String");
    jclass Integer = (*jenv)->FindClass(jenv, "java/lang/Integer");
	jclass Float = (*jenv)->FindClass(jenv, "java/lang/Float");
	jclass Double = (*jenv)->FindClass(jenv, "java/lang/Double");

    uintptr_t* values = __builtin_alloca((argc+1) * sizeof(uintptr_t));
    values[0] = OL_deref(ol.vm, ol.eval);
    for (int i = 0; i < argc; i++) {
        jobject arg = (*jenv)->GetObjectArrayElement(jenv, args, i);
        // if((*env)->ExceptionOccurred(env)) {
        //     break;
        // }
        if ((*jenv)->IsInstanceOf(jenv, arg, String)) {
            char* value = (char*) (*jenv)->GetStringUTFChars(jenv, arg, 0);
            values[i+1] = new_string(&ol, value); // no release "value" required?
        }
        else
        if ((*jenv)->IsInstanceOf(jenv, arg, Integer)) {
            jmethodID intValue = (*jenv)->GetMethodID(jenv, Integer, "intValue", "()I");
            jint value = (*jenv)->CallIntMethod(jenv, arg, intValue);

            values[i+1] = make_integer(value);
        }
        else
        if ((*jenv)->IsInstanceOf(jenv, arg, Float)) {
            jmethodID floatValue = (*jenv)->GetMethodID(jenv, Float, "floatValue", "()F");
            jint value = (*jenv)->CallFloatMethod(jenv, arg, floatValue);

            values[i+1] = new_rational(&ol, value);
        }
        else
        if ((*jenv)->IsInstanceOf(jenv, arg, Double)) {
            jmethodID doubleValue = (*jenv)->GetMethodID(jenv, Double, "doubleValue", "()D");
            jint value = (*jenv)->CallDoubleMethod(jenv, arg, doubleValue);

            values[i+1] = new_rational(&ol, value);
        }
        else
            values[i+1] = IFALSE;
    }

    word r = OL_continue(ol.vm, argc+1, (void**)values);
    if (r == ITRUE) {
        jclass Boolean = (*jenv)->FindClass(jenv, "java/lang/Boolean");
        jfieldID TRUE = (*jenv)->GetStaticFieldID(jenv, Boolean, "TRUE", "Ljava/lang/Boolean;");
            return (*jenv)->GetStaticObjectField(jenv, Boolean, TRUE);
    }
    if (is_number(r)) { // type-enum+, type-enum-, type-int+, type-int-
        jmethodID valueOf = (*jenv)->GetStaticMethodID(jenv, Integer, "valueOf", "(I)Ljava/lang/Integer;");
        return (*jenv)->CallStaticObjectMethod(jenv, Integer, valueOf, (void*)ol2int(r));
    }
	else
    if (is_string(r)) {
        size_t len = string_length(r);
        const char* value = string_value(r);
        jchar* chars = __builtin_alloca((len + 1) * sizeof(jchar));
        if (reftype(r) == 3) { // type-string
            for (int i = 0; i < len; i++)
                chars[i] = (jchar) value[i];
        }
        if (reftype(r) == 5) { // type-string-wide
            for (int i = 0; i < len; i++)
                chars[i] = (jchar) ol2int(((uintptr_t*)value)[i]);
        }
        chars[len] = 0;
        return (*jenv)->NewString(jenv, chars, len);
    }
    //    LOGI("arg1: %s", (char*)(*jenv)->GetObjectArrayElement(jenv, args, 1));

    // all other cases: false
    jclass Boolean = (*jenv)->FindClass(jenv, "java/lang/Boolean");
    jfieldID FALSE = (*jenv)->GetStaticFieldID(jenv, Boolean, "FALSE", "Ljava/lang/Boolean;");
    return (*jenv)->GetStaticObjectField(jenv, Boolean, FALSE);
}
