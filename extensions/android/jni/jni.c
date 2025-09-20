// Otus Lisp Java Native Interface
#include <jni.h>

// package name.otuslisp
#define NATIVE(name) Java_lang_otuslisp_Ol_ ## name

// ----------------
#include <stdint.h>
#include <stdlib.h>
#include <sys/stat.h>

#include <string.h>
#include <strings.h>

#include <fcntl.h>
#include <unistd.h>

#include <stdio.h>
#include <errno.h>

#include <ol/ol.h>

// ------------------------------------------------------------
// TODO: make dynamic
ol_t ol;
struct {
	// saved i/o functions:
	open_t* open;
	close_t* close;
	read_t* read;
	write_t* write;
	stat_t* stat;
} old;

// ---------------------------------------------------------------------------------
// logging
#ifdef __ANDROID__
#	include <android/log.h>
#	define LOG_NAME "ol"
#	define LOGD(...) __android_log_print(ANDROID_LOG_DEBUG,   LOG_NAME, __VA_ARGS__)
#	define LOGI(...) __android_log_print(ANDROID_LOG_INFO,    LOG_NAME, __VA_ARGS__)
#	define LOGE(...) __android_log_print(ANDROID_LOG_ERROR,   LOG_NAME, __VA_ARGS__)
#	define LOGV(...) __android_log_print(ANDROID_LOG_VERBOSE, LOG_NAME, __VA_ARGS__)
#else
#	define LOGD(...) printf(__VA_ARGS__)
#	define LOGI(...) printf(__VA_ARGS__)
#	define LOGE(...) printf(__VA_ARGS__)
#	define LOGV(...) printf(__VA_ARGS__)
#endif


///////////////////////////////////////////////////////////////////////////
// android i/o
#ifdef __ANDROID__
#include <android/asset_manager_jni.h>

static AAssetManager *asset_manager = NULL;
static AAsset **fds = NULL; // file descriptors
static int fds_size = 0;

JNIEXPORT
void jniSetAssetManager(jobject assetManager)
{
	asset_manager = assetManager;

	// file system init:
	fds_size = 16;
	if (fds)
		free(fds);
	fds = (AAsset **)calloc(fds_size, sizeof(*fds));
}

int assets_open(const char *filename, int flags, int mode, void *userdata)
{
	LOGD("assets_open(%s, %o, %o, %p)", filename, flags, mode, userdata);
	// try to open file as usual
	int file = old.open(filename, flags, mode, userdata);
	LOGV("open file: %s -> %d(%s)", filename, file, file != -1 ? "Ok" : strerror(errno));
	if (file != -1)
		return file;

	// no file? handle project assets
	// 1. remove "./" if any
	if (strncmp(filename, "./", 2) == 0)
		filename += 2;

	AAsset *asset = AAssetManager_open(asset_manager, filename, 0);
	LOGV("open asset: %s -> %p(%s)", filename, asset, asset != 0 ? "Ok" : "Fail");
	if (asset)
	{
		int i = 3; // 0, 1, 2 - reserved
		for (; i < fds_size; i++) {
			if (fds[i] == 0) {
				LOGV("asset %p -> file %d", asset, -i);
				fds[i] = asset;
				file = -i;
				break;
			}
		}
		// no available descriptors?
		if (i == fds_size) {
			int fds_size_new = fds_size * 5 / 8; // 1.6, ~1.618
			AAsset **fds_new = realloc(fds, fds_size_new);
			if (fds_new) {
				fds = fds_new;
				fds_size = fds_size_new;

				LOGV("asset %p -> file %d", asset, -i);
				fds[i] = asset;
				file = -i;
			}
		}
	}

	return file;
}

int assets_close(int fd, void *userdata)
{
	LOGD("close file(%d, %p)", fd, userdata);
	// assets ?
	if (fd < 0) {
		fd = -fd;
		if (fd < fds_size) {
			AAsset *aa = fds[fd];
			if (aa != 0) {
				AAsset_close(aa);
				fds[fd] = 0;
				return 0;
			}
		}
		// error:
		return -1;
	}
	// regular file descriptor
	else
		return old.close(fd, userdata);
}

ssize_t assets_read(int fd, void *buf, size_t count, void *userdata)
{
	//LOGD("read file: %d, %p, %d", fd, buf, (int)count);
	// assets ?
	if (fd < 0) {
		fd = -fd;
		if (fd < fds_size) {
			AAsset *aa = fds[fd];
			if (aa != 0) {
				//LOGD("    asset: %p", aa);
				int rr = AAsset_read(aa, buf, count);
				//LOGD("    return %d", rr);
				return rr;
			}
		}
		// error:
		//LOGD("    return %d", -1);
		return -1;
	}
	// regular file descriptor
	else {
		int rr = old.read(fd, buf, count, userdata);
		//LOGD("    return %d", rr);
		return rr;
	}
}

// redirect stdout/stderr to logcat!
ssize_t assets_write(int fd, void *buf, size_t count, void *userdata)
{
	(void)userdata;

	switch (fd) {
		case 0: // stdin means "debug" (?)
			LOGD("%.*s", (int)count, (char *)buf);
			return count;
		case 1: // stdout means "info"
			LOGI("%.*s", (int)count, (char *)buf);
			return count;
		case 2: // stderr means "error"
			LOGE("%.*s", (int)count, (char *)buf);
			return count;
		default:
			;// nothing
	}
	// default:
	return old.write(fd, buf, count, userdata);
}

// todo: handle folders (AAssetDir)
int assets_stat(const char *filename, struct stat *st, void* userdata)
{
	LOGD("assets_stat(%s, %p, %p)", filename, st, userdata);
	// try to system stat
	int ret = old.stat(filename, st, userdata);
	if (ret >= 0)
		return ret;

	// no file? handle project assets
	if (!st) return -1;
	AAsset *asset = AAssetManager_open(asset_manager, filename, 0);
	if (!asset)
		return -1;

	bzero(st, sizeof(*st));
	st->st_mode |= 0100000; // file
	st->st_size = AAsset_getLength(asset);

	AAsset_close(asset);
	return 0;
}
#endif

// ========================================================================
// #ifndef OL_HOME
// #define OL_HOME "/sdcard/ol"
// #endif//OL_HOME

#ifdef REPL
	extern unsigned char REPL[];
#endif

#ifdef __ANDROID__
// static jobject java_asset_manager = NULL;
JNIEXPORT void JNICALL NATIVE(nativeSetAssetManager)(JNIEnv *jenv, jobject jobj, jobject assetManager)
{
	LOGD("> nativeSetAssetManager()");
	jobject java_asset_manager = (*jenv)->NewGlobalRef(jenv, assetManager);
	jobject asset_manager = AAssetManager_fromJava(jenv, java_asset_manager);

	jniSetAssetManager(asset_manager);
	LOGV("< nativeSetAssetManager()");
}
#endif

JNIEXPORT
void jniNew()
{
	ol.vm = OLVM_new(REPL);
	ol.eval = 0;
	OLVM_userdata(ol.vm, ol.vm);

#ifdef __ANDROID__
	old.open = OLVM_set_open(ol.vm, assets_open);
	old.close = OLVM_set_close(ol.vm, assets_close);
	old.read = OLVM_set_read(ol.vm, assets_read);
	old.write = OLVM_set_write(ol.vm, assets_write); // stdout/stderr to logcat redirector included
	old.stat = OLVM_set_stat(ol.vm, assets_stat);
#endif
}

JNIEXPORT
void jniNewEmbed()
{
	OL_new(&ol, REPL);
	OLVM_userdata(ol.vm, &ol);

#ifdef __ANDROID__
	old.open = OLVM_set_open(ol.vm, assets_open);
	old.close = OLVM_set_close(ol.vm, assets_close);
	old.read = OLVM_set_read(ol.vm, assets_read);
	old.write = OLVM_set_write(ol.vm, assets_write); // stdout/stderr to logcat redirector included
	old.stat = OLVM_set_stat(ol.vm, assets_stat);
#endif
}

JNIEXPORT
void jniRun(int argc, char** argv)
{
	(void)OLVM_run(ol.vm, argc, argv);
}

JNIEXPORT void JNICALL NATIVE(nativeNew)(JNIEnv *jenv, jobject class)
{
	LOGD("> nativeNew()");
	// setenv("OL_HOME", OL_HOME, 0);
	jniNewEmbed();
	LOGV("< nativeNew()");
	return;
}

JNIEXPORT
void jniDelete()
{
	OL_delete(&ol);
}

JNIEXPORT void JNICALL NATIVE(nativeDelete)(JNIEnv *jenv, jobject class)
{
	LOGD("> nativeDelete()");
	jniDelete();
	LOGV("< nativeDelete()");
}

JNIEXPORT jobject JNICALL NATIVE(eval)(JNIEnv *jenv, jobject jobj, jarray args)
{
	LOGV("> eval()");

	jint argc = (*jenv)->GetArrayLength(jenv, args);
	LOGV("  argumets count: %d", argc);

	// TODO: optimize
	jclass String = (*jenv)->FindClass(jenv, "java/lang/String");
	jclass Integer = (*jenv)->FindClass(jenv, "java/lang/Integer");
	jclass Float = (*jenv)->FindClass(jenv, "java/lang/Float");
	jclass Double = (*jenv)->FindClass(jenv, "java/lang/Double");
	jclass Boolean = (*jenv)->FindClass(jenv, "java/lang/Boolean");

	// convert Java arguments into internal Lisp arguments:
	uintptr_t *values = __builtin_alloca(argc * sizeof(uintptr_t));
	for (int i = 0; i < argc; i++)
	{
		jobject arg = (*jenv)->GetObjectArrayElement(jenv, args, i);
		// if((*env)->ExceptionOccurred(env)) {
		//     break;
		// }
		if (0) {
			LOGE("Struck by lightning, struck by lightning!"); // assert
		}
		else if ((*jenv)->IsInstanceOf(jenv, arg, String))
		{
			char *value = (char *)(*jenv)->GetStringUTFChars(jenv, arg, 0);

			LOGV("  char* %d: %s", i, value);
			values[i] = new_string(&ol, value); // no release "value" required?
		}
		else if ((*jenv)->IsInstanceOf(jenv, arg, Integer))
		{
			jmethodID intValue = (*jenv)->GetMethodID(jenv, Integer, "intValue", "()I");
			jint value = (*jenv)->CallIntMethod(jenv, arg, intValue);

			LOGV("  int %d: %d", i, value);
			values[i] = make_integer(value);
		}
		else if ((*jenv)->IsInstanceOf(jenv, arg, Boolean))
		{
			jmethodID booleanValue = (*jenv)->GetMethodID(jenv, Boolean, "booleanValue", "()Z");
			jboolean value = (*jenv)->CallBooleanMethod(jenv, arg, booleanValue);

			LOGV("  bool %d: %d", i, value == JNI_TRUE);
			values[i] = (value == JNI_TRUE) ? 0x136 : 0x036; // #true : #false
		}
		else if ((*jenv)->IsInstanceOf(jenv, arg, Float))
		{
			jmethodID floatValue = (*jenv)->GetMethodID(jenv, Float, "floatValue", "()F");
			jfloat value = (*jenv)->CallFloatMethod(jenv, arg, floatValue);

			LOGV("  float %d: %f", i, value);
			values[i] = new_rational(&ol, value);
		}
		else if ((*jenv)->IsInstanceOf(jenv, arg, Double))
		{
			jmethodID doubleValue = (*jenv)->GetMethodID(jenv, Double, "doubleValue", "()D");
			jdouble value = (*jenv)->CallDoubleMethod(jenv, arg, doubleValue);

			LOGV("  double %d: %f", i, value);
			values[i] = new_rational(&ol, value);
		}
		else {
			LOGE("  ? %d: unsupported type", i);
			values[i] = IFALSE;
		}
	}

	// make arguments list:
	uintptr_t *fp;
	uintptr_t userdata = 0x236; // #null

	fp = ol.vm->fp;
	for (int i = argc - 1; i >= 0; i--, fp += 3) {
		fp[0] = 0x30006; // TPAIR
		fp[1] = values[i];
		fp[2] = userdata;
		userdata = (uintptr_t)fp;
	}
	ol.vm->fp = fp;

	// eval:
	uintptr_t r = OLVM_evaluate(ol.vm,
	                 OLVM_deref(ol.vm, ol.eval),
	                 1, &userdata);
	LOGV("  eval = %p", (void*)r);

	// process result:
	if (r == ITRUE)
	{
		jclass Boolean = (*jenv)->FindClass(jenv, "java/lang/Boolean");
		jfieldID TRUE = (*jenv)->GetStaticFieldID(jenv, Boolean, "TRUE", "Ljava/lang/Boolean;");
		return (*jenv)->GetStaticObjectField(jenv, Boolean, TRUE);
	}
	if (is_number(r))
	{ // type-value+, type-value-, type-integer+, type-integer-
		jmethodID valueOf = (*jenv)->GetStaticMethodID(jenv, Integer, "valueOf", "(I)Ljava/lang/Integer;");
		return (*jenv)->CallStaticObjectMethod(jenv, Integer, valueOf, (void *)ol2int(r));
	}
	if (is_string(r))
	{
		size_t len = string_length(r);
		const char *value = string_value(r);
		jchar *chars = __builtin_alloca((len + 1) * sizeof(jchar));
		if (reftype(r) == 3)
		{ // type-string
			for (int i = 0; i < len; i++)
				chars[i] = (jchar)value[i];
		}
		if (reftype(r) == 5)
		{ // type-string-wide
			for (int i = 0; i < len; i++)
				chars[i] = (jchar)ol2int(((uintptr_t *)value)[i]);
		}
		chars[len] = 0;
		return (*jenv)->NewString(jenv, chars, len);
	}
	//    LOGI("arg1: %s", (char*)(*jenv)->GetObjectArrayElement(jenv, args, 1));

	// else: return #false
	jfieldID FALSE = (*jenv)->GetStaticFieldID(jenv, Boolean, "FALSE", "Ljava/lang/Boolean;");
	LOGV("< eval()");
	return (*jenv)->GetStaticObjectField(jenv, Boolean, FALSE);
}
