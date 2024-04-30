//package github.otus_lisp.ol
#define NATIVE(name) Java_github_otus_1lisp_ol_Olvm_ ## name

// Otus Lisp Java Native Interface
#include <jni.h>

#include <android/log.h>
#define APP_NAME "ol"
#define LOGD(...) __android_log_print(ANDROID_LOG_DEBUG,   APP_NAME, __VA_ARGS__)
#define LOGI(...) __android_log_print(ANDROID_LOG_INFO,    APP_NAME, __VA_ARGS__)
#define LOGE(...) __android_log_print(ANDROID_LOG_ERROR,   APP_NAME, __VA_ARGS__)
#define LOGV(...) __android_log_print(ANDROID_LOG_VERBOSE, APP_NAME, __VA_ARGS__)

// ----------------
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include <strings.h>

#include <fcntl.h>
#include <unistd.h>

#include <errno.h>
#include <string.h>

//////////////////////////////////////////////////////////////////////
// input/output
#include <android/asset_manager_jni.h>

static jobject java_asset_manager = NULL;
static AAssetManager *asset_manager = NULL;
// static AAsset **fds = NULL; // file descriptors
// static int fds_size = 0;

JNIEXPORT void JNICALL NATIVE(nativeSetAssetManager)(JNIEnv *jenv, jobject jobj, jobject assetManager)
{
	LOGD("> nativeSetAssetManager()");
	java_asset_manager = (*jenv)->NewGlobalRef(jenv, assetManager);
	asset_manager = AAssetManager_fromJava(jenv, java_asset_manager);
	LOGD("< nativeSetAssetManager()");
}

int assets_open(const char *filename, int flags, int mode, void *userdata)
{
	char *filename1 = (char*)filename;

	int file = open(filename1, flags, mode);
	LOGD("open file:1: %s -> %d(%s)", filename1, file, file != -1 ? "Ok" : strerror(errno));
	if (file != -1)
		return file;

	// no file, try to open $OL_HOME/file
	char *home = getenv("OL_HOME");
	char *filename2 = (char *)(__builtin_alloca(strlen(home) + strlen(filename) + 2));
	snprintf(filename2, strlen(home) + strlen(filename) + 2, "%s/%s", home, filename);

	file = open(filename2, flags, mode);
	LOGD("open file:2: %s -> %d(%s)", filename2, file, file != -1 ? "Ok" : strerror(errno));
	if (file != -1)
		return file;

	// TODO: handle java assets

	return file;
}

// todo: assets_close
// todo: assets_read

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
	return write(fd, buf, count);
}

// ========================================================================
#include <ol/ol.h>

ol_t ol; // TODO: make dynamic
// ------------------------------------------------------------------------

#ifndef OL_HOME
#define OL_HOME "/sdcard/ol"
#endif//OL_HOME

#ifdef REPL
	extern unsigned char REPL[];
#endif

// public static native void nativeNew();
JNIEXPORT void JNICALL NATIVE(nativeNew)(JNIEnv *jenv, jobject class)
{
	LOGD("> nativeNew()");
	setenv("OL_HOME", OL_HOME, 0);

	OL_new(&ol, REPL);
	OLVM_userdata(ol.vm, &ol);

	//OLVM_set_open(ol.vm, assets_open);
	//OLVM_set_close(ol.vm, assets_close);
	//OLVM_set_read(ol.vm, assets_read);

	// stdout/stderr to logcat redirector
	OLVM_set_write(ol.vm, assets_write);

	LOGD("< nativeNew()");
	return;
}

JNIEXPORT void JNICALL NATIVE(nativeDelete)(JNIEnv *jenv, jobject class)
{
	LOGD("> nativeDelete()");
	OL_delete(&ol);
	LOGD("< nativeDelete()");
}

JNIEXPORT jobject JNICALL NATIVE(eval)(JNIEnv *jenv, jobject jobj, jarray args)
{
	LOGD("> eval()");

	jint argc = (*jenv)->GetArrayLength(jenv, args);
	LOGD("  argumets count: %d", argc);

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
			LOGD("Struck by lightning, struck by lightning!"); // assert
		}
		else if ((*jenv)->IsInstanceOf(jenv, arg, String))
		{
			char *value = (char *)(*jenv)->GetStringUTFChars(jenv, arg, 0);

			LOGD("  char* %d: %s", i, value);
			values[i] = new_string(&ol, value); // no release "value" required?
		}
		else if ((*jenv)->IsInstanceOf(jenv, arg, Integer))
		{
			jmethodID intValue = (*jenv)->GetMethodID(jenv, Integer, "intValue", "()I");
			jint value = (*jenv)->CallIntMethod(jenv, arg, intValue);

			LOGD("  int %d: %d", i, value);
			values[i] = make_integer(value);
		}
		else if ((*jenv)->IsInstanceOf(jenv, arg, Boolean))
		{
			jmethodID booleanValue = (*jenv)->GetMethodID(jenv, Boolean, "booleanValue", "()Z");
			jboolean value = (*jenv)->CallBooleanMethod(jenv, arg, booleanValue);

			LOGD("  bool %d: %d", i, value == JNI_TRUE);
			values[i] = (value == JNI_TRUE) ? 0x136 : 0x036; // #true : #false
		}
		else if ((*jenv)->IsInstanceOf(jenv, arg, Float))
		{
			jmethodID floatValue = (*jenv)->GetMethodID(jenv, Float, "floatValue", "()F");
			jfloat value = (*jenv)->CallFloatMethod(jenv, arg, floatValue);

			LOGD("  float %d: %f", i, value);
			values[i] = new_rational(&ol, value);
		}
		else if ((*jenv)->IsInstanceOf(jenv, arg, Double))
		{
			jmethodID doubleValue = (*jenv)->GetMethodID(jenv, Double, "doubleValue", "()D");
			jdouble value = (*jenv)->CallDoubleMethod(jenv, arg, doubleValue);

			LOGD("  double %d: %f", i, value);
			values[i] = new_rational(&ol, value);
		}
		else {
			LOGE("  ? %d: unsupported type", i);
			values[i] = IFALSE;
		}
	}

	// make arguments list:
	uintptr_t userdata = 0x236; // #null
	uintptr_t *fp = ol.vm->fp;
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
	LOGD("  eval = %p", (void*)r);

	// process result:
	if (r == ITRUE)
	{
		jclass Boolean = (*jenv)->FindClass(jenv, "java/lang/Boolean");
		jfieldID TRUE = (*jenv)->GetStaticFieldID(jenv, Boolean, "TRUE", "Ljava/lang/Boolean;");
		return (*jenv)->GetStaticObjectField(jenv, Boolean, TRUE);
	}
	if (is_number(r))
	{ // type-enum+, type-enum-, type-int+, type-int-
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
	LOGI("< eval()");
	return (*jenv)->GetStaticObjectField(jenv, Boolean, FALSE);
}
