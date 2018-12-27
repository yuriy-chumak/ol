// Otus Lisp Java (Android) Interface

#include <stdint.h>
#include <stdlib.h>
#include <jni.h>
#include <android/native_window.h> // requires ndk r5 or newer
#include <android/native_window_jni.h> // requires ndk r5 or newer
#include <stdio.h>

#include <strings.h>
#include <android/log.h>
#include <pthread.h>

#include <olvm.h>
#include <fcntl.h>

#include <unistd.h>

#include <EGL/eglplatform.h>
// #include <zip.h>

extern unsigned char _binary_repl_start[]; // otus lisp binary (please, build and link repl.o)

#define LOGD(...) //__android_log_print(ANDROID_LOG_DEBUG, "ol", __VA_ARGS__)
#define LOGI(...) __android_log_print(ANDROID_LOG_INFO, "ol", __VA_ARGS__)
#define LOGE(...) __android_log_print(ANDROID_LOG_ERROR, "ol", __VA_ARGS__)

struct vm_t {
	struct ol_t* ol;
	pthread_t thread;

	pthread_mutex_t mutex;
	pthread_cond_t barrier;
	int suspended;
	int please_stop;
} vm;

struct event_t {
	int button;
	int x;
	int y;
} events[128];
int event_head = 0;
int event_tail = 0;

void push_event(int button, int x, int y)
{
	int tail = event_tail;
	events[tail].button = button;
	events[tail].x = x;
	events[tail].y = y;

	event_tail = (tail+1) % (sizeof(events)/sizeof(events[0]));
}

void idler(void* userdata)
{
	// this is safe to call pthread mutex at this point
	// because this point executes not so often
	//sched_yield();

	if (vm.please_stop > 0)
		pthread_exit(0);

	// while (__sync_sub_and_fetch(&vm.suspended, 0) > 0) {
	// 	pthread_mutex_lock(&vm.mutex);
	// 	pthread_cond_wait(&vm.barrier, &vm.mutex);
	// 	pthread_mutex_unlock(&vm.mutex);
	// }
}

//pthread_mutex_t _mutex;

char* apk_location = 0;
char* ol_home = 0;
char* executable = 0;

// struct zip_t *apk;
int openx(const char *filename, int flags, int mode, void* userdata)
{
	LOGD("open0: %s", filename);
	//int error = zip_entry_open(apk, filename);
	int file = open(filename, flags, mode);
	LOGI("open0: %s(%d)", filename, file);
	return file;
}

// redirect stdout/stderr to logcat
ssize_t logx(int fd, void *buf, size_t count, void* userdata)
{
	if (fd == 1) { // stdout
		LOGI("%.*s", count, (char*)buf);
		return count;
	}
	if (fd == 2) { // stderr
		LOGE("%.*s", count, (char*)buf);
		return count;
	}
	return write(fd, buf, count);
}

// ---------------------------------------
void* threadStartCallback(void* userdata)
{
	unsigned char* bootstrap = _binary_repl_start;

	pthread_mutex_init(&vm.mutex, 0);
	pthread_cond_init(&vm.barrier, 0);

	if (ol_home)
		setenv("OL_HOME", ol_home, 1);

	struct ol_t* ol;

	ol = OL_new(bootstrap);
	OL_set_write(ol, logx); // redirects to logcat
	OL_set_open(ol, openx); // todo: add reading zip archive

	OL_set_idle(ol, idler); // todo: add reading zip archive

	vm.ol = ol;
	vm.suspended = 1; // initially suspended
	vm.please_stop = 0;

	char* args[] = { "#", executable };
	uintptr_t r = OL_run(ol, sizeof(args)/sizeof(args[0]), args);

	pthread_cond_destroy(&vm.barrier);
	pthread_mutex_destroy(&vm.mutex);

	if (apk_location) free(apk_location);
	if (ol_home) free(ol_home);
	if (executable) free(executable);
	pthread_exit(0);
}

// ---------------------------------------
// support functions:
static ANativeWindow *window = 0;

JNIEXPORT EGLNativeWindowType androidGetWindow()
{
	LOGI("window = %p", window);
	return window;
}

JNIEXPORT struct event_t* androidPopEvent()
{
	if (event_head == event_tail)
		return NULL;
	int head = event_head;
	LOGI("androidPopEvent(): has event %d,%d", events[head].x, events[head].y);
	event_head = (head+1) % (sizeof(events)/sizeof(events[0]));
	return &events[head];
}

#include <ft2build.h>
#include <freetype/freetype.h>
// ---------------------------------------

JNIEXPORT void JNICALL Java_name_yuriy_1chumak_ol_MainActivity_nativeOnStart(JNIEnv* jenv, jobject obj)
{
	LOGI("nativeOnStart");
	//pthread_mutex_init(&_mutex, 0);

	LOGI("sizeof(FT_FaceRec): %d", sizeof(FT_FaceRec));
	LOGI("offsetof(FT_FaceRec, glyph): %d", offsetof(FT_FaceRec, glyph));

	LOGI("sizeof(FT_GlyphSlotRec): %d", sizeof(FT_GlyphSlotRec));
	LOGI("offsetof(FT_GlyphSlotRec, bitmap): %d", offsetof(FT_GlyphSlotRec, bitmap));
	LOGI("offsetof(FT_GlyphSlotRec, bitmap_left): %d", offsetof(FT_GlyphSlotRec, bitmap_left));
	LOGI("offsetof(FT_GlyphSlotRec, bitmap_top): %d", offsetof(FT_GlyphSlotRec, bitmap_top));
	LOGI("sizeof(FT_Bitmap): %d", sizeof(FT_Bitmap));
	LOGI("offsetof(FT_Bitmap, rows): %d", offsetof(FT_Bitmap, rows));
	LOGI("offsetof(FT_Bitmap, width): %d", offsetof(FT_Bitmap, width));
	LOGI("offsetof(FT_Bitmap, pitch): %d", offsetof(FT_Bitmap, pitch));
	LOGI("offsetof(FT_Bitmap, buffer): %d", offsetof(FT_Bitmap, buffer));

	// let's start our application
	pthread_create(&vm.thread, 0, threadStartCallback, 0);
	return;
}

JNIEXPORT void JNICALL Java_name_yuriy_1chumak_ol_MainActivity_nativeOnResume(JNIEnv* jenv, jobject obj)
{
	LOGI("nativeOnResume");

	// resume:
	if (__sync_sub_and_fetch(&vm.suspended, 1) == 0) {
		pthread_mutex_lock(&vm.mutex);
		pthread_cond_signal(&vm.barrier);
		pthread_mutex_unlock(&vm.mutex);
	}

	return;
}

JNIEXPORT void JNICALL Java_name_yuriy_1chumak_ol_MainActivity_nativeOnPause(JNIEnv* jenv, jobject obj)
{
	LOGI("nativeOnPause");

	// // pause:
	__sync_add_and_fetch(&vm.suspended, 1);
	return;
}

JNIEXPORT void JNICALL Java_name_yuriy_1chumak_ol_MainActivity_nativeOnStop(JNIEnv* jenv, jobject obj)
{
	LOGI("nativeOnStop");
	vm.please_stop = 1;

	pthread_kill(vm.thread, SIGKILL); // stop the thread
	pthread_join(vm.thread, 0);
	return;
}

JNIEXPORT void JNICALL Java_name_yuriy_1chumak_ol_MainActivity_nativeSetSurface(JNIEnv* jenv, jobject obj, jobject surface)
{
	if (surface != 0) {
		window = ANativeWindow_fromSurface(jenv, surface);
		LOGI("Received new window %p", window);
	} else {
		LOGI("Releasing window %p", window);
		ANativeWindow_release(window);
	}

	return;
}

JNIEXPORT void JNICALL Java_name_yuriy_1chumak_ol_MainActivity_nativeSetApkLocation(JNIEnv* jenv, jobject obj, jstring apkLocation)
{
	const char *string = (*jenv)->GetStringUTFChars(jenv, apkLocation, 0);
	LOGI("nativeSetApkLocation: %s", string);
	apk_location = strdup(string);
	(*jenv)->ReleaseStringUTFChars(jenv, apkLocation, string);

	// int errorp = 0;
	// apk = zip_open(apk_location, 0, 'r');
	// if (errorp)
	// 	LOGE("Can't open apk (%s) file to read", apk_location);
}

JNIEXPORT void JNICALL Java_name_yuriy_1chumak_ol_MainActivity_nativeSetOlHome(JNIEnv* jenv, jobject obj, jstring olHome)
{
	const char *string = (*jenv)->GetStringUTFChars(jenv, olHome, 0);
	LOGI("nativeSetOlHome: %s", string);
	ol_home = strdup(string);
	(*jenv)->ReleaseStringUTFChars(jenv, olHome, string);
}

JNIEXPORT void JNICALL Java_name_yuriy_1chumak_ol_MainActivity_nativeSetExecutable(JNIEnv* jenv, jobject obj, jstring olHome)
{
	const char *string = (*jenv)->GetStringUTFChars(jenv, olHome, 0);
	LOGI("nativeSetExecutable: %s", string);
	executable = strdup(string);
	(*jenv)->ReleaseStringUTFChars(jenv, olHome, string);
}

JNIEXPORT void JNICALL Java_name_yuriy_1chumak_ol_MainActivity_nativePostEvent(JNIEnv* jenv, jobject obj, jint button, jint x, jint y)
{
	LOGI("nativePostEvent: %d,%d", x, y);
	push_event(button, x, y);
}
