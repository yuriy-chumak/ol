#include <sources/android/native_app_glue/android_native_app_glue.c>

//////////////////////////////////////////////////////////////////////
// To allow easier porting to android, we allow the user to define a
// main function which we call from android_main, defined by ourselves
extern int main(int argc, char *argv[]);

struct android_app *application;
void android_main(struct android_app *appl)
{
    char arg0[] = "ol";
    application = appl;

    (void)main(1, (char *[]){ arg0, NULL });
}

// // NOTE: Add this to header (if apps really need it)
// struct android_app *GetAndroidApp(void)
// {
//     return application;
// }

//////////////////////////////////////////////////////////////////////
// Otus Lisp
#include <fcntl.h>
#include <errno.h>
#include <string.h>

//////////////////////////////////////////////////////////////////////
// Android <-> Ol interop
#include <android/log.h>

#define DLOG(...) __android_log_print(ANDROID_LOG_DEBUG, "ol", __VA_ARGS__)
#define ILOG(...) __android_log_print(ANDROID_LOG_INFO, "ol", __VA_ARGS__)
#define ELOG(...) __android_log_print(ANDROID_LOG_ERROR, "ol", __VA_ARGS__)

int assets_open(const char *filename, int flags, int mode, void *userdata)
{
	char *filename1 = (char*)filename;

	int file = open(filename1, flags, mode);
	DLOG("open file.1: %s -> %d(%s)", filename1, file, file != -1 ? "ok" : strerror(errno));
	if (file != -1)
		return file;

	// no file, try to open $OL_HOME/file
	char *home = getenv("OL_HOME");
	char *filename2 = (char *)(__builtin_alloca(strlen(home) + strlen(filename) + 2));
	snprintf(filename2, strlen(home) + strlen(filename) + 2, "%s/%s", home, filename);

	file = open(filename2, flags, mode);
	DLOG("open file.2: %s -> %d(%s)", filename2, file, file != -1 ? "ok" : strerror(errno));
	if (file != -1)
		return file;

	// // at least try to read assets

	// // remove "./"
	// if (strncmp(filename, "./", 2) == 0)
	// 	filename += 2;

	// AAsset *asset = AAssetManager_open(asset_manager, filename, 0);
	// LOGD("open asset: %s -> %p", filename, asset);
	// if (asset)
	// {
	// 	int i = 3; // 0, 1, 2 - reserved
	// 	for (; i < fds_size; i++)
	// 	{
	// 		if (fds[i] == 0)
	// 		{
	// 			LOGD("   %d/%p", i, asset);
	// 			fds[i] = asset;
	// 			file = -i;
	// 			break;
	// 		}
	// 	}
	// 	// no available descriptors?
	// 	if (i == fds_size)
	// 	{
	// 		int fds_size_new = fds_size * 5 / 8; // 1.6, ~1.618
	// 		AAsset **fds_new = realloc(fds, fds_size_new);
	// 		if (fds_new)
	// 		{
	// 			fds = fds_new;
	// 			fds_size = fds_size_new;

	// 			fds[i] = asset;
	// 			file = -i;
	// 		}
	// 	}
	// }

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
			DLOG("%.*s", (int)count, (char *)buf);
			return count;
		case 1: // stdout means "info"
			ILOG("%.*s", (int)count, (char *)buf);
			return count;
		case 2: // stderr means "error"
			ELOG("%.*s", (int)count, (char *)buf);
			return count;
		default:
			;// nothing
	}
	// default:
	return write(fd, buf, count);
}

//////////////////////////////////////////////////////////////////////
// main
#include <ol/ol.h>

#ifndef OL_HOME
#define OL_HOME "/sdcard/ol"
#endif//OL_HOME
extern unsigned char REPL[];

int main(int argc, char * argv[])
{
	setenv("OL_HOME", OL_HOME, 0);

	// create ol
	struct olvm_t* olvm = OLVM_new(REPL);

	// i/o setup
	OLVM_userdata(olvm, olvm);

	OLVM_set_open(olvm, assets_open);
	// OLVM_set_close(ol.vm, assets_close);
	// OLVM_set_read(ol.vm, assets_read);

	// stdout/stderr to logcat redirector
	OLVM_set_write(olvm, assets_write);

	// run
    (void)OLVM_run(olvm, 1, (char *[]) {
		"main.lisp", NULL
	});
	// cleanup
	OLVM_delete(olvm);

	return 0;
}
