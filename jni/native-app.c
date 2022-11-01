#include <sources/android/native_app_glue/android_native_app_glue.c>

#include <android/log.h>
#define APP_NAME "ol"
#define DLOG(...) __android_log_print(ANDROID_LOG_DEBUG,   APP_NAME, __VA_ARGS__)
#define ILOG(...) __android_log_print(ANDROID_LOG_INFO,    APP_NAME, __VA_ARGS__)
#define ELOG(...) __android_log_print(ANDROID_LOG_ERROR,   APP_NAME, __VA_ARGS__)
#define VLOG(...) __android_log_print(ANDROID_LOG_VERBOSE, APP_NAME, __VA_ARGS__)
#define FLOG(...) __android_log_print(ANDROID_LOG_FATAL,   APP_NAME, __VA_ARGS__)
/////////////////////////////////////////////////////////////////////////////////

// Public API
void anlSwapBuffers();
void anlProcessEvents();
#include <GLES/gl.h>
void anlBindFramebuffer(GLenum target, GLuint framebuffer);

// public api: get left eye matrix
// public api: get right eye matrix

//////////////////////////////////////////
// portable api
int Oculus = 0; // Running under Oculus Go
int Opengl = 0;
extern void initialize_gl4es(); // gl4es

extern int oculusgo_init(struct android_app *app);
extern int opengles_init(struct android_app *app);
static void init(struct android_app *app)
{
	ILOG("Init.");
	if ((Oculus = oculusgo_init(app)))
		return;
	if ((Opengl = opengles_init(app)))
		return;
	FLOG("No GL backend supported!!!");
	exit(-1);
}

extern void oculusgo_done(struct android_app *app);
extern void opengles_done(struct android_app *app);
static void done(struct android_app *app)
{
	if (Oculus) oculusgo_done(app);
	if (Opengl) opengles_done(app);
}

extern void oculusgo_swap(void);
extern void opengles_swap(void);
static void swap(void)
{
	if (Oculus) oculusgo_swap();
	if (Opengl) opengles_swap();
}

static void handle_input(void)
{
	if (Oculus) 0;
	if (Opengl) 0;
}

// todo: или glFlush, может?
void anlBindFramebuffer(GLenum target, GLuint framebuffer)
{
	//if (target ==
}

// -------------------------------
// private api
static void AndroidLifecycleCallback(struct android_app *app, int32_t cmd);       // Process Android activity lifecycle commands
static int32_t AndroidInputCallback(struct android_app *app, AInputEvent *event); // Process Android inputs

//////////////////////////////////////////////////////////////////////
// To allow easier porting to android, we allow the user to define a
// main function which we call from android_main, defined by ourselves
extern int main(int argc, char *argv[]);

typedef int bool;
#define true 1
#define false 0

#include <dlfcn.h>

struct android_app *application;
struct android_poll_source* poll_source;
bool windowReady = false;
bool appEnabled = false;

// main
void android_main(struct android_app *app)
{
    application = app;

    // ANativeActivity_setWindowFlags(appl->activity, AWINDOW_FLAG_FULLSCREEN, 0);
	// AConfiguration_setOrientation(appl->config, ACONFIGURATION_ORIENTATION_LAND);

	// -----------------------------------------------------------------------------

	// AConfiguration_getOrientation
	// AConfiguration_getDensity
	// AConfiguration_getScreenSize
	// ...
	// Screen.width = ANativeWindow_getWidth(appl->window);
	// Screen.height = ANativeWindow_getHeight(appl->window);

	// android native code handlers
    application->onAppCmd = AndroidLifecycleCallback;
    application->onInputEvent = AndroidInputCallback;

    // Initialize assets manager
    // InitAssetManager(appl->activity->assetManager, appl->activity->internalDataPath);

	// CORE.Storage.basePath = appl->activity->internalDataPath;

    // Android ALooper_pollAll() variables
    int pollResult = 0;
    int pollEvents = 0;

    // Wait for window to be initialized (display and context)
    // NOTE: Never close window, native activity is controlled by the system!
    while (!windowReady)
        while ((pollResult = ALooper_pollAll(0, NULL, &pollEvents, (void**)&poll_source)) >= 0)
            if (poll_source != NULL) poll_source->process(application, poll_source);

	init(app);

	void (*initialize_gl4es)() = dlsym(dlopen("libgl4es.so", RTLD_LAZY), "initialize_gl4es");
	if (initialize_gl4es)
		initialize_gl4es();

    (void) main(1, (char *[]){ "ol", NULL });
	done(app);
}

//////////////////////////////////////////////////////////////////////
// process android lifecycle
#include <android/window.h>

// Android GL Swap Buffers
void anlSwapBuffers(void)
{
	swap();
	//eglSwapBuffers(Display, Surface);
}

void anlProcessEvents(void)
{
    // Android ALooper_pollAll() variables
    int pollResult = 0;
    int pollEvents = 0;

    // Poll Events (registered events)
    // NOTE: Activity is paused if not enabled (CORE.Android.appEnabled)
    while ((pollResult = ALooper_pollAll(appEnabled ? 0 : -1, NULL, &pollEvents, (void**)&poll_source)) >= 0) {
        if (poll_source != NULL)
			poll_source->process(application, poll_source);
	}
}

// ANDROID: Process activity lifecycle commands
// упростим код по максимуму, а значит не будем обрабатывать команды
// типа APP_CMD_GAINED_FOCUS и т.д.
bool contextRebindRequired = false;
static void AndroidLifecycleCallback(struct android_app *app, int32_t cmd)
{
    switch (cmd)
    {
        case APP_CMD_START:          ILOG("APP_CMD_START");
			break;
        case APP_CMD_RESUME:         ILOG("APP_CMD_RESUME");
			break;
        case APP_CMD_GAINED_FOCUS:   ILOG("APP_CMD_GAINED_FOCUS");
            appEnabled = true;
			break;
        case APP_CMD_LOST_FOCUS:     ILOG("APP_CMD_LOST_FOCUS");
            appEnabled = false;
        	break;
        case APP_CMD_PAUSE:          ILOG("APP_CMD_PAUSE");
			break;

        case APP_CMD_SAVE_STATE:     ILOG("APP_CMD_SAVE_STATE");
			break;
        case APP_CMD_STOP:           ILOG("APP_CMD_STOP");
			break;

        case APP_CMD_DESTROY:        ILOG("APP_CMD_DESTROY");
        	break;
        case APP_CMD_CONFIG_CHANGED: ILOG("APP_CMD_CONFIG_CHANGED");
        	break;

        case APP_CMD_INIT_WINDOW:    ILOG("APP_CMD_INIT_WINDOW");
			windowReady = true;
        	break;

        case APP_CMD_TERM_WINDOW:    ILOG("APP_CMD_TERM_WINDOW");
			windowReady = false;
        	break;

        default: break;
    }
}

static int32_t AndroidInputCallback(struct android_app *app, AInputEvent *event)
{
    int type = AInputEvent_getType(event);
    int source = AInputEvent_getSource(event);

	DLOG("type: %d, source: %d", type, source);
    if (type == AINPUT_EVENT_TYPE_MOTION)
    {
        if (((source & AINPUT_SOURCE_JOYSTICK) == AINPUT_SOURCE_JOYSTICK) ||
            ((source & AINPUT_SOURCE_GAMEPAD) == AINPUT_SOURCE_GAMEPAD))
        {
			// ...
            return 1; // Handled gamepad axis motion
		}
	}
    else if (type == AINPUT_EVENT_TYPE_KEY)
    {
        int32_t keycode = AKeyEvent_getKeyCode(event);

        // Handle gamepad button presses and releases
        if (((source & AINPUT_SOURCE_JOYSTICK) == AINPUT_SOURCE_JOYSTICK) ||
            ((source & AINPUT_SOURCE_GAMEPAD) == AINPUT_SOURCE_GAMEPAD))
        {
			// ...
            return 1; // Handled gamepad axis motion
        }

        if (keycode == AKEYCODE_POWER)
        {
            return 0;
        }
        else if ((keycode == AKEYCODE_BACK) || (keycode == AKEYCODE_MENU))
        {
            // Eat BACK_BUTTON and AKEYCODE_MENU, just do nothing... and don't let to be handled by OS!
            return 1;
        }
        else if ((keycode == AKEYCODE_VOLUME_UP) || (keycode == AKEYCODE_VOLUME_DOWN))
        {
            // Set default OS behaviour
            return 0;
        }

        return 0;
    }

    // Register touch points count
    int pointCount = AMotionEvent_getPointerCount(event);

    int32_t action = AMotionEvent_getAction(event);
    unsigned int flags = action & AMOTION_EVENT_ACTION_MASK;

	return 0;
}

//////////////////////////////////////////////////////////////////////
// Otus Lisp <-> Android
//
#include <fcntl.h>
#include <errno.h>
#include <string.h>

int assets_open(const char *filename, int flags, int mode, void *userdata)
{
	char *filename1 = (char*)filename;

	int file = open(filename1, flags, mode);
	DLOG("open file.1: %s -> %d(%s)", filename1, file, file != -1 ? "Ok" : strerror(errno));
	if (file != -1)
		return file;

	// no file, try to open $OL_HOME/file
	char *home = getenv("OL_HOME");
	char *filename2 = (char *)(__builtin_alloca(strlen(home) + strlen(filename) + 2));
	snprintf(filename2, strlen(home) + strlen(filename) + 2, "%s/%s", home, filename);

	file = open(filename2, flags, mode);
	DLOG("open file.2: %s -> %d(%s)", filename2, file, file != -1 ? "Ok" : strerror(errno));
	if (file != -1)
		return file;

	// // at least try to read assets

	// // remove "./"
	// if (strncmp(filename, "./", 2) == 0)
	// 	filename += 2;

	// AAsset *asset = AAssetManager_open(asset_manager, filename, 0);
	// DLOG("open asset: %s -> %p", filename, asset);
	// if (asset)
	// {
	// 	int i = 3; // 0, 1, 2 - reserved
	// 	for (; i < fds_size; i++)
	// 	{
	// 		if (fds[i] == 0)
	// 		{
	// 			DLOG("   %d/%p", i, asset);
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
			LOGE("%.*s", (int)count, (char *)buf);
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
