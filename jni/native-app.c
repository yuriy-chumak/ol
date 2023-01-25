#include <sources/android/native_app_glue/android_native_app_glue.c>

#include <android/log.h>
#define APP_NAME "ol"
#define DLOG(...) __android_log_print(ANDROID_LOG_DEBUG,   APP_NAME, __VA_ARGS__)
#define ILOG(...) __android_log_print(ANDROID_LOG_INFO,    APP_NAME, __VA_ARGS__)
#define ELOG(...) __android_log_print(ANDROID_LOG_ERROR,   APP_NAME, __VA_ARGS__)
#define VLOG(...) __android_log_print(ANDROID_LOG_VERBOSE, APP_NAME, __VA_ARGS__)
#define FLOG(...) __android_log_print(ANDROID_LOG_FATAL,   APP_NAME, __VA_ARGS__)
/////////////////////////////////////////////////////////////////////////////////

#include <stdbool.h>

// Public ANL (Android Native Level) API
__attribute__((visibility("default")))
void anlSwapBuffers();
__attribute__((visibility("default")))
void anlPollEvents();
__attribute__((visibility("default")))
uint anlNextEvent();
__attribute__((visibility("default")))
bool anlKeyPressed(int keycode);

#include <GLES/gl.h>
__attribute__((visibility("default")))
void anlBindFramebuffer(GLenum target, GLuint framebuffer);

// keyboard events queue
#define TYPE_KEYWPRESS 2
#define TYPE_KEYWRELEASE 3
struct event_t {
	int type;
	int value;
};

// one producer, one consumer
struct events_t {
	struct event_t* begin;
	struct event_t* end;
	
	struct event_t* push;
	struct event_t* pop;
};

struct events_t events;
static void event_push(int type, int value)
{
	if (events.push == events.pop - 1 || (events.push == events.end && events.pop == events.begin))
		return;
	*events.push++ = (struct event_t) {
						.type = type,
						.value = (unsigned short) value };
	if (events.push == events.end)
		events.push = events.begin;
}

// public api: get left eye matrix
// public api: get right eye matrix

//////////////////////////////////////////
// portable api
int Oculus = 0; // Running under Oculus Go
int Opengl = 0;

extern int oculusgo_init(struct android_app *app);
extern int opengles_init(struct android_app *app);
static void init(struct android_app *app)
{
	ILOG("Init.");
	size_t count = 64;
	events.begin = malloc(sizeof(struct event_t) * count);
	events.end = events.begin + count;
	events.push = events.pop = events.begin;

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
	if (Oculus) (void)0;
	if (Opengl) (void)0;
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

	// пока что не можем привязать к gl2es через линковку, так как
	// не все нужные функции для ovr экспозятся.  TODO: исправить!
	void* gl2es = dlopen("libgl2es.so", RTLD_LAZY);
	void (*initialize_gl2es)() = dlsym(gl2es, "initialize_gl2es");
	if (initialize_gl2es)
		initialize_gl2es();
	void (*glHint)(GLenum pname, GLenum mode) = dlsym(gl2es, "glHint");
	if (glHint)   // GL_VR, сообщаем о поддержке VR
		glHint(0x10C33, Oculus ? GL_TRUE : GL_FALSE);

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
}

void anlPollEvents(void)
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

uint anlNextEvent(void)
{
	struct event_t event = {0};
	if (events.pop != events.push)
		event = *events.pop++;
		if (events.pop == events.end)
			events.pop = events.begin;
	return (event.type << 16) + event.value;
}

#define KEYCODE_MAX 512
static char keys[KEYCODE_MAX/8 + 1];
bool anlKeyPressed(int keycode)
{
	int byte = keycode / 8;
	int bit = keycode % 8;
	return (byte < sizeof(keys))
		&& (keys[byte] & (1 << bit));
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

	// DLOG("type: %d, source: %d", type, source);
	switch (type) {
		case AINPUT_EVENT_TYPE_KEY: {
			// Handle gamepad button presses and releases
			if (((source & AINPUT_SOURCE_JOYSTICK) == AINPUT_SOURCE_JOYSTICK) ||
				((source & AINPUT_SOURCE_GAMEPAD) == AINPUT_SOURCE_GAMEPAD))
			{
				// ...
				return 1; // Handled gamepad axis motion
			}

			int action = AKeyEvent_getAction(event);
			int32_t keycode = AKeyEvent_getKeyCode(event);
			// DLOG("action: %d, keycode: %d", action, keycode);

			switch (action) {
				case AKEY_EVENT_ACTION_DOWN: { // todo: push "key pressed event"
					int byte = keycode / 8, bit = keycode % 8;
					if (byte < sizeof(keys))
						keys[byte] |= (1 << bit);
					event_push(2, keycode);
					break;
				}
				case AKEY_EVENT_ACTION_MULTIPLE: //todo: push repeat_count "key pressed event"s
					break;
				case AKEY_EVENT_ACTION_UP: {
					int byte = keycode / 8, bit = keycode % 8;
					if (byte < sizeof(keys))
						keys[byte] &= ~(1 << bit);
					event_push(3, keycode);
					break;
				}
			};

			switch (keycode) {
				case AKEYCODE_POWER: // default OS behaviour
				case AKEYCODE_VOLUME_UP:
				case AKEYCODE_VOLUME_DOWN:
					return 0;
				case AKEYCODE_BACK:  // disable
				case AKEYCODE_MENU:
					return 1;
			}

			return 0;
		}

		case AINPUT_EVENT_TYPE_MOTION: {
			if (((source & AINPUT_SOURCE_JOYSTICK) == AINPUT_SOURCE_JOYSTICK) ||
				((source & AINPUT_SOURCE_GAMEPAD) == AINPUT_SOURCE_GAMEPAD))
			{
				// ...
				return 1; // Handled gamepad axis motion
			}
			break;
		}

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
	if (file != -1) {
		DLOG("open file.2: %s -> %d(Ok)", filename1, file);
		return file;
	}

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
