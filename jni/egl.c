#include <sources/android/native_app_glue/android_native_app_glue.h>

int opengles_init(struct android_app *app)
{
	return 0;
}
int opengles_done(struct android_app *app)
{
	return 0;
}
int opengles_swap(void)
{
	return 0;
}


// void init_egl2(struct android_app *app)
// {
// 	LOGI("init_egl2");
// 	if (app->window != NULL) {
// 		LOGI("app->window: %p", app->window);
// 		int width = ANativeWindow_getWidth(app->window);
// 		int height = ANativeWindow_getHeight(app->window);
// 		// Init Graphics Device

// 		EGLint samples = 0;
// 		EGLint sampleBuffer = 0;
// 		if (0/*FLAG_MSAA_4X_HINT*/) {
// 			samples = 4;
// 			sampleBuffer = 1;
// 		}

// 		// get display
// 		Display = eglGetDisplay(EGL_DEFAULT_DISPLAY);
// 		if (Display == EGL_NO_DISPLAY) {
// 			LOGE("DISPLAY: Failed to initialize EGL device");
// 		}

// 		if (eglInitialize(Display, NULL, NULL) == EGL_FALSE) {
// 			LOGE("DISPLAY: Failed to initialize EGL device");
// 		}

// 		// config
// 		EGLint numConfigs = 0;
// 		EGLint framebufferAttribs[] =
// 		{
// 			EGL_RENDERABLE_TYPE, EGL_OPENGL_ES2_BIT,     // Type of context support -> Required on RPI?
// 			EGL_RED_SIZE, 8,            // RED color bit depth (alternative: 5)
// 			EGL_GREEN_SIZE, 8,          // GREEN color bit depth (alternative: 6)
// 			EGL_BLUE_SIZE, 8,           // BLUE color bit depth (alternative: 5)
// 			//EGL_TRANSPARENT_TYPE, EGL_NONE, // Request transparent framebuffer (EGL_TRANSPARENT_RGB does not work on RPI)
// 			EGL_DEPTH_SIZE, 16,         // Depth buffer size (Required to use Depth testing!)
// 			//EGL_STENCIL_SIZE, 8,      // Stencil buffer size
// 			EGL_SAMPLE_BUFFERS, sampleBuffer,    // Activate MSAA
// 			EGL_SAMPLES, samples,       // 4x Antialiasing if activated (Free on MALI GPUs)
// 			EGL_NONE
// 		};
// 		eglChooseConfig(Display, framebufferAttribs, &Config, 1, &numConfigs);
// 		eglBindAPI(EGL_OPENGL_ES_API); // ?

// 		// context
// 		EGLint contextAttribs[] =
// 		{
// 			EGL_CONTEXT_CLIENT_VERSION, 3,
// 			EGL_NONE
// 		};
// 		Context = eglCreateContext(Display, Config, EGL_NO_CONTEXT, contextAttribs);
// 		if (Context == EGL_NO_CONTEXT) {
// 			LOGE("DISPLAY: Failed to create EGL context");
// 		}

// 		// Create an EGL window surface
// 		EGLint displayFormat = 0;
// 		eglGetConfigAttrib(Display, Config, EGL_NATIVE_VISUAL_ID, &displayFormat);

// 		ANativeWindow_setBuffersGeometry(app->window, width, height, displayFormat);
// 		Surface = eglCreateWindowSurface(Display, Config, app->window, NULL);

// 		// Enable Context and Surface
// 		if (eglMakeCurrent(Display, Surface, Surface, Context) == EGL_FALSE) {
// 			LOGE("DISPLAY: Failed to attach EGL rendering context to EGL surface");
// 		}
// 	}
// 	LOGI("init_egl2 done");
// }
