#include <sources/android/native_app_glue/android_native_app_glue.h>

#include <android/log.h>
#define APP_NAME "ol-ovr"            // "Pacific" means "Oculus Go" ?
#define DLOG(...) __android_log_print(ANDROID_LOG_DEBUG,   APP_NAME, __VA_ARGS__)
#define ILOG(...) __android_log_print(ANDROID_LOG_INFO,    APP_NAME, __VA_ARGS__)
#define ELOG(...) __android_log_print(ANDROID_LOG_ERROR,   APP_NAME, __VA_ARGS__)
#define VLOG(...) __android_log_print(ANDROID_LOG_VERBOSE, APP_NAME, __VA_ARGS__)
#define FLOG(...) __android_log_print(ANDROID_LOG_FATAL,   APP_NAME, __VA_ARGS__)
/////////////////////////////////////////////////////////////////////////////////
#include <assert.h>
#include <stdlib.h> // malloc

#include <EGL/egl.h>
#include <EGL/eglext.h>
#include <GLES3/gl3.h>
#include <GLES2/gl2ext.h>
#include <GLES3/gl3ext.h>

#include <dlfcn.h>

static const char * EglErrorString(const EGLint error)
{
	switch (error)
	{
		case EGL_SUCCESS:				return "EGL_SUCCESS";
		case EGL_NOT_INITIALIZED:		return "EGL_NOT_INITIALIZED";
		case EGL_BAD_ACCESS:			return "EGL_BAD_ACCESS";
		case EGL_BAD_ALLOC:				return "EGL_BAD_ALLOC";
		case EGL_BAD_ATTRIBUTE:			return "EGL_BAD_ATTRIBUTE";
		case EGL_BAD_CONTEXT:			return "EGL_BAD_CONTEXT";
		case EGL_BAD_CONFIG:			return "EGL_BAD_CONFIG";
		case EGL_BAD_CURRENT_SURFACE:	return "EGL_BAD_CURRENT_SURFACE";
		case EGL_BAD_DISPLAY:			return "EGL_BAD_DISPLAY";
		case EGL_BAD_SURFACE:			return "EGL_BAD_SURFACE";
		case EGL_BAD_MATCH:				return "EGL_BAD_MATCH";
		case EGL_BAD_PARAMETER:			return "EGL_BAD_PARAMETER";
		case EGL_BAD_NATIVE_PIXMAP:		return "EGL_BAD_NATIVE_PIXMAP";
		case EGL_BAD_NATIVE_WINDOW:		return "EGL_BAD_NATIVE_WINDOW";
		case EGL_CONTEXT_LOST:			return "EGL_CONTEXT_LOST";
		default:						return "unknown";
	}
}
static const char * GlFrameBufferStatusString(GLenum status)
{
	switch ( status )
	{
		case GL_FRAMEBUFFER_UNDEFINED:						return "GL_FRAMEBUFFER_UNDEFINED";
		case GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT:			return "GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT";
		case GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT:	return "GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT";
		case GL_FRAMEBUFFER_UNSUPPORTED:					return "GL_FRAMEBUFFER_UNSUPPORTED";
		case GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE:			return "GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE";
		default:											return "unknown";
	}
}

void begin();
void update(int eye);
void flush();
void end();

#include "VrApi.h"
#include "VrApi_Helpers.h"

ovrJava Java;
ovrMobile* Ovr;

EGLConfig Config;        // Graphic config
EGLDisplay Display;      // Native display device (physical screen connection)
EGLSurface Surface;      // Surface to draw on, framebuffers (connected to context)
EGLContext Context;      // Graphic context, mode in which drawing can be done

PFNGLRENDERBUFFERSTORAGEMULTISAMPLEEXTPROC glRenderbufferStorageMultisampleEXT;
PFNGLFRAMEBUFFERTEXTURE2DMULTISAMPLEEXTPROC glFramebufferTexture2DMultisampleEXT;
PFNGLFRAMEBUFFERTEXTUREMULTIVIEWOVRPROC glFramebufferTextureMultiviewOVR;
PFNGLFRAMEBUFFERTEXTUREMULTISAMPLEMULTIVIEWOVRPROC glFramebufferTextureMultisampleMultiviewOVR;
#define GL_CLAMP_TO_BORDER			0x812D
#define GL_TEXTURE_BORDER_COLOR		0x1004

// PFNGLHINT glHint;

// -=( gl2es )=------------------------------
#define GL_MATRIX_MODE 0x0BA0

#define VR_PROJECTION  0x11701
#define VR_MODELVIEW   0x11700

typedef void (PFNGLMATRIXMODE) (GLenum mode);
typedef void (PFNGLGETINTEGERV)(GLenum pname, GLint *params);
typedef void (PFNGLLOADMATRIXF)(const GLfloat *m);

PFNGLMATRIXMODE *vrMatrixMode;
PFNGLGETINTEGERV *vrGetIntegerv;
PFNGLLOADMATRIXF *vrLoadMatrixf;
// -----------------------------------

// настройки
static const int CPU_LEVEL			= 2;
static const int GPU_LEVEL			= 3;
static const int NUM_MULTI_SAMPLES	= 0;

// фреймбуферы для левого и правого глаз
typedef struct {
	int						Width;
	int						Height;
	int						Multisamples;
	int						TextureSwapChainLength;
	int						TextureSwapChainIndex;
	ovrTextureSwapChain *	ColorTextureSwapChain;
	GLuint *				DepthBuffers;
	GLuint *				FrameBuffers;
} ovrFramebuffer;

ovrFramebuffer FrameBuffer[2];
static bool ovrFramebuffer_Create(ovrFramebuffer *frameBuffer, GLenum colorFormat, int width, int height, int multisamples);

long long FrameIndex = 1;
double DisplayTime = 0.0;

/****************************************************************************************/
int oculusgo_init(struct android_app *app)
{
	// Initialize Java
	Java.Vm = app->activity->vm;
	(*Java.Vm)->AttachCurrentThread(Java.Vm, &Java.Env, NULL);
	Java.ActivityObject = app->activity->clazz;
	// Note that AttachCurrentThread will reset the thread name.

	// init
	ovrInitParms initParms = vrapi_DefaultInitParms(&Java);
	int32_t initResult = vrapi_Initialize(&initParms);
	if (initResult != VRAPI_INITIALIZE_SUCCESS) {
		ELOG("Failed to initialize VrApi");
		return 0;
	}

	// ------------------------------------------------------
	// copy-paste of VrApi SDK sample code
	Display = eglGetDisplay(EGL_DEFAULT_DISPLAY);
	VLOG("Display: %p", Display);

	EGLint MajorVersion;
	EGLint MinorVersion;
	eglInitialize(Display, &MajorVersion, &MinorVersion);
	VLOG("eglInitialize: %d.%d", MajorVersion, MinorVersion);

	const int MAX_CONFIGS = 1024;
	EGLConfig configs[MAX_CONFIGS];
	EGLint numConfigs = 0;
	if (eglGetConfigs(Display, configs, MAX_CONFIGS, &numConfigs) == EGL_FALSE) {
		ELOG("eglGetConfigs() failed: %s", EglErrorString(eglGetError()));
		return 0;
	}

	const EGLint configAttribs[] =
	{
		EGL_RED_SIZE,		8,
		EGL_GREEN_SIZE,		8,
		EGL_BLUE_SIZE,		8,
		EGL_ALPHA_SIZE,		8, // need alpha for the multi-pass timewarp compositor
		EGL_DEPTH_SIZE,		0,
		EGL_STENCIL_SIZE,	0,
		EGL_SAMPLES,		0,
		EGL_NONE
	};

	EGLConfig Config = 0;
	for ( int i = 0; i < numConfigs; i++ )
	{
		EGLint value = 0;

		eglGetConfigAttrib(Display, configs[i], EGL_RENDERABLE_TYPE, &value);
		// Accepted as a bitfield value in the EGL_RENDERABLE_TYPE config attribute to eglChooseConfig:
		if ((value & EGL_OPENGL_ES3_BIT_KHR) != EGL_OPENGL_ES3_BIT_KHR) {
			VLOG("    %d skip, no EGL_OPENGL_ES3_BIT_KHR", i);
			continue;
		}

		// The pbuffer config also needs to be compatible with normal window rendering
		// so it can share textures with the window context.
		eglGetConfigAttrib(Display, configs[i], EGL_SURFACE_TYPE, &value);
		if ((value & (EGL_WINDOW_BIT | EGL_PBUFFER_BIT)) != (EGL_WINDOW_BIT | EGL_PBUFFER_BIT)) {
			VLOG("    %d skip, no EGL_WINDOW_BIT | EGL_PBUFFER_BIT", i);
			continue;
		}

		int	j = 0;
		for (; configAttribs[j] != EGL_NONE; j += 2) {
			eglGetConfigAttrib(Display, configs[i], configAttribs[j], &value);
			if (value != configAttribs[j + 1]) // skip the same?
				break;
		}
		if (configAttribs[j] == EGL_NONE) {
			VLOG("    configAttribs[%d] == EGL_NONE, return %d", j, i);
			Config = configs[i];
			break;
		}
	}
	if (Config == 0) {
		ELOG("eglChooseConfig() failed: %s", EglErrorString(eglGetError()));
		return 0;
	}

	VLOG("eglCreateContext(Display, Config, EGL_NO_CONTEXT, contextAttribs)");
	EGLint contextAttribs[] = {
		EGL_CONTEXT_CLIENT_VERSION, 3,
		EGL_NONE
	};
	Context = eglCreateContext(Display, Config, EGL_NO_CONTEXT, contextAttribs);
	if (Context == EGL_NO_CONTEXT) {
		ELOG("eglCreateContext() failed: %s", EglErrorString(eglGetError()));
		return 0;
	}

	// Create Tiny Surface
	const EGLint surfaceAttribs[] =
	{
		EGL_WIDTH, 16,
		EGL_HEIGHT, 16,
		EGL_NONE
	};
	Surface = eglCreatePbufferSurface(Display, Config, surfaceAttribs);
	if (Surface == EGL_NO_SURFACE) {
		ELOG("eglCreatePbufferSurface() failed: %s", EglErrorString(eglGetError()));
		eglDestroyContext(Display, Context);
		Context = EGL_NO_CONTEXT;
		return 0;
	}

	if (eglMakeCurrent(Display, Surface, Surface, Context) == EGL_FALSE) {
		ELOG("eglMakeCurrent() failed: %s", EglErrorString(eglGetError()));
		eglDestroySurface(Display, Surface);
		eglDestroyContext(Display, Context);
		Context = EGL_NO_CONTEXT;
	}

	// init extensions
	glRenderbufferStorageMultisampleEXT = (PFNGLRENDERBUFFERSTORAGEMULTISAMPLEEXTPROC)eglGetProcAddress("glRenderbufferStorageMultisampleEXT");
	glFramebufferTexture2DMultisampleEXT = (PFNGLFRAMEBUFFERTEXTURE2DMULTISAMPLEEXTPROC)eglGetProcAddress("glFramebufferTexture2DMultisampleEXT");
	glFramebufferTextureMultiviewOVR = (PFNGLFRAMEBUFFERTEXTUREMULTIVIEWOVRPROC) eglGetProcAddress("glFramebufferTextureMultiviewOVR");
	glFramebufferTextureMultisampleMultiviewOVR = (PFNGLFRAMEBUFFERTEXTUREMULTISAMPLEMULTIVIEWOVRPROC) eglGetProcAddress("glFramebufferTextureMultisampleMultiviewOVR");

	// ...

	// appState.CpuLevel = CPU_LEVEL;
	// appState.GpuLevel = GPU_LEVEL;
	// appState.MainThreadTid = gettid();

	// тут надо подготовить буфера для вывода
	// 1. получим две текстурки (для левого и правого глаз)
	// assert GL_OVR_multiview2 && GL_OVR_multiview_multisampled_render_to_texture
	assert (vrapi_GetSystemPropertyInt(&Java, VRAPI_SYS_PROP_MULTIVIEW_AVAILABLE));

	// создадим фреймбуферы для левого и правого глаз
	for (int i = 0; i < 2; i++)
	 	ovrFramebuffer_Create(&FrameBuffer[i], GL_RGBA8,
	 							vrapi_GetSystemPropertyInt(&Java, VRAPI_SYS_PROP_SUGGESTED_EYE_TEXTURE_WIDTH),
	 							vrapi_GetSystemPropertyInt(&Java, VRAPI_SYS_PROP_SUGGESTED_EYE_TEXTURE_HEIGHT),
	 							NUM_MULTI_SAMPLES);

	// ----------------------------------------------
	// Let's enter VR mode simulation
	ovrModeParms parms = vrapi_DefaultModeParms(&Java);
	// No need to reset the FLAG_FULLSCREEN window flag when using a View
	parms.Flags &= ~VRAPI_MODE_FLAG_RESET_WINDOW_FULLSCREEN;
	parms.Flags |= VRAPI_MODE_FLAG_NATIVE_WINDOW;
	parms.Display = (size_t)Display;
	parms.WindowSurface = (size_t)app->window;
	parms.ShareContext = (size_t)Context;

	VLOG("eglGetCurrentSurface(EGL_DRAW) = %p", eglGetCurrentSurface(EGL_DRAW));
	VLOG("vrapi_EnterVrMode>");
	Ovr = vrapi_EnterVrMode(&parms);
	VLOG("eglGetCurrentSurface(EGL_DRAW) = %p", eglGetCurrentSurface(EGL_DRAW));

	// vrapi_SetClockLevels( app->Ovr, app->CpuLevel, app->GpuLevel );
	// AVLOG( "		vrapi_SetClockLevels( %d, %d )", app->CpuLevel, app->GpuLevel );
	// vrapi_SetPerfThread( app->Ovr, VRAPI_PERF_THREAD_TYPE_MAIN, app->MainThreadTid );
	// AVLOG( "		vrapi_SetPerfThread( MAIN, %d )", app->MainThreadTid );
	// vrapi_SetPerfThread( app->Ovr, VRAPI_PERF_THREAD_TYPE_RENDERER, app->RenderThreadTid );
	// AVLOG( "		vrapi_SetPerfThread( RENDERER, %d )", app->RenderThreadTid );

	// покажем картинку загрузки?
#if 1
	int frameFlags = 0;
	frameFlags |= VRAPI_FRAME_FLAG_FLUSH;

	ovrLayerProjection2 blackLayer = vrapi_DefaultLayerBlackProjection2();
	blackLayer.Header.Flags |= VRAPI_FRAME_LAYER_FLAG_INHIBIT_SRGB_FRAMEBUFFER;

	ovrLayerLoadingIcon2 iconLayer = vrapi_DefaultLayerLoadingIcon2();
	iconLayer.Header.Flags |= VRAPI_FRAME_LAYER_FLAG_INHIBIT_SRGB_FRAMEBUFFER;

	double predictedDisplayTime = vrapi_GetPredictedDisplayTime(Ovr, 0);

	{
		const ovrLayerHeader2 * layers[] = {
			&blackLayer.Header,
			&iconLayer.Header,
		};

		ovrSubmitFrameDescription2 frameDesc = { 0 };
		frameDesc.Flags = frameFlags;
		frameDesc.SwapInterval = 1;
		frameDesc.FrameIndex = FrameIndex;
		frameDesc.DisplayTime = predictedDisplayTime;
		frameDesc.LayerCount = 2;
		frameDesc.Layers = layers;

		vrapi_SubmitFrame2(Ovr, &frameDesc);
	}
#endif

	// загрузим наш OpenGL 2.1 -> GLES слой
	void* gl2es = dlopen("libgl2es.so", RTLD_LAZY);
	vrMatrixMode = dlsym(gl2es, "glMatrixMode");
	vrGetIntegerv = dlsym(gl2es, "glGetIntegerv");
	vrLoadMatrixf = dlsym(gl2es, "glLoadMatrixf");

	return 1;
}

int oculusgo_done(struct android_app *app)
{
	// todo: destroy all buffers. ovrFramebuffer_Destroy()

	VLOG("eglGetCurrentSurface(EGL_DRAW) = %p", eglGetCurrentSurface(EGL_DRAW));
	VLOG("vrapi_LeaveVrMode>");
	vrapi_LeaveVrMode(Ovr);
	VLOG("eglGetCurrentSurface(EGL_DRAW) = %p", eglGetCurrentSurface(EGL_DRAW));

	vrapi_Shutdown();

	eglMakeCurrent(Display, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT);
	eglDestroySurface(Display, Surface);
	eglDestroyContext(Display, Context);
	eglTerminate(Display);

	(*Java.Vm)->DetachCurrentThread(Java.Vm);

	ILOG("oculus_main done.");
	return 1;
}

void oculusgo_swap(void)
{
	// FrameIndex++;
	// 
}

void oculusgo_handle_input(void)
{
	// тут заполним кватернион поворота
	// а так же джойстик
}

// заведем пару новых функций оберток для тестового рендерера
ovrLayerProjection2 worldLayer;
ovrMatrix4f eyeViewMatrixTransposed[2];
ovrMatrix4f projectionMatrixTransposed[2];

void begin()
{
	worldLayer = vrapi_DefaultLayerProjection2();
	worldLayer.Header.Flags |= VRAPI_FRAME_LAYER_FLAG_CHROMATIC_ABERRATION_CORRECTION;

	// замапим нужные нам буфера для рисования (используем из примера)
	double predictedDisplayTime = vrapi_GetPredictedDisplayTime(Ovr, ++FrameIndex);
	ovrTracking2 tracking = vrapi_GetPredictedTracking2(Ovr, predictedDisplayTime);

	DisplayTime = predictedDisplayTime;

	// типа две матрицы трансформации
	eyeViewMatrixTransposed[0] = ovrMatrix4f_Transpose(&tracking.Eye[0].ViewMatrix);
	eyeViewMatrixTransposed[1] = ovrMatrix4f_Transpose(&tracking.Eye[1].ViewMatrix);

	// ILOG("-------------------------------------");
	// ILOG("tracking.Eye[0].ProjectionMatrix[0] = [%f %f %f %f]",
	// 	tracking.Eye[0].ProjectionMatrix.M[0][0],
	// 	tracking.Eye[0].ProjectionMatrix.M[0][1],
	// 	tracking.Eye[0].ProjectionMatrix.M[0][2],
	// 	tracking.Eye[0].ProjectionMatrix.M[0][3]);
	// ILOG("tracking.Eye[0].ProjectionMatrix[1] = [%f %f %f %f]",
	// 	tracking.Eye[0].ProjectionMatrix.M[1][0],
	// 	tracking.Eye[0].ProjectionMatrix.M[1][1],
	// 	tracking.Eye[0].ProjectionMatrix.M[1][2],
	// 	tracking.Eye[0].ProjectionMatrix.M[1][3]);
	// ILOG("tracking.Eye[0].ProjectionMatrix[2] = [%f %f %f %f]",
	// 	tracking.Eye[0].ProjectionMatrix.M[2][0],
	// 	tracking.Eye[0].ProjectionMatrix.M[2][1],
	// 	tracking.Eye[0].ProjectionMatrix.M[2][2],
	// 	tracking.Eye[0].ProjectionMatrix.M[2][3]);
	// ILOG("tracking.Eye[0].ProjectionMatrix[3] = [%f %f %f %f]",
	// 	tracking.Eye[0].ProjectionMatrix.M[3][0],
	// 	tracking.Eye[0].ProjectionMatrix.M[3][1],
	// 	tracking.Eye[0].ProjectionMatrix.M[3][2],
	// 	tracking.Eye[0].ProjectionMatrix.M[3][3]);
	projectionMatrixTransposed[0] = ovrMatrix4f_Transpose(&tracking.Eye[0].ProjectionMatrix);
	projectionMatrixTransposed[1] = ovrMatrix4f_Transpose(&tracking.Eye[1].ProjectionMatrix);

	worldLayer.HeadPose = tracking.HeadPose;
	for (int eye = 0; eye < 2; eye++) {
		ovrFramebuffer * frameBuffer = &FrameBuffer[eye];
		worldLayer.Textures[eye].ColorSwapChain = frameBuffer->ColorTextureSwapChain;
		worldLayer.Textures[eye].SwapChainIndex = frameBuffer->TextureSwapChainIndex;
		worldLayer.Textures[eye].TexCoordsFromTanAngles = ovrMatrix4f_TanAngleMatrixFromProjection(&tracking.Eye[eye].ProjectionMatrix);
	}
}

void update(int eye)
{
	ovrFramebuffer * frameBuffer = &FrameBuffer[eye];
	glBindFramebuffer(GL_DRAW_FRAMEBUFFER, frameBuffer->FrameBuffers[frameBuffer->TextureSwapChainIndex]); // _SetCurrent

	glViewport(0, 0, frameBuffer->Width, frameBuffer->Height);

	// ---------------------------------------
	// Загрузим наши матрицы в движок
	GLint matrixmode;
	vrGetIntegerv(GL_MATRIX_MODE, &matrixmode);

	vrMatrixMode(VR_PROJECTION);
	vrLoadMatrixf((float*)&projectionMatrixTransposed[eye]);
	vrMatrixMode(VR_MODELVIEW);
	vrLoadMatrixf((float*)&eyeViewMatrixTransposed[eye]);

	vrMatrixMode(matrixmode);
}

void flush()
{
	GLenum depthAttachment[1] = { GL_DEPTH_ATTACHMENT }; // _Resolve
#ifndef NO_GLES3
	glInvalidateFramebuffer(GL_DRAW_FRAMEBUFFER, 1, depthAttachment);
#endif

	glFlush(); // TODO: проверить а нужно ли?
}

void end()
{
	glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0); // _SetNone

	for (int eye = 0; eye < 2; eye++) {
		ovrFramebuffer * frameBuffer = &FrameBuffer[eye];
		frameBuffer->TextureSwapChainIndex = (frameBuffer->TextureSwapChainIndex + 1) % frameBuffer->TextureSwapChainLength; // _Advance
	}

	const ovrLayerHeader2 * layers[] = {
		&worldLayer.Header
	};

	ovrSubmitFrameDescription2 frameDesc = { 0 };
	frameDesc.Flags = 0;
	frameDesc.SwapInterval = 1;
	frameDesc.FrameIndex = FrameIndex;
	frameDesc.DisplayTime = DisplayTime;
	frameDesc.LayerCount = 1;
	frameDesc.Layers = layers;

	// Hand over the eye images to the time warp.
	vrapi_SubmitFrame2(Ovr, &frameDesc);

}


static bool ovrFramebuffer_Create(ovrFramebuffer *frameBuffer, GLenum colorFormat, int width, int height, int multisamples)
{
	ILOG("Creating framebuffer %dx%d", width, height);
	frameBuffer->Width = width;
	frameBuffer->Height = height;
	frameBuffer->Multisamples = multisamples;

	frameBuffer->ColorTextureSwapChain = vrapi_CreateTextureSwapChain3(VRAPI_TEXTURE_TYPE_2D, colorFormat, width, height, 1, 3);
	frameBuffer->TextureSwapChainLength = vrapi_GetTextureSwapChainLength(frameBuffer->ColorTextureSwapChain);
	frameBuffer->DepthBuffers = (GLuint *)malloc(frameBuffer->TextureSwapChainLength * sizeof(GLuint));
	frameBuffer->FrameBuffers = (GLuint *)malloc(frameBuffer->TextureSwapChainLength * sizeof(GLuint));

	GLfloat borderColor[] = { 0.0f, 0.0f, 0.0f, 0.0f };

	VLOG("frameBuffer->TextureSwapChainLength = %d", frameBuffer->TextureSwapChainLength);
	for (int i = 0; i < frameBuffer->TextureSwapChainLength; i++)
	{
		// Create the color buffer texture.
		GLuint colorTexture = vrapi_GetTextureSwapChainHandle(frameBuffer->ColorTextureSwapChain, i);
		glBindTexture(GL_TEXTURE_2D, colorTexture);
			glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
			glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
			glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
			glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
			glTexParameterfv(GL_TEXTURE_2D, GL_TEXTURE_BORDER_COLOR, borderColor);
		glBindTexture(GL_TEXTURE_2D, 0);

		GLenum renderFramebufferStatus;
		if (multisamples > 1) {
			// Create multisampled depth buffer.
			glGenRenderbuffers(1, &frameBuffer->DepthBuffers[i]);
			glBindRenderbuffer(GL_RENDERBUFFER, frameBuffer->DepthBuffers[i]);
			glRenderbufferStorageMultisampleEXT(GL_RENDERBUFFER, multisamples, GL_DEPTH_COMPONENT24, width, height);
			glBindRenderbuffer(GL_RENDERBUFFER, 0);

			// Create the frame buffer.
			// NOTE: glFramebufferTexture2DMultisampleEXT only works with GL_FRAMEBUFFER.
			glGenFramebuffers(1, &frameBuffer->FrameBuffers[i]);
			glBindFramebuffer(GL_FRAMEBUFFER, frameBuffer->FrameBuffers[i]);
			glFramebufferTexture2DMultisampleEXT(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, colorTexture, 0, multisamples);
			glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, frameBuffer->DepthBuffers[i]);

			renderFramebufferStatus = glCheckFramebufferStatus(GL_FRAMEBUFFER);
			glBindFramebuffer(GL_FRAMEBUFFER, 0);
		}
		else {
			// Create depth buffer.
			glGenRenderbuffers(1, &frameBuffer->DepthBuffers[i]);
			glBindRenderbuffer(GL_RENDERBUFFER, frameBuffer->DepthBuffers[i]);
			glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT24, width, height);
			glBindRenderbuffer(GL_RENDERBUFFER, 0);

			// Create the frame buffer.
			glGenFramebuffers(1, &frameBuffer->FrameBuffers[i]);
			glBindFramebuffer(GL_DRAW_FRAMEBUFFER, frameBuffer->FrameBuffers[i]);
			glFramebufferRenderbuffer(GL_DRAW_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, frameBuffer->DepthBuffers[i]);
			glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, colorTexture, 0);

			renderFramebufferStatus = glCheckFramebufferStatus(GL_DRAW_FRAMEBUFFER);
			glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);
		}

		if (renderFramebufferStatus != GL_FRAMEBUFFER_COMPLETE)
		{
			ELOG("Incomplete frame buffer object: %s", GlFrameBufferStatusString(renderFramebufferStatus));
			return 0;
		}
	}

	return 1;
}
