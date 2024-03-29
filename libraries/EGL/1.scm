(define-library (EGL 1.0) ; (23 Jul 2003)
   (description
      "OpenGL ES Native Platform Graphics Interface")
   (export
      EGL_DEFAULT_DISPLAY
      EGL_NO_CONTEXT
      EGL_NO_DISPLAY
      EGL_NO_SURFACE

      EGL_VERSION_1_0

      EGLBoolean
      EGLint EGLint*
      EGLConfig
      EGLDisplay
      EGLSurface
      EGLContext

      EGL_FALSE
      EGL_TRUE

      EGL_SUCCESS
      EGL_NOT_INITIALIZED
      EGL_BAD_ACCESS
      EGL_BAD_ALLOC
      EGL_BAD_ATTRIBUTE
      EGL_BAD_CONFIG
      EGL_BAD_CONTEXT
      EGL_BAD_CURRENT_SURFACE
      EGL_BAD_DISPLAY
      EGL_BAD_MATCH
      EGL_BAD_NATIVE_PIXMAP
      EGL_BAD_NATIVE_WINDOW
      EGL_BAD_PARAMETER
      EGL_BAD_SURFACE

      EGL_BUFFER_SIZE
      EGL_ALPHA_SIZE
      EGL_BLUE_SIZE
      EGL_GREEN_SIZE
      EGL_RED_SIZE
      EGL_DEPTH_SIZE
      EGL_STENCIL_SIZE
      EGL_CONFIG_CAVEAT
      EGL_CONFIG_ID
      EGL_LEVEL
      EGL_MAX_PBUFFER_HEIGHT
      EGL_MAX_PBUFFER_PIXELS
      EGL_MAX_PBUFFER_WIDTH
      EGL_NATIVE_RENDERABLE
      EGL_NATIVE_VISUAL_ID
      EGL_NATIVE_VISUAL_TYPE
      EGL_SAMPLES
      EGL_SAMPLE_BUFFERS
      EGL_SURFACE_TYPE
      EGL_TRANSPARENT_TYPE
      EGL_TRANSPARENT_BLUE_VALUE
      EGL_TRANSPARENT_GREEN_VALUE
      EGL_TRANSPARENT_RED_VALUE
      EGL_NONE

      EGL_DONT_CARE

      EGL_SLOW_CONFIG
      EGL_NON_CONFORMANT_CONFIG
      EGL_TRANSPARENT_RGB

      EGL_PBUFFER_BIT
      EGL_PIXMAP_BIT
      EGL_WINDOW_BIT

      EGL_VENDOR
      EGL_VERSION
      EGL_EXTENSIONS

      EGL_HEIGHT
      EGL_WIDTH
      EGL_LARGEST_PBUFFER

      EGL_DRAW
      EGL_READ

      EGL_CORE_NATIVE_ENGINE

      ; functions
      eglGetError

      eglGetDisplay
      eglInitialize
      ;eglTerminate
      eglQueryString
      ;eglGetProcAddress

      eglGetConfigs
      eglChooseConfig
      ;eglGetConfigAttrib (EGLDisplay dpy, EGLConfig config, EGLint attribute, EGLint *value);

      eglCreateWindowSurface
      ;eglCreatePixmapSurface ; EGLSurface (EGLDisplay dpy, EGLConfig config, NativePixmapType pixmap, const EGLint *attrib_list);
      ;eglCreatePbufferSurface ; EGLSurface (EGLDisplay dpy, EGLConfig config, const EGLint *attrib_list);
      ;eglDestroySurface (EGLDisplay dpy, EGLSurface surface);
      eglQuerySurface

      eglCreateContext ; (EGLDisplay dpy, EGLConfig config, EGLContext share_list, const EGLint *attrib_list);
      ;eglDestroyContext (EGLDisplay dpy, EGLContext ctx);
      eglMakeCurrent ; (EGLDisplay dpy, EGLSurface draw, EGLSurface read, EGLContext ctx);
      ;eglGetCurrentContext (void);
      ;eglGetCurrentSurface (EGLint readdraw);
      ;eglGetCurrentDisplay (void);
      ;eglQueryContext (EGLDisplay dpy, EGLContext ctx, EGLint attribute, EGLint *value);

      ;eglWaitGL (void);
      ;eglWaitNative (EGLint engine);
      eglSwapBuffers ; (EGLDisplay dpy, EGLSurface draw);
      ;eglCopyBuffers (EGLDisplay dpy, EGLSurface surface, NativePixmapType target);

      (exports (EGL platform))
   )
   (import
      (scheme core)
      (EGL platform))

(begin

   ; Types and resources
   (define EGLBoolean fft-unsigned-int)
   (define EGLint fft-int)
      (define EGLint* (fft* EGLint))
      (define EGLint& (fft& EGLint))

   (define EGLDisplay fft-void*)
   (define EGLConfig  fft-void*)
      (define EGLConfig* (fft* EGLConfig))
      (define EGLConfig& (fft& EGLConfig))
   (define EGLSurface fft-void*)
   (define EGLContext fft-void*)
)

(cond-expand
   ((or Linux Android Windows)
      (begin
         (define NativeDisplayType fft-void*)
         (define NativeWindowType fft-void*)
         (define NativePixmapType fft-void*) ))
   (Emscripten
      (begin
         (define NativeDisplayType fft-int)
         (define NativeWindowType fft-int)
         (define NativePixmapType fft-int) )))

(begin

   ; EGL and native handle values
   (define EGL_DEFAULT_DISPLAY (vm:cast 0 NativeDisplayType))
   (define EGL_NO_CONTEXT  (vm:cast 0 EGLContext))
   (define EGL_NO_DISPLAY  (vm:cast 0 EGLDisplay))
   (define EGL_NO_SURFACE  (vm:cast 0 EGLSurface))

   ; Versioning and extensions
   (define EGL_VERSION_1_0 1)

   ; Boolean
   (define EGL_FALSE       0)
   (define EGL_TRUE        1)

   ; Errors
   (define EGL_SUCCESS                 #x3000)
   (define EGL_NOT_INITIALIZED         #x3001)
   (define EGL_BAD_ACCESS              #x3002)
   (define EGL_BAD_ALLOC               #x3003)
   (define EGL_BAD_ATTRIBUTE           #x3004)
   (define EGL_BAD_CONFIG              #x3005)
   (define EGL_BAD_CONTEXT             #x3006)
   (define EGL_BAD_CURRENT_SURFACE     #x3007)
   (define EGL_BAD_DISPLAY             #x3008)
   (define EGL_BAD_MATCH               #x3009)
   (define EGL_BAD_NATIVE_PIXMAP       #x300A)
   (define EGL_BAD_NATIVE_WINDOW       #x300B)
   (define EGL_BAD_PARAMETER           #x300C)
   (define EGL_BAD_SURFACE             #x300D)
   ; #x300F - #x301F reserved for additional errors.

   ; Config attributes
   (define EGL_BUFFER_SIZE             #x3020)
   (define EGL_ALPHA_SIZE              #x3021)
   (define EGL_BLUE_SIZE               #x3022)
   (define EGL_GREEN_SIZE              #x3023)
   (define EGL_RED_SIZE                #x3024)
   (define EGL_DEPTH_SIZE              #x3025)
   (define EGL_STENCIL_SIZE            #x3026)
   (define EGL_CONFIG_CAVEAT           #x3027)
   (define EGL_CONFIG_ID               #x3028)
   (define EGL_LEVEL                   #x3029)
   (define EGL_MAX_PBUFFER_HEIGHT      #x302A)
   (define EGL_MAX_PBUFFER_PIXELS      #x302B)
   (define EGL_MAX_PBUFFER_WIDTH       #x302C)
   (define EGL_NATIVE_RENDERABLE       #x302D)
   (define EGL_NATIVE_VISUAL_ID        #x302E)
   (define EGL_NATIVE_VISUAL_TYPE      #x302F)
   ;define EGL_PRESERVED_RESOURCES     #x3030
   (define EGL_SAMPLES                 #x3031)
   (define EGL_SAMPLE_BUFFERS          #x3032)
   (define EGL_SURFACE_TYPE            #x3033)
   (define EGL_TRANSPARENT_TYPE        #x3034)
   (define EGL_TRANSPARENT_BLUE_VALUE  #x3035)
   (define EGL_TRANSPARENT_GREEN_VALUE #x3036)
   (define EGL_TRANSPARENT_RED_VALUE   #x3037)
   (define EGL_NONE                    #x3038)    ; Also a config value

   ; Config values
   (define EGL_DONT_CARE           #xFFFFFFFF)    ; (EGLint) -1

   (define EGL_SLOW_CONFIG             #x3050)    ; EGL_CONFIG_CAVEAT value
   (define EGL_NON_CONFORMANT_CONFIG   #x3051)    ; "
   (define EGL_TRANSPARENT_RGB         #x3052)    ; EGL_TRANSPARENT_TYPE value

   ; Config attribute mask bits
   (define EGL_PBUFFER_BIT               #x01)    ; EGL_SURFACE_TYPE mask bit
   (define EGL_PIXMAP_BIT                #x02)    ; "
   (define EGL_WINDOW_BIT                #x04)    ; "

   ; String names
   (define EGL_VENDOR                  #x3053)    ; eglQueryString target
   (define EGL_VERSION                 #x3054)    ; "
   (define EGL_EXTENSIONS              #x3055)    ; "

   ; Surface attributes
   (define EGL_HEIGHT                  #x3056)
   (define EGL_WIDTH                   #x3057)
   (define EGL_LARGEST_PBUFFER         #x3058)

   ; Current surfaces
   (define EGL_DRAW                    #x3059)
   (define EGL_READ                    #x305A)

   ; Engines
   (define EGL_CORE_NATIVE_ENGINE      #x305B)

   ; 0x305C-0x3FFFF reserved for future use
)

(begin
   (setq EGL EGL_LIBRARY)

   (define eglChooseConfig (EGL EGLBoolean "eglChooseConfig" EGLDisplay EGLint* EGLConfig& EGLint EGLint&))
   (define eglCopyBuffers (EGL EGLBoolean "eglCopyBuffers" EGLDisplay EGLSurface EGLNativePixmapType))
   (define eglCreateContext (EGL EGLContext "eglCreateContext" EGLDisplay EGLConfig EGLContext EGLint*))
   (define eglCreatePbufferSurface (EGL EGLSurface "eglCreatePbufferSurface" EGLDisplay EGLConfig EGLint*))
   (define eglCreatePixmapSurface (EGL EGLSurface "eglCreatePixmapSurface" EGLDisplay EGLConfig EGLNativePixmapType EGLint*))
   (define eglCreateWindowSurface (EGL EGLSurface "eglCreateWindowSurface" EGLDisplay EGLConfig EGLNativeWindowType EGLint*))
   (define eglDestroyContext (EGL EGLBoolean "eglDestroyContext" EGLDisplay EGLContext))
   (define eglDestroySurface (EGL EGLBoolean "eglDestroySurface" EGLDisplay EGLSurface))
   (define eglGetConfigAttrib (EGL EGLBoolean "eglGetConfigAttrib" EGLDisplay EGLConfig EGLint EGLint*))
   (define eglGetConfigs (EGL EGLBoolean "eglGetConfigs" EGLDisplay EGLConfig* EGLint EGLint&))
   (define eglGetCurrentDisplay (EGL EGLDisplay "eglGetCurrentDisplay"))
   (define eglGetCurrentSurface (EGL EGLSurface "eglGetCurrentSurface" EGLint))
   (define eglGetDisplay (EGL EGLDisplay "eglGetDisplay" EGLNativeDisplayType))
   (define eglGetError (EGL EGLint "eglGetError"))
   (define eglGetProcAddress (EGL type-vptr "eglGetProcAddress" type-string))
   (define eglInitialize (EGL EGLBoolean "eglInitialize" EGLDisplay EGLint& EGLint&))
   (define eglMakeCurrent (EGL EGLBoolean "eglMakeCurrent" EGLDisplay EGLSurface EGLSurface EGLContext))
   (define eglQueryContext (EGL EGLBoolean "eglQueryContext" EGLDisplay EGLContext EGLint EGLint*))
   (define eglQueryString (EGL type-string "eglQueryString" EGLDisplay EGLint))
   (define eglQuerySurface (EGL EGLBoolean "eglQuerySurface" EGLDisplay EGLSurface EGLint EGLint&))
   (define eglSwapBuffers (EGL EGLBoolean "eglSwapBuffers" EGLDisplay EGLSurface))
   (define eglTerminate (EGL EGLBoolean "eglTerminate" EGLDisplay))
   (define eglWaitGL (EGL EGLBoolean "eglWaitGL"))
   (define eglWaitNative (EGL EGLBoolean "eglWaitNative" EGLint))
))
