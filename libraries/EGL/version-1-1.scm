; July 23, 2003
; based on https://www.khronos.org/registry/egl/
(define-library (EGL version-1-1)
   (export
      EGL_DEFAULT_DISPLAY
      EGL_NO_CONTEXT
      EGL_NO_DISPLAY
      EGL_NO_SURFACE

      EGL_VERSION_1_0
      EGL_VERSION_1_1

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
      EGL_CONTEXT_LOST

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
      EGL_BIND_TO_TEXTURE_RGB
      EGL_BIND_TO_TEXTURE_RGBA
      EGL_MIN_SWAP_INTERVAL
      EGL_MAX_SWAP_INTERVAL

      EGL_DONT_CARE

      EGL_SLOW_CONFIG
      EGL_NON_CONFORMANT_CONFIG
      EGL_TRANSPARENT_RGB
      EGL_NO_TEXTURE
      EGL_TEXTURE_RGB
      EGL_TEXTURE_RGBA
      EGL_TEXTURE_2D

      EGL_PBUFFER_BIT
      EGL_PIXMAP_BIT
      EGL_WINDOW_BIT

      EGL_VENDOR
      EGL_VERSION
      EGL_EXTENSIONS

      EGL_HEIGHT
      EGL_WIDTH
      EGL_LARGEST_PBUFFER
      EGL_TEXTURE_FORMAT
      EGL_TEXTURE_TARGET
      EGL_MIPMAP_TEXTURE
      EGL_MIPMAP_LEVEL

      EGL_BACK_BUFFER

      EGL_DRAW
      EGL_READ

      EGL_CORE_NATIVE_ENGINE

      ; -------
      eglGetError

      eglGetDisplay
      eglInitialize
      ;eglTerminate
      eglQueryString
      ;eglGetProcAddress

      eglGetConfigs
      eglChooseConfig
      ;eglGetConfigAttrib (EGLDisplay dpy, EGLConfig config, EGLint attribute, EGLint *value);

      eglCreateWindowSurface ; EGLSurface (EGLDisplay dpy, EGLConfig config, NativeWindowType window, const EGLint *attrib_list);
      ;eglCreatePixmapSurface ; EGLSurface (EGLDisplay dpy, EGLConfig config, NativePixmapType pixmap, const EGLint *attrib_list);
      ;eglCreatePbufferSurface ; EGLSurface (EGLDisplay dpy, EGLConfig config, const EGLint *attrib_list);
      ;eglDestroySurface (EGLDisplay dpy, EGLSurface surface);
      ;eglQuerySurface (EGLDisplay dpy, EGLSurface surface, EGLint attribute, EGLint *value);

      ;eglSurfaceAttrib (EGLDisplay dpy, EGLSurface surface, EGLint attribute, EGLint value);
      ;eglBindTexImage(EGLDisplay dpy, EGLSurface surface, EGLint buffer);
      ;eglReleaseTexImage(EGLDisplay dpy, EGLSurface surface, EGLint buffer);

      ;eglSwapInterval(EGLDisplay dpy, EGLint interval);

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
   )
   (import
      (scheme core) (owl string)
      (owl io)

      (otus ffi))

   (begin

; Types and resources

(define EGLBoolean fft-int)
(define EGLint fft-int) (define EGLint* (fft* EGLint)) (define EGLint& (fft& EGLint))

(define EGLDisplay fft-int)
(define EGLConfig  type-vptr)  (define EGLConfig* (fft* EGLConfig))   (define EGLConfig& (fft& EGLConfig))
(define EGLSurface fft-int)
(define EGLContext fft-int)
(define NativeDisplayType fft-void*)
(define NativeWindowType  fft-void*)
(define NativePixmapType  fft-void*)


; EGL and native handle values

(define EGL_DEFAULT_DISPLAY 0) ;NativeDisplayType
(define EGL_NO_CONTEXT  0) ;EGLContext
(define EGL_NO_DISPLAY  0) ;EGLDisplay
(define EGL_NO_SURFACE  0) ;EGLSurface


; Versioning and extensions

(define EGL_VERSION_1_0 1)
(define EGL_VERSION_1_1 1)


; Boolean

(define EGL_FALSE       0)
(define EGL_TRUE        1)

;/*
;** Errors
;*/
(define EGL_SUCCESS               #x3000)
(define EGL_NOT_INITIALIZED       #x3001)
(define EGL_BAD_ACCESS            #x3002)
(define EGL_BAD_ALLOC             #x3003)
(define EGL_BAD_ATTRIBUTE         #x3004)
(define EGL_BAD_CONFIG            #x3005)
(define EGL_BAD_CONTEXT           #x3006)
(define EGL_BAD_CURRENT_SURFACE   #x3007)
(define EGL_BAD_DISPLAY           #x3008)
(define EGL_BAD_MATCH             #x3009)
(define EGL_BAD_NATIVE_PIXMAP     #x300A)
(define EGL_BAD_NATIVE_WINDOW     #x300B)
(define EGL_BAD_PARAMETER         #x300C)
(define EGL_BAD_SURFACE           #x300D)
(define EGL_CONTEXT_LOST          #x300E)
;/* 0x300F - 0x301F reserved for additional errors. */

;/*
;** Config attributes
;*/
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
;/*#define EGL_PRESERVED_RESOURCES     0x3030*/
(define EGL_SAMPLES                 #x3031)
(define EGL_SAMPLE_BUFFERS          #x3032)
(define EGL_SURFACE_TYPE            #x3033)
(define EGL_TRANSPARENT_TYPE        #x3034)
(define EGL_TRANSPARENT_BLUE_VALUE  #x3035)
(define EGL_TRANSPARENT_GREEN_VALUE #x3036)
(define EGL_TRANSPARENT_RED_VALUE   #x3037)
(define EGL_NONE                    #x3038)    ;/* Also a config value #/)
(define EGL_BIND_TO_TEXTURE_RGB     #x3039)
(define EGL_BIND_TO_TEXTURE_RGBA    #x303A)
(define EGL_MIN_SWAP_INTERVAL       #x303B)
(define EGL_MAX_SWAP_INTERVAL       #x303C)

;/*
;** Config values
;*/
(define EGL_DONT_CARE           #xFFFFFFFF)    ;((EGLint) -1)

(define EGL_SLOW_CONFIG             #x3050)    ;/* EGL_CONFIG_CAVEAT value */
(define EGL_NON_CONFORMANT_CONFIG   #x3051)    ;/* " */
(define EGL_TRANSPARENT_RGB         #x3052)    ;/* EGL_TRANSPARENT_TYPE value */
(define EGL_NO_TEXTURE              #x305C)    ;/* EGL_TEXTURE_FORMAT/TARGET value */
(define EGL_TEXTURE_RGB             #x305D)    ;/* EGL_TEXTURE_FORMAT value */
(define EGL_TEXTURE_RGBA            #x305E)    ;/* " */
(define EGL_TEXTURE_2D              #x305F)    ;/* EGL_TEXTURE_TARGET value */

;/*
;** Config attribute mask bits
;*/
(define EGL_PBUFFER_BIT               #x01)    ;/* EGL_SURFACE_TYPE mask bit */
(define EGL_PIXMAP_BIT                #x02)    ;/* " */
(define EGL_WINDOW_BIT                #x04)    ;/* " */

;/*
;** String names
;*/
(define EGL_VENDOR                  #x3053)    ;/* eglQueryString target */
(define EGL_VERSION                 #x3054)    ;/* " */
(define EGL_EXTENSIONS              #x3055)    ;/* " */

;/*
;** Surface attributes
;*/
(define EGL_HEIGHT                  #x3056)
(define EGL_WIDTH                   #x3057)
(define EGL_LARGEST_PBUFFER         #x3058)
(define EGL_TEXTURE_FORMAT          #x3080)    ;/* For pbuffers bound as textures */
(define EGL_TEXTURE_TARGET          #x3081)    ;/* " */
(define EGL_MIPMAP_TEXTURE          #x3082)    ;/* " */
(define EGL_MIPMAP_LEVEL            #x3083)    ;/* " */

;/*
;** BindTexImage / ReleaseTexImage buffer target
;*/
(define EGL_BACK_BUFFER             #x3084)

;/*
;** Current surfaces
;*/
(define EGL_DRAW                    #x3059)
(define EGL_READ                    #x305A)

;/*
;** Engines
;*/
(define EGL_CORE_NATIVE_ENGINE      #x305B)

;/* 0x305C-0x3FFFF reserved for future use */

; --------------------------------
(define uname (syscall 63 #f #f #f))

(define $ (or
   (load-dynamic-library
      (cond
         ((string-ci=? (ref uname 1) "windows")  "libEGL.dll")
         ((string-ci=? (ref uname 1) "linux")    "libEGL.so") ; GLESv2 for v2
         ((string-ci=? (ref uname 1) "emscripten") #f)  ; self for Emscripten
         (else
            (runtime-error "Unknown platform" uname))))
   (runtime-error "Can't load EGL library")))


(define eglGetError ($ EGLint "eglGetError"))

(define eglGetDisplay ($ EGLDisplay "eglGetDisplay" NativeDisplayType))
(define eglInitialize ($ EGLBoolean "eglInitialize" EGLDisplay EGLint& EGLint&))
;GLAPI EGLBoolean APIENTRY eglTerminate (EGLDisplay dpy);
(define eglQueryString ($ type-string "eglQueryString" EGLDisplay EGLint))
;GLAPI void (* APIENTRY eglGetProcAddress (const char *procname))();

(define eglGetConfigs ($ EGLBoolean "eglGetConfigs" EGLDisplay EGLConfig& EGLint EGLint&))
(define eglChooseConfig ($ EGLBoolean "eglChooseConfig" EGLDisplay EGLint* EGLConfig* EGLint EGLint&))
;GLAPI EGLBoolean APIENTRY eglGetConfigAttrib (EGLDisplay dpy, EGLConfig config, EGLint attribute, EGLint *value);

(define eglCreateWindowSurface ($ EGLSurface "eglCreateWindowSurface" EGLDisplay EGLConfig NativeWindowType EGLint*))
;GLAPI EGLSurface APIENTRY eglCreatePixmapSurface (EGLDisplay dpy, EGLConfig config, NativePixmapType pixmap, const EGLint *attrib_list);
;GLAPI EGLSurface APIENTRY eglCreatePbufferSurface (EGLDisplay dpy, EGLConfig config, const EGLint *attrib_list);
;GLAPI EGLBoolean APIENTRY eglDestroySurface (EGLDisplay dpy, EGLSurface surface);
;GLAPI EGLBoolean APIENTRY eglQuerySurface (EGLDisplay dpy, EGLSurface surface, EGLint attribute, EGLint *value);
;
;/* EGL 1.1 render-to-texture APIs */
;GLAPI EGLBoolean APIENTRY eglSurfaceAttrib (EGLDisplay dpy, EGLSurface surface, EGLint attribute, EGLint value);
;GLAPI EGLBoolean APIENTRY eglBindTexImage(EGLDisplay dpy, EGLSurface surface, EGLint buffer);
;GLAPI EGLBoolean APIENTRY eglReleaseTexImage(EGLDisplay dpy, EGLSurface surface, EGLint buffer);
;
;/* EGL 1.1 swap control API */
;GLAPI EGLBoolean APIENTRY eglSwapInterval(EGLDisplay dpy, EGLint interval);

(define eglCreateContext ($ EGLContext "eglCreateContext" EGLDisplay EGLConfig EGLContext EGLint*))
;GLAPI EGLBoolean APIENTRY eglDestroyContext (EGLDisplay dpy, EGLContext ctx);
(define eglMakeCurrent   ($ EGLBoolean "eglMakeCurrent" EGLDisplay EGLSurface EGLSurface EGLContext))
;GLAPI EGLContext APIENTRY eglGetCurrentContext (void);
;GLAPI EGLSurface APIENTRY eglGetCurrentSurface (EGLint readdraw);
;GLAPI EGLDisplay APIENTRY eglGetCurrentDisplay (void);
;GLAPI EGLBoolean APIENTRY eglQueryContext (EGLDisplay dpy, EGLContext ctx, EGLint attribute, EGLint *value);
;
;GLAPI EGLBoolean APIENTRY eglWaitGL (void);
;GLAPI EGLBoolean APIENTRY eglWaitNative (EGLint engine);
(define eglSwapBuffers   ($ EGLBoolean "eglSwapBuffers" EGLDisplay EGLSurface))
;GLAPI EGLBoolean APIENTRY eglCopyBuffers (EGLDisplay dpy, EGLSurface surface, NativePixmapType target);

))