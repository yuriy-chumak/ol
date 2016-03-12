; July 23, 2003
; https://www.khronos.org/registry/egl/
(define-library (OpenGL EGL version-1-1)
   (export
      EGL_LIBRARY    ; internal variable

      ; EGL Types
      EGLBoolean     ; typedef unsigned int
      EGLint         ; typedef int32_t
      EGLDisplay     ; typedef void*
      EGLConfig      ; typedef void*
      EGLSurface     ; typedef void*
      EGLContext     ; typedef void*

      EGLint*

      ; EGL and native handle values
      NativeDisplayType

      EGL_DEFAULT_DISPLAY
      EGL_NO_CONTEXT
      EGL_NO_DISPLAY
      EGL_NO_SURFACE

      ; Versioning and extensions
      EGL_VERSION_1_0
      EGL_VERSION_1_1

      ; Boolean
      EGL_FALSE EGL_TRUE

      ; Errors
      EGL_SUCCESS                  ;0x3000
      EGL_NOT_INITIALIZED          ;0x3001
      EGL_BAD_ACCESS               ;0x3002
      EGL_BAD_ALLOC                ;0x3003
      EGL_BAD_ATTRIBUTE            ;0x3004
      EGL_BAD_CONFIG               ;0x3005
      EGL_BAD_CONTEXT              ;0x3006
      EGL_BAD_CURRENT_SURFACE      ;0x3007
      EGL_BAD_DISPLAY              ;0x3008
      EGL_BAD_MATCH                ;0x3009
      EGL_BAD_NATIVE_PIXMAP        ;0x300A
      EGL_BAD_NATIVE_WINDOW        ;0x300B
      EGL_BAD_PARAMETER            ;0x300C
      EGL_BAD_SURFACE              ;0x300D
      EGL_CONTEXT_LOST             ;0x300E
      ;/* 0x300F - 0x301F reserved for additional errors. */

      ; Config attributes
      EGL_BUFFER_SIZE              ;0x3020
      EGL_ALPHA_SIZE               ;0x3021
      EGL_BLUE_SIZE                ;0x3022
      EGL_GREEN_SIZE               ;0x3023
      EGL_RED_SIZE                 ;0x3024
      EGL_DEPTH_SIZE               ;0x3025
      EGL_STENCIL_SIZE             ;0x3026
      EGL_CONFIG_CAVEAT            ;0x3027
      EGL_CONFIG_ID                ;0x3028
      EGL_LEVEL                    ;0x3029
      EGL_MAX_PBUFFER_HEIGHT       ;0x302A
      EGL_MAX_PBUFFER_PIXELS       ;0x302B
      EGL_MAX_PBUFFER_WIDTH        ;0x302C
      EGL_NATIVE_RENDERABLE        ;0x302D
      EGL_NATIVE_VISUAL_ID         ;0x302E
      EGL_NATIVE_VISUAL_TYPE       ;0x302F
      ;EGL_PRESERVED_RESOURCES     ;0x3030
      EGL_SAMPLES                  ;0x3031
      EGL_SAMPLE_BUFFERS           ;0x3032
      EGL_SURFACE_TYPE             ;0x3033
      EGL_TRANSPARENT_TYPE         ;0x3034
      EGL_TRANSPARENT_BLUE_VALUE   ;0x3035
      EGL_TRANSPARENT_GREEN_VALUE  ;0x3036
      EGL_TRANSPARENT_RED_VALUE    ;0x3037
      EGL_NONE                     ;0x3038     /* Also a config value */
      EGL_BIND_TO_TEXTURE_RGB      ;0x3039
      EGL_BIND_TO_TEXTURE_RGBA     ;0x303A
      EGL_MIN_SWAP_INTERVAL        ;0x303B
      EGL_MAX_SWAP_INTERVAL        ;0x303C

      ; Config values
      EGL_DONT_CARE                ;((EGLint) -1)

      EGL_SLOW_CONFIG              ;0x3050     /* EGL_CONFIG_CAVEAT value */
      EGL_NON_CONFORMANT_CONFIG    ;0x3051     /* '' */
      EGL_TRANSPARENT_RGB          ;0x3052     /* EGL_TRANSPARENT_TYPE value */
      EGL_NO_TEXTURE               ;0x305C     /* EGL_TEXTURE_FORMAT/TARGET value */
      EGL_TEXTURE_RGB              ;0x305D     /* EGL_TEXTURE_FORMAT value */
      EGL_TEXTURE_RGBA             ;0x305E     /* '' */
      EGL_TEXTURE_2D               ;0x305F     /* EGL_TEXTURE_TARGET value */

      ; Config attribute mask bits
      EGL_PBUFFER_BIT              ;0x01 /* EGL_SURFACE_TYPE mask bit */
      EGL_PIXMAP_BIT               ;0x02 /* '' */
      EGL_WINDOW_BIT               ;0x04 /* '' */

      ; String names
      EGL_VENDOR                   ;0x3053     /* eglQueryString target */
      EGL_VERSION                  ;0x3054     /* '' */
      EGL_EXTENSIONS               ;0x3055     /* '' */

      ; Surface attributes
      EGL_HEIGHT                   ;0x3056
      EGL_WIDTH                    ;0x3057
      EGL_LARGEST_PBUFFER          ;0x3058
      EGL_TEXTURE_FORMAT           ;0x3080     /* For pbuffers bound as textures */
      EGL_TEXTURE_TARGET           ;0x3081     /* '' */
      EGL_MIPMAP_TEXTURE           ;0x3082     /* '' */
      EGL_MIPMAP_LEVEL             ;0x3083     /* '' */

      ; BindTexImage / ReleaseTexImage buffer target
      EGL_BACK_BUFFER              ;0x3084

      ; Current surfaces
      EGL_DRAW                     ;0x3059
      EGL_READ                     ;0x305A

      ; Engines
      EGL_CORE_NATIVE_ENGINE       ;0x305B

      ; 0x305C-0x3FFFF reserved for future use

      ; ** Functions
      ;GLAPI EGLint APIENTRY eglGetError (void);

      eglGetDisplay ; EGLDisplay (EGLNativeDisplayType display_id)
      ;GLAPI EGLDisplay APIENTRY eglGetDisplay (NativeDisplayType display);
      eglInitialize ; EGLBoolean (EGLDisplay dpy, EGLint *major, EGLint *minor)
      ;GLAPI EGLBoolean APIENTRY eglTerminate (EGLDisplay dpy);
      eglQueryString ; const char * eglQueryString (EGLDisplay dpy, EGLint name)
      ;GLAPI void (* APIENTRY eglGetProcAddress (const char *procname))();

      ;GLAPI EGLBoolean APIENTRY eglGetConfigs (EGLDisplay dpy, EGLConfig *configs, EGLint config_size, EGLint *num_config);
      ;GLAPI EGLBoolean APIENTRY eglChooseConfig (EGLDisplay dpy, const EGLint *attrib_list, EGLConfig *configs, EGLint config_size, EGLint *num_config);
      ;GLAPI EGLBoolean APIENTRY eglGetConfigAttrib (EGLDisplay dpy, EGLConfig config, EGLint attribute, EGLint *value);

      ;GLAPI EGLSurface APIENTRY eglCreateWindowSurface (EGLDisplay dpy, EGLConfig config, NativeWindowType window, const EGLint *attrib_list);
      ;GLAPI EGLSurface APIENTRY eglCreatePixmapSurface (EGLDisplay dpy, EGLConfig config, NativePixmapType pixmap, const EGLint *attrib_list);
      ;GLAPI EGLSurface APIENTRY eglCreatePbufferSurface (EGLDisplay dpy, EGLConfig config, const EGLint *attrib_list);
      ;GLAPI EGLBoolean APIENTRY eglDestroySurface (EGLDisplay dpy, EGLSurface surface);
      ;GLAPI EGLBoolean APIENTRY eglQuerySurface (EGLDisplay dpy, EGLSurface surface, EGLint attribute, EGLint *value);

      ;/* EGL 1.1 render-to-texture APIs */
      ;GLAPI EGLBoolean APIENTRY eglSurfaceAttrib (EGLDisplay dpy, EGLSurface surface, EGLint attribute, EGLint value);
      ;GLAPI EGLBoolean APIENTRY eglBindTexImage(EGLDisplay dpy, EGLSurface surface, EGLint buffer);
      ;GLAPI EGLBoolean APIENTRY eglReleaseTexImage(EGLDisplay dpy, EGLSurface surface, EGLint buffer);

      ;/* EGL 1.1 swap control API */
      ;GLAPI EGLBoolean APIENTRY eglSwapInterval(EGLDisplay dpy, EGLint interval);

      ;GLAPI EGLContext APIENTRY eglCreateContext (EGLDisplay dpy, EGLConfig config, EGLContext share_list, const EGLint *attrib_list);
      ;GLAPI EGLBoolean APIENTRY eglDestroyContext (EGLDisplay dpy, EGLContext ctx);
      ;GLAPI EGLBoolean APIENTRY eglMakeCurrent (EGLDisplay dpy, EGLSurface draw, EGLSurface read, EGLContext ctx);
      ;GLAPI EGLContext APIENTRY eglGetCurrentContext (void);
      ;GLAPI EGLSurface APIENTRY eglGetCurrentSurface (EGLint readdraw);
      ;GLAPI EGLDisplay APIENTRY eglGetCurrentDisplay (void);
      ;GLAPI EGLBoolean APIENTRY eglQueryContext (EGLDisplay dpy, EGLContext ctx, EGLint attribute, EGLint *value);

      ;GLAPI EGLBoolean APIENTRY eglWaitGL (void);
      ;GLAPI EGLBoolean APIENTRY eglWaitNative (EGLint engine);
      ;GLAPI EGLBoolean APIENTRY eglSwapBuffers (EGLDisplay dpy, EGLSurface draw);
      ;GLAPI EGLBoolean APIENTRY eglCopyBuffers (EGLDisplay dpy, EGLSurface surface, NativePixmapType target);

)

   (import
      (r5rs base) (owl io) (owl string)
      (OpenGL ES version-1-0)

      (owl pinvoke) (lib platform) (lib x11)
      (owl interop) (owl list))

(begin

(define uname (syscall 63 #f #f #f))
(define EGL_LIBRARY
   (c-string
   (cond
      ;"Windows"
      ((string-ci=? (ref uname 1) "Linux")    "libEGL.so") ; GLESv2 for v2
      ;"HP-UX"
      ;"SunOS"
      ;"Darwin"
      ;"FreeBSD"
      ;"CYGWIN_NT-5.2-WOW64"
      ;"MINGW32_NT-5.2"
      ;...
      (else
         (runtime-error "Unknown platform")))))

(define $ (dlopen EGL_LIBRARY))
(if (not $)
   (runtime-error "Can't load EGL library"))

(define EGLDisplay type-port)
(define EGLNativeDisplayType type-port)


; поддержка расширений :
;(define GetProcAddress ; internal function
;   (dlsym $ type-port (c-string
;      (cond
;         ((string-ci=? (ref uname 1) "Windows")  "wglGetProcAddress")
;         ((string-ci=? (ref uname 1) "Linux")    "glXGetProcAddress")
;         (else
;            (runtime-error "Unknown platform"))))
;   type-string))
;
;(define (glGetProcAddress type name . prototype)
;   (let ((rtty (cons type prototype))
;         (function (GetProcAddress (c-string name))))
;      (if function
;      (lambda args
;         (exec pinvoke function rtty args)))))
;


(define EGLBoolean type-int+) ; typedef int
(define EGLint type-int+)     ; typedef int32_t
(define EGLDisplay type-port) ; typedef void *
(define EGLConfig type-port)  ; typedef void *
(define EGLSurface type-port) ; typedef void *
(define EGLContext type-port) ; typedef void *

(define NativeDisplayType type-port)

(define EGLint* type-vector-raw) ;?


(define EGL_DEFAULT_DISPLAY (raw type-port '(0)))
(define EGL_NO_CONTEXT (raw type-port '(0)))
(define EGL_NO_DISPLAY (raw type-port '(0)))
(define EGL_NO_SURFACE (raw type-port '(0)))

(define EGL_VERSION_1_0 1)
(define EGL_VERSION_1_1 1)

(define EGL_FALSE 0)
(define EGL_TRUE 1)

; ** Errors
(define EGL_SUCCESS                  #x3000)
(define EGL_NOT_INITIALIZED          #x3001)
(define EGL_BAD_ACCESS               #x3002)
(define EGL_BAD_ALLOC                #x3003)
(define EGL_BAD_ATTRIBUTE            #x3004)
(define EGL_BAD_CONFIG               #x3005)
(define EGL_BAD_CONTEXT              #x3006)
(define EGL_BAD_CURRENT_SURFACE      #x3007)
(define EGL_BAD_DISPLAY              #x3008)
(define EGL_BAD_MATCH                #x3009)
(define EGL_BAD_NATIVE_PIXMAP        #x300A)
(define EGL_BAD_NATIVE_WINDOW        #x300B)
(define EGL_BAD_PARAMETER            #x300C)
(define EGL_BAD_SURFACE              #x300D)
(define EGL_CONTEXT_LOST             #x300E)
;0x300F - 0x301F reserved for additional errors.

; ** Config attributes
(define EGL_BUFFER_SIZE              #x3020)
(define EGL_ALPHA_SIZE               #x3021)
(define EGL_BLUE_SIZE                #x3022)
(define EGL_GREEN_SIZE               #x3023)
(define EGL_RED_SIZE                 #x3024)
(define EGL_DEPTH_SIZE               #x3025)
(define EGL_STENCIL_SIZE             #x3026)
(define EGL_CONFIG_CAVEAT            #x3027)
(define EGL_CONFIG_ID                #x3028)
(define EGL_LEVEL                    #x3029)
(define EGL_MAX_PBUFFER_HEIGHT       #x302A)
(define EGL_MAX_PBUFFER_PIXELS       #x302B)
(define EGL_MAX_PBUFFER_WIDTH        #x302C)
(define EGL_NATIVE_RENDERABLE        #x302D)
(define EGL_NATIVE_VISUAL_ID         #x302E)
(define EGL_NATIVE_VISUAL_TYPE       #x302F)
;define EGL_PRESERVED_RESOURCES      #x3030)
(define EGL_SAMPLES                  #x3031)
(define EGL_SAMPLE_BUFFERS           #x3032)
(define EGL_SURFACE_TYPE             #x3033)
(define EGL_TRANSPARENT_TYPE         #x3034)
(define EGL_TRANSPARENT_BLUE_VALUE   #x3035)
(define EGL_TRANSPARENT_GREEN_VALUE  #x3036)
(define EGL_TRANSPARENT_RED_VALUE    #x3037)
(define EGL_NONE                     #x3038)     ; Also a config value
(define EGL_BIND_TO_TEXTURE_RGB      #x3039)
(define EGL_BIND_TO_TEXTURE_RGBA     #x303A)
(define EGL_MIN_SWAP_INTERVAL        #x303B)
(define EGL_MAX_SWAP_INTERVAL        #x303C)

; ** Config values
(define EGL_DONT_CARE                -1)

(define EGL_SLOW_CONFIG              #x3050)     ;/* EGL_CONFIG_CAVEAT value */
(define EGL_NON_CONFORMANT_CONFIG    #x3051)     ;/* '' */
(define EGL_TRANSPARENT_RGB          #x3052)     ;/* EGL_TRANSPARENT_TYPE value */
(define EGL_NO_TEXTURE               #x305C)     ;/* EGL_TEXTURE_FORMAT/TARGET value */
(define EGL_TEXTURE_RGB              #x305D)     ;/* EGL_TEXTURE_FORMAT value */
(define EGL_TEXTURE_RGBA             #x305E)     ;/* '' */
(define EGL_TEXTURE_2D               #x305F)     ;/* EGL_TEXTURE_TARGET value */

; ** Config attribute mask bits
(define EGL_PBUFFER_BIT              #x01)       ;/* EGL_SURFACE_TYPE mask bit */
(define EGL_PIXMAP_BIT               #x02)       ;/* '' */
(define EGL_WINDOW_BIT               #x04)       ;/* '' */

; ** String names
(define EGL_VENDOR                   #x3053)     ;/* eglQueryString target */
(define EGL_VERSION                  #x3054)     ;/* '' */
(define EGL_EXTENSIONS               #x3055)     ;/* '' */

; ** Surface attributes
(define EGL_HEIGHT                   #x3056)
(define EGL_WIDTH                    #x3057)
(define EGL_LARGEST_PBUFFER          #x3058)
(define EGL_TEXTURE_FORMAT           #x3080)     ;/* For pbuffers bound as textures */
(define EGL_TEXTURE_TARGET           #x3081)     ;/* '' */
(define EGL_MIPMAP_TEXTURE           #x3082)     ;/* '' */
(define EGL_MIPMAP_LEVEL             #x3083)     ;/* '' */

; ** BindTexImage / ReleaseTexImage buffer target
(define EGL_BACK_BUFFER              #x3084)

; ** Current surfaces
(define EGL_DRAW                     #x3059)
(define EGL_READ                     #x305A)

; ** Engines
(define EGL_CORE_NATIVE_ENGINE       #x305B)

; 0x305C-0x3FFFF reserved for future use

; ** Functions
(define eglGetError (dlsym $ EGLint "eglGetError"))

(define eglGetDisplay (dlsym $ EGLDisplay "eglGetDisplay" EGLNativeDisplayType))
(define eglInitialize (dlsym $ EGLBoolean "eglInitialize" EGLDisplay EGLint* EGLint*))
;GLAPI EGLBoolean APIENTRY eglTerminate (EGLDisplay dpy);
(define eglQueryString (dlsym $ type-string "eglQueryString" EGLDisplay EGLint))
;GLAPI void (* APIENTRY eglGetProcAddress (const char *procname))();

;GLAPI EGLBoolean APIENTRY eglGetConfigs (EGLDisplay dpy, EGLConfig *configs, EGLint config_size, EGLint *num_config);
;GLAPI EGLBoolean APIENTRY eglChooseConfig (EGLDisplay dpy, const EGLint *attrib_list, EGLConfig *configs, EGLint config_size, EGLint *num_config);
;GLAPI EGLBoolean APIENTRY eglGetConfigAttrib (EGLDisplay dpy, EGLConfig config, EGLint attribute, EGLint *value);

;GLAPI EGLSurface APIENTRY eglCreateWindowSurface (EGLDisplay dpy, EGLConfig config, NativeWindowType window, const EGLint *attrib_list);
;GLAPI EGLSurface APIENTRY eglCreatePixmapSurface (EGLDisplay dpy, EGLConfig config, NativePixmapType pixmap, const EGLint *attrib_list);
;GLAPI EGLSurface APIENTRY eglCreatePbufferSurface (EGLDisplay dpy, EGLConfig config, const EGLint *attrib_list);
;GLAPI EGLBoolean APIENTRY eglDestroySurface (EGLDisplay dpy, EGLSurface surface);
;GLAPI EGLBoolean APIENTRY eglQuerySurface (EGLDisplay dpy, EGLSurface surface, EGLint attribute, EGLint *value);

;/* EGL 1.1 render-to-texture APIs */
;GLAPI EGLBoolean APIENTRY eglSurfaceAttrib (EGLDisplay dpy, EGLSurface surface, EGLint attribute, EGLint value);
;GLAPI EGLBoolean APIENTRY eglBindTexImage(EGLDisplay dpy, EGLSurface surface, EGLint buffer);
;GLAPI EGLBoolean APIENTRY eglReleaseTexImage(EGLDisplay dpy, EGLSurface surface, EGLint buffer);

;/* EGL 1.1 swap control API */
;GLAPI EGLBoolean APIENTRY eglSwapInterval(EGLDisplay dpy, EGLint interval);

;GLAPI EGLContext APIENTRY eglCreateContext (EGLDisplay dpy, EGLConfig config, EGLContext share_list, const EGLint *attrib_list);
;GLAPI EGLBoolean APIENTRY eglDestroyContext (EGLDisplay dpy, EGLContext ctx);
;GLAPI EGLBoolean APIENTRY eglMakeCurrent (EGLDisplay dpy, EGLSurface draw, EGLSurface read, EGLContext ctx);
;GLAPI EGLContext APIENTRY eglGetCurrentContext (void);
;GLAPI EGLSurface APIENTRY eglGetCurrentSurface (EGLint readdraw);
;GLAPI EGLDisplay APIENTRY eglGetCurrentDisplay (void);
;GLAPI EGLBoolean APIENTRY eglQueryContext (EGLDisplay dpy, EGLContext ctx, EGLint attribute, EGLint *value);

;GLAPI EGLBoolean APIENTRY eglWaitGL (void);
;GLAPI EGLBoolean APIENTRY eglWaitNative (EGLint engine);
;GLAPI EGLBoolean APIENTRY eglSwapBuffers (EGLDisplay dpy, EGLSurface draw);
;GLAPI EGLBoolean APIENTRY eglCopyBuffers (EGLDisplay dpy, EGLSurface surface, NativePixmapType target);

))