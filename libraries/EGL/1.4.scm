(define-library (EGL 1.4)
   (export
      (exports (EGL 1.3))

      EGL_VERSION_1_4

      EGL_DEFAULT_DISPLAY
      EGL_MULTISAMPLE_RESOLVE_BOX_BIT
      EGL_MULTISAMPLE_RESOLVE
      EGL_MULTISAMPLE_RESOLVE_DEFAULT
      EGL_MULTISAMPLE_RESOLVE_BOX
      EGL_OPENGL_API
      EGL_OPENGL_BIT
      EGL_SWAP_BEHAVIOR_PRESERVED_BIT

      eglGetCurrentContext
   )
   (import
      (scheme core)
      (EGL 1.3))

(begin
   (define EGL_VERSION_1_4 1)

   (define EGL_DEFAULT_DISPLAY             (vm:cast 0 EGLNativeDisplayType))
   (define EGL_MULTISAMPLE_RESOLVE_BOX_BIT #x0200)
   (define EGL_MULTISAMPLE_RESOLVE         #x3099)
   (define EGL_MULTISAMPLE_RESOLVE_DEFAULT #x309A)
   (define EGL_MULTISAMPLE_RESOLVE_BOX     #x309B)
   (define EGL_OPENGL_API                  #x30A2)
   (define EGL_OPENGL_BIT                  #x0008)
   (define EGL_SWAP_BEHAVIOR_PRESERVED_BIT #x0400)

   (define EGL EGL_LIBRARY)
   (define eglGetCurrentContext (EGL EGLContext "eglGetCurrentContext"))
))
