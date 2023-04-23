(define-library (EGL 1.1)
   (export
      (exports (EGL 1.0))

      EGL_VERSION_1_1

      EGL_BACK_BUFFER
      EGL_BIND_TO_TEXTURE_RGB
      EGL_BIND_TO_TEXTURE_RGBA
      EGL_CONTEXT_LOST
      EGL_MIN_SWAP_INTERVAL
      EGL_MAX_SWAP_INTERVAL
      EGL_MIPMAP_TEXTURE
      EGL_MIPMAP_LEVEL
      EGL_NO_TEXTURE
      EGL_TEXTURE_2D
      EGL_TEXTURE_FORMAT
      EGL_TEXTURE_RGB
      EGL_TEXTURE_RGBA
      EGL_TEXTURE_TARGET

      eglBindTexImage
      eglReleaseTexImage
      eglSurfaceAttrib
      eglSwapInterval
   )
   (import
      (scheme core)
      (EGL 1.0))

(begin
   (define EGL_VERSION_1_1 1)

   (define EGL_BACK_BUFFER                   #x3084)
   (define EGL_BIND_TO_TEXTURE_RGB           #x3039)
   (define EGL_BIND_TO_TEXTURE_RGBA          #x303A)
   (define EGL_CONTEXT_LOST                  #x300E)
   (define EGL_MIN_SWAP_INTERVAL             #x303B)
   (define EGL_MAX_SWAP_INTERVAL             #x303C)
   (define EGL_MIPMAP_TEXTURE                #x3082)
   (define EGL_MIPMAP_LEVEL                  #x3083)
   (define EGL_NO_TEXTURE                    #x305C)
   (define EGL_TEXTURE_2D                    #x305F)
   (define EGL_TEXTURE_FORMAT                #x3080)
   (define EGL_TEXTURE_RGB                   #x305D)
   (define EGL_TEXTURE_RGBA                  #x305E)
   (define EGL_TEXTURE_TARGET                #x3081)

   (define EGL EGL_LIBRARY)
   (define eglBindTexImage (EGL EGLBoolean "eglBindTexImage" EGLDisplay EGLSurface EGLint))
   (define eglReleaseTexImage (EGL EGLBoolean "eglReleaseTexImage" EGLDisplay EGLSurface EGLint))
   (define eglSurfaceAttrib (EGL EGLBoolean "eglSurfaceAttrib" EGLDisplay EGLSurface EGLint EGLint))
   (define eglSwapInterval (EGL EGLBoolean "eglSwapInterval" EGLDisplay EGLint))
))
