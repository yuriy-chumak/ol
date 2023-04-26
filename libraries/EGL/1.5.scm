(define-library (EGL 1.5)
   (export
      (exports (EGL 1.4))

      EGL_VERSION_1_5

      EGLSync
      EGLAttrib
      EGLTime
      EGLImage

      EGL_CONTEXT_MAJOR_VERSION
      EGL_CONTEXT_MINOR_VERSION
      EGL_CONTEXT_OPENGL_PROFILE_MASK
      EGL_CONTEXT_OPENGL_RESET_NOTIFICATION_STRATEGY
      EGL_NO_RESET_NOTIFICATION
      EGL_LOSE_CONTEXT_ON_RESET
      EGL_CONTEXT_OPENGL_CORE_PROFILE_BIT
      EGL_CONTEXT_OPENGL_COMPATIBILITY_PROFILE_BIT
      EGL_CONTEXT_OPENGL_DEBUG
      EGL_CONTEXT_OPENGL_FORWARD_COMPATIBLE
      EGL_CONTEXT_OPENGL_ROBUST_ACCESS
      EGL_OPENGL_ES3_BIT
      EGL_CL_EVENT_HANDLE
      EGL_SYNC_CL_EVENT
      EGL_SYNC_CL_EVENT_COMPLETE
      EGL_SYNC_PRIOR_COMMANDS_COMPLETE
      EGL_SYNC_TYPE
      EGL_SYNC_STATUS
      EGL_SYNC_CONDITION
      EGL_SIGNALED
      EGL_UNSIGNALED
      EGL_SYNC_FLUSH_COMMANDS_BIT
      EGL_FOREVER
      EGL_TIMEOUT_EXPIRED
      EGL_CONDITION_SATISFIED
      EGL_NO_SYNC
      EGL_SYNC_FENCE
      EGL_GL_COLORSPACE
      EGL_GL_COLORSPACE_SRGB
      EGL_GL_COLORSPACE_LINEAR
      EGL_GL_RENDERBUFFER
      EGL_GL_TEXTURE_2D
      EGL_GL_TEXTURE_LEVEL
      EGL_GL_TEXTURE_3D
      EGL_GL_TEXTURE_ZOFFSET
      EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_X
      EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_X
      EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_Y
      EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_Y
      EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_Z
      EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_Z
      EGL_IMAGE_PRESERVED
      EGL_NO_IMAGE

      eglCreateSync
      eglDestroySync
      eglClientWaitSync
      eglGetSyncAttrib
      eglCreateImage
      eglDestroyImage
      eglGetPlatformDisplay
      eglCreatePlatformWindowSurface
      eglCreatePlatformPixmapSurface
      eglWaitSync

   )
   (import
      (scheme core)
      (EGL 1.4))

(begin
   (define EGL_VERSION_1_5 1)

   (define EGLSync fft-void*)
   (define EGLAttrib fft-void*) (define EGLAttrib* (fft* EGLAttrib))
   (define EGLTime fft-uint64) ; khronos_utime_nanoseconds_t
   (define EGLImage fft-void*)

   (define EGL_CONTEXT_MAJOR_VERSION          #x3098)
   (define EGL_CONTEXT_MINOR_VERSION          #x30FB)
   (define EGL_CONTEXT_OPENGL_PROFILE_MASK    #x30FD)
   (define EGL_CONTEXT_OPENGL_RESET_NOTIFICATION_STRATEGY #x31BD)
   (define EGL_NO_RESET_NOTIFICATION          #x31BE)
   (define EGL_LOSE_CONTEXT_ON_RESET          #x31BF)
   (define EGL_CONTEXT_OPENGL_CORE_PROFILE_BIT #x00000001)
   (define EGL_CONTEXT_OPENGL_COMPATIBILITY_PROFILE_BIT #x00000002)
   (define EGL_CONTEXT_OPENGL_DEBUG           #x31B0)
   (define EGL_CONTEXT_OPENGL_FORWARD_COMPATIBLE #x31B1)
   (define EGL_CONTEXT_OPENGL_ROBUST_ACCESS   #x31B2)
   (define EGL_OPENGL_ES3_BIT                 #x00000040)
   (define EGL_CL_EVENT_HANDLE                #x309C)
   (define EGL_SYNC_CL_EVENT                  #x30FE)
   (define EGL_SYNC_CL_EVENT_COMPLETE         #x30FF)
   (define EGL_SYNC_PRIOR_COMMANDS_COMPLETE   #x30F0)
   (define EGL_SYNC_TYPE                      #x30F7)
   (define EGL_SYNC_STATUS                    #x30F1)
   (define EGL_SYNC_CONDITION                 #x30F8)
   (define EGL_SIGNALED                       #x30F2)
   (define EGL_UNSIGNALED                     #x30F3)
   (define EGL_SYNC_FLUSH_COMMANDS_BIT        #x0001)
   (define EGL_FOREVER                        #xFFFFFFFFFFFFFFFF)
   (define EGL_TIMEOUT_EXPIRED                #x30F5)
   (define EGL_CONDITION_SATISFIED            #x30F6)
   (define EGL_NO_SYNC                        (vm:cast 0 EGLSync))
   (define EGL_SYNC_FENCE                     #x30F9)
   (define EGL_GL_COLORSPACE                  #x309D)
   (define EGL_GL_COLORSPACE_SRGB             #x3089)
   (define EGL_GL_COLORSPACE_LINEAR           #x308A)
   (define EGL_GL_RENDERBUFFER                #x30B9)
   (define EGL_GL_TEXTURE_2D                  #x30B1)
   (define EGL_GL_TEXTURE_LEVEL               #x30BC)
   (define EGL_GL_TEXTURE_3D                  #x30B2)
   (define EGL_GL_TEXTURE_ZOFFSET             #x30BD)
   (define EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_X #x30B3)
   (define EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_X #x30B4)
   (define EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_Y #x30B5)
   (define EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_Y #x30B6)
   (define EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_Z #x30B7)
   (define EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_Z #x30B8)
   (define EGL_IMAGE_PRESERVED                #x30D2)
   (define EGL_NO_IMAGE                       (vm:cast 0 EGLImage))

   (define EGL EGL_LIBRARY)
   (define eglCreateSync (EGL EGLSync "eglCreateSync" EGLDisplay EGLenum EGLAttrib*))
   (define eglDestroySync (EGL EGLBoolean "eglDestroySync" EGLDisplay EGLSync))
   (define eglClientWaitSync (EGL EGLint "eglClientWaitSync" EGLDisplay EGLSync EGLint EGLTime))
   (define eglGetSyncAttrib (EGL EGLBoolean "eglGetSyncAttrib" EGLDisplay EGLSync EGLint EGLAttrib*))
   (define eglCreateImage (EGL EGLImage "eglCreateImage" EGLDisplay EGLContext EGLenum EGLClientBuffer EGLAttrib*))
   (define eglDestroyImage (EGL EGLBoolean "eglDestroyImage" EGLDisplay EGLImage))
   (define eglGetPlatformDisplay (EGL EGLDisplay "eglGetPlatformDisplay" EGLenum fft-void* EGLAttrib*))
   (define eglCreatePlatformWindowSurface (EGL EGLSurface "eglCreatePlatformWindowSurface" EGLDisplay EGLConfig fft-void* EGLAttrib*))
   (define eglCreatePlatformPixmapSurface (EGL EGLSurface "eglCreatePlatformPixmapSurface" EGLDisplay EGLConfig fft-void* EGLAttrib*))
   (define eglWaitSync (EGL EGLBoolean "eglWaitSync" EGLDisplay EGLSync EGLint))
))
