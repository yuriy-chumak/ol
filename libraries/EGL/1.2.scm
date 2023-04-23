(define-library (EGL 1.2)
   (export
      (exports (EGL 1.1))

      EGL_VERSION_1_2

      EGLenum
      EGLClientBuffer

      EGL_ALPHA_FORMAT
      EGL_ALPHA_FORMAT_NONPRE
      EGL_ALPHA_FORMAT_PRE
      EGL_ALPHA_MASK_SIZE
      EGL_BUFFER_PRESERVED
      EGL_BUFFER_DESTROYED
      EGL_CLIENT_APIS
      EGL_COLORSPACE
      EGL_COLORSPACE_sRGB
      EGL_COLORSPACE_LINEAR
      EGL_COLOR_BUFFER_TYPE
      EGL_CONTEXT_CLIENT_TYPE
      EGL_DISPLAY_SCALING
      EGL_HORIZONTAL_RESOLUTION
      EGL_LUMINANCE_BUFFER
      EGL_LUMINANCE_SIZE
      EGL_OPENGL_ES_BIT
      EGL_OPENVG_BIT
      EGL_OPENGL_ES_API
      EGL_OPENVG_API
      EGL_OPENVG_IMAGE
      EGL_PIXEL_ASPECT_RATIO
      EGL_RENDERABLE_TYPE
      EGL_RENDER_BUFFER
      EGL_RGB_BUFFER
      EGL_SINGLE_BUFFER
      EGL_SWAP_BEHAVIOR
      EGL_UNKNOWN
      EGL_VERTICAL_RESOLUTION

      eglBindAPI
      eglQueryAPI
      eglCreatePbufferFromClientBuffer
      eglReleaseThread
      eglWaitClient
   )
   (import
      (scheme core)
      (EGL 1.1))

(begin
   (define EGL_VERSION_1_2 1)

   (define EGLenum fft-unsigned-int)
   (define EGLClientBuffer fft-void*)

   (define EGL_ALPHA_FORMAT                  #x3088)
   (define EGL_ALPHA_FORMAT_NONPRE           #x308B)
   (define EGL_ALPHA_FORMAT_PRE              #x308C)
   (define EGL_ALPHA_MASK_SIZE               #x303E)
   (define EGL_BUFFER_PRESERVED              #x3094)
   (define EGL_BUFFER_DESTROYED              #x3095)
   (define EGL_CLIENT_APIS                   #x308D)
   (define EGL_COLORSPACE                    #x3087)
   (define EGL_COLORSPACE_sRGB               #x3089)
   (define EGL_COLORSPACE_LINEAR             #x308A)
   (define EGL_COLOR_BUFFER_TYPE             #x303F)
   (define EGL_CONTEXT_CLIENT_TYPE           #x3097)
   (define EGL_DISPLAY_SCALING                10000)
   (define EGL_HORIZONTAL_RESOLUTION         #x3090)
   (define EGL_LUMINANCE_BUFFER              #x308F)
   (define EGL_LUMINANCE_SIZE                #x303D)
   (define EGL_OPENGL_ES_BIT                 #x0001)
   (define EGL_OPENVG_BIT                    #x0002)
   (define EGL_OPENGL_ES_API                 #x30A0)
   (define EGL_OPENVG_API                    #x30A1)
   (define EGL_OPENVG_IMAGE                  #x3096)
   (define EGL_PIXEL_ASPECT_RATIO            #x3092)
   (define EGL_RENDERABLE_TYPE               #x3040)
   (define EGL_RENDER_BUFFER                 #x3086)
   (define EGL_RGB_BUFFER                    #x308E)
   (define EGL_SINGLE_BUFFER                 #x3085)
   (define EGL_SWAP_BEHAVIOR                 #x3093)
   (define EGL_UNKNOWN                       #xFFFFFFFF) ; (EGLint) -1
   (define EGL_VERTICAL_RESOLUTION           #x3091)

   (define EGL EGL_LIBRARY)
   (define eglBindAPI (EGL EGLBoolean "eglBindAPI" EGLenum))
   (define eglQueryAPI (EGL EGLenum "eglQueryAPI"))
   (define eglCreatePbufferFromClientBuffer (EGL EGLSurface "eglCreatePbufferFromClientBuffer" EGLDisplay EGLenum EGLClientBuffer EGLConfig EGLint*))
   (define eglReleaseThread (EGL EGLBoolean "eglReleaseThread"))
   (define eglWaitClient (EGL EGLBoolean "eglWaitClient"))
))
