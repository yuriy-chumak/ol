(define-library (EGL 1.3)
   (export
      (exports (EGL 1.2))

      EGL_VERSION_1_3

      EGL_CONFORMANT
      EGL_CONTEXT_CLIENT_VERSION
      EGL_MATCH_NATIVE_PIXMAP
      EGL_OPENGL_ES2_BIT
      EGL_VG_ALPHA_FORMAT
      EGL_VG_ALPHA_FORMAT_NONPRE
      EGL_VG_ALPHA_FORMAT_PRE
      EGL_VG_ALPHA_FORMAT_PRE_BIT
      EGL_VG_COLORSPACE
      EGL_VG_COLORSPACE_sRGB
      EGL_VG_COLORSPACE_LINEAR
      EGL_VG_COLORSPACE_LINEAR_BIT
   )
   (import
      (scheme core)
      (EGL 1.2))

(begin
   (define EGL_VERSION_1_3 1)

   (define EGL_CONFORMANT                    #x3042)
   (define EGL_CONTEXT_CLIENT_VERSION        #x3098)
   (define EGL_MATCH_NATIVE_PIXMAP           #x3041)
   (define EGL_OPENGL_ES2_BIT                #x0004)
   (define EGL_VG_ALPHA_FORMAT               #x3088)
   (define EGL_VG_ALPHA_FORMAT_NONPRE        #x308B)
   (define EGL_VG_ALPHA_FORMAT_PRE           #x308C)
   (define EGL_VG_ALPHA_FORMAT_PRE_BIT       #x0040)
   (define EGL_VG_COLORSPACE                 #x3087)
   (define EGL_VG_COLORSPACE_sRGB            #x3089)
   (define EGL_VG_COLORSPACE_LINEAR          #x308A)
   (define EGL_VG_COLORSPACE_LINEAR_BIT      #x0020)
))
