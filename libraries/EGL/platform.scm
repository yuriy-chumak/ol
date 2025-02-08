(define-library (EGL platform)
   (description
      "Platform-specific types and definitions for EGL")

(export
   (exports (otus ffi))

   EGLNativeDisplayType
   EGLNativePixmapType
   EGLNativeWindowType
   
   EGL_LIBRARY)

(import
   (scheme core)
   (otus ffi))

(cond-expand
   ((or Linux Android)
      (begin
         (define EGLNativeDisplayType type-vptr)
         (define EGLNativePixmapType type-vptr)
         (define EGLNativeWindowType type-vptr)

         (setq library-name "libEGL.so")
      ))
   (Windows
      (begin
         (define EGLNativeDisplayType fft-int32)
         (define EGLNativePixmapType fft-int32)
         (define EGLNativeWindowType fft-int32)

         (setq library-name "libEGL.dll")
      ))
   (Emscripten
      (begin
         (define EGLNativeDisplayType fft-int)
         (define EGLNativePixmapType fft-int)
         (define EGLNativeWindowType fft-int)

         (setq library-name "gl2es.wasm")
      ))
   )

(begin
   (define EGL_LIBRARY (load-dynamic-library library-name))
   (unless EGL_LIBRARY
      (runtime-error "Can't load EGL library" library-name))

))
