(import (otus ffi))
(define egl (load-dynamic-library #f))
(define NULL #false)

(print "uname: " (syscall 63 0 0 0))

(define eglGetDisplay (egl fft-int "eglGetDisplay" type-vptr))
(define eglInitialize (egl fft-int "eglInitialize" fft-int (fft& fft-int) (fft& fft-int)))
(define eglGetConfigs (egl fft-int "eglGetConfigs" fft-int type-string fft-int (fft& fft-int)))
(define eglChooseConfig (egl fft-int "eglChooseConfig" fft-int (fft* fft-int) type-string fft-int (fft& fft-int)))
(define eglCreateWindowSurface (egl fft-int "eglCreateWindowSurface" fft-int fft-unsigned-int fft-int (fft* fft-int))) ; temp "unsigned int" instead of void*
(define eglCreateContext (egl fft-int "eglCreateContext" fft-int fft-unsigned-int fft-int (fft* fft-int))); temp "unsigned int" instead of void*

(define eglMakeCurrent (egl fft-int "eglMakeCurrent" fft-int fft-int fft-int fft-int))
(define eglSwapBuffers (egl fft-int "eglSwapBuffers" fft-int fft-int))

(define major '(0))
(define minor '(0))

(define display (eglGetDisplay NULL))
(print "eglInitialize: " (eglInitialize display major minor))
(print "major: " (car major) ", minor: " (car minor))

(define numConfigs '(0))
(print "eglGetConfigs: " (eglGetConfigs display NULL 0 numConfigs))

(define attribList '(
   #x3024 5 ; red
   #x3023 6 ; green
   #x3022 5 ; blue
   #x3021 8 ; alpha
   #x3025 8 ; depth
   ;#x3026 ; stencil
   #x3032 1 ; sample buffers
   #x3038 ; EGL_NONE
))
(define config (vm:new-raw-object type-vector-raw '(0 0 0 0)))

(print "eglChooseConfig: " (eglChooseConfig display attribList config 1 numConfigs))
(define surface (eglCreateWindowSurface display (int32->ol config 0) 2 NULL))                 ; temp "2" instead of XCreateWindow

(define contextAttribs '(
     #x3098 2 #x3038 #x3038 ; EGL_CONTEXT_CLIENT_VERSION, 2, EGL_NONE, EGL_NONE
))

(define context (eglCreateContext display (int32->ol config 0) 0 contextAttribs))           ; 0 == EGL_NO_CONTEXT


(print "eglMakeCurrent: " (eglMakeCurrent display surface surface context))


(define glClearColor (egl fft-void "glClearColor" fft-float fft-float fft-float fft-float))
(define glClear (egl fft-void "glClear" fft-int))

;(glClearColor 0 0.4 1 1)
;(glClear #x00004000)

(let loop ((color 0))
   (if (> color 1)
      (loop 0)
      (begin
         (glClearColor color 0 0 1)
         (glClear #x00004000)
         (eglSwapBuffers display surface)
         (syscall 24 0 0 0) ; (yield) - prevent web freezing
         (loop (+ color 0.01)))))

