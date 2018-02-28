(import (lib opengl))
(print)

(define NULL #false)
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
(define config (make-vptr-array 1))

(print "eglChooseConfig: " (eglChooseConfig display attribList config (car numConfigs) numConfigs))

(define surface (eglCreateWindowSurface display (car config) 2 NULL))                 ; temp "2" instead of XCreateWindow
(define contextAttribs '(
     #x3098 2 #x3038 #x3038 ; EGL_CONTEXT_CLIENT_VERSION, 2, EGL_NONE, EGL_NONE
))

(define context (eglCreateContext display (car config) EGL_NO_CONTEXT contextAttribs))

; --
(glClearColor 0.3 0.3 0.3 1)
(glClear GL_COLOR_BUFFER_BIT)
