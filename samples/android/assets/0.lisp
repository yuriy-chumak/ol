#!/usr/bin/ol

(import (lib gl2))

;; (print (syscall 63 #f #f #f))

;; (print "loading ffi...")
;; (import (otus ffi))

;; (let ((self (load-dynamic-library #f)))
;; (let ((androidGetWindow (self fft-int "androidGetWindow"))
;;       (androidGetFloat  (self fft-float "androidGetFloat" fft-float fft-int fft-int fft-int fft-int fft-float)))
;;    ;(define display (eglGetDisplay EGL_DEFAULT_DISPLAY))
;;    (print "androidGetWindow: " (androidGetWindow))
;;    (print "androidGetFloat: " (exact (androidGetFloat 1 2 3 4 5 6)))
;;    #true))

(import (OpenGL ES version-1-1))

(glDisable GL_DITHER)
(glHint GL_PERSPECTIVE_CORRECTION_HINT GL_FASTEST)
(glShadeModel GL_SMOOTH)

(gl:set-renderer (lambda (mouse)
   (glClearColor 1 0 0 0)
   (glClear GL_COLOR_BUFFER_BIT)
))

(print "ok.")
