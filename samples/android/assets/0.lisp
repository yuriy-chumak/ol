#!/usr/bin/ol

(print *features*)
(import (lib gl))
(import (otus ffi))

(import (OpenGL OES read_format))
(import (OpenGL OES compressed_paletted_texture))
(import (OpenGL OES point_size_array))
(import (OpenGL OES point_sprite))

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

(define fft-unsigned-char* (fft* fft-unsigned-char))
(define fft-unsigned-short* (fft* fft-unsigned-short))
(define fft-float* (fft* fft-float))


(import (OpenGL ES version-1-1))

(glDisable GL_DITHER)
(glHint GL_PERSPECTIVE_CORRECTION_HINT GL_FASTEST)
(glShadeModel GL_SMOOTH)

(glEnable GL_COLOR_MATERIAL) ;?

;; (glMatrixMode GL_PROJECTION)
;; (glLoadIdentity)
;(glFrustumf -2.6 2.6 -2.6 2.6 -2.0 2.0)
;(glLoadIdentity)
(glOrthof 0 1 0 1 0 1)
;; (glMatrixMode GL_MODELVIEW)

(define positionsBufferObject '(0))
(glGenBuffers 1 positionsBufferObject)
(print "positionsBufferObject: " positionsBufferObject)

(define colourBufferObject '(0))
(glGenBuffers 1 colourBufferObject)
(print "colourBufferObject: " colourBufferObject)

(define vertexPositions '(
   0 0
   0.5 0.7
   1 0))
(glBindBuffer GL_ARRAY_BUFFER (car positionsBufferObject))
(glBufferData GL_ARRAY_BUFFER (* (length vertexPositions) (sizeof GLfloat)) (cons fft-float* vertexPositions) GL_STATIC_DRAW)
(glVertexPointer 2 GL_FLOAT 0 #f)

(define vertexColours '(
   1 0 0
   0 0 1
   0 1 0))
(glBindBuffer GL_ARRAY_BUFFER (car colourBufferObject))
(glBufferData GL_ARRAY_BUFFER (* (length vertexColours) (sizeof GLfloat)) (cons fft-float* vertexColours) GL_STATIC_DRAW)
(glColorPointer 3 GL_FLOAT 0 #f)

; --- time
(define (time-ms) (let* ((ss ms (clock)))
   (+ (* ss 1000) ms)))

(define started (time-ms))
(define ms '(0))

(gl:set-renderer (lambda (mouse)
   (glClearColor 0.2 0.2 0.2 1)
   (glClear GL_COLOR_BUFFER_BIT)

   (glEnableClientState GL_VERTEX_ARRAY)
   (glEnableClientState GL_COLOR_ARRAY)

   (glDrawArrays GL_TRIANGLES 0 3)

   (glDisableClientState GL_VERTEX_ARRAY)
   (glDisableClientState GL_COLOR_ARRAY)

   ;; ;(glLoadIdentity)
   ;; ;(glTranslatef 0 0 -6)
   ;; ;(glRotatef 55 0.3 1 1.3)
   ;; (glDrawElements GL_TRIANGLE_FAN 8 GL_UNSIGNED_SHORT (cons fft-unsigned-short* '(
   ;;    0 1 2 3 6 5 4 1)))
   ;; (glDrawElements GL_TRIANGLE_FAN 8 GL_UNSIGNED_SHORT (cons fft-unsigned-short* '(
   ;;    7 6 3 2 1 4 5 6)))

   (let ((now (- (time-ms) started)))
      ;(print (- now (car ms)))
      (set-car! ms now))
))

(print "ok.")
