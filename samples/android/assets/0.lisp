#!/usr/bin/ol

(print *features*)
(import (lib gl))
(import (otus ffi))

(import (OpenGL OES read_format))
(import (OpenGL OES compressed_paletted_texture))
(import (OpenGL OES point_size_array))
(import (OpenGL OES point_sprite))

(define fft-unsigned-char* (fft* fft-unsigned-char))
(define fft-unsigned-short* (fft* fft-unsigned-short))
(define fft-float* (fft* fft-float))

(import (OpenGL ES version-1-1))
;(import (OpenGL OES texture_npot))
; todo: GL_ARB_texture_non_power_of_two

;(glDisable GL_DITHER)
;(glHint GL_PERSPECTIVE_CORRECTION_HINT GL_FASTEST)
(glShadeModel GL_SMOOTH)
;(glDisable GL_BLEND)

(print (glGetString GL_EXTENSIONS))


;;    (define atlas '(0))
;;    (glGenTextures 1 atlas)
;;    (glBindTexture GL_TEXTURE_2D (car atlas))
;;    ;(glEnable GL_TEXTURE_2D)
;;    (glPixelStorei GL_UNPACK_ALIGNMENT 1)
;;    ;; (glPixelStorei GL_PACK_ALIGNMENT 1)

;;    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
;;    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
;;    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
;;    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)

;;    ; создадим текстуру
;;       (define sample '(
;;          255 0 0 255  255 255 255 255  255 255 255 255  255 255 255 255  255 255 255 255  255 255 255 255  255 255 255 255  255 255 255 255
;;          255 255 255 255  255 255 255 255  255 255 255 255  255 255 255 255  255 255 255 255  255 255 255 255  255 255 255 255  255 255 255 255
;;          255 255 255 255  255 255 255 255  255 255 255 255  255 255 255 255  255 255 255 255  255 255 255 255  255 255 255 255  255 255 255 255
;;          255 255 255 255  255 255 255 255  255 255 255 255  255 255 255 255  255 255 255 255  255 255 255 255  255 255 255 255  255 255 255 255
;;          255 255 255 255  255 255 255 255  255 255 255 255  255 255 255 255  255 255 255 255  255 255 255 255  255 255 255 255  255 255 255 255
;;          255 255 255 255  255 255 255 255  255 255 255 255  255 255 255 255  255 255 255 255  255 255 255 255  255 255 255 255  255 255 255 255
;;          255 255 255 255  255 255 255 255  255 255 255 255  255 255 255 255  255 255 255 255  255 255 255 255  255 255 255 255  255 255 255 255
;;          255 255 255 255  255 255 255 255  255 255 255 255  255 255 255 255  255 255 255 255  255 255 255 255  255 255 255 255  255 255 255 255
;;       ))

;;    (glTexImage2D GL_TEXTURE_2D 0
;;       GL_LUMINANCE_ALPHA
;;       8 8 0
;;       GL_LUMINANCE_ALPHA
;;       GL_UNSIGNED_BYTE #f) ;(cons (fft* fft-char) sample))
;;    (print "error: " (glGetError))

;;    (glTexSubImage2D GL_TEXTURE_2D 0
;;       0 ; x
;;       0 ; y
;;       8 8 ; width, height
;;       GL_LUMINANCE_ALPHA GL_UNSIGNED_BYTE (cons (fft* fft-char) sample))
;;    (print "error: " (glGetError))

;; (define positionsBufferObject '(0))
;; (glGenBuffers 1 positionsBufferObject)
;; (print "positionsBufferObject: " positionsBufferObject)

;; (define colourBufferObject '(0))
;; (glGenBuffers 1 colourBufferObject)
;; (print "colourBufferObject: " colourBufferObject)

;; (define texcoordBufferObject '(0))
;; (glGenBuffers 1 texcoordBufferObject)
;; (print "texcoordBufferObject: " texcoordBufferObject)

;; (define vertexPositions '(
;;    -1 -1
;;    -1 1
;;    1 1

;;    -1 -1
;;    1 1
;;    1 -1))
;; (glBindBuffer GL_ARRAY_BUFFER (car positionsBufferObject))
;; (glBufferData GL_ARRAY_BUFFER (* (length vertexPositions) (sizeof GLfloat)) (cons fft-float* vertexPositions) GL_STATIC_DRAW)
;; (glVertexPointer 2 GL_FLOAT 0 #f)

;; (define vertexColours '(
;;    ;; 1 0 0 1
;;    ;; 0 0 1 1
;;    ;; 1 1 0 1
;;    ;; 1 0 0 1
;;    ;; 1 1 0 1
;;    ;; 0 1 0 1))
;;    1 1 1 1
;;    1 1 1 1
;;    1 1 1 1
;;    1 1 1 1
;;    1 1 1 1
;;    1 1 1 1))
;; (glBindBuffer GL_ARRAY_BUFFER (car colourBufferObject))
;; (glBufferData GL_ARRAY_BUFFER (* (length vertexColours) (sizeof GLfloat)) (cons fft-float* vertexColours) GL_STATIC_DRAW)
;; (glColorPointer 4 GL_FLOAT 0 #f)

;; (define vertexTexcoords '(
;;    0 0
;;    0 1
;;    1 1

;;    0 0
;;    1 1
;;    1 0))
;; (glBindBuffer GL_ARRAY_BUFFER (car texcoordBufferObject))
;; (glBufferData GL_ARRAY_BUFFER (* (length vertexTexcoords) (sizeof GLfloat)) (cons fft-float* vertexTexcoords) GL_STATIC_DRAW)
;; (glTexCoordPointer 2 GL_FLOAT 0 #f)


; --- time
(define (time-ms) (let* ((ss ms (clock)))
   (+ (* ss 1000) ms)))

(define started (time-ms))
(define ms '(0))

(import (lib gl console))
; libfreetype

(define welcome (create-window 5 5 10 10))
(set-window-border welcome GREEN)
(set-window-writer welcome (lambda (write)
   (write "Hello " GREEN "me:)")
))

(gl:set-renderer (lambda (mouse)
   (glClearColor 0.2 0.2 0.2 1)
   (glClear GL_COLOR_BUFFER_BIT)

   ;; (glBegin GL_TRIANGLES)
   ;;    (glColor3f 0 1 0)

   ;;    (glVertex2f 0 0)
   ;;    (glVertex2f 0.5 0.7)
   ;;    (glVertex2f 1 0)
   ;; (glEnd)

   (render-windows)

   ;; (glEnableClientState GL_VERTEX_ARRAY)
   ;; (glEnableClientState GL_COLOR_ARRAY)
   ;; (glEnableClientState GL_TEXTURE_COORD_ARRAY)

   ;; (glDrawArrays GL_TRIANGLES 0 6)

   ;; (glDisableClientState GL_TEXTURE_COORD_ARRAY)
   ;; (glDisableClientState GL_COLOR_ARRAY)
   ;; (glDisableClientState GL_VERTEX_ARRAY)

   (let ((now (- (time-ms) started)))
      ;(print (- now (car ms)))
      (set-car! ms now))
))

(print "ok.")
