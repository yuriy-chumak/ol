#!/usr/bin/env ol
(import (lib gl))
(import (otus random!))
(import (scheme inexact))

; the size of world
(define WIDTH (* 100 2))  ; x
(define HEIGHT (* 100 2)) ; y
(define DEPTH (* 100 2))  ; z

; the color palette in rgb ([0..1][0..1][0..1])
(define colors (vector-map (lambda (color) (map (lambda (v) (inexact (/ v 255))) (append color '(255))))
   '[(255 0 0) (255 90 0) (255 154 0) (255 206 0) (255 232 8)
   (255 232  18) (255 232  38) (255 232  58) (255 232  78) (255 232  98) (255 232 118) (255 232 138) (255 232 158)
   (255 232 178) (255 232 198) (255 232 218) (255 232 238) (255 232 258)
   ]))

(define (color age)
   (ref colors (min age (size colors))))

(define SHIFT 10) ; 1024 maximal
(assert (value? (<< (<< DEPTH SHIFT) SHIFT)) ===> #true)
(define $HASH (<< 1 SHIFT))

(define (hash x y z)
   (let ((x (mod (+ x WIDTH) WIDTH))
         (y (mod (+ y HEIGHT) HEIGHT))
         (z (mod (+ z DEPTH) DEPTH)))
   (+ x
      (<< y SHIFT)
      (<< z (+ SHIFT SHIFT)))))

(define (alive gen x y z)
   (let ((n
      (fold (lambda (s dx dy dz)
               (+ s (if (getf gen (hash (+ x dx) (+ y dy) (+ z dz))) 1 0)))
         0
         '(-1  0 +1   -1  0 +1   -1  0 +1     -1  0 +1   -1    +1   -1  0 +1     -1  0 +1   -1  0 +1   -1  0 +1)
         '(-1 -1 -1    0  0  0   +1 +1 +1     -1 -1 -1    0     0   +1 +1 +1     -1 -1 -1    0  0  0   +1 +1 +1)
         '(-1 -1 -1   -1 -1 -1   -1 -1 -1      0  0  0    0     0    0  0  0     +1 +1 +1   +1 +1 +1   +1 +1 +1))))
      ; the rules:
      ;; (if (getf gen (hash x y z))
      ;;    (<= 4 n 7)
      ;;    (<= 5 n 5)))) ; dead, should resurrect?
      (if (gen (hash x y z) #false)
         (<= 5 n 7) ; alive, should be still alive?
         (<= 5 n 5)))) ; dead, should resurrect?


(gl:set-window-title "Convey's The game of Life")
(import (OpenGL version-1-0))

(glShadeModel GL_SMOOTH)
(glClearColor 0.11 0.11 0.11 1)

(glEnable GL_DEPTH_TEST)
(glDisable GL_CULL_FACE)

(glEnable GL_LIGHTING)
(glLightModelf GL_LIGHT_MODEL_TWO_SIDE GL_TRUE)
;; (glEnable GL_NORMALIZE)

(glEnable GL_LIGHT0)
(glLightfv GL_LIGHT0 GL_POSITION '(50 50 50 0))
;; (glLightfv GL_LIGHT0 GL_DIFFUSE '(1 1 1))
;; (glLightfv GL_LIGHT0 GL_SPECULAR '(1.0 1.0 1.0  1))
;; (glLightfv GL_LIGHT0 GL_AMBIENT  '(0.6 0.6 0.6  1))

(define cube (glGenLists 1))
(glNewList cube GL_COMPILE)
   (glBegin GL_QUADS)
      ; нижняя
      (glNormal3f 0 0 -1)
      (glVertex3f -0.5 -0.5 -0.5)
      (glVertex3f -0.5 +0.5 -0.5)
      (glVertex3f +0.5 +0.5 -0.5)
      (glVertex3f +0.5 -0.5 -0.5)
      ; верхняя
      (glNormal3f 0 0 +1)
      (glVertex3f -0.5 -0.5 +0.5)
      (glVertex3f -0.5 +0.5 +0.5)
      (glVertex3f +0.5 +0.5 +0.5)
      (glVertex3f +0.5 -0.5 +0.5)

      ; левая
      (glNormal3f -1 0 0)
      (glVertex3f -0.5 -0.5 -0.5)
      (glVertex3f -0.5 -0.5 +0.5)
      (glVertex3f +0.5 -0.5 +0.5)
      (glVertex3f +0.5 -0.5 -0.5)
      ; правая
      (glNormal3f +1 0 0)
      (glVertex3f -0.5 +0.5 -0.5)
      (glVertex3f -0.5 +0.5 +0.5)
      (glVertex3f +0.5 +0.5 +0.5)
      (glVertex3f +0.5 +0.5 -0.5)

      ; задняя
      (glNormal3f 0 -1 0)
      (glVertex3f -0.5 -0.5 -0.5)
      (glVertex3f -0.5 -0.5 +0.5)
      (glVertex3f -0.5 +0.5 +0.5)
      (glVertex3f -0.5 +0.5 -0.5)
      ; передняя
      (glNormal3f 0 +1 0)
      (glVertex3f +0.5 -0.5 -0.5)
      (glVertex3f +0.5 -0.5 +0.5)
      (glVertex3f +0.5 +0.5 +0.5)
      (glVertex3f +0.5 +0.5 -0.5)
   (glEnd)
(glEndList)

(gl:set-resize-handler (lambda (width height)
   (glViewport 0 0 width height)

   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity)
   (gluPerspective 45 (/ width height) 1 10000)))

; read initial population
(import (owl parse))
(import (file xpm))

(define population (parse xpm-parser (file->bytestream
   (or
      (and (pair? *vm-args*) (car *vm-args*))
      "initial.xpm"))
   #f #f #f))

(gl:set-userdata
   (let ((initial population))
      (fold (lambda (ff row y)
               (fold (lambda (ff col x)
                        (if (eq? col #\space)
                           ff
                           (put ff (hash x y (/ DEPTH 2)) 1)))
                  ff row (iota (initial 'width) (/ (- WIDTH (initial 'width)) 2))))
         #empty
         (initial 'bitmap)
         (iota (initial 'height) (/ (- HEIGHT (initial 'height)) 2)))))

(define delta-z (<< (<< 1 SHIFT) SHIFT))
(gl:set-userdata
   (ff-fold (lambda (p key value)
               (put p (+ key delta-z) value))
      (gl:get-userdata)
      (gl:get-userdata)))


; add some random userdata
(import (otus random!))
(gl:set-userdata
   (fold (lambda (ff n)
         (define x (+ -25 (rand! 50) (/ WIDTH 2)))
         (define y (+ -25 (rand! 50) (/ HEIGHT 2)))
         (define z (+ -25 (rand! 50) (/ DEPTH 2)))
         (put ff (hash x y z) 1))
      (gl:get-userdata)
      (iota (* 50 50 50 0.01))))


(define angle (box 10))

(gl:set-renderer (lambda (mouse)
(let ((generation (gl:get-userdata)))
   (glClear (vm:ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   ;; (print "cells count: "
   ;; (ff-fold (lambda (st key value)
   ;;             (+ st 1))
   ;;    0
   ;;    generation))

   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (define A 230)
   (set-car! angle (mod (+ (unbox angle) 1) 2261))
   (define eyeX (* A (sin (/ (unbox angle) 360))))
   (define eyeY (* A (cos (/ (unbox angle) 360))))

   (gluLookAt eyeX eyeY 180
      ; to:
      (/ WIDTH 2) (/ HEIGHT 2) (/ DEPTH 2)
      ; head up:
      0 0 1)

   (glDisable GL_LIGHTING)
   ;; (glDisable GL_COLOR_MATERIAL)

   (glColor3f 0 0 0.7)
   (define W WIDTH)
   (define H HEIGHT)
   (define D DEPTH)

   (glBegin GL_LINE_STRIP)
      (glVertex3f 0 0 0)
      (glVertex3f 0 H 0)
      (glVertex3f W H 0)
      (glVertex3f W 0 0)
      (glVertex3f 0 0 0)
   (glEnd)
   (glBegin GL_LINE_STRIP)
      (glVertex3f 0 0 D)
      (glVertex3f 0 H D)
      (glVertex3f W H D)
      (glVertex3f W 0 D)
      (glVertex3f 0 0 D)
   (glEnd)
   (glBegin GL_LINES)
      (glVertex3f 0 0 0)
      (glVertex3f 0 0 D)
      (glVertex3f 0 H 0)
      (glVertex3f 0 H D)
      (glVertex3f W H 0)
      (glVertex3f W H D)
      (glVertex3f W 0 0)
      (glVertex3f W 0 D)
   (glEnd)


   (glEnable GL_LIGHTING)
   (glColor3f 1 1 1)
   (ff-fold (lambda (st key value)
         ;; (apply glColor3f (color value))
         (glMaterialfv GL_FRONT_AND_BACK GL_SPECULAR (color value))
         (let ((x (mod key $HASH))
               (y (mod (>> key SHIFT) $HASH))
               (z (>> key (+ SHIFT SHIFT))))
            (glPushMatrix)
            (glTranslatef x y z)
            (glCallList cube)
            (glPopMatrix)))
      #f generation)

   (gl:set-userdata
      (ff-fold (lambda (st key value)
         (let ((X (mod key $HASH))
               (Y (mod (>> key SHIFT) $HASH))
               (Z (>> key (+ SHIFT SHIFT))))
            (fold (lambda (st dx dy dz)
                     (define x (+ X dx))
                     (define y (+ Y dy))
                     (define z (+ Z dz))
                     (if (alive generation x y z)
                        (put st (hash x y z) (+ (generation (hash x y z) 0) 1))
                        st))
               st
               '(-1  0 +1   -1  0 +1   -1  0 +1     -1  0 +1   -1  0 +1   -1  0 +1     -1  0 +1   -1  0 +1   -1  0 +1)
               '(-1 -1 -1    0  0  0   +1 +1 +1     -1 -1 -1    0  0  0   +1 +1 +1     -1 -1 -1    0  0  0   +1 +1 +1)
               '(-1 -1 -1   -1 -1 -1   -1 -1 -1      0  0  0    0  0  0    0  0  0     +1 +1 +1   +1 +1 +1   +1 +1 +1))))
         {} generation)))))
