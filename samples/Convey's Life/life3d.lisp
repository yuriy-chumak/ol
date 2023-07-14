#!/usr/bin/env ol
(import (lib gl-2)
        (lib GLU))
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


(gl:set-window-title "Convey's The game of Life (3D)")

(glShadeModel GL_SMOOTH)
(glClearColor 0.11 0.11 0.11 1)

(glEnable GL_DEPTH_TEST)
(glEnable GL_CULL_FACE)
;; (glCullFace GL_BACK)
;; (glFrontFace GL_CW)

;; (gl:set-resize-handler (lambda (width height)
;;    (glViewport 0 0 width height)

;;    (glMatrixMode GL_PROJECTION)
;;    (glLoadIdentity)
;;    (gluPerspective 45 (/ width height) 1 10000)))

; read initial population
(import (owl parse))
(import (file xpm))

(define population (parse xpm-parser (file->bytestream
   (or
      (and (pair? (command-line)) (car (command-line)))
      "initial.xpm"))
   #f #f #f))

(import (scheme dynamic-bindings))
(define userdata (make-parameter
   (let ((initial population))
      (fold (lambda (ff row y)
               (fold (lambda (ff col x)
                        (if (eq? col #\space)
                           ff
                           (put ff (hash x y (/ DEPTH 2)) 1)))
                  ff row (iota (initial 'width) (/ (- WIDTH (initial 'width)) 2))))
         #empty
         (initial 'bitmap)
         (iota (initial 'height) (/ (- HEIGHT (initial 'height)) 2))))))

(define delta-z (<< (<< 1 SHIFT) SHIFT))
(userdata
   (ff-fold (lambda (p key value)
               (put p (+ key delta-z) value))
      (userdata)
      (userdata)))


; создадим шейдер превращения точек в кубики
(define po (gl:create-program
"#version 120 // OpenGL 2.1
   varying vec4 vertexPosition;
   varying vec4 vertexNormal;

   void main() {
      vertexPosition = gl_ModelViewMatrix * gl_Vertex; // vertex position in the modelview space (not just in world space)
      vertexNormal   = gl_ModelViewMatrix * vec4(gl_Normal, 0.0);

      gl_Position = gl_ProjectionMatrix * vertexPosition;
      gl_FrontColor = gl_Color;
   }"
GL_POINTS GL_TRIANGLE_STRIP 14
"#version 120
   #extension GL_EXT_geometry_shader4 : enable

   // more info: https://www.khronos.org/opengl/wiki/Geometry_Shader_Examples
   void emit(float dx, float dy, float dz)
   {
      gl_Position = gl_PositionIn[0] + gl_ModelViewProjectionMatrix * vec4(dx, dy, dz, 0.0);
      gl_FrontColor = gl_FrontColorIn[0];
      EmitVertex();
   }
   void main()
   {
      float D = 0.46;
      emit(-D, D,-D); // Front-top-left
      emit( D, D,-D); // Front-top-right
      emit(-D,-D,-D); // Front-bottom-left
      emit( D,-D,-D); // Front-bottom-right
      emit( D,-D, D); // Back-bottom-right
      emit( D, D,-D); // Front-top-right
      emit( D, D, D); // Back-top-right
      emit(-D, D,-D); // Front-top-left
      emit(-D, D, D); // Back-top-left
      emit(-D,-D,-D); // Front-bottom-left
      emit(-D,-D, D); // Back-bottom-left
      emit( D,-D, D); // Back-bottom-right
      emit(-D, D, D); // Back-top-left
      emit( D, D, D); // Back-top-right
   }"

"#version 120 // OpenGL 2.1
   void main(void) {
      gl_FragColor = gl_Color;
   }
"))


; add some random userdata
(import (otus random!))
(userdata
   (fold (lambda (ff n)
         (define x (+ -25 (rand! 50) (/ WIDTH 2)))
         (define y (+ -25 (rand! 50) (/ HEIGHT 2)))
         (define z (+ -25 (rand! 50) (/ DEPTH 2)))
         (put ff (hash x y z) 1))
      (userdata)
      (iota (* 50 50 50 0.01))))



(define angle (box 10))

(gl:set-renderer (lambda (mouse)
(let ((generation (userdata)))
   (glViewport 0 0 (gl:get-window-width) (gl:get-window-height))
   (glClear (vm:ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   (glEnable GL_LIGHT0)
   (glLightfv GL_LIGHT0 GL_POSITION '(0 0 -50 0))

   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity)
   (gluPerspective 45 (/ (gl:get-window-width) (gl:get-window-height)) 1 1000)

   ;; (print "cells count: "
   ;; (ff-fold (lambda (st key value)
   ;;             (+ st 1))
   ;;    0
   ;;    generation))

   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (define A 130)
   (set-car! angle (mod (+ (unbox angle) 1) 2261))
   (define eyeX (* A (sin (/ (unbox angle) 360))))
   (define eyeY (* A (cos (/ (unbox angle) 360))))

   (gluLookAt 150 150 150
      ;eyeX eyeY 180
      ; to:
      ;(/ WIDTH 2) (/ HEIGHT 2) (/ DEPTH 2)
      0 0 0
      ; head up:
      0 1 0)

   (glUseProgram 0)

   (define W WIDTH)
   (define H HEIGHT)
   (define D DEPTH)

   ; draw axis
   (glBegin GL_LINES)
   (glColor3f 1 0 0)
   (glVertex3f 0 0 0)
   (glVertex3f W 0 0)
   (glColor3f 0 1 0)
   (glVertex3f 0 0 0)
   (glVertex3f 0 H 0)
   (glColor3f 0 0 1)
   (glVertex3f 0 0 0)
   (glVertex3f 0 0 D)
   (glEnd)

   ; ...
   (glUseProgram po)
   (glColor3f 1 1 1)
   (glBegin GL_POINTS)
   (ff-fold (lambda (st key value)
         (glColor3fv (color value))
         (let ((x (mod key $HASH))
               (y (mod (>> key SHIFT) $HASH))
               (z (>> key (+ SHIFT SHIFT))))
            (glVertex3f x y z)))
      #f generation)
   (glEnd)

   (userdata
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
