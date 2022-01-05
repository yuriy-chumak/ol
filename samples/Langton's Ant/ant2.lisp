#!/usr/bin/env ol
(import (otus random!))

(define MAX 65536)  ; should be power of two
; size of game board (should be less than MAX)
(define WIDTH 170)
(define HEIGHT 96)

; helper function
(define (hash x y)
   (let ((x (mod (+ x WIDTH) WIDTH))
         (y (mod (+ y HEIGHT) HEIGHT)))
   (+ (* y MAX) x)))

;; ; helper function
(define directions '(
   (0 . 1) (1 . 0) (0 . -1) (-1 . 0)
))

; the color palette in rgb ([0..1][0..1][0..1])
(define colors (vector-map (lambda (color) (map (lambda (v) (inexact (/ v 255))) color))
   '[(255 0 0) (255 90 0) (255 154 0) (255 206 0) (255 232 8)
   (255 232  18) (255 232  38) (255 232  58) (255 232  78) (255 232  98) (255 232 118) (255 232 138) (255 232 158)
   (255 232 178) (255 232 198) (255 232 218) (255 232 238) (255 232 258)
   ]))

(define (color age)
   (ref colors (min age (size colors))))


; ---------------
(import (lib gl-2))
(gl:set-window-title "Langton's Ant")

(glShadeModel GL_SMOOTH)
(glClearColor 0.11 0.11 0.11 1)
(glOrtho 0 WIDTH 0 HEIGHT 0 1)

; -------------------------------------------
; создадим шейдер превращения точек в квадратики
(define po (gl:create-program GL_POINTS GL_TRIANGLE_STRIP 4
"#version 120
   #extension GL_EXT_geometry_shader4 : enable

   void main()
   {
      gl_Position = gl_PositionIn[0];
      gl_FrontColor = gl_FrontColorIn[0];
      EmitVertex();

      gl_Position = gl_PositionIn[0] + gl_ModelViewProjectionMatrix * vec4(1.0, 0.0, 0.0, 0.0);
      gl_FrontColor = gl_FrontColorIn[0];
      EmitVertex();

      gl_Position = gl_PositionIn[0] + gl_ModelViewProjectionMatrix * vec4(0.0, 1.0, 0.0, 0.0);
      gl_FrontColor = gl_FrontColorIn[0];
      EmitVertex();

      gl_Position = gl_PositionIn[0] + gl_ModelViewProjectionMatrix * vec4(1.0, 1.0, 0.0, 0.0);
      gl_FrontColor = gl_FrontColorIn[0];
      EmitVertex();
   }"
"#version 120 // OpenGL 2.1
   void main() {
      gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
      gl_FrontColor = gl_Color;
   }"
"#version 120 // OpenGL 2.1
   void main(void) {
      gl_FragColor = gl_Color;
   }
"))

; -------------------------------------------
(gl:set-resize-handler (lambda (width height)
   (glViewport 0 0 width height)))

; generate random field
(import (scheme dynamic-bindings))
(define userdata (make-parameter
   (pairs->ff (map (lambda (i) (let ((x (rand! WIDTH)) (y (rand! HEIGHT)))
                                 (cons (hash x y) 0))) (iota 1000)))))

(define ant (cons
   (rand! WIDTH)
   (rand! HEIGHT)))
(define dir (list (rand! 4))) ; 0, 1, 2, 3

; ts
(define timestamp (box 1))
; main game loop
(gl:set-renderer (lambda (mouse)
(let ((generation (userdata)))
   (glClear GL_COLOR_BUFFER_BIT)

   ; draw the cells
   (glUseProgram po)
   (glBegin GL_POINTS)
      (ff-fold (lambda (st key value)
         (glColor3fv (color (- (car timestamp) value)))
         (glVertex2f (mod key MAX)
                     (div key MAX))
      ) #f generation)
      (glColor3f 0.8 0.2 0.1)
      (glVertex2f (car ant) (cdr ant))
   (glEnd)

   (userdata
      (let*((x (car ant))
            (y (cdr ant))
            (stamp (car timestamp))
            (generation (if (get generation (hash x y) #f)
                           then ; black cell
                              (set-car! dir (mod (+ (car dir) 1) 4))
                              (del generation (hash x y))
                           else
                              (set-car! dir (mod (+ (car dir) 7) 4))
                              (put generation (hash x y) stamp))))
         (set-car! ant (mod (+ x (car (lref directions (car dir)))) WIDTH))
         (set-cdr! ant (mod (+ y (cdr (lref directions (car dir)))) HEIGHT))
         generation))
   
   (set-car! timestamp (+ (car timestamp) 1))


)))
