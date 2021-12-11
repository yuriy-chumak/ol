#!/usr/bin/env ol
(import (lib gl))
(import (otus random!))
(import (scheme dynamic-bindings))

; read initial population
(import (owl parse))
(import (file xpm))

(define population (parse xpm-parser (file->bytestream
   (or
      (and (pair? (command-line)) (car (command-line)))
      "initial.xpm"))
   #f #f #f))

; the size of world
(define WIDTH  (* 2 (population 'width)))
(define HEIGHT (* 2 (population 'height)))

; the color palette in rgb ([0..1][0..1][0..1])
(define colors (vector-map (lambda (color) (map (lambda (v) (inexact (/ v 255))) color))
   '[(255 0 0) (255 90 0) (255 154 0) (255 206 0) (255 232 8)
   (255 232  18) (255 232  38) (255 232  58) (255 232  78) (255 232  98) (255 232 118) (255 232 138) (255 232 158)
   (255 232 178) (255 232 198) (255 232 218) (255 232 238) (255 232 258)
   ]))

(define (color age)
   (ref colors (min age (size colors))))



(define (hash x y)
   (let ((x (mod (+ x WIDTH) WIDTH))
         (y (mod (+ y HEIGHT) HEIGHT)))
   (+ (* y 65536) x)))

(define (alive gen key)
   (let ((x (mod key 65536))
         (y (div key 65536)))
   (let ((n (fold
               (lambda (st dx dy)
                  (+ st (if (gen (hash (+ x dx) (+ y dy)) #f) 1 0)))
               0
               '(-1  0 +1   -1  +1   -1  0 +1)
               '(-1 -1 -1    0   0   +1 +1 +1))))
      (if (eq? n 2)
         (gen (hash x y) #f)
      (if (eq? n 3)
         #true)))))

(gl:set-window-title "Convey's The game of Life")
(import (OpenGL version-2-1))
(import (OpenGL EXT geometry_shader4))

(glShadeModel GL_SMOOTH)
(glClearColor 0.11 0.11 0.11 1)
(glOrtho 0 WIDTH 0 HEIGHT -1 1)

; создадим шейдер превращения точек в квадратики
(define po (glCreateProgram))
(define vs (glCreateShader GL_VERTEX_SHADER))
(define gs (glCreateShader GL_GEOMETRY_SHADER))
(define fs (glCreateShader GL_FRAGMENT_SHADER))

(glShaderSource vs 1 (list "
   #version 120 // OpenGL 2.1
   void main() {
      gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
      gl_FrontColor = gl_Color;
   }") #false)
(glCompileShader vs)
(glAttachShader po vs)

; more info: https://www.khronos.org/opengl/wiki/Geometry_Shader_Examples
(glShaderSource gs 1 (list "
   #version 120
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
   }") #false)
(glCompileShader gs)
(glAttachShader po gs)
(glProgramParameteri po GL_GEOMETRY_INPUT_TYPE GL_POINTS)
(glProgramParameteri po GL_GEOMETRY_OUTPUT_TYPE GL_TRIANGLE_STRIP) ; only POINTS, LINE_STRIP and TRIANGLE_STRIP is allowed
(glProgramParameteri po GL_GEOMETRY_VERTICES_OUT 4)

(glShaderSource fs 1 (list "
   #version 120 // OpenGL 2.1
   void main(void) {
      gl_FragColor = gl_Color;
   }
") #false)
(glCompileShader fs)
(glAttachShader po fs)

(glLinkProgram po)

(glDetachShader po fs)
(glDetachShader po gs)
(glDetachShader po vs)

; shaders done
(gl:set-resize-handler (lambda (width height)
   (glViewport 0 0 width height)
   (glPointSize (/ width WIDTH))))


(define userdata (make-parameter
   (let ((initial population))
      (fold (lambda (ff row y)
               (fold (lambda (ff col x)
                        (if (eq? col #\space)
                           ff
                           (put ff (hash x y) 1)))
                  ff row (iota (initial 'width) (/ (- WIDTH (initial 'width)) 2))))
         #empty
         (initial 'bitmap)
         (iota (initial 'height) (/ (- HEIGHT (initial 'height)) 2))))))


(gl:set-renderer (lambda (mouse)
(let ((generation (userdata)))
   (glClear GL_COLOR_BUFFER_BIT)

   (glUseProgram po)
   (glColor3f 0.2 0.5 0.2)
   (glBegin GL_POINTS)
      (ff-fold (lambda (st key value)
         (apply glColor3f (color value))
         (glVertex2f (mod key 65536)
                     (div key 65536))
      ) #f generation)
   (glEnd)

   (print "cells count: "
   (ff-fold (lambda (st key value)
               (+ st 1))
      0
      generation))

   ;; speedup:
   ;; 1. select all possible points
   (define new-generation
      (ff-union
      (ff-fold
         (lambda (st key value)
            (let ((x (mod key 65536))
                  (y (div key 65536)))
               (fold
                  (lambda (st dx dy) (put st (hash (+ x dx) (+ y dy)) 0))
                  st
                  '(-1  0 +1  -1 +1  -1  0 +1)
                  '(-1 -1 -1   0  0  +1 +1 +1))))
         {} generation) generation (lambda (a b) b))) ; save the current point age

   (userdata
      (ff-fold (lambda (st key value)
            (if (alive generation key)
               (put st key (+ value 1))
               st))
         #empty new-generation)))))
