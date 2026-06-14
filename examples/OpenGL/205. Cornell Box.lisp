#!/usr/bin/env ol

(import (lib gl 2.1))
(gl:set-window-title "5. Cornell Box")

(import (lib GLU))

; init
(glShadeModel GL_SMOOTH)
(glClearColor 0.8 0.8 0.8 1)
(glEnable GL_DEPTH_TEST)

,load "media/cornell-box.scm"

; shaders
(define colorize (gl:create-program
"#version 110 // OpenGL 2.0
   void main() {
      gl_Position = ftransform();
      gl_FrontColor = gl_Color;
   }"
"#version 110 // OpenGL 2.0
   void main() {
      gl_FragColor = gl_Color;
   }"))


; draw
(gl:set-renderer (lambda ()
   (glClear (bor GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity)
   (gluPerspective 45 (/ (gl:get-window-width) (gl:get-window-height)) 0.1 100)

   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)

   (gluLookAt
      -0.278 0.273 0.8
      -0.278 0.273 -0.2
      0 1 0)

   (define (vertex x y z  nx ny nz  u v  c)
      (define color (ref colors c))
      (glColor3fv color)
      (glNormal3f nx ny nz)
      (glVertex3f x y z))

   (glUseProgram colorize)
   (glBegin GL_TRIANGLES)
      (for-each (lambda (i)
            (apply vertex (ref vertices i)))
         indices)
   (glEnd)

))
