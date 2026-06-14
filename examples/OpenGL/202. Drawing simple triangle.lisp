#!/usr/bin/env ol

; create OpenGL window and import OpenGL functions
(import (lib gl 2.1))
(gl:set-window-title "2. Drawing simple triangle")

; global init
(glClearColor 0.3 0.3 0.3 1)

(define po (gl:create-program
"#version 120 // OpenGL 2.1
   void main() {
      gl_Position = gl_Vertex;
      gl_FrontColor = gl_Color;
   }"
"#version 120 // OpenGL 2.1
   void main(void) {
      gl_FragColor = gl_Color;
   }"))

; render loop
(gl:set-renderer (lambda ()
   (glClear GL_COLOR_BUFFER_BIT)

   (glUseProgram po)

   (glBegin GL_TRIANGLES)
      (glColor3f 1 0 0)
      (glVertex2f -0.6 -0.6)

      (glColor3f 0 1 0)
      (glVertex2f +0.6 -0.6)

      (glColor3f 0 0 1)
      (glVertex2f -0.0 +0.7)
   (glEnd) ))
