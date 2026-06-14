#!/usr/bin/env ol

; create OpenGL window and import OpenGL functions
(import (lib gl 3.2 core))
(gl:set-window-title "2. Drawing simple triangle")
(import (lib gl 3 shading)) ; temp

; global init
(glClearColor 0.3 0.3 0.3 1)

(define po (gl:create-program
"#version 330 core
   layout (location = 0) in vec3 aPos;
   layout (location = 1) in vec3 aColor;
   out vec3 Color;
   
   void main() {
       gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0);
       Color = aColor;
   }"
"#version 330 core
   in vec3 Color;
   out vec4 FragColor;
   
   void main() {
       FragColor = vec4(Color, 1.0f);
   }"))

(define VAO (box 0))
(glGenVertexArrays 1 VAO)
(define VAO (unbox VAO))

(define VBO (box 0))
(glGenBuffers 1 VBO)
(define VBO (unbox VBO))

(glBindVertexArray VAO)
(glBindBuffer GL_ARRAY_BUFFER VBO)
(define fft-float* (fft* fft-float))

                  ; x    y    z   r g b
(define Vertices '(-0.6 -0.6  0   1 0 0
                    0.6 -0.6  0   0 1 0
                    0.0  0.7  0   0 0 1))
(glBufferData GL_ARRAY_BUFFER (* (sizeof fft-float) (length Vertices)) (cons fft-float* Vertices) GL_STATIC_DRAW)

; map attrib location(s)
(define &xyz 0) (define &rgb (* (sizeof fft-float) 3))
(define stride (* (sizeof fft-float) 6)) ; 6 for "x,y,z,r,g,b"
(glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE stride &xyz) ; 0 for "location = 0", 3 for vec3
(glEnableVertexAttribArray 0)
(glVertexAttribPointer 1 3 GL_FLOAT GL_FALSE stride &rgb) ; 1 for "location = 1", 3 for vec3
(glEnableVertexAttribArray 1)

(glBindVertexArray 0)

; render loop
(gl:set-renderer (lambda ()
   (glClear GL_COLOR_BUFFER_BIT)

   (glUseProgram po)
   (glBindVertexArray VAO)
   (glDrawArrays GL_TRIANGLES 0 3) )) ; 3 for "3 triangle vertices"
