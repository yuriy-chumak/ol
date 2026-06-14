#!/usr/bin/env ol
(import (lib gl 4.6 core))
(gl:set-window-title "2. Drawing a Point")

; let's check context version
(define major (box 0))
(define minor (box 0))
(glGetIntegerv GL_MAJOR_VERSION major)
(glGetIntegerv GL_MINOR_VERSION minor)
(print "Context version: " (unbox major) "." (unbox minor))
(print "OpenGL version: " (glGetString GL_VERSION))

; init
(glClearColor 0.3 0.3 0.3 1)

(define VAO (box 0))
(glGenVertexArrays 1 VAO)
(define VAO (unbox VAO))

(define VBO (box 0))
(glGenBuffers 1 VBO)
(define VBO (unbox VBO))

(glBindVertexArray VAO)
(glBindBuffer GL_ARRAY_BUFFER VBO)
(define fft-float* (fft* fft-float))

(define Vertices '((-0.7 -0.7 0) (0.7 -0.7 0)  (0.7 0.7 0)  (-0.7 0.7 0)))
(glBufferData GL_ARRAY_BUFFER (* (sizeof fft-float) 3 (length Vertices)) (cons fft-float* (apply append Vertices)) GL_STATIC_DRAW) ; 3 for x,y,z

; map attrib location
(glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE 0 nullptr) ; 0 for "location = 0", 3 for vec3
(glEnableVertexAttribArray 0)

(glBindVertexArray 0)

(define p (gl:create-program
"#version 330 core
layout (location = 0) in vec3 aPos;
 
void main()
{
    gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0);
}"

"#version 330 core
out vec4 FragColor;
 
void main()
{
    FragColor = vec4(1.0f, 0.5f, 0.2f, 1.0f);
}"))


; draw loop
(gl:set-renderer (lambda ()
   (glClear GL_COLOR_BUFFER_BIT)

   (glUseProgram p)
   (glBindVertexArray VAO)
   (glDrawArrays GL_LINE_LOOP 0 4))) ; totally 4 points
