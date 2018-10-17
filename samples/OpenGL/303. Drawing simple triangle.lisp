#!/usr/bin/ol
(import (lib gl))
(gl:set-window-title "1. Creating an OpenGL 3.0 Window")

(gl:set-context-version 3 0) ; use OpenGL version 3.0
(import (OpenGL version-3-0))

; let's check context version
(define major (box 0))
(define minor (box 0))
(glGetIntegerv GL_MAJOR_VERSION major)
(glGetIntegerv GL_MINOR_VERSION minor)
(print "Context version: " (unbox major) "." (unbox minor))
(print "OpenGL version: " (glGetString GL_VERSION))

; init
(glShadeModel GL_SMOOTH)
(glClearColor 0.3 0.3 0.3 1)

(define vbo (box 0))
(glGenBuffers 1 vbo)
(define vbo (unbox vbo))

(glBindBuffer GL_ARRAY_BUFFER vbo)

(define Vertices '(
      -1 -1 0
       1 -1 0
       0  1 0))
(glBufferData GL_ARRAY_BUFFER (* (sizeof fft-float) (length Vertices)) (cons (fft* fft-float) Vertices) GL_STATIC_DRAW)
(glEnableVertexAttribArray 0)

(define po (gl:CreateProgram
"#version 330 core
	in vec4 position;
	void main() {
		gl_Position = position;
	}"
"#version 330 core
	out vec4 out_color;
	void main(void) {
		out_color = vec4(1.0, 1.0, 1.0, 1.0);
	}"))


(define vao (box 0))
(glGenVertexArrays 1 vao)
(glBindVertexArray (unbox vao))

; draw loop
(gl:set-renderer (lambda ()
   (glClear GL_COLOR_BUFFER_BIT)
   (glColor3f 1 1 0)

))
   ;(glBindBuffer GL_ARRAY_BUFFER vbo)
   ;(glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE 0 NULL)
   ;(glDrawArrays GL_TRIANGLES 0 3)))
