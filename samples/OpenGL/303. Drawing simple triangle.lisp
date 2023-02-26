#!/usr/bin/env ol
(import (lib gl-3))
(gl:set-window-title "3. Drawing simple triangle")

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

(define VBO (box 0))
(glGenBuffers 1 VBO)
(define VBO (unbox VBO))

(glBindBuffer GL_ARRAY_BUFFER VBO)
(define fft-float* (fft* fft-float))

(define Vertices '((-0.6 -0.6 0) (0.6 -0.6 0)  (0 0.7 0)))
(glBufferData GL_ARRAY_BUFFER (* (sizeof fft-float) 3 (length Vertices)) (cons fft-float* (apply append Vertices)) GL_STATIC_DRAW) ; 3 for x,y,z
(glEnableVertexAttribArray 0)

(define po (gl:create-program
"#version 130
	in vec3 position;
	void main() {
		gl_Position = vec4(position, 1.0);
	}"
"#version 130
	out vec4 out_color;
	void main(void) {
		out_color = vec4(1.0, 0.0, 1.0, 1.0);
	}"))
(glUseProgram po)


; draw loop
(gl:set-renderer (lambda ()
   (glClear GL_COLOR_BUFFER_BIT)

   (glBindBuffer GL_ARRAY_BUFFER VBO)
   (glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE 0 nullptr) ; 3 for x,y,z
   (glDrawArrays GL_TRIANGLES 0 3))) ; totally 1 triangle (3 vertices)
