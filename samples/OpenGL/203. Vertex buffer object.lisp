#!/usr/bin/ol
(import (lib gl))
(gl:set-window-title "3. VBO")

(import (OpenGL version-2-1))

; init
(glShadeModel GL_SMOOTH)
(glClearColor 0.11 0.11 0.11 1)

(define po (gl:CreateProgram
"#version 120 // OpenGL 2.1
	void main() {
		gl_Position = gl_Vertex;
	}"
"#version 120 // OpenGL 2.1
	uniform vec2 resolution;
	void main(void) {
		vec2 pos = (gl_FragCoord.xy / resolution.xy);
		gl_FragColor = vec4(clamp(pos, 0.0, 1.0), 0.0, 1.0);
	}"))
(glUseProgram po)

(define VBO (box 0))
(glGenBuffers 1 VBO)
(define VBO (unbox VBO))

(glBindBuffer GL_ARRAY_BUFFER VBO)
(define fft-float* (fft* fft-float))

(define Vertices '((-0.6 -0.6 0) (0.6 -0.6 0)  (0 0.7 0)))
(glBufferData GL_ARRAY_BUFFER (* (sizeof fft-float) 3 (length Vertices)) (cons fft-float* (apply append Vertices)) GL_STATIC_DRAW) ; 3 for x,y,z
(glEnableVertexAttribArray 0)


; draw
(gl:set-renderer (lambda (mouse)
   (glClear GL_COLOR_BUFFER_BIT)

   (glUniform2f (glGetUniformLocation po "resolution")
      (gl:get-window-width) (gl:get-window-height))

   (glBindBuffer GL_ARRAY_BUFFER VBO)
   (glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE 0 nullptr) ; 3 for x,y,z
   (glDrawArrays GL_TRIANGLES 0 3))) ; 1 triangle (3 vertices)
