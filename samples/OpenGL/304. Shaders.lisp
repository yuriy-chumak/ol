#!/usr/bin/env ol
(import (lib gl-3))
(gl:set-window-title "4. Shaders")

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

(define Vertices '((-0.6 -0.6 0) (-0.6 0.6 0) (0.6 -0.6 0) (0.6 0.6 0)))
(glBufferData GL_ARRAY_BUFFER (* (sizeof fft-float) 3 (length Vertices)) (cons fft-float* (apply append Vertices)) GL_STATIC_DRAW) ; 3 for x,y,z
(glEnableVertexAttribArray 0)

(define po (gl:create-program
"#version 330 core
	layout (location = 0) in vec3 position;
	void main() {
		gl_Position = vec4(position, 1.0);
	}"
"#version 330 core
	out vec4 out_color;
	uniform vec2 resolution;
	void main(void) {
		vec2 pos = (gl_FragCoord.xy/resolution.xy);
		vec2 c = pos*vec2(4,3) - vec2(2.5, 1.5);
		vec2 z = vec2(0.0,0.0);
		vec4 col = vec4(0);
		for (float i = 0.0; i < 100.0; i += 1.0) {
			if (z.x*z.x + z.y*z.y >= 4.0) {
				col = vec4(smoothstep(0.0, 100.0, i), smoothstep(4.0, 90.0, i), 0.0, 1.0);
				break;
			}
			col = vec4(0.5, 1.0, 0.0, 1.0);
			z = vec2(z.x*z.x-z.y*z.y, 2.*z.x*z.y)+c;
		}
		gl_FragColor = col;
	}"))


; draw loop
(gl:set-renderer (lambda ()
   (glClear GL_COLOR_BUFFER_BIT)

   (glUseProgram po)
   (glUniform2f (glGetUniformLocation po "resolution")
      (gl:get-window-width) (gl:get-window-height))

   (glBindBuffer GL_ARRAY_BUFFER VBO)
   (glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE 0 nullptr) ; 3 for x,y,z
   (glDrawArrays GL_TRIANGLE_STRIP 0 4))) ; 2 triangles (4 vertices)
