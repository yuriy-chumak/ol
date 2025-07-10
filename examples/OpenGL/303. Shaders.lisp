#!/usr/bin/env ol
(import (lib gl-3))
(gl:set-window-title "4. Shaders")

(gl:set-context-version 3 0) ; use OpenGL version 3.0

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
   uniform float time;
   uniform vec2 resolution;
   void main(void) {
      vec2  p = 7.*(2.*gl_FragCoord.xy-resolution.xy)/resolution.y;
      float m1 = sin(length(p)*0.3-time*0.3);
      float m2 = sin(0.3*(length(p)*0.3-time*0.3));
      float c1 = 0.012/abs(length(mod(p,2.0*m1)-m1)-0.3);
      float c2 = 0.012/abs(length(mod(p,2.0*m2)-m2)-0.3);
      out_color = vec4(vec3(1.,2.,8.)*c1+vec3(8.,2.,1.)*c2, 1.);
   }"))


; draw loop
(gl:set-renderer (lambda ()
   (glClear GL_COLOR_BUFFER_BIT)

   (glUseProgram po)
   (glUniform1f (glGetUniformLocation po "time") (/ (mod (time-ms) 1000000) #i1000))
   (glUniform2f (glGetUniformLocation po "resolution")
      (gl:get-window-width) (gl:get-window-height))

   (glBindBuffer GL_ARRAY_BUFFER VBO)
   (glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE 0 nullptr) ; 3 for x,y,z
   (glDrawArrays GL_TRIANGLE_STRIP 0 4))) ; 2 triangles (4 vertices)
