#!/usr/bin/env ol

; create OpenGL window and import OpenGL functions
(import (lib gl 2.1))
(gl:set-window-title "2. Drawing simple triangle")

; global init
(glClearColor 0.3 0.3 0.3 1)

(define shader (gl:create-program
"#version 120 // OpenGL 2.1
   void main() {
      gl_Position = ftransform();
   }"
"#version 120 // OpenGL 2.1
   uniform float time;
   uniform vec2 dimensions;
   void main(void) {
      vec2  p = 7.*(2.*gl_FragCoord.xy-dimensions.xy)/dimensions.y;
      float m1 = sin(length(p)*0.3-time*0.3);
      float m2 = sin(0.3*(length(p)*0.3-time*0.3));
      float c1 = 0.012/abs(length(mod(p,2.0*m1)-m1)-0.3);
      float c2 = 0.012/abs(length(mod(p,2.0*m2)-m2)-0.3);
      gl_FragColor = vec4(vec3(1.,2.,8.)*c1+vec3(8.,2.,1.)*c2, 1.);
   }"))

; render pass
(gl:set-renderer (lambda ()
   (glClear GL_COLOR_BUFFER_BIT)

   (glUseProgram shader)
   (glUniform1f (glGetUniformLocation shader "time") (/ (mod (clock-ms) 1000000) #i1000))
   (glUniform2f (glGetUniformLocation shader "dimensions") (gl:get-window-width) (gl:get-window-height))

   (glBegin GL_TRIANGLES)
      (glVertex2f -0.6 -0.6)

      (glVertex2f +0.6 -0.6)

      (glVertex2f -0.0 +0.7)
   (glEnd) ))
