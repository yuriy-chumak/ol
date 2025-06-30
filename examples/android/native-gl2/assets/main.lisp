#!/usr/bin/env ol

(import (lib gl))
(import (OpenGL 2.1))

(print "gl:create-program: " gl:create-program)
(define program (gl:create-program
"#version 120 // OpenGL 2.1
   void main() {
      gl_Position = gl_Vertex;
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

; draw
(gl:set-renderer (lambda ()
   (glClearColor 0.2 0.2 0.2 1)
   (glClear (vm:ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   (glUseProgram program)
   (glUniform1f (glGetUniformLocation program "time") (/ (mod (time-ms) 1000000) #i1000))
   (glUniform2f (glGetUniformLocation program "dimensions") (gl:get-window-width) (gl:get-window-height))

   (glBegin GL_QUADS)
      (glVertex2f -1.0 +1.0)
      (glVertex2f +1.0 +1.0)
      (glVertex2f +1.0 -1.0)
      (glVertex2f -1.0 -1.0)
   (glEnd)
))
