#!/usr/bin/env ol

(import (lib gl))
(gl:set-window-title "2. Drawing simple triangle (shadered)")

(import (OpenGL 1.0))
(import (OpenGL ARB shader_objects))
(import (OpenGL ARB shading_language_100))

; note:
;  extension functions and constants has no additional suffixes
;  like ARB and _ARB, so you must change any thirdparty examples
;  by renaming symbols like "glUniform1fARB" into "glUniform1f".
;                             - or -
;  if you still want to use suffixes "ARB" you must add two special imports:
;  the "(import (suffix (OpenGL ARB shader_objects) ARB))" for functions
;  and "(import (suffix (OpenGL ARB shader_objects) _ARB))" for constants.

; global init
(glShadeModel GL_SMOOTH)
(glClearColor 0.11 0.11 0.11 1)

(define shader (gl:create-program
"// vertex shader
   void main() {
      gl_Position = ftransform();
   }"
"// fragment shader
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
   (glClear GL_COLOR_BUFFER_BIT)

   (glUseProgramObject shader)
   (glUniform1f (glGetUniformLocation shader "time") (/ (mod (clock-ms) 1000000) #i1000))
   (glUniform2f (glGetUniformLocation shader "dimensions") (gl:get-window-width) (gl:get-window-height))

   (glColor3f 1 1 1)
   (glBegin GL_TRIANGLES)
      (glColor3f 1 0 0)
      (glVertex2f -0.6 -0.6)

      (glColor3f 0 1 0)
      (glVertex2f +0.6 -0.6)

      (glColor3f 0 0 1)
      (glVertex2f -0.0 +0.7)
   (glEnd) ))
