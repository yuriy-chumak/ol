#!/usr/bin/ol
(import (lib gl))
(gl:set-window-title "2. Drawing simple triangle")

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

; draw
(gl:set-renderer (lambda (mouse)
   (glClear GL_COLOR_BUFFER_BIT)

   (glUseProgram po)
   (glUniform2f (glGetUniformLocation po (c-string "resolution")) 854 480)

   (glColor3f 1 1 1)
   (glBegin GL_TRIANGLES)
      (glVertex2f -0.6 -0.6)
      (glVertex2f +0.6 -0.6)
      (glVertex2f -0.0 +0.7)
   (glEnd)))
