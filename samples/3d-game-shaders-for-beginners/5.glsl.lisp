#!/usr/bin/env ol

;; initialize OpenGL
(import (lib gl))
(gl:set-window-title "4. reference-frames.lisp")
(import (OpenGL version-2-1))
; todo: splash screen

(import (scene))

; load (and create if no one) a models cache
(define models (prepare-models "cache.bin"))
(print "compiled models:\n" models)

;; load a scene
(import (file json))
(define scene (read-json-file "scene1.json"))

;; shaders
(define vertex-shader "#version 120 // OpenGL 2.1
   void main() {
      // gl_NormalMatrix is transpose(inverse(gl_ModelViewMatrix))
   	gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
   }")
(define fragment-shader "#version 120 // OpenGL 2.1
   void main() {
   	gl_FragColor = vec4(0.0, 1.0, 0.0, 1.0);
   }")

(define po (gl:CreateProgram vertex-shader fragment-shader))

;; draw

(import (lib math))
(import (owl math fp))

; настройки
(glShadeModel GL_SMOOTH)
(glClearColor 0.2 0.2 0.2 1)

(glEnable GL_DEPTH_TEST)
(glEnable GL_NORMALIZE)

(glEnable GL_LIGHTING)
(glLightModelf GL_LIGHT_MODEL_TWO_SIDE GL_TRUE)
(glEnable GL_LIGHT0)

; draw
(gl:set-renderer (lambda (mouse)
   (glClear (vm:ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
   (glUseProgram po)

   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity)
   (gluPerspective 45 (/ (gl:get-window-width) (gl:get-window-height)) 0.1 100)
   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (let*((ss ms (clock))
         (x (* -17 (sin (+ ss (/ ms 1000)))))
         (y (* -17 (cos (+ ss (/ ms 1000)))))
         (z 15))
      (gluLookAt x y z
         0 0 -5
         0 0 1))

   ; set and show lighting point
   (glDisable GL_LIGHTING)
   (let*((ss ms (clock))
         (x (- (* 7 (sin (+ ss (/ ms 1000)))) 3))
         (y (- (* 7 (cos (+ ss (/ ms 1000)))) 3))
         (z 5))
      (glPointSize 5)
      (glBegin GL_POINTS)
      (glColor3f #xff/255 #xbf/255 0)
      (glVertex3f x y z)
      (glEnd)
      
      (glLightfv GL_LIGHT0 GL_POSITION (list x y z 1)))
   (glEnable GL_LIGHTING)
      
   (draw-geometry scene models)
))
