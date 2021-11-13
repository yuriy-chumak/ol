#!/usr/bin/env ol

;; initialize OpenGL
(import (lib gl))
(gl:set-window-title "6. render-to-texture")
(import (OpenGL version-2-1))
; todo: splash screen

;; load and compile all models
(import (scene))

; загрузить нужные модели
(define models (prepare-models "Mill"))
(print models)

;; load a scene
(import (file json))
(define scene (read-json-file "scene1.json"))

;; shaders
(define vertex-shader "#version 120 // OpenGL 2.1
   varying float z;
   void main() {
      gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
      z = (gl_Position.z / 14);
   }")
(define fragment-shader "#version 120 // OpenGL 2.1
   varying float z;
   void main() {
      gl_FragColor = vec4(z, z, z, 1.0);
   }")

(define po (gl:CreateProgram vertex-shader fragment-shader))

;; render buffer
(import (OpenGL EXT framebuffer_object))

(define framebuffer '(0))
(glGenFramebuffers (length framebuffer) framebuffer)
(print "framebuffer: " framebuffer)

(define texture '(0))
(glGenTextures (length texture) texture)
(print "texture: " texture)
(glBindTexture GL_TEXTURE_2D (car texture))
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
(glTexImage2D GL_TEXTURE_2D 0 GL_RGBA 1024 1024 0 GL_RGBA GL_UNSIGNED_BYTE 0)
(glBindTexture GL_TEXTURE_2D 0)

(glBindFramebuffer GL_FRAMEBUFFER (car framebuffer))
(glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D (car texture) 0)

;; draw

(import (lib math))
(import (owl math fp))

; настройки
(glShadeModel GL_SMOOTH)
(glClearColor 0.3 0.3 0.3 1)

(glMatrixMode GL_PROJECTION)
(glLoadIdentity)
(gluPerspective 45 (/ 854 480) 0.1 100)

(glEnable GL_DEPTH_TEST)

(glEnable GL_LIGHTING)
(glLightModelf GL_LIGHT_MODEL_TWO_SIDE GL_TRUE)
(glEnable GL_NORMALIZE)

(glEnable GL_LIGHT0)

(glEnable GL_LIGHTING)
(glLightfv GL_LIGHT0 GL_POSITION '(7.0 7.0 7.0 0.0))

; draw
(gl:set-renderer (lambda (mouse)
   (glBindFramebuffer GL_FRAMEBUFFER (car framebuffer))
   (glViewport 0 0 1024 1024)
   (glClear (vm:ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
   (glUseProgram po)

   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity)
   (gluPerspective 45 (/ (gl:get-window-width) (gl:get-window-height)) 0.1 1000)
   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (gluLookAt -5 -9 5
      0 0 2
      0 0 1)

   ; set and show lighting point
   (glDisable GL_LIGHTING)
   (let*((ss ms (clock))
         (x (* 5 (sin (+ ss (/ ms 1000)))))
         (y (* 5 (cos (+ ss (/ ms 1000))))) ;(+ 1 (* 3 (sin (/ (+ ss (/ ms 1000)) 8)))))
         (z 2)) ;(* 3 (cos (+ ss (/ ms 1000))))))
      (glPointSize 5)
      (glBegin GL_POINTS)
      (glColor3f #xff/255 #xbf/255 0)
      (glVertex3f x y z)
      (glEnd)
      
      (glLightfv GL_LIGHT0 GL_POSITION (list x y z 1)))
   (glEnable GL_LIGHTING)
      
   (draw-geometry scene models)

   (glBindFramebuffer GL_FRAMEBUFFER 0)
   (glUseProgram 0)
   (glDisable GL_LIGHTING)

   (glViewport 0 0 (gl:get-window-width) (gl:get-window-height))
   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity)
   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (glOrtho 0 1 0 1 0 1)

   (glEnable GL_TEXTURE_2D)
   (glBindTexture GL_TEXTURE_2D (car texture))
   (glClearColor 0 0 0 1)
   (glClear (vm:ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   (glBegin GL_QUADS)
      (glColor3f 1 1 1)

      (glTexCoord2f 0 0)
      (glVertex2f 0 0)
      (glTexCoord2f 1 0)
      (glVertex2f 1 0)
      (glTexCoord2f 1 1)
      (glVertex2f 1 1)
      (glTexCoord2f 0 1)
      (glVertex2f 0 1)
   (glEnd)
))
