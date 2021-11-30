#!/usr/bin/env ol

;; initialize OpenGL
(import (lib gl))
(gl:set-window-title "7. Render to the Depth Buffer")
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
      gl_Position = gl_ProjectionMatrix * gl_ModelViewMatrix * gl_Vertex;
   }")
(define fragment-shader "#version 120 // OpenGL 2.1
   void main() {
      // nothing to do
   }")

(define po (gl:CreateProgram vertex-shader fragment-shader))

;; render buffer
(import (OpenGL EXT framebuffer_object))

(define depth-map '(0))
(glGenTextures (length depth-map) depth-map)
(print "depth-map: " depth-map)
(glBindTexture GL_TEXTURE_2D (car depth-map))
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
(glTexImage2D GL_TEXTURE_2D 0 GL_DEPTH_COMPONENT 1024 1024 0 GL_DEPTH_COMPONENT GL_FLOAT 0)
(glBindTexture GL_TEXTURE_2D 0)

(define depth-fbo '(0))
(glGenFramebuffers (length depth-fbo) depth-fbo)
(print "depth-fbo: " depth-fbo)
(glBindFramebuffer GL_FRAMEBUFFER (car depth-fbo))
(glFramebufferTexture2D GL_FRAMEBUFFER GL_DEPTH_ATTACHMENT GL_TEXTURE_2D (car depth-map) 0)
(glDrawBuffer GL_NONE)
(glReadBuffer GL_NONE)
(glBindFramebuffer GL_FRAMEBUFFER 0)

;; draw

(import (lib math))
(import (owl math fp))

; настройки
(glShadeModel GL_SMOOTH)
(glClearColor 0.2 0.2 0.2 1)

(glEnable GL_DEPTH_TEST)
;(glEnable GL_NORMALIZE)

; draw
(gl:set-renderer (lambda (mouse)
(let*((ss ms (clock))
      (ticks (/ (+ ss (/ ms 1000)) 2)))

   (define x (* 15 (sin ticks)))
   (define y (* 15 (cos ticks)))
   (define z 8)

   ;; (define x 15)
   ;; (define y 15)
   ;; (define z 15)

   (glViewport 0 0 1024 1024)
   (glBindFramebuffer GL_FRAMEBUFFER (car depth-fbo))
   (glClear GL_DEPTH_BUFFER_BIT)
   (glUseProgram po)

   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity)
   (glOrtho -20 20 -20 20 0 50) ; gl_Projection is a light projection matrix
   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (gluLookAt x y z ; gl_ModelView is a light space matrix
      0 0 0
      0 0 1)

   (glLightfv GL_LIGHT0 GL_POSITION (list x y z 1)) ; не надо

   (glCullFace GL_FRONT)
   (draw-geometry scene models)
   (glCullFace GL_BACK)
   (glBindFramebuffer GL_FRAMEBUFFER 0)

   ;; draw a texture
   (glBindFramebuffer GL_FRAMEBUFFER 0)
   (glUseProgram 0)

   (glViewport 0 0 (gl:get-window-width) (gl:get-window-height))
   (glClearColor 0 0 0 1)
   (glClear (vm:ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity)
   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (glOrtho 0 1 0 1 0 1)

   (glEnable GL_TEXTURE_2D)
   (glBindTexture GL_TEXTURE_2D (car depth-fbo))

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
)))
