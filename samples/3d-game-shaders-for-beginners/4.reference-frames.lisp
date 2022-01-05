#!/usr/bin/env ol

;; initialize OpenGL
(import (lib gl))
(gl:set-window-title "4.reference-frames.lisp")
(import (OpenGL version-2-1))

(import (scene))

; load (and create if no one) a models cache
(define models (prepare-models "cache.bin"))

;; load a scene
(import (file json))
(define scene (read-json-file "scene1.json"))

;; init
(glShadeModel GL_SMOOTH)
(glEnable GL_DEPTH_TEST)

(glEnable GL_CULL_FACE)
(glCullFace GL_BACK)

; draw
(gl:set-renderer (lambda (mouse)
   (glClearColor 0.1 0.1 0.1 1)
   (glClear (vm:ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   ; Camera setup
   (begin
      (define camera (ref (scene 'Cameras) 1))

      (define angle (camera 'angle))
      (define location (camera 'location))
      (define target (camera 'target))

      (glMatrixMode GL_PROJECTION)
      (glLoadIdentity)
      (gluPerspective angle (/ (gl:get-window-width) (gl:get-window-height)) 0.1 100) ; (camera 'clip_start) (camera 'clip_end)

      (glMatrixMode GL_MODELVIEW)
      (glLoadIdentity)
      (gluLookAt
         (ref location 1) (ref location 2) (ref location 3)
         (ref target 1) (ref target 2) (ref target 3)
         0 0 1))

   ; Draw a geometry with colors
   (for-each (lambda (entity)
         (define model (entity 'model))

         (glMatrixMode GL_MODELVIEW)
         (glPushMatrix)
         (let ((xyz (entity 'location)))
            (glTranslatef (ref xyz 1) (ref xyz 2) (ref xyz 3)))
         (let ((ypr (entity 'rotation)))
            (glRotatef (ref ypr 1) 1 0 0)
            (glRotatef (ref ypr 2) 0 1 0)
            (glRotatef (ref ypr 3) 0 0 1))

         (for-each glCallList
            (models (string->symbol model)))
         (glPopMatrix))
      (scene 'Objects))
))
