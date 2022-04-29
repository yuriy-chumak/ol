#!/usr/bin/env ol

; initialize OpenGL
(import (lib gl-2))
(gl:set-window-title "4.reference-frames.lisp")
(import (scheme dynamic-bindings))

; gl global init
(glShadeModel GL_SMOOTH)
(glEnable GL_DEPTH_TEST)

(glEnable GL_CULL_FACE) ; GL_BACK

; scene
(import (scene))

; load (and create if no one) a models cache
(define models (prepare-models "cache.bin"))
(define geometry (compile-triangles models))

; load a scene
(import (file json))
(define scene (read-json-file "scene.json"))

; scene lights
(define Lights (vector->list (scene 'Lights)))
(print "Lights: " Lights)

; scene objects
(define Objects (vector->list (scene 'Objects)))
(print "Objects: " Objects)

; rotating ceiling fan
(define (ceilingFan? entity) (string-eq? (entity 'name "") "ceilingFan"))
(define ceilingFan (make-parameter (car (keep ceilingFan? Objects))))
(define Objects (remove ceilingFan? Objects))

; lights init
(glEnable GL_COLOR_MATERIAL)
(glLightModelfv GL_LIGHT_MODEL_AMBIENT '(0.1 0.1 0.1 1))
; set lights specular colors
(for-each (lambda (i)
      (glEnable (+ GL_LIGHT0 i)))
   (iota (length Lights)))

(glPolygonMode GL_FRONT_AND_BACK GL_FILL)
(define quadric (gluNewQuadric))

; draw
(gl:set-renderer (lambda ()
   (glClearColor 0.1 0.1 0.1 1)
   (glClear (vm:ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   ;; rotate ceilingFan
   (rotate ceilingFan 0.1)

   ; Lights
   (glEnable GL_LIGHTING)
   (for-each (lambda (light i)
         (glEnable (+ GL_LIGHT0 i))
         (glLightfv (+ GL_LIGHT0 i) GL_POSITION (light 'position)))
      Lights
      (iota (length Lights)))

   ; camera setup
   (begin
      (define Camera (ref (scene 'Cameras) 1))

      (glMatrixMode GL_PROJECTION)
      (glLoadIdentity)
      (gluPerspective
         (Camera 'angle)
         (/ (gl:get-window-width) (gl:get-window-height))
         (Camera 'clip_start) (Camera 'clip_end))

      (define target (vector->list (Camera 'target)))
      (define location (vector->list (Camera 'location)))
      (define up (vector->list [0 0 1]))

      (glMatrixMode GL_MODELVIEW)
      (glLoadIdentity)
      (apply gluLookAt (append location target up)))

   ; draw a geometry with colors
   (glEnable GL_TEXTURE_2D)
   (for-each (lambda (entity)
         (define model (entity 'model))

         (glMatrixMode GL_MODELVIEW)
         (glPushMatrix)
         ; transformations
         (let ((xyz (entity 'location)))
            (glTranslatef (ref xyz 1) (ref xyz 2) (ref xyz 3)))
         ;  blender rotation mode is "XYZ": yaw, pitch, roll
         (let ((ypr (entity 'rotation)))
            (glRotatef (ref ypr 1) 1 0 0)
            (glRotatef (ref ypr 2) 0 1 0)
            (glRotatef (ref ypr 3) 0 0 1))
         ; precompiled geometry
         (for-each glCallList
            (geometry (string->symbol model)))
         (glPopMatrix))
      (cons (ceilingFan) Objects))

   ; Draw a light bulbs
   (glMatrixMode GL_MODELVIEW)
   (glDisable GL_LIGHTING)
   (for-each (lambda (light i)
         ; show only "point" light sources
         (when (eq? (ref (light 'position) 4) 1)
            (glColor3fv (light 'color))
            (glPushMatrix)
            (glTranslatef (ref (light 'position) 1)
                          (ref (light 'position) 2)
                          (ref (light 'position) 3))
            (gluSphere quadric 0.2 32 10)
            (glPopMatrix)))
      Lights
      (iota (length Lights)))
))
