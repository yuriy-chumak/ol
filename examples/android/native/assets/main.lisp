#!/usr/bin/env ol

(import (lib gl))

(import (OpenGL 1.0))

;; (define program (gl:create-program
;; "#version 120 // OpenGL 2.1

;; void main()
;; {
;;    gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
;;    gl_FrontColor = gl_Color;
;; }"

;; "#version 120 // OpenGL 2.1
;; void main()
;; {
;;    gl_FragColor = gl_Color;
;; }"))

; draw
;; (import (lib GLU))
(mail 'opengl ['set 'autorender #t])

(gl:set-renderer (lambda ()
   ;; (glViewport 0 0 (gl:get-window-width) (gl:get-window-height))
   (glClearColor 0 1 0 1)
   (glClear (vm:ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   ;; (define FOVY 45.0)
   ;; (define ASPECT (/ (gl:get-window-width) (gl:get-window-height)))

   ;; (glMatrixMode GL_PROJECTION)
   ;; (glLoadIdentity)
   ;; (gluPerspective FOVY ASPECT 0.1 1000)

   ;; (glMatrixMode GL_MODELVIEW)
   ;; (glLoadIdentity)
   ;; (gluLookAt 0 0 9  ; eye
   ;;            0 0 5  ; center
   ;;            0 1 0) ; up

   ;; (glUseProgram program)
   ;; (glBegin GL_QUADS)
   ;;    (glColor3f 1 0 0)
   ;;    (glVertex3f -3 +3 -2)
   ;;    (glColor3f 0 1 0)
   ;;    (glVertex3f +3 +3 -2)
   ;;    (glColor3f 0 0 1)
   ;;    (glVertex3f +3 -3 -2)
   ;;    (glColor3f 0 1 1)
   ;;    (glVertex3f -3 -3 -2)
   ;; (glEnd)
))
