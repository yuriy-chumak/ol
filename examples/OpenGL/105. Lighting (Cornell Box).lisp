#!/usr/bin/env ol

(import (lib gl))
(gl:set-window-title "5. Cornell Box")

(import (OpenGL 1.0)
   (lib GLU))

; init
(glShadeModel GL_SMOOTH)
(glClearColor 0.8 0.8 0.8 1)
(glEnable GL_DEPTH_TEST)

,include "media/cornell-box.scm"

; setup light
(glEnable GL_LIGHT0)
(glLightfv GL_LIGHT0 GL_AMBIENT  '(0.2 0.2 0.2 1.0)) ; gray
(glLightfv GL_LIGHT0 GL_DIFFUSE  '(1.8 1.8 1.8 1.0)) ; bright
(glLightfv GL_LIGHT0 GL_SPECULAR '(1.0 1.0 1.0 1.0)) ; white

(glEnable GL_COLOR_MATERIAL)

(glLightf GL_LIGHT0 GL_CONSTANT_ATTENUATION  1.0)
(glLightf GL_LIGHT0 GL_LINEAR_ATTENUATION    3)
(glLightf GL_LIGHT0 GL_QUADRATIC_ATTENUATION 8)

; generate qobj
(import (lib GLU))
(define qobj (gluNewQuadric))
;; (gluQuadricDrawStyle qobj GLU_LINE)

(import (scheme inexact)) ; sin, cos

; draw
(gl:set-renderer (lambda ()
   (glClear (bor GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   ; setup projection
   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity)
   (gluPerspective 45 (/ (gl:get-window-width) (gl:get-window-height)) 0.1 100)

   ; setup camera
   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (gluLookAt
      -0.278 0.273 0.5
      -0.278 0.273 -0.2
      0 1 0)

   (glDisable GL_LIGHTING)

   ;; (glLightfv GL_LIGHT0 GL_POSITION '(-0.278 0.540 -0.2795 1.0))
   (define t (/ (mod (time-ms) 6283) #i1000)) ; 0 .. 2*Pi
   (define x (* (sin t) 0.1))
   (define z (* (cos t) 0.1))
   (glPushMatrix)
   (glTranslatef (+ x -0.2780) 0.45 (+ z -0.2795))
   (gluSphere qobj 0.01 16 8)
   (glPopMatrix)

   (glEnable GL_LIGHTING)

   (glLightfv GL_LIGHT0 GL_POSITION (list
      (+ x -0.2780) 0.45 (+ z -0.2795)  1.0))
   ;; (glLightfv GL_LIGHT0 GL_POSITION '(0.5 0.51 0.5  1.0))

   (define (vertex x y z  nx ny nz  u v  r g b)
      ;; (glTexCoord2f u v) ; we don't use it
      (glColor3f r g b)
      (glNormal3f nx ny nz)
      (glVertex3f x y z))

   (glBegin GL_TRIANGLES)
      (for-each (lambda (i)
            (apply vertex (ref Vertices i)))
         Indices)
   (glEnd)

))
