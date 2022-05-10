#!/usr/bin/env ol
(import (lib gl))
(gl:set-window-title "2. Point, line, triangle, quad, polygon")

(import (OpenGL version-1-0))
(import (lib math))

; init
(glShadeModel GL_SMOOTH)
(glClearColor 0.8 0.8 0.8 1)

; draw
(gl:set-renderer (lambda ()
   (glClear GL_COLOR_BUFFER_BIT)

   (glLoadIdentity)
   (glOrtho -1 1 -1 1 -1 1)

   (glLineWidth 2.0)
   (glColor3f #xF2/255 #xE2/255 #x05/255)
   (glBegin GL_LINES)
      (glVertex2f -1 0)
      (glVertex2f  1 0)

      (glVertex2f 0 -1)
      (glVertex2f 0  1)
   (glEnd)

   (glOrtho -2 2 -2 2 -2 2)
   (glColor3f 0.1 0.7 0.3)

   (glPushMatrix)
   (glTranslatef -1 -1 0)
   (glPointSize 8)
   (glColor3f #xF2/255 #x5C/255 #x05/255)
   (glEnable GL_POINT_SMOOTH)
   (glBegin GL_POINTS)
      (glVertex2f 0 0)
   (glEnd)
   (glPopMatrix)

   (glPushMatrix)
   (glTranslatef  1 -1 0)
   (glColor3f #xF2/255 #xCB/255 #x05/255)
   (glBegin GL_TRIANGLES)
      (glVertex2f -0.6 -0.9)
      (glVertex2f  0.6 -0.9)
      (glVertex2f  0  0.9)
   (glEnd)
   (glPopMatrix)

   (glPushMatrix)
   (glTranslatef  1  1 0)
   (glColor3f #x04/255 #xD9/255 #x76/255)
   (glBegin GL_QUADS)
      (glVertex2f -0.45 -0.80)
      (glVertex2f  0.45 -0.80)
      (glVertex2f  0.45  0.80)
      (glVertex2f -0.45  0.80)
   (glEnd)
   (glPopMatrix)

   (glPushMatrix)
   (glTranslatef -1  1 0)
   (glColor3f #xAF/255 #x4B/255 #xF2/255)
   (glBegin GL_POLYGON)
      (let ((n 7))
         (for-each (lambda (i)
               (define angle (/ (* i 2 3.1415928) n))
               (glVertex2f (* 0.45 (sin angle))
                           (* 0.80 (cos angle))))
            (iota n)))
   (glEnd)
   (glPopMatrix)

))
