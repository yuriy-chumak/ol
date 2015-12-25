#!/usr/bin/ol

(import (lib linux opengl) (owl io))

(gl:run "3. Real 3D" 640 480

; init
(lambda ()
   (glShadeModel GL_SMOOTH)
   (glClearColor 0.11 0.11 0.11 1)

   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity)
   (gluPerspective 45 (/ 640 480) 0.1 100)

   (glEnable GL_DEPTH_TEST))

; draw
(lambda ()
   (glClear (fx:or GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (gluLookAt 2 3 5
      0 0 0
      0 1 0)

   (glBegin GL_LINES)
      ; Ox
      (glColor3f 1 0 0)
      (glVertex3f 0 0 0)
      (glVertex3f 2 0 0)
         (glVertex3f 2 0 0)
         (glVertex3f 1.9 0.1 0)
         (glVertex3f 2 0 0)
         (glVertex3f 1.9 0 0.1)
      ; Oy
      (glColor3f 0 1 0)
      (glVertex3f 0 0 0)
      (glVertex3f 0 2 0)
         (glVertex3f 0 2 0)
         (glVertex3f 0.1 1.9 0)
         (glVertex3f 0 2 0)
         (glVertex3f 0 1.9 0.1)
      ; Oz
      (glColor3f 0 0 1)
      (glVertex3f 0 0 0)
      (glVertex3f 0 0 2)
         (glVertex3f 0 0 2)
         (glVertex3f 0.1 0 1.9)
         (glVertex3f 0 0 2)
         (glVertex3f 0 0.1 1.9)
   (glEnd)


   (glBegin GL_QUADS)
      ; front
      (glColor3f 0.7 0.7 0.7)
      (glVertex3f  1 -1 -1)
      (glVertex3f  1  1 -1)
      (glVertex3f -1  1 -1)
      (glVertex3f -1 -1 -1)

      ; back
      (glColor3f 0.9 0.8 0.5)
      (glVertex3f  1 -1  1)
      (glVertex3f  1  1  1)
      (glVertex3f -1  1  1)
      (glVertex3f -1 -1  1)

      ; right
      (glColor3f 0.7 0.2 0.2)
      (glVertex3f  1 -1 -1)
      (glVertex3f  1  1 -1)
      (glVertex3f  1  1  1)
      (glVertex3f  1 -1  1)

      ; left
      (glColor3f 0.2 0.2 0.7)
      (glVertex3f -1 -1 -1)
      (glVertex3f -1  1 -1)
      (glVertex3f -1  1  1)
      (glVertex3f -1 -1  1)

      ; top
      (glColor3f 0.7 0.7 0.2)
      (glVertex3f  1  1  1)
      (glVertex3f  1  1 -1)
      (glVertex3f -1  1 -1)
      (glVertex3f -1  1  1)

      ; bottom
      (glColor3f 0.2 0.7 0.7)
      (glVertex3f  1 -1  1)
      (glVertex3f  1 -1 -1)
      (glVertex3f -1 -1 -1)
      (glVertex3f -1 -1  1)

   (glEnd)))