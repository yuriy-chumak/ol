#!/usr/bin/ol
(import (lib opengl))

(define (sin x)
(let ((a (lambda (x)
   (-
      (+ x
         (/
            (* x x x x x)
            (* 1 2 3 4 5)))
      (+
         (/
            (* x x x)
            (* 1 2 3)))))))
   (if (< x 0)
      (- (sin (- x)))
   (if (< x 314/200)
      (a x)
   (if (< x 314/100)
      (- (a (- x 314/100)))
   (- (sin (- x 314/100))))))))

(define (cos x)
(let ((a (lambda (x)
   (-
      (+ 1
         (/
            (* x x x x)
            (* 1 2 3 4)))
      (+
         (/
            (* x x)
            (* 1 2)))))))
   (if (< x 0)
      (+ (cos (- x)))
   (if (< x 314/200)
      (a x)
   (if (< x 314/100)
      (- (a (- 314/100 x)))
   (- (cos (- x 314/100))))))))



(gl:run

   "4. Rotations"

; init
(lambda ()
   (glShadeModel GL_SMOOTH)
   (glClearColor 0.11 0.11 0.11 1)

   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity)
   (gluPerspective 45 (/ 640 480) 0.1 100)

   (glEnable GL_DEPTH_TEST)
   (list 1 0.02 3 0.03))

; draw
(lambda (x   dx y   dy)
   (glClear (fx:or GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (let ((n 15))
   ;(gluLookAt (* (sin a) n) 7 (* (cos a) n)
   (gluLookAt x y 5
      0 0 0
      0 1 0))

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

   ;(glColor3f 0 1 1)
   ;(glBegin GL_LINE_STRIP)
   ;   (let L ((a -628/100))
   ;      (glVertex3f a (cos a) 0)
   ;      (if (< a 628/100)
   ;         (L (+ a 7/100))))
   ;(glEnd)


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

   (glEnd)

   (let ((nx (if (or (> x 2) (< x -2)) (- dx) dx))
         (ny (if (or (> y 4) (< y -4)) (- dy) dy)))
      (list (+ x nx) nx (+ y ny) ny))
   ;(list (if (< a 628/100)
   ;         (+ a 7/100)
   ;         0))
;   (list (+ a 7/100))
))
