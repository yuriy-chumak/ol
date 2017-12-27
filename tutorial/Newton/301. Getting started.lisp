#!/usr/bin/ol
;(import (otus ffi))
(import (lib newton))
(import (lib opengl))

(define (gettimeofday) (syscall 96 #f #f #f))

(define (CreateBackgroundBody world)
   (let ((points '(
                  -100 0.1  100
                   100 0.1  100
                   100 0.1 -100
                  -100 0.1 -100))
         (collision (NewtonCreateTreeCollision world 0)))

      (NewtonTreeCollisionBeginBuild collision)
      (NewtonTreeCollisionAddFace collision 4 points (* 3 4) 0) ; (* 3 4) is stride, where 4 is sizeof(float)
      (NewtonTreeCollisionEndBuild collision 1)
      (NewtonCreateDynamicBody world collision
         '(1 0 0 0  0 1 0 0  0 0 1 0  0 0 0 1))
      (NewtonDestroyCollision collision)))

(define (CreateFreeFallBall world height)
   (let*((sphere (NewtonCreateSphere world 10  0 #f))
         (body   (NewtonCreateDynamicBody world sphere
            `(;x y z w
               1 0 0 0
               0 1 0 0
               0 0 1 0
               0 ,height ,(/ height 20) 1))))
      (NewtonBodySetMassProperties body 1.0 sphere)
      (NewtonDestroyCollision sphere)
      body))

(define ApplyGravity (syscall 85 (cons
   (list type-vptr fft-float type-int+)
   (lambda (body timestep threadIndex)
      (NewtonBodySetForce body '(0 -9.8 0 0))
)) #f #f))


; ====================================================================
; создадим "мир"
(define world (or
   (NewtonCreate)
   (runtime-error "Can't create newton world" #f)))
(CreateBackgroundBody world) ; create the "table"

(define body1 (CreateFreeFallBall world 50))
(NewtonBodySetForceAndTorqueCallback body1 ApplyGravity)
(define body2 (CreateFreeFallBall world 90))
(NewtonBodySetForceAndTorqueCallback body2 ApplyGravity)

(NewtonInvalidateCache world) ; say "world construction finished"

(define Context (gl:Create "301. Getting started"))
(define gl-sphere (gluNewQuadric))
(gluQuadricDrawStyle gl-sphere GLU_FILL)



(gl:run

   Context

; init
(lambda ()
   (glShadeModel GL_SMOOTH)
   (glClearColor 0.11 0.11 0.11 1)

   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity)
   (gluPerspective 45 (/ 640 480) 0.1 1000)

   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (gluLookAt 100 150 200
      0 0 0
      0 1 0)

   (glEnable GL_DEPTH_TEST)

   ; http://www.glprogramming.com/red/chapter12.html
   (glEnable GL_LIGHTING)
   (glLightModelf GL_LIGHT_MODEL_TWO_SIDE GL_TRUE)
   (glEnable GL_NORMALIZE)

   ; http://compgraphics.info/OpenGL/lighting/light_sources.php
   (glEnable GL_LIGHT0)
   (glLightfv GL_LIGHT0 GL_DIFFUSE '(0.7 0.7 0.7  1.0))
   (glLightfv GL_LIGHT0 GL_POSITION '(30 30 30  1))

   ; colors in opengl when lighting use: http://stackoverflow.com/questions/8494942/why-does-my-color-go-away-when-i-enable-lighting-in-opengl
   (glEnable GL_COLOR_MATERIAL)
   (glColorMaterial GL_FRONT GL_DIFFUSE)

   ; return parameter list:
   (list (gettimeofday)))

; draw
(lambda (oldtime)
(let ((newtime (gettimeofday)))
   ; обновим мир
   (let ((ms (* (+ (- (car newtime) (car oldtime)) (/ (- (cdr newtime) (cdr oldtime)) 1000000)) 2)))
      (NewtonUpdate world (if (> ms 0.006) 0.006 ms)))

   ; и нарисуем его
   (glClear (vm:or GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   ; платформа
   (glColor3f 0.8 0.8 0.8)
   (glBegin GL_QUADS)
      (glVertex3f -100 0.1  100)
      (glVertex3f  100 0.1  100)
      (glVertex3f  100 0.1 -100)
      (glVertex3f -100 0.1 -100)
   (glEnd)

;  (glMaterialfv GL_FRONT GL_DIFFUSE '(1 0 0 1))
   (for-each (lambda (body)
      (let ((matrix '(0.1 0.1 0.1 0.1  0.1 0.1 0.1 0.1  0.1 0.1 0.1 0.1  0.1 0.1 0.1 0.1)))
         (NewtonBodyGetMatrix body matrix)
         (glPushMatrix)
         (glMultMatrixf matrix)
         (gluSphere gl-sphere 10 16 8)
         (glPopMatrix)))
      (list body1 body2))

   ; return new parameter list:
   (list newtime))
))

(NewtonWorldSetDestructorCallback world destructor)
(NewtonDestroy world)
