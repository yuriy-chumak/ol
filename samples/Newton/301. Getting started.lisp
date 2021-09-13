#!/usr/bin/env ol

(import (lib newton-dynamics))
(import (lib gl))

(gl:set-window-title "301. Getting started")
(import (OpenGL version-1-0))

(define (gettimeofday) (syscall 96))

(define aTable (map inexact '(
   -100 0  100
    100 0  100
    100 30 -100
   -100 30 -100)))

(define (create-background-body world)
   (let ((collision (NewtonCreateTreeCollision world 0)))

      (NewtonTreeCollisionBeginBuild collision)
      (NewtonTreeCollisionAddFace collision 4 aTable (* 3 4) 0) ; (* 3 4) is stride, where 4 is sizeof(float)
      (NewtonTreeCollisionEndBuild collision 1)
      (NewtonCreateDynamicBody world collision
         '(1 0 0 0  0 1 0 0  0 0 1 0  0 0 0 1)) ; identity matrix
      (NewtonDestroyCollision collision))) ; collision is no more required

(define ApplyGravityCallback (NewtonApplyForceAndTorque
   (lambda (body timestep threadIndex)
      (NewtonBodySetForce body '(0 -9.8 0 0)))))

(define (create-freefall-ball world height)
   (let*((sphere (NewtonCreateSphere world 10  0 #f))
         (body   (NewtonCreateDynamicBody world sphere
            `(;x y z w
               1 0 0 0
               0 1 0 0
               0 0 1 0
               0 ; x
               ,height  ; y
               ,(/ height 20) ; z
               1))))
      (NewtonBodySetMassProperties body 1.0 sphere)
      (NewtonDestroyCollision sphere)
      (NewtonBodySetForceAndTorqueCallback body ApplyGravityCallback)
      body))


; ====================================================================
; создадим "мир"
(define world (or
   (NewtonCreate)
   (runtime-error "Can't create newton world" #f)))
(create-background-body world) ; create the "table"

(define body1 (create-freefall-ball world 50))
(define body2 (create-freefall-ball world 90))

(NewtonSetSolverIterations world 1)
(NewtonInvalidateCache world) ;say "world construction finished"

(define gl-sphere (gluNewQuadric))
(gluQuadricDrawStyle gl-sphere GLU_FILL)


;; init
(glShadeModel GL_SMOOTH)
(glClearColor 0.11 0.11 0.11 1)

(glMatrixMode GL_PROJECTION)
(glLoadIdentity)
(gluPerspective 45 (/ 854 480) 0.1 1000)

(glMatrixMode GL_MODELVIEW)
(glLoadIdentity)
(gluLookAt 100 150 200
   0 0 0
   0 1 0)

(glEnable GL_DEPTH_TEST)

;; lighting
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
(import (scheme dynamic-bindings))
(define userdata (make-parameter
   (gettimeofday)))

; draw
(gl:set-renderer (lambda (mouse)
(let ((oldtime (userdata))
      (newtime (gettimeofday)))
   ;; ; обновим мир
   (let ((ms (* (+ (- (car newtime) (car oldtime)) (/ (- (cdr newtime) (cdr oldtime)) 1000000)) 2)))
      (NewtonUpdate world (if (> ms 0.01) 0.01 ms)))

   ; и нарисуем его
   (glClear (vm:ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   ; платформа
   (glColor3f 0.8 0.8 0.8)
   (glBegin GL_QUADS)
      (glVertex3f (lref aTable 0) (lref aTable 1) (lref aTable 2))
      (glVertex3f (lref aTable 3) (lref aTable 4) (lref aTable 5))
      (glVertex3f (lref aTable 6) (lref aTable 7) (lref aTable 8))
      (glVertex3f (lref aTable 9) (lref aTable 10) (lref aTable 11))
   (glEnd)

;  (glMaterialfv GL_FRONT GL_DIFFUSE '(1 0 0 1))
   (for-each (lambda (body)
      (let ((matrix (map inexact (repeat 0 16))))
         (NewtonBodyGetMatrix body matrix)
         (glPushMatrix)
         (glMultMatrixf matrix)
         (gluSphere gl-sphere 10 16 8)
         (glPopMatrix)))
      (list body1 body2))

   ; return new parameter list:
   (userdata newtime))))

(gl:finish)
(NewtonDestroy world)
(print "NewtonGetMemoryUsed = " (NewtonGetMemoryUsed))
