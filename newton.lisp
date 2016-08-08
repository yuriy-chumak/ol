#!/usr/bin/ol
(import (lib opengl))
(import (otus random!))
(import (otus pinvoke))

(define $ (or
;   (dlopen "/home/uri/Desktop/Otus Lisp/src/tutorial/newton-dynamics/coreLibrary_300/projects/posix64/libNewton.so")
   (dlopen "newton")
   (runtime-error "Can't load newton library" #f)))

(define type-callback 61)
(define type-float* (vm:or type-float #x40))

(define NewtonWorldGetVersion (dlsym $ type-fix+ "NewtonWorldGetVersion"))
(define NewtonWorldFloatSize  (dlsym $ type-fix+ "NewtonWorldFloatSize"))
(define NewtonGetMemoryUsed   (dlsym $ type-int+ "NewtonGetMemoryUsed"))

(define NewtonCreate  (dlsym $ type-vptr "NewtonCreate"))
(define NewtonDestroy (dlsym $ type-void "NewtonDestroy" type-vptr))
(define NewtonWorldSetDestructorCallback (dlsym $ type-void "NewtonWorldSetDestructorCallback" type-vptr type-callback))

;
(define NewtonCreateBox (dlsym $ type-vptr "NewtonCreateBox" type-vptr type-float type-float type-float type-int+ type-float*))
(define NewtonCreateDynamicBody (dlsym $ type-vptr "NewtonCreateDynamicBody" type-vptr type-vptr type-float*))
(define NewtonBodySetForceAndTorqueCallback (dlsym $ type-void "NewtonBodySetForceAndTorqueCallback" type-vptr type-callback))
(define NewtonBodySetMassProperties (dlsym $ type-void "NewtonBodySetMassProperties" type-vptr type-float type-vptr))
(define NewtonDestroyCollision (dlsym $ type-void "NewtonDestroyCollision" type-vptr))

(define NewtonBodySetForce (dlsym $ type-void "NewtonBodySetForce" type-vptr type-float*))
(define NewtonBodySetMatrix (dlsym $ type-void "NewtonBodySetMatrix" type-vptr (vm:or type-float #x40)))
(define NewtonBodyGetMatrix (dlsym $ type-void "NewtonBodyGetMatrix" type-vptr (vm:or type-float #x80)))

(define NewtonCreateTreeCollision (dlsym $ type-vptr "NewtonCreateTreeCollision" type-vptr type-fix+))
(define NewtonTreeCollisionBeginBuild (dlsym $ type-void "NewtonTreeCollisionBeginBuild" type-vptr))
(define NewtonTreeCollisionAddFace (dlsym $ type-void "NewtonTreeCollisionAddFace" type-vptr type-int+ type-float* type-fix+ type-fix+))
(define NewtonTreeCollisionEndBuild (dlsym $ type-void "NewtonTreeCollisionEndBuild" type-vptr type-fix+))


(define NewtonInvalidateCache (dlsym $ type-void "NewtonInvalidateCache" type-vptr))
(define NewtonUpdate (dlsym $ type-void "NewtonUpdate" type-vptr type-float))

(print "NewtonWorldGetVersion = " (NewtonWorldGetVersion))
(print "NewtonWorldFloatSize = "  (NewtonWorldFloatSize))
(print "1.NewtonGetMemoryUsed = " (NewtonGetMemoryUsed))


; создадим "мир"
(define world (or
   (NewtonCreate)
   (runtime-error "Can't create newton world" #f)))

(print "2.NewtonGetMemoryUsed = " (NewtonGetMemoryUsed))

;(NewtonWorldSetDestructorCallback world (cons
;   (list type-vptr)
;   (lambda (world)
;      (print "world destroyed"))))


; создадим "пол"
(define collision (or
   (NewtonCreateTreeCollision world 0)
   (runtime-error "Can't create bacground" #f)))
(NewtonTreeCollisionBeginBuild collision)
(NewtonTreeCollisionAddFace collision 4 '(
   -100 0  100
    100 0  100
    100 0 -100
   -100 0 -100) (* 3 4) 0) ; (* 3 4) is stride, where 4 is sizeof(float)
(NewtonTreeCollisionEndBuild collision 1)
(NewtonCreateDynamicBody world collision '(1 0 0 0  0 1 0 0  0 0 1 0  0 0 0 1))
(NewtonDestroyCollision collision)


(define ApplyGravity (cons
   (list type-vptr type-void* type-int+)
   (lambda (body timestep threadIndex)
      (NewtonBodySetForce body '(0 -9.8 0 0))
)))


; добавим один куб
(define collision (or
   (NewtonCreateBox world  1 1 1  0  #f)
   (runtime-error "Can't create box" #f)))

(define cubes (map
   (lambda (id)
      (let ((x (* id 0.7))
            (y (* id 1)))
         (NewtonCreateDynamicBody world collision
            `(;x y z w
               1 0 0 0 ; front
               0 1 0 0 ; up
               0 0 1 0 ; right

               ,(/ (- (rand! 400) 200) 100) ; x
               ,id              ; y
               ,(/ (- (rand! 400) 200) 100) ; z
               1       ; posit
             ))))
   (iota 20)))

(for-each (lambda (cube)
   (NewtonBodySetMassProperties cube 1.0 collision)
   (NewtonBodySetForceAndTorqueCallback cube ApplyGravity)
) cubes)
;
;
;(define rigidBody (or
;   (NewtonCreateDynamicBody world collision
;      '(;x y z w
;         1 0 0 0 ; front
;         0 1 0 0 ; up
;         0 0 1 0 ; right
;         0 20 0 1 ; posit
;      ))
;   (runtime-error "Can't create rigid body" #f)))
;(define rigidBody2 (or
;   (NewtonCreateDynamicBody world collision
;      '(;x y z w
;         1 0 0 0 ; front
;         0 1 0 0 ; up
;         0 0 1 0 ; right
;         0.8 0 0 1 ; posit
;      ))
;   (runtime-error "Can't create rigid body" #f)))
;
;(print "Created rigid body")
;
;(NewtonBodySetMassProperties rigidBody 1.0 collision)
;(NewtonBodySetMassProperties rigidBody2 1.0 collision)
;
;(NewtonBodySetForceAndTorqueCallback rigidBody ApplyGravity)
;(NewtonBodySetForceAndTorqueCallback rigidBody2 ApplyGravity)
;(print "To rigid body added callback")

(NewtonDestroyCollision collision)
(print "3.NewtonGetMemoryUsed = " (NewtonGetMemoryUsed))

(define (glCube)
   (glPushMatrix)
   (glScalef 0.5 0.5 0.5)
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
   (glPopMatrix))



(gl:run

   "7. Newton"

; init
(lambda ()
   (glShadeModel GL_SMOOTH)
   (glClearColor 0.11 0.11 0.11 1)

   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity)
   (gluPerspective 45 (/ 640 480) 0.1 100)

   (glEnable GL_DEPTH_TEST)

   (NewtonInvalidateCache world))

; draw
(lambda ()
   ; обновим мир
   (NewtonUpdate world 1/400)
   ; и нарисуем его
   (glClear (vm:or GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (gluLookAt 10 15 20
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

   (let ((matrix '(0.1 0.1 0.1 0.1  0.1 0.1 0.1 0.1  0.1 0.1 0.1 0.1  0.1 0.1 0.1 0.1)))
      (for-each (lambda (cube)
         (NewtonBodyGetMatrix cube matrix)
         (glPushMatrix)
         (glMultMatrixf matrix)
         (glCube)
         (glPopMatrix)) cubes))
))

(NewtonDestroy world)
(print "4.NewtonGetMemoryUsed = " (NewtonGetMemoryUsed))
