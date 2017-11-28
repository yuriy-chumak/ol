#!/usr/bin/ol

(import (lib opengl))
(import (otus random!))
(import (otus ffi))
(import (lib newton))

(define (gettimeofday) (syscall 96 #f #f #f))

(define Context (gl:Create "7. Newton"))




;(define $ (or
;   (dlopen "./libNewton.so")
;   (dlopen "newton")
;   (runtime-error "Can't load newton library" #f)))

(define type-callback 61)
;(define fft-float* (vm:or fft-float #x40))
;(define NewtonWorld* type-vptr)
;(define dFloat* (vm:or fft-float #x40))
;
;(define NewtonWorldGetVersion (dlsym $ type-fix+ "NewtonWorldGetVersion"))
;(define NewtonWorldFloatSize  (dlsym $ type-fix+ "NewtonWorldFloatSize"))
;(define NewtonGetMemoryUsed   (dlsym $ type-int+ "NewtonGetMemoryUsed"))
;
;(define NewtonCreate  (dlsym $ type-vptr "NewtonCreate"))
;(define NewtonDestroy (dlsym $ fft-void "NewtonDestroy" type-vptr))
;(define NewtonWorldSetDestructorCallback (dlsym $ fft-void "NewtonWorldSetDestructorCallback" type-vptr type-callback))
;
;
;(define NewtonCreateSphere (dlsym $ type-vptr "NewtonCreateSphere" NewtonWorld* fft-float type-fix+ dFloat*))
;(define NewtonCreateBox (dlsym $ type-vptr "NewtonCreateBox" type-vptr fft-float fft-float fft-float type-int+ fft-float*))
;
;
;(define NewtonCreateDynamicBody (dlsym $ type-vptr "NewtonCreateDynamicBody" type-vptr type-vptr fft-float*))
;(define NewtonBodySetForceAndTorqueCallback (dlsym $ fft-void "NewtonBodySetForceAndTorqueCallback" type-vptr type-callback))
;(define NewtonBodySetMassProperties (dlsym $ fft-void "NewtonBodySetMassProperties" type-vptr fft-float type-vptr))
;(define NewtonDestroyCollision (dlsym $ fft-void "NewtonDestroyCollision" type-vptr))
;
;(define NewtonBodySetForce (dlsym $ fft-void "NewtonBodySetForce" type-vptr fft-float*))
;(define NewtonBodySetMatrix (dlsym $ fft-void "NewtonBodySetMatrix" type-vptr (vm:or fft-float #x40)))
;(define NewtonBodyGetMatrix (dlsym $ fft-void "NewtonBodyGetMatrix" type-vptr (vm:or fft-float #x80)))
;
;(define NewtonCreateTreeCollision (dlsym $ type-vptr "NewtonCreateTreeCollision" type-vptr type-fix+))
;(define NewtonTreeCollisionBeginBuild (dlsym $ fft-void "NewtonTreeCollisionBeginBuild" type-vptr))
;(define NewtonTreeCollisionAddFace (dlsym $ fft-void "NewtonTreeCollisionAddFace" type-vptr type-int+ fft-float* type-fix+ type-fix+))
;(define NewtonTreeCollisionEndBuild (dlsym $ fft-void "NewtonTreeCollisionEndBuild" type-vptr type-fix+))
;
;
;(define NewtonInvalidateCache (dlsym $ fft-void "NewtonInvalidateCache" type-vptr))
;(define NewtonUpdate (dlsym $ fft-void "NewtonUpdate" type-vptr fft-float))
;
;(print "NewtonWorldGetVersion = " (NewtonWorldGetVersion))
;(print "NewtonWorldFloatSize = "  (NewtonWorldFloatSize))
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
   (runtime-error "Can't create background" #f)))
(NewtonTreeCollisionBeginBuild collision)
; пол
(NewtonTreeCollisionAddFace collision 4 '(
   -9 0.1  9
    9 0.1  9
    9 0.1 -9
   -9 0.1 -9) (* 3 4) 0) ; (* 3 4) is stride, where 4 is sizeof(float)
; ступеньки
(NewtonTreeCollisionAddFace collision 4 '(
   -4 0.6  4
    4 0.6  4
    4 0.6 -4
   -4 0.6 -4) (* 3 4) 0) ; (* 3 4) is stride, where 4 is sizeof(float)
(NewtonTreeCollisionAddFace collision 4 '(
   -2 1.2  2
    2 1.2  2
    2 1.2 -2
   -2 1.2 -2) (* 3 4) 0) ; (* 3 4) is stride, where 4 is sizeof(float)

(NewtonTreeCollisionEndBuild collision 1)
(NewtonCreateDynamicBody world collision '(1 0 0 0  0 1 0 0  0 0 1 0  0 0 0 1))
(NewtonDestroyCollision collision)


; ...
(define ApplyGravity (syscall 85 (cons
   (list type-vptr fft-float type-int+)
   (lambda (body timestep threadIndex)
      (NewtonBodySetForce body '(0 -4.8 0 0))
)) #f #f))


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

               ,(/ (- (rand! 2000) 1000) 100) ; x
               ,(+ (/ id 3) 5)                ; y
               ,(/ (- (rand! 2000) 1000) 100) ; z
               1       ; posit
             ))))
   (iota 100)))
(for-each (lambda (cube)
   (NewtonBodySetMassProperties cube 1.0 collision)
   (NewtonBodySetForceAndTorqueCallback cube ApplyGravity)
) cubes)
(NewtonDestroyCollision collision)


(define collision (or
   (NewtonCreateSphere world  0.5  0  #f)
   (runtime-error "Can't create box" #f)))
(NewtonDestroyCollision collision)

(define spheres null);(map
;   (lambda (id)
;      (NewtonCreateDynamicBody world collision
;         `(;x y z w
;            1 0 0 0 ; front
;            0 1 0 0 ; up
;            0 0 1 0 ; right
;
;            ,(/ (- (rand! 400) 200) 100) ; x
;            ,(+ (/ id 3) 1)             ; y
;            ,(/ (- (rand! 400) 200) 100) ; z
;            1       ; posit
;          )))
;   (iota 50)))
;(for-each (lambda (sphere)
;   (NewtonBodySetMassProperties sphere 1.0 collision)
;   (NewtonBodySetForceAndTorqueCallback sphere ApplyGravity)
;) spheres)
; we need this collision for feature use
;(NewtonDestroyCollision collision)


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


(NewtonInvalidateCache world) ; say "world construction finished"

(print "3.NewtonGetMemoryUsed = " (NewtonGetMemoryUsed))

(define (glCube)
   (glColor3f 0.7 0.7 0.7)

   (glPushMatrix)
   (glScalef 0.5 0.5 0.5)
   (glBegin GL_QUADS)
      ; front
      (glNormal3f  0  0 -1)
      (glVertex3f  1  1 -1)
      (glVertex3f  1 -1 -1)
      (glVertex3f -1 -1 -1)
      (glVertex3f -1  1 -1)

      ; back
      (glNormal3f  0  0  1)
      (glVertex3f -1  1  1)
      (glVertex3f -1 -1  1)
      (glVertex3f  1 -1  1)
      (glVertex3f  1  1  1)

      ; right
      (glNormal3f  1  0  0)
      (glVertex3f  1 -1 -1)
      (glVertex3f  1  1 -1)
      (glVertex3f  1  1  1)
      (glVertex3f  1 -1  1)

      ; left
      (glNormal3f  1  0  0)
      (glVertex3f -1  1  1)
      (glVertex3f -1 -1  1)
      (glVertex3f -1 -1 -1)
      (glVertex3f -1  1 -1)

      ; top
      (glNormal3f  0  1  0)
      (glVertex3f -1  1 -1)
      (glVertex3f -1  1  1)
      (glVertex3f  1  1  1)
      (glVertex3f  1  1 -1)

      ; bottom
      (glNormal3f  0  1  0)
      (glVertex3f  1 -1  1)
      (glVertex3f  1 -1 -1)
      (glVertex3f -1 -1 -1)
      (glVertex3f -1 -1  1)

   (glEnd)
   (glPopMatrix))

(define gl-sphere (gluNewQuadric))
(gluQuadricDrawStyle gl-sphere GLU_FILL)

(define gl-cone (gluNewQuadric))
(gluQuadricDrawStyle gl-cone GLU_FILL)

(define (glSphere)
   (glPushMatrix)
   (glScalef 0.5 0.5 0.5)
   (gluSphere gl-sphere 1 16 8)
   (glPopMatrix))
(define (glCone)
   (glPushMatrix)
   (glRotatef 90.0 0 1 0)
   (gluCylinder gl-cone 0.5 0 1  15 5)
   (glPopMatrix))


(define collision (or
   ;(NewtonCreateCone world  0.5 1  0 #f)
   (NewtonCreateSphere world  0.5  0  #f)
   (runtime-error "Can't create box" #f)))




(gl:run

   Context

; init
(lambda ()
   (glShadeModel GL_SMOOTH)
   (glClearColor 0.11 0.11 0.11 1)

   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity)
   (gluPerspective 45 (/ 640 480) 0.1 1000)

   (glEnable GL_DEPTH_TEST)

   ; http://www.glprogramming.com/red/chapter12.html
   (glEnable GL_LIGHTING)
   (glLightModelf GL_LIGHT_MODEL_TWO_SIDE GL_TRUE)
   (glEnable GL_NORMALIZE)

   ; http://compgraphics.info/OpenGL/lighting/light_sources.php
   (glEnable GL_LIGHT0)
   (glLightfv GL_LIGHT0 GL_DIFFUSE '(0.7 0.7 0.7 1.0))

   (glEnable GL_COLOR_MATERIAL)


   ; return parameter list:
   (let ((oldtime (gettimeofday)))
   (list oldtime 1 cubes spheres)))

; draw
(lambda (oldtime i cubes spheres)
(let ((newtime (gettimeofday)))
   ; обновим мир
   (let ((ms (* (+ (- (car newtime) (car oldtime)) (/ (- (cdr newtime) (cdr oldtime)) 1000000)) 2)))
      (NewtonUpdate world (if (> ms 0.006) 0.006 ms)))

   ; и нарисуем его
   (glClear (vm:or GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))


   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (gluLookAt 10 15 20
      0 0 0
      0 1 0)

   (glLightfv GL_LIGHT0 GL_POSITION (list 10 5 2 1))
   ; colors in opengl when lighting use: http://stackoverflow.com/questions/8494942/why-does-my-color-go-away-when-i-enable-lighting-in-opengl
   (glColorMaterial GL_FRONT GL_DIFFUSE)

;   (glLightfv GL_LIGHT0 GL_POSITION (list 7 3.0 3.0 0.0))

   ; платформа
;  (glBegin GL_LINE_STRIP)
   (glPushMatrix)
   (glScalef 18 0.2 18)
   (glCube)
   (glPopMatrix)

   (glPushMatrix)
   (glScalef 8 1.2 8)
   (glCube)
   (glPopMatrix)

   (glPushMatrix)
   (glScalef 4 2.4 4)
   (glCube)
   (glPopMatrix)


   (glBegin GL_LINES)
      ; Ox
      (glColor3f 1 0 0)
      (glVertex3f 0 0 0)
      (glVertex3f 20 0 0)
         (glVertex3f 20 0 0)
         (glVertex3f 19 1 0)
         (glVertex3f 20 0 0)
         (glVertex3f 19 0 1)
      ; Oy
      (glColor3f 0 1 0)
      (glVertex3f 0 0 0)
      (glVertex3f 0 20 0)
         (glVertex3f 0 20 0)
         (glVertex3f 1 19 0)
         (glVertex3f 0 20 0)
         (glVertex3f 0 19 1)
      ; Oz
      (glColor3f 0 0 1)
      (glVertex3f 0 0 0)
      (glVertex3f 0 0 20)
         (glVertex3f 0 0 20)
         (glVertex3f 1 0 19)
         (glVertex3f 0 0 20)
         (glVertex3f 0 1 19)
   (glEnd)

;  (glMaterialfv GL_FRONT GL_DIFFUSE '(1 0 0 1))
   (let ((matrix '(0.1 0.1 0.1 0.1  0.1 0.1 0.1 0.1  0.1 0.1 0.1 0.1  0.1 0.1 0.1 0.1)))
      (for-each (lambda (cube)
         (NewtonBodyGetMatrix cube matrix)
         (glPushMatrix)
         (glMultMatrixf matrix)
         (glCube)
         (glPopMatrix)) cubes))
   (let ((matrix '(0.1 0.1 0.1 0.1  0.1 0.1 0.1 0.1  0.1 0.1 0.1 0.1  0.1 0.1 0.1 0.1)))
      (for-each (lambda (sphere)
         (NewtonBodyGetMatrix sphere matrix)
         (glPushMatrix)
         (glMultMatrixf matrix)
         (glSphere)
         ;(glCone)
         (glPopMatrix)) spheres))

   (if (and (> i 1000)
            (eq? (mod i 100) 0))
      (list
         newtime (+ i 1) cubes
            (let ((sphere (NewtonCreateDynamicBody world collision
                  `(;x y z w
                     1 0 0 0 ; front
                     0 1 0 0 ; up
                     0 0 1 0 ; right

                     ,(/ (- (rand! 200) 100) 100) ; x
                     ,8             ; y
                     ,(/ (- (rand! 200) 100) 100) ; z
                     1       ; posit
                   ))))
               (NewtonBodySetMassProperties sphere 1.0 collision)
               (NewtonBodySetForceAndTorqueCallback sphere ApplyGravity)
               (print "objects count: " (length spheres))
               (cons sphere spheres)))
   ; return new parameter list:
      (list
         newtime (+ i 1) cubes spheres)))
))

(NewtonDestroy world)
(print "4.NewtonGetMemoryUsed = " (NewtonGetMemoryUsed))
