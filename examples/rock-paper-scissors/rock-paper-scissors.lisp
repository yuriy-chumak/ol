#!/usr/bin/env ol

(import (lib gl))
(gl:set-window-title "Rock Paper Scissors")
(let ((ws 80))
   (gl:set-window-size (* 16 ws) (* 9 ws)))
(import (OpenGL 1.0))

(import (lib GLU))
(import (lib newton-dynamics))
(import (lib soil))

(define world (or
   (NewtonCreate)
   (runtime-error "Can't create newtonian world")))
(print "NewtonGetMemoryUsed = " (NewtonGetMemoryUsed))

; создадим "пол"
(define collision (or
   (NewtonCreateTreeCollision world 0)
   (runtime-error "Can't create background")))
(NewtonTreeCollisionBeginBuild collision)

(define S 4)
(define-values (W H) (values (* S 16) (* S 9)))
(define speed 12)

; пол
(NewtonTreeCollisionAddFace collision 4 [
   0 0 0
   W 0 0
   W H 0
   0 H 0 ] (* 3 4) 0) ; (* 3 4) is a stride

(NewtonTreeCollisionAddFace collision 4 [
   0 0 1.1
   0 H 1.1
   W H 1.1
   W 0 1.1] (* 3 4) 0)

; стены
(for-each (lambda (a b)
      (vector-apply a (lambda (ax ay)
      (vector-apply b (lambda (bx by)
         (NewtonTreeCollisionAddFace collision 4 [
            ax ay 0
            ax ay 9
            bx by 9
            bx by 0 ] (* 3 4) 0))))))
   (list [0 0] [W 0] [W H] [0 H])
   (list [W 0] [W H] [0 H] [0 0]) )

(NewtonTreeCollisionEndBuild collision 1)
(define cell
(NewtonCreateDynamicBody world collision [
   1 0 0 0
   0 1 0 0
   0 0 1 0
   0 0 0 1]))
(NewtonBodySetLinearDamping cell 0)
(NewtonBodySetAngularDamping cell [0 0 0])
(NewtonDestroyCollision collision)

; default gravity callback
(define ApplyGravityCallback (NewtonApplyForceAndTorque
   (lambda (body timestep threadIndex)
      (NewtonBodySetForce body [0 0 -9.8])
      
      (define v [#i0 #i0 #i0])
      (NewtonBodyGetVelocity body v)
      
      (vector-apply v (lambda (x y z)
         (define q (/ speed (sqrt (inexact (+ (* x x) (* y y))))))
         (NewtonBodySetVelocity body [(* x q) (* y q) z]) ))
      
   1)))

(define OnBodyAABBOverlap (NewtonOnAABBOverlap
   (lambda (contact timestep threadIndex)
      (call/cc (lambda (return)
         (define body1 (NewtonJointGetBody0 contact))
         (define body2 (NewtonJointGetBody1 contact))

         (define sphere1 (vm:deref (NewtonBodyGetUserData body1)))
         (define sphere2 (vm:deref (NewtonBodyGetUserData body2))) ))

   1)))

(define OnContactCollision (NewtonContactsProcess
   (lambda (joint timestep threadIndex)
      (define body1 (NewtonJointGetBody0 joint))
      (define body2 (NewtonJointGetBody1 joint))

      (define sphere1 (vm:deref (NewtonBodyGetUserData body1)))
      (define sphere2 (vm:deref (NewtonBodyGetUserData body2)))

      (when (and sphere1 sphere2)
         (define new (cond
            ((and (eq? (sphere1 'state #f) 'rock   ) (eq? (sphere2 'state #f) 'paper  )) 'paper)
            ((and (eq? (sphere1 'state #f) 'rock   ) (eq? (sphere2 'state #f) 'scissor)) 'rock)
            ((and (eq? (sphere1 'state #f) 'paper  ) (eq? (sphere2 'state #f) 'rock   )) 'paper)
            ((and (eq? (sphere1 'state #f) 'paper  ) (eq? (sphere2 'state #f) 'scissor)) 'scissor)
            ((and (eq? (sphere1 'state #f) 'scissor) (eq? (sphere2 'state #f) 'rock   )) 'rock)
            ((and (eq? (sphere1 'state #f) 'scissor) (eq? (sphere2 'state #f) 'paper  )) 'scissor)))

         (when new
            (put! sphere1 'state new)
            (put! sphere2 'state new)))

      1)))


(import (srfi 27)) ; random
(define w (- W 3))
(define h (- H 3))
(define s 3)
(define collision (NewtonCreateSphere world 0.5 0 #f))
(define defaultMaterialID (NewtonMaterialGetDefaultGroupID world))

(define spheres (map (lambda (n)
      (define x (+ 1.5 (* (random-real) w)))
      (define y (+ 1.5 (* (random-real) h)))
      (define z 0.5)

      (define u (* s (- (random-real) 0.5)))
      (define v (* s (- (random-real) 0.5)))

      (define body (NewtonCreateDynamicBody world collision [
         1 0 0 0
         0 1 0 0
         0 0 1 0
         x y z 1 ]))
      (NewtonBodySetMassProperties body 1.0 collision)
      (NewtonBodySetVelocity body [u v 0])
      (NewtonBodySetLinearDamping body 0)
      (NewtonBodySetAngularDamping body [0 0 0])

      (NewtonBodySetForceAndTorqueCallback body ApplyGravityCallback)
      (NewtonMaterialSetCollisionCallback world defaultMaterialID defaultMaterialID OnBodyAABBOverlap OnContactCollision)

      (define sphere {
         'body body
         'state (vector-ref ['rock 'paper 'scissor] (random-integer 3))
      })
      (NewtonBodySetUserData body (vm:pin sphere))
      sphere)
   (iota 100)))
(NewtonDestroyCollision collision)

; draw spheres
(define gl-sphere (gluNewQuadric))
(gluQuadricDrawStyle gl-sphere GLU_FILL)
(define (glSphere)
   (gluSphere gl-sphere 0.5 16 8))

; init
(glShadeModel GL_SMOOTH)
(glClearColor 0.11 0.11 0.11 1)

(glEnable GL_DEPTH_TEST)

(glEnable GL_LIGHTING)
(glLightModelf GL_LIGHT_MODEL_TWO_SIDE GL_TRUE)
(glEnable GL_NORMALIZE)
(glEnable GL_LIGHT0)
(glLightfv GL_LIGHT0 GL_DIFFUSE '(0.7 0.7 0.7 1.0))
(glEnable GL_COLOR_MATERIAL)

; draw
(gl:set-renderer (lambda (mouse)
   (glClear (bor GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity)
   (glOrtho 0 W  0 H  -10 1)

   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)

   (glBegin GL_QUADS)
      (glColor3f 183/255 179/255 161/255)
      (glVertex3f 0 0 0)
      (glVertex3f W 0 0)
      (glVertex3f W H 0)
      (glVertex3f 0 H 0)
   (glEnd)

   (for-each (lambda (sphere)
         (define matrix [
            #i1 #i0 #i0 #i0
            #i0 #i1 #i0 #i0
            #i0 #i0 #i1 #i0
            #i0 #i0 #i0 #i1 ])
         (NewtonBodyGetMatrix (sphere 'body) matrix)

         (glPushMatrix)
         (glMultMatrixd matrix)
         (case (sphere 'state)
            ('rock (glColor3f 1 0 0))
            ('paper (glColor3f 0 1 0))
            ('scissor (glColor3f 0 0 1)))
         (glSphere)
         (glPopMatrix))
      spheres)
))

(import (scheme dynamic-bindings))
(define time (make-parameter (time-ms)))
; physics
(gl:set-calculator (lambda ()
   ; delta_t - дельта по времени (seconds)
   (define now (time-ms))
   (define old (time now))
   ; временной интервал с прошлой математики
   (define delta_t (/ (- now old) #i1000))
   (if (< delta_t 0)
      (print "DEPTA_T !!! " delta_t))
   (if (> delta_t 0.1)
      (print "DEPTA_T !!! " delta_t))

   (NewtonUpdate world (min 1/60 (max 1/200 delta_t)))
))
