#!/usr/bin/env ol

(import (lib gl))
(import (OpenGL 1.1))
(import (lib GLU))

(import (lib newton-dynamics))

; create the "world"
(define world (or
   (NewtonCreate)
   (runtime-error "Can't create newton world")))
(NewtonSetSolverIterations world 1)

; opengl init
; (glShadeModel GL_SMOOTH) TODO: add
(glClearColor 0.11 0.11 0.11 1)
(glEnable GL_DEPTH_TEST)

; utils
,load "models/box.lisp"

; ------------------------------------
; newton body creation

; 1. create collision model (the cube)
;    "1 1 1" is box sizes
(define collision (NewtonCreateBox world  1 1 1  0  #f))

; 2. create a dynamic body (body than can move)
(define cube (NewtonCreateDynamicBody world collision
      ; column-major body matrix: "all-in-one" location and rotation
      `( 1 0 0 0
         0 1 0 0
         0 0 1 0
         0 10 0 1; x y z w
      )))

; 3. set body mass
(NewtonBodySetMassProperties cube 1.0 collision)

; 4. set "apply-gravity" callback to body
;    (Oy is "up")
(define ApplyGravityCallback (NewtonApplyForceAndTorque
   (lambda (body timestep threadIndex)
      (NewtonBodySetForce body '(0 -9.8 0 0)))))

(NewtonBodySetForceAndTorqueCallback cube ApplyGravityCallback)

; 5. delete collision, we don't need it anymore
(NewtonDestroyCollision collision)


; -- floor --------------------
(define collision (NewtonCreateBox world  2 2 2  0  #f))
(define floor (NewtonCreateKinematicBody world collision
      ; column-major body matrix: "all-in-one" location and rotation
      `( 1 0 0 0
         0 1 0 0
         0 0 1 0
         1.1 0 -1.1 1; x y z w
      )))
(NewtonDestroyCollision collision)


;; ; keyboard handler
;; (import (lib keyboard))
;; (gl:set-keyboard-handler (lambda (key)
;;    (case key
;;       (KEY_ESC
;;          (NewtonBodySetVelocity cube [0 0 0]) ; stop moving
;;          (NewtonBodySetMatrix cube            ; move to start location
;;                ; column-major body matrix: location and rotation
;;                `( 1 0 0 0
;;                   0 1 0 0
;;                   0 0 1 0
;;                   0 10 0 1; x y z w
;;                )))
;;    )))


; draw
(define old (inexact 0.0))
(define ol2 (inexact 0.0))

(gl:set-renderer (lambda (mouse)
   ; let's calculate newtonian world
   (define now (/ (mod (clock-ms) 1000000) #i1000))
   (NewtonUpdate world (min (- now old) 0.05))
   (vm:set! old now)

   ; reset cube after 5 second
   (when (> (- now ol2) 5)
      (print "reset cube")
      (vm:set! ol2 now)
      (NewtonBodySetVelocity cube [0 0 0]) ; stop moving
      (NewtonBodySetMatrix cube            ; move to start location
            ; column-major body matrix: location and rotation
            `( 1 0 0 0
               0 1 0 0
               0 0 1 0
               0 10 0 1; x y z w
            )))

   ; clear frame
   (glClear (vm:ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity)
   (gluPerspective 45.0
      (/ (gl:get-window-width) (gl:get-window-height))
      0.1 1000)

   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (gluLookAt 15 10 20
      0 0 0
      0 1 0)

   ; get body matrix
   (define matrix [
      #i0 #i0 #i0 #i0
      #i0 #i0 #i0 #i0
      #i0 #i0 #i0 #i0
      #i0 #i0 #i0 #i0 ])
   (NewtonBodyGetMatrix cube matrix)

   ; apply matrix to the opengl cube
   (glPushMatrix)
   (glMultMatrixf matrix)
   (draw-Box 1 1 1)
   (glPopMatrix)

   ; draw the "floor" cube
   (NewtonBodyGetMatrix floor matrix)
   (glPushMatrix)
   (glMultMatrixf matrix)
   (draw-Box 2 2 2)
   (glPopMatrix)
   

   ; draw xyz axis
   (glBegin GL_LINES)
      (glColor3f 1 0 0)
      (glVertex3f -1 0 0)
      (glVertex3f 10 0 0)
      (glColor3f 0 1 0)
      (glVertex3f 0 -1 0)
      (glVertex3f 0 10 0)
      (glColor3f 0 0 1)
      (glVertex3f 0 0 -1)
      (glVertex3f 0 0 10)
   (glEnd)
))

(print "Press [ESC] to reset cube position")
