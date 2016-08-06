#!/usr/bin/ol
(import (otus pinvoke))
;(define pthread (or
;   (dlopen "/lib/x86_64-linux-gnu/libpthread.so.0")
;   (runtime-error "No pthread library found." #f)))
(define $ (or
   (dlopen "/home/uri/Desktop/Otus Lisp/src/tutorial/newton-dynamics/coreLibrary_300/projects/posix64/libNewton.so")
   (runtime-error "Can't load newton library" #f)))

(define type-callback 61)
(define type-float* (vm:or type-float #x40))

(define NewtonWorldGetVersion (dlsym $ type-fix+ "NewtonWorldGetVersion"))
(define NewtonWorldFloatSize  (dlsym $ type-fix+ "NewtonWorldFloatSize"))
(define NewtonGetMemoryUsed   (dlsym $ type-int+ "NewtonGetMemoryUsed"))

(define NewtonCreate  (dlsym $ type-vptr "NewtonCreate"))
(define NewtonDestroy (dlsym $ type-void "NewtonDestroy" type-vptr))
(define NewtonWorldSetDestructorCallback (dlsym $ type-void "NewtonWorldSetDestructorCallback" type-vptr type-callback))

(define NewtonWorldDestructorCallback (cons
   (list type-vptr)
   (lambda (world) (print "world destroyed"))))

;
(define NewtonCreateBox (dlsym $ type-vptr "NewtonCreateBox" type-vptr type-float type-float type-float type-int+ type-float*))
(define NewtonCreateDynamicBody (dlsym $ type-vptr "NewtonCreateDynamicBody" type-vptr type-vptr type-float*))
(define NewtonBodySetForceAndTorqueCallback (dlsym $ type-void "NewtonBodySetForceAndTorqueCallback" type-vptr type-callback))
(define NewtonBodySetMassProperties (dlsym $ type-void "NewtonBodySetMassProperties" type-vptr type-float type-vptr))
(define NewtonDestroyCollision (dlsym $ type-void "NewtonDestroyCollision" type-vptr))

(define NewtonBodySetForce (dlsym $ type-void "NewtonBodySetForce" type-vptr type-float*))
(define NewtonBodyGetMatrix (dlsym $ type-void "NewtonBodyGetMatrix" type-vptr (vm:or type-float #x80)))

(define NewtonInvalidateCache (dlsym $ type-void "NewtonInvalidateCache" type-vptr))
(define NewtonUpdate (dlsym $ type-void "NewtonUpdate" type-vptr type-float))




(print "NewtonWorldGetVersion = " (NewtonWorldGetVersion))
(print "NewtonWorldFloatSize = "  (NewtonWorldFloatSize))
(print "1.NewtonGetMemoryUsed = " (NewtonGetMemoryUsed))

(define world (or
   (NewtonCreate)
   (runtime-error "Can't create newton world" #f)))

(print "2.NewtonGetMemoryUsed = " (NewtonGetMemoryUsed))

(NewtonWorldSetDestructorCallback world NewtonWorldDestructorCallback)


(define collision (or
   (NewtonCreateBox world  1 1 1  0  #f)
   (runtime-error "Can't create box" #f)))
(define rigidBody (or
   (NewtonCreateDynamicBody world collision  
      '(;x y z w
         1 0 0 0 ; front
         0 1 0 0 ; up
         0 0 1 0 ; right
         0 0 0 1 ; posit
      ))
   (runtime-error "Can't create rigid body" #f)))

(define ApplyGravity (cons
   (list type-vptr type-void* type-int+)
   (lambda (body timestep threadIndex)
      (display ".")
      (NewtonBodySetForce body '(0 -9.8 0 0)))))
(print "Created rigid body")

(NewtonBodySetMassProperties rigidBody 1.0 collision)

(NewtonBodySetForceAndTorqueCallback rigidBody ApplyGravity)
(print "To rigid body added callback")

(NewtonDestroyCollision collision)
(print "3.NewtonGetMemoryUsed = " (NewtonGetMemoryUsed))


; run the world
(NewtonInvalidateCache world)
(for-each (lambda (x)
      (NewtonUpdate world 1/60)

      ; print something

      (let ((matrix '(0.1 0.1 0.1 0.1  0.1 0.1 0.1 0.1  0.1 0.1 0.1 0.1  0.1 0.1 0.1 0.1)))
         (NewtonBodyGetMatrix rigidBody matrix)
         (print "height: " (list-ref matrix 13))))

   (iota 100))
(print)



(NewtonDestroy world)
(print "4.NewtonGetMemoryUsed = " (NewtonGetMemoryUsed))



;(define (smth x)
;   (syscall 1111
;      (lambda ()
;         ;(print "from callback: " x " -> " (generate-unique-id))
;         (print "from callback: " x)
;      ) #f #f))
;(print "callback result: " (smth 17))
;(smth 1)
;(smth 2)
;(smth 3)
