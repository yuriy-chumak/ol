; About Newton Dynamics
; Newton Dynamics is a cross-platform life-like physics simulation library.
; It can easily be integrated into game engines and other applications and
; provides top of it's class performance and simulation stability. Ongoing
; developement and a permissive license makes Newton Dynamics a top choice
; for all kinds of projects from scientific projects to game engines.
;
; Newton Dynamics implements a deterministic solver, which is not based on
; traditional LCP or iterative methods, but possesses the stability and
; speed of both respectively. This feature makes Newton Dynamics a tool
; not only for games, but also for any real-time physics simulation.
;
; Links to demos, tutorial, FAQ, etc: github.com/newton-dynamics/wiki
; Main project page: http://newtondynamics.com
; Forums and public discussion: http://newtondynamics.com/forum
;
; License
; Newton is licensed under the liberal zlib open source license, with
; little if any practical difference between them.
;
; Authors
; Newton Dynamics is a project of Julio Jerez and Alain Suero and various
; other contributors. 
; See the AUTHORS page on github for a full list of all contributors.

(define-library (lib newton)
   (import
      (otus lisp)
      (otus pinvoke))
   (export
      NewtonWorld*)
   (begin

(define $ (or
   (dlopen "libNewton.so")
   (dlopen "newton")
   (runtime-error "Can't load newton library" #f)))

(define type-callback 61)
(define type-float* (vm:or type-float #x40))
(define NewtonWorld* type-vptr)
(define dFloat* (vm:or type-float #x40))

(define NewtonWorldGetVersion (dlsym $ type-fix+ "NewtonWorldGetVersion"))
(define NewtonWorldFloatSize  (dlsym $ type-fix+ "NewtonWorldFloatSize"))
(define NewtonGetMemoryUsed   (dlsym $ type-int+ "NewtonGetMemoryUsed"))

(define NewtonCreate  (dlsym $ type-vptr "NewtonCreate"))
(define NewtonDestroy (dlsym $ type-void "NewtonDestroy" type-vptr))
(define NewtonWorldSetDestructorCallback (dlsym $ type-void "NewtonWorldSetDestructorCallback" type-vptr type-callback))

;
(define NewtonCreateSphere (dlsym $ type-vptr "NewtonCreateSphere" NewtonWorld* type-float type-fix+ dFloat*))
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

))