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
      NewtonWorld*
      NewtonCollision*
      NewtonBody*

      NewtonWorldGetVersion
      NewtonWorldFloatSize

      NewtonGetMemoryUsed
      ;NewtonSetMemorySystem

      NewtonCreate
      NewtonDestroy
      NewtonDestroyAllBodies

      ;NewtonAlloc
      ;NewtonFree

      ;NewtonEnumerateDevices
      ;NewtonGetCurrentDevice
      ;NewtonSetCurrentDevice
      ;NewtonGetDeviceString

      ;NewtonGetContactMergeTolerance
      ;NewtonSetContactMergeTolerance

      NewtonInvalidateCache
      ;NewtonSetColverModel
      ;NewtonSetColverConvergenceQuality

      ;NewtonSetMultiThreadSolverOnSingleIsland
      ;NewtonGetMultiThreadSolverOnSingleIsland

      ;NewtonGetBroadphaseAlgorithm
      ;NewtonSelectBroadphraseAlgorithm

      NewtonUpdate
      ;NewtonUpdateAsync
      ;NewtonWaitForUpdateToFinish

      ;NewtonSerializeToFile
      ;NewtonDeserializeFromFile

      ;NewtonSetJointSerializationCallbacks
      ;NewtonGetJointSerializationCallbacks

      ; ...

      ;NewtonAtomicAdd
      ;NewtonAtomicSwap
      ;NewtonYield

      ;NewtonSetFrictionModel
      ;NewtonSetMinimumFrameRate
      ;NewtonSetIslandUpdateEvent
      ;NewtonWorldForEachJointDo
      ;NewtonWorldForEachBodyInAABBDo

      ;NewtonWorldSetUserData
      ;NewtonWorldGetUserData

      ; ...

      NewtonWorldSetDestructorCallback NewtonWorldDestructorCallback

      ; ...

      ;NewtonWorldRayCast
      ;NewtonWorldConvexCast
      ;NewtonWorldCollide

      ;NewtonBodyGetBodyCount
      ;NewtonWorldGetConstraintCount

      ; ...

      ; physics contact control functions
      ; convex collision primitives creation functions
      NewtonCreateNull
      NewtonCreateSphere
      NewtonCreateBox
      NewtonCreateCone ; world  radius height  shapeID offsetMatrix
      NewtonCreateCapsule
      NewtonCreateCylinder ; world  radio0 radio1 height  shapeID offsetMatrix
      NewtonCreateChamferCylinder
      NewtonCreateConvexHull
      NewtonCreateConvexHullFromMesh

      ;NewtonCollisionGetMode
      ;NewtonCollisionSetMode

      ;NewtonConvexHullGetFaceIndices
      ;NewtonConvexHullGetVertexData

      ;NewtonConvexCollisionCalculateVolume
      ;NewtonConvexCollisionCalculateInertialMatrix
      ;NewtonConvexCollisionCalculateBuoyancyAcceleration

      ;NewtonCollisionDataPointer

      ; compound collision primitives creation functions
      ; ...

      ; collision library functions
      ;NewtonCollisionCreateInstance
      ;NewtonCollisionGetType
      ;NewtonCollisionIsConvexShape
      ;NewtonCollisionIsStaticShape

      ;NewtonCollisionSetUserData
      ;NewtonCollisionGetUserData

      ;NewtonCollisionSetUserData1
      ;NewtonCollisionGetUserData1

      ;NewtonCollisionSetUserID
      ;NewtonCollisionGetUserID

      ;NewtonCollisionGetSubCollisionHandle
      ;NewtonCollisionGetParentInstance

      ;NewtonCollisionSetMatrix
      ;NewtonCollisionGetMatrix

      ;NewtonCollisionSetScale
      ;NewtonCollisionGetScale
      NewtonDestroyCollision


      ; body manipulation functions
      NewtonCreateDynamicBody
      ;NewtonCreateKinematicBody
      ;NewtonCreateDeformableBody
      NewtonDestroyBody

      ;NewtonBodyGetSimulationState
      ;NewtonBodySetSimulationState

      ;NewtonBodyGetType
      ;NewtonBodyGetCollidable
      ;NewtonBodySetCollidable

      ;NewtonBodyAddForce
      ;NewtonBodyAddTorque
      ;NewtonBodyCalculateInverseDynamicForce

      ;NewtonBodySetCentreOfMass
      ;NewtonBodySetMassMatrix
      ;NewtonBodySetFullMassMatrix

      NewtonBodySetMassProperties
      NewtonBodySetMatrix
      ;NewtonBodySetMatrixNoSleep
      ;NewtonBodySetMatrixRecursive

      ;NewtonBodySetMaterialGroupID
      ;NewtonBodySetContinuousCollisionMode
      ;NewtonBodySetJointRecursiveCollision
      ;NewtonBodySetOmega
      ;NewtonBodySetOmegaNoSleep
      ;NewtonBodySetVelocity
      ;NewtonBodySetVelocityNoSleep
      NewtonBodySetForce
      ;NewtonBodySetTorque

      ;NewtonBodySetLinearDamping
      ;NewtonBodySetAngularDamping
      ;NewtonBodySetCollision
      ;NewtonBodySetCollisionScale

      ;NewtonBodyGetMaxRotationPerStep
      ;NewtonBodySetMaxRotationPerStep

      ;NewtonBodyGetSleepState
      ;NewtonBodySetSleepState

      ;NewtonBodyGetAutoSleep
      ;NewtonBodySetAutoSleep

      ;NewtonBodyGetFreezeState
      ;NewtonBodySetFreezeState

      ;NewtonBodySetDestructorCallback
      ;NewtonBodyGetDestructorCallback

      ;NewtonBodySetTransformCallback
      ;NewtonBodyGetTransformCallback

      NewtonBodySetForceAndTorqueCallback
      ;NewtonBodyGetForceAndTorqueCallback

      ;NewtonBodyGetID

      ;NewtonBodySetUserData
      ;NewtonBodyGetUserData

      ;NewtonBodyGetWorld
      ;NewtonBodyGetCollision
      ;NewtonBodyGetMaterialGroupID

      ;NewtonBodyGetContinuousCollisionMode
      ;NewtonBodyGetJointRecursiveCollision

      ;NewtonBodyGetPosition
      NewtonBodyGetMatrix
      ;NewtonBodyGetRotation
      ;NewtonBodyGetMass
      ;NewtonBodyGetInvMass
      ;NewtonBodyGetInertiaMatrix
      ;NewtonBodyGetInvInetiaMatrix
      ;NewtonBodyGetOmega
      ;NewtonBodyGetVelocity
      ;NewtonBodyGetForce
      ;NewtonBodyGetTorque
      ;NewtonBodyGetForceAcc
      ;NewtonBodyGetTorqueAcc
      ;NewtonBodyGetCentreOfMass

      ;NewtonBodyGetPointVelocity
      ;NewtonBodyAddImpulse
      ;NewtonBodyApplyImpulseArray

      ;NewtonBodyApplyImpulsePair

      ;NewtonBodyIntegrateVelocity

      ;NewtonBodyGetAngularDamping
      ;NewtonBodyGetAngularDamping
      ;NewtonBodyGetAABB

      ;NewtonBodyGetFirstJoint
      ;NewtonBodyGetNextJoint
      ;NewtonBodyGetFirstContactJoint
      ;NewtonBodyGetNextContactJoint

      ;NewtonBodyGetSkeleton


      ; contact joints interface

      NewtonCreateTreeCollision
      NewtonTreeCollisionBeginBuild
      NewtonTreeCollisionAddFace
      NewtonTreeCollisionEndBuild

      NewtonInvalidateCache
      NewtonUpdate

)
   (begin

(define $ (or
   (dlopen "libNewton.so")
   (dlopen "newton.dll")
   (runtime-error "Can't load newton library" #f)))
(define (make-cb p l)
   (syscall 175 (cons p l) #f #f))

(define type-callback 61)
(define type-float* (vm:or type-float #x40))

(define NewtonMesh* type-vptr)
(define NewtonBody* type-vptr)
(define NewtonWorld* type-vptr)
(define NewtonCollision* type-vptr)
(define dFloat* (vm:or type-float #x40))

(define (NewtonWorldDestructorCallback callback)
   (make-cb (list type-void)
      callback))



(define NewtonWorldGetVersion (dlsym $ type-fix+ "NewtonWorldGetVersion"))
(define NewtonWorldFloatSize  (dlsym $ type-fix+ "NewtonWorldFloatSize"))
(define NewtonGetMemoryUsed   (dlsym $ type-int+ "NewtonGetMemoryUsed"))

(define NewtonCreate  (dlsym $ NewtonWorld* "NewtonCreate"))
(define NewtonDestroy (dlsym $ type-void "NewtonDestroy" NewtonWorld*))
(define NewtonDestroyAllBodies (dlsym $ type-void "NewtonDestroyAllBodies" NewtonWorld*))
(define NewtonWorldSetDestructorCallback (dlsym $ type-void "NewtonWorldSetDestructorCallback" NewtonWorld* type-callback))

;
(define NewtonCreateNull (dlsym $ NewtonCollision* "NewtonCreateNull" NewtonWorld*))
(define NewtonCreateSphere (dlsym $ NewtonCollision* "NewtonCreateSphere" NewtonWorld* type-float type-fix+ dFloat*))
(define NewtonCreateBox (dlsym $ NewtonCollision* "NewtonCreateBox" NewtonWorld* type-float type-float type-float type-int+ dFloat*))
(define NewtonCreateCone (dlsym $ NewtonCollision* "NewtonCreateCone" NewtonWorld* type-float type-float type-int+ dFloat*))
(define NewtonCreateCapsule (dlsym $ NewtonCollision* "NewtonCreateCapsule" NewtonWorld* type-float type-float type-float type-int+ dFloat*))
(define NewtonCreateCylinder (dlsym $ NewtonCollision* "NewtonCreateCylinder" NewtonWorld* type-float type-float type-float type-int+ dFloat*))
(define NewtonCreateChamferCylinder (dlsym $ NewtonCollision* "NewtonCreateChamferCylinder" NewtonWorld* type-float type-float type-int+ dFloat*))
(define NewtonCreateConvexHull (dlsym $ NewtonCollision* "NewtonCreateConvexHull" NewtonWorld* type-int+ dFloat* type-int+ type-float type-int+ dFloat*))
(define NewtonCreateConvexHullFromMesh (dlsym $ NewtonCollision* "NewtonCreateConvexHullFromMesh" NewtonWorld* NewtonMesh* type-float type-int+))


(define NewtonCreateDynamicBody (dlsym $ NewtonBody* "NewtonCreateDynamicBody" NewtonWorld* NewtonCollision* dFloat*))
(define NewtonDestroyBody (dlsym $ type-void "NewtonDestroyBody" NewtonBody*))

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