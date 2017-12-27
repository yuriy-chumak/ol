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
      (otus ffi))
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
      NewtonCreateKinematicBody
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

(define newton (or
   (load-dynamic-library "libNewton.so")
   (load-dynamic-library "newton.dll")
   (runtime-error "Can't load newton library" '())))
(define (make-cb p l)
   (syscall 85 (cons p l) #f #f))

(define fft-float* (fft* fft-float))

(define NewtonMesh* type-vptr)
(define NewtonBody* type-vptr)
(define NewtonWorld* type-vptr)
(define NewtonCollision* type-vptr)

(define NewtonWorldDestructorCallback type-callable)
(define NewtonApplyForceAndTorque type-callable)

(define dFloat fft-float)
(define dFloat* (fft* dFloat))
(define dFloat& (fft& dFloat))

(define (NewtonWorldDestructorCallback callback)
   (make-cb (list fft-void)
      callback))


(define NewtonWorldGetVersion (newton fft-int "NewtonWorldGetVersion"))
(define NewtonWorldFloatSize  (newton fft-int "NewtonWorldFloatSize"))
(define NewtonGetMemoryUsed   (newton fft-int "NewtonGetMemoryUsed"))


(define NewtonCreate  (newton NewtonWorld* "NewtonCreate"))
(define NewtonDestroy (newton fft-void "NewtonDestroy" NewtonWorld*))
(define NewtonDestroyAllBodies (newton fft-void "NewtonDestroyAllBodies" NewtonWorld*))
(define NewtonWorldSetDestructorCallback (newton fft-void "NewtonWorldSetDestructorCallback" NewtonWorld* NewtonWorldDestructorCallback))

(define NewtonInvalidateCache (newton fft-void "NewtonInvalidateCache" NewtonWorld*))
(define NewtonUpdate (newton fft-void "NewtonUpdate" NewtonWorld* dFloat))
;NewtonUpdateAsync
;NewtonWaitForUpdateToFinish


(define NewtonCreateNull (newton NewtonCollision* "NewtonCreateNull" NewtonWorld*))
(define NewtonCreateSphere (newton NewtonCollision* "NewtonCreateSphere" NewtonWorld* dFloat fft-int dFloat*)) ; radius, shapeID, offsetMatrix
(define NewtonCreateBox (newton NewtonCollision* "NewtonCreateBox" NewtonWorld* dFloat dFloat dFloat fft-int dFloat*)) ; dx, dy, dz, ...
(define NewtonCreateCone (newton NewtonCollision* "NewtonCreateCone" NewtonWorld* dFloat dFloat fft-int dFloat*)) ; radius, height, ...
(define NewtonCreateCapsule (newton NewtonCollision* "NewtonCreateCapsule" NewtonWorld* dFloat dFloat dFloat fft-int dFloat*)) ; radius0, radius1, height
(define NewtonCreateCylinder (newton NewtonCollision* "NewtonCreateCylinder" NewtonWorld* dFloat dFloat dFloat fft-int dFloat*)) ; radius0, radius1, height
(define NewtonCreateChamferCylinder (newton NewtonCollision* "NewtonCreateChamferCylinder" NewtonWorld* dFloat dFloat fft-int dFloat*)) ; radius, height
(define NewtonCreateConvexHull (newton NewtonCollision* "NewtonCreateConvexHull" NewtonWorld* fft-int dFloat* fft-int dFloat fft-int dFloat*)) ; count, vertexCloud, strideInBytes, tolerance
(define NewtonCreateConvexHullFromMesh (newton NewtonCollision* "NewtonCreateConvexHullFromMesh" NewtonWorld* NewtonMesh* dFloat fft-int)) ; mesh, tolerance

(define NewtonDestroyCollision (newton fft-void "NewtonDestroyCollision" NewtonCollision*))


(define NewtonCreateDynamicBody (newton NewtonBody* "NewtonCreateDynamicBody" NewtonWorld* NewtonCollision* dFloat*))
(define NewtonCreateKinematicBody (newton NewtonBody* "NewtonCreateKinematicBody" NewtonWorld* NewtonCollision* dFloat*))
(define NewtonDestroyBody (newton fft-void "NewtonDestroyBody" NewtonBody*))

;NewtonBodyGetSimulationState
;NewtonBodySetSimulationState

;NewtonBodyGetType
;NewtonBodyGetCollidable
;NewtonBodySetCollidable

;NewtonBodyAddForce
;NewtonBodyAddTorque
;NewtonBodyCalculateInverseDynamicsForce

;NewtonBodySetCentreOfMass
;NewtonBodySetMassMatrix
;NewtonBodySetFullMassMatrix

(define NewtonBodySetMassProperties (newton fft-void "NewtonBodySetMassProperties" NewtonBody* dFloat NewtonCollision*))
(define NewtonBodySetMatrix (newton fft-void "NewtonBodySetMatrix" NewtonBody* dFloat*))
;NewtonBodySetMatrixNoSleep
;NewtonBodySetMatrixRecursive

;NewtonBodySetMaterialGroupID
;NewtonBodySetContinuousCollisionMode
;NewtonBodySetJointRecursiveCollision
;NewtonBodySetOmega
;NewtonBodySetOmegaNoSleep
;NewtonBodySetVelocity
;NewtonBodySetVelocityNoSleep
;NewtonBodySetForce
(define NewtonBodySetForce (newton fft-void "NewtonBodySetForce" NewtonBody* dFloat*))
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

(define NewtonBodySetForceAndTorqueCallback (newton fft-void "NewtonBodySetForceAndTorqueCallback" NewtonBody* NewtonApplyForceAndTorque))
;NewtonBodyGetForceAndTorqueCallback

;NewtonBodyGetID

;NewtonBodySetUserData
;NewtonBodyGetUserData

;NewtonBodyGetWorld
;NewtonBodyGetCollision
;NewtonBodyGetMaterialGroupID

;NewtonBodyGetSerializedID
;NewtonBodyGetContinuousCollisionMode
;NewtonBodyGetJointRecursiveCollision

;NewtonBodyGetPosition
(define NewtonBodyGetMatrix (newton fft-void "NewtonBodyGetMatrix" NewtonBody* dFloat&))
;NewtonBodyGetMatrix
;NewtonBodyGetRotation
;NewtonBodyGetMass
;NewtonBodyGetInvMass
;NewtonBodyGetInertiaMatrix
;NewtonBodyGetInvInertiaMatrix
;NewtonBodyGetOmega
;NewtonBodyGetVelocity
;NewtonBodyGetAlpha
;NewtonBodyGetAcceleration
;NewtonBodyGetForce
;NewtonBodyGetTorque
;NewtonBodyGetCentreOfMass

;NewtonBodyGetPointVelocity

;NewtonBodyApplyImpulsePair
;NewtonBodyAddImpulse
;NewtonBodyApplyImpulseArray

;NewtonBodyIntegrateVelocity

;NewtonBodyGetLinearDamping
;NewtonBodyGetAngularDamping
;NewtonBodyGetAABB

;NewtonBodyGetFirstJoint
;NewtonBodyGetNextJoint
;NewtonBodyGetFirstContactJoint
;NewtonBodyGetNextContactJoint

;; Static collision shapes functions
;NewtonCreateHeightFieldCollision
;NewtonHeightFieldSetUserRayCastCallback
;NewtonHeightFieldSetHorizontalDisplacement

(define NewtonCreateTreeCollision (newton NewtonCollision* "NewtonCreateTreeCollision" NewtonWorld* fft-int))
;NewtonCreateTreeCollisionFromMesh
;NewtonTreeCollisionSetUserRayCastCallback

(define NewtonTreeCollisionBeginBuild (newton fft-void "NewtonTreeCollisionBeginBuild" NewtonCollision*))
(define NewtonTreeCollisionAddFace (newton fft-void "NewtonTreeCollisionAddFace" NewtonCollision* fft-int dFloat* fft-int fft-int))
(define NewtonTreeCollisionEndBuild (newton fft-void "NewtonTreeCollisionEndBuild" NewtonCollision* fft-int))

;NewtonTreeCollisionGetFaceAttribute
;NewtonTreeCollisionSetFaceAttribute

;NewtonTreeCollisionForEachFace

;NewtonTreeCollisionGetVertexListTriangleListInAABB
;NewtonStaticCollisionSetDebugCallback

(print "NewtonWorldGetVersion = " (NewtonWorldGetVersion))
(print "NewtonWorldFloatSize = "  (NewtonWorldFloatSize))

))