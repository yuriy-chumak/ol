#!/usr/bin/ol

(import (lib gl))
(gl:set-window-title "4. Shadow mapping")
;(gl:set-window-size 854 480)

(import (OpenGL version-1-1))
(import (OpenGL ARB depth_texture))
(import (OpenGL ARB shadow))

(unless (and ARB_depth_texture ARB_shadow)
   (begin
      (print "ARB_depth_texture and ARB_shadow extensions required.")
      (halt 1)))

(import (lib math))

; load the model
(import (lang sexp))
(import (file wavefront obj))
(import (file wavefront mtl))

(define (syntax-fail pos info lst)
   (list 'fail info '()))


(define obj-filename "obj/Palm_Tree.obj")
(define object
(let*((file (fopen obj-filename 0))
      (stream (fd->exp-stream file "" wavefront-obj-parser syntax-fail)))
   (fclose file)
   (car stream)))

(define mtl-filename (string-append "obj/" (get object 'mtllib "")))
(define materials
(let*((file (fopen mtl-filename 0))
      (stream (fd->exp-stream file "" wavefront-mtl-parser syntax-fail)))
   (fclose file)
   (car stream)))
(define materials
   (fold (lambda (o material)
            (cons (getf material 'name) (cons material o)))
      #null materials))

; prepare model
(define obj (reverse
(let*((facegroups (get object 'facegroups '()))
      (from-index (glGenLists (length facegroups)))
      (vertices (list->vector (get object 'v #null)))
      (normals (list->vector (get object 'vn #null))))
   (fold (lambda (o group i)
            (let*((mtl (car group))
                  (material (cadr (member mtl materials))))
               (glNewList i GL_COMPILE)
               (glBegin GL_TRIANGLES)
               (for-each (lambda (face)
                     (let ((a (ref (ref face 1) 1)) ; vertices
                           (b (ref (ref face 2) 1))
                           (c (ref (ref face 3) 1))

                           (r (ref (ref face 1) 3)) ; normals
                           (s (ref (ref face 2) 3))
                           (t (ref (ref face 3) 3)))
                        (glNormal3fv (ref normals r))
                        (glVertex3fv (ref vertices a))
                        (glNormal3fv (ref normals s))
                        (glVertex3fv (ref vertices b))
                        (glNormal3fv (ref normals t))
                        (glVertex3fv (ref vertices c))))
                  (cdr group))
               (glEnd)
               (glEndList)
               (cons (cons i material) o)))
      #null
      facegroups
      (iota (length facegroups) from-index)))))

; ---------------------------------------------
; setup
(glMatrixMode GL_MODELVIEW)
(glLoadIdentity)

(glShadeModel GL_SMOOTH)
(glClearColor 0 0 0 0)
(glColor4f 1 1 1 1)
(glHint GL_PERSPECTIVE_CORRECTION_HINT GL_NICEST)

; depth states
(glClearDepth 1)
(glDepthFunc GL_LEQUAL)
(glEnable GL_DEPTH_TEST)

(glEnable GL_CULL_FACE)

; We use glScale when drawing the scene (? maybe not required)
(glEnable GL_NORMALIZE)

; Create the shadow map texture
(define shadowMapTexture (box 0))
(glGenTextures 1 shadowMapTexture)
(define shadowMapTexture (unbox shadowMapTexture))
(print "shadowMapTexture: " shadowMapTexture)

(define shadowMapSize 512)

(glBindTexture GL_TEXTURE_2D shadowMapTexture)
(glTexImage2D  GL_TEXTURE_2D 0 GL_DEPTH_COMPONENT shadowMapSize shadowMapSize 0 GL_DEPTH_COMPONENT GL_UNSIGNED_BYTE NULL)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP)

; Use the color as the ambient and diffuse material
(glColorMaterial GL_FRONT GL_AMBIENT_AND_DIFFUSE)
(glEnable GL_COLOR_MATERIAL)

(define white '(1 1 1 1))
(glMaterialfv GL_FRONT GL_SPECULAR white)
(glMaterialf GL_FRONT GL_SHININESS 16)

; prepare matrices
(glPushMatrix)

(define (make-mat4x4) (map (lambda (_) (inexact 0)) (iota 16)))

(define lightProjectionMatrix (make-mat4x4))
(define lightViewMatrix (make-mat4x4))
(define cameraProjectionMatrix (make-mat4x4))
(define cameraViewMatrix (make-mat4x4))

(glLoadIdentity)
(gluPerspective 45.0 (/ 854 480) 1.0 100)
(glGetFloatv GL_MODELVIEW_MATRIX cameraProjectionMatrix)

(glLoadIdentity)
(gluLookAt 6 9 6 ;cameraPosition
           1 3 0
           0 1 0)
(glGetFloatv GL_MODELVIEW_MATRIX cameraViewMatrix)

(glLoadIdentity)
(gluPerspective 45.0 1.0 2.0 32.0)
(glGetFloatv GL_MODELVIEW_MATRIX lightProjectionMatrix)
;(print "lightProjectionMatrix: " lightProjectionMatrix)

(glLoadIdentity)
(gluLookAt 0 9 3 ;lightPosition
           1 3 0
           0 1 0)
(glGetFloatv GL_MODELVIEW_MATRIX lightViewMatrix)

(glPopMatrix)

(define (DrawScene)
   ; floor
   (glColor4f '(0.2 0.2 0.2 1))
   (glBegin GL_QUADS)
      (glNormal3f 0 1 0)
      (glVertex3f -5 0 -5)
      (glVertex3f  5 0 -5)
      (glVertex3f  5 0  5)
      (glVertex3f -5 0  5)
   (glEnd)

   ; show model
   (for-each (lambda (o)
         (let ((diffuse-color (get (cdr o) 'kd #f)))
            (glColor3f diffuse-color))
         (glCallList (car o)))
      obj)
)

(define (skip l n) ; util
   (unless (zero? n)
      (skip (cdr l) (- n 1))
      l))


; draw
(gl:set-renderer (lambda ()
   ; First pass - from light's point of view
   (glClear (vm:or GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   (glMatrixMode GL_PROJECTION)
   (glLoadMatrixf lightProjectionMatrix)
 
   (glMatrixMode GL_MODELVIEW)
   (glLoadMatrixf lightViewMatrix)
 
   ; //Use viewport the same size as the shadow map
   (glViewport 0 0 shadowMapSize shadowMapSize)
 
   ; //Draw back faces into the shadow map
   (glCullFace GL_FRONT)
 
   ; //Disable color writes, and use flat shading for speed
   (glShadeModel GL_FLAT)
   (glColorMask 0 0 0 0)
     
   ; //Draw the scene
   (DrawScene)
 
   ; //Read the depth buffer into the shadow map texture
   (glBindTexture GL_TEXTURE_2D shadowMapTexture)
   (glCopyTexSubImage2D GL_TEXTURE_2D 0 0 0 0 0 shadowMapSize shadowMapSize)
 
   ; //restore states
   (glColorMask 1 1 1 1)
   (glShadeModel GL_SMOOTH)
   (glCullFace GL_BACK)

   ; DEBUG:
   ;; (glViewport 0 0 854 480)
   ;; (glClear GL_DEPTH_BUFFER_BIT)

   ;; (glMatrixMode GL_PROJECTION)
   ;; (glLoadIdentity)
   ;; (glMatrixMode GL_MODELVIEW)
   ;; (glLoadIdentity)

   ;; (glEnable GL_TEXTURE_2D)
   ;; (glBindTexture GL_TEXTURE_2D shadowMapTexture)

   ;; (glColor3f 0.8 0.8 0.8)
   ;; (glBegin GL_QUADS)
   ;; (glTexCoord2f  0  0)
   ;; (glVertex2f   -1 -1)
   ;; (glTexCoord2f  1  0)
   ;; (glVertex2f    1 -1)
   ;; (glTexCoord2f  1  1)
   ;; (glVertex2f    1  1)
   ;; (glTexCoord2f  0  1)
   ;; (glVertex2f   -1  1)
   ;; (glEnd)

   ;; (glDisable GL_TEXTURE_2D)
 
   ;; ; end of DEBUG

   ;#|
   ;; ; //2nd pass - Draw from camera's point of view
   (glViewport 0 0 854 480)
   (glClear GL_DEPTH_BUFFER_BIT)
 
   (glMatrixMode GL_PROJECTION)
   ;(glLoadMatrixf cameraProjectionMatrix)
   (glLoadIdentity)
   (gluPerspective 45.0 (/ 854 480) 1.0 100)
     
   (glMatrixMode GL_MODELVIEW)
   ;(glLoadMatrixf cameraViewMatrix)
   (glLoadIdentity)
   (gluLookAt 6 9 6 ;cameraPosition
            1 3 0
            0 1 0)
 
   ; //Use dim light to represent shadowed areas
   (glLightfv GL_LIGHT1 GL_POSITION '(0 9 3 1)) ; lightPosition
   (glLightfv GL_LIGHT1 GL_AMBIENT '(0.2 0.2 0.2 1)) ; white * 0.2
   (glLightfv GL_LIGHT1 GL_DIFFUSE '(0.2 0.2 0.2 1)) ; white * 0.2
   (glLightfv GL_LIGHT1 GL_SPECULAR '(0 0 0 1)) ; black
   (glEnable GL_LIGHT1)
   (glEnable GL_LIGHTING)

   ;; (DrawScene)

   ;#|
   ;; ; //3rd pass
   ;; ; //Draw with bright light
   (glLightfv GL_LIGHT1 GL_DIFFUSE white)
   (glLightfv GL_LIGHT1 GL_SPECULAR white)

   (glMatrixMode GL_MODELVIEW)
   (glPushMatrix)
   (glLoadIdentity)
   (glLoadMatrixf '( ; biasMatrix
      0.5  0.0  0.0  0.0
      0.0  0.5  0.0  0.0
      0.0  0.0  0.5  0.0
      0.5  0.5  0.5  1.0)) ; //bias from [-1, 1] to [0, 1]
   (glMultMatrixf lightProjectionMatrix)
   (glMultMatrixf lightViewMatrix)

   (define textureMatrix (make-mat4x4))
   (glGetFloatv GL_MODELVIEW_MATRIX textureMatrix)

   (glPopMatrix)

   (glTexGeni GL_S GL_TEXTURE_GEN_MODE GL_EYE_LINEAR)
   (glTexGenfv GL_S GL_EYE_PLANE (take (skip textureMatrix 0) 4)) ;GetRow(0))
   (glEnable GL_TEXTURE_GEN_S)
 
   (glTexGeni GL_T GL_TEXTURE_GEN_MODE GL_EYE_LINEAR)
   (glTexGenfv GL_T GL_EYE_PLANE (take (skip textureMatrix 4) 4)) ;GetRow(1))
   (glEnable GL_TEXTURE_GEN_T)
 
   (glTexGeni GL_R GL_TEXTURE_GEN_MODE GL_EYE_LINEAR)
   (glTexGenfv GL_R GL_EYE_PLANE (take (skip textureMatrix 8) 4)) ;GetRow(2))
   (glEnable GL_TEXTURE_GEN_R)
 
   (glTexGeni GL_Q GL_TEXTURE_GEN_MODE GL_EYE_LINEAR)
   (glTexGenfv GL_Q GL_EYE_PLANE (take (skip textureMatrix 12) 4)) ;GetRow(3))
   (glEnable GL_TEXTURE_GEN_Q)

   (glBindTexture GL_TEXTURE_2D shadowMapTexture)
   (glEnable GL_TEXTURE_2D)

   ; Enable shadow comparison
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_COMPARE_MODE_ARB GL_COMPARE_R_TO_TEXTURE_ARB)

   ; Shadow comparison should be true (ie not in shadow) if r<=texture
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_COMPARE_FUNC_ARB GL_LEQUAL)

   ; Shadow comparison should generate an INTENSITY result
   (glTexParameteri GL_TEXTURE_2D GL_DEPTH_TEXTURE_MODE_ARB GL_INTENSITY)

   ; Set alpha test to discard false comparisons
   (glAlphaFunc GL_GEQUAL 0.99)
   (glEnable GL_ALPHA_TEST)

   (DrawScene)

   (glDisable GL_TEXTURE_2D)
 
   (glDisable GL_TEXTURE_GEN_S)
   (glDisable GL_TEXTURE_GEN_T)
   (glDisable GL_TEXTURE_GEN_R)
   (glDisable GL_TEXTURE_GEN_Q)
 
   (glDisable GL_LIGHTING)
   (glDisable GL_ALPHA_TEST)

   #||#

;  OLD: regular 
   ;; (glViewport 0 0 854 480)

   ;; (glMatrixMode GL_PROJECTION)
   ;; (glLoadIdentity)
   ;; (gluPerspective 45 (/ 854 480) 0.1 100)


   ;; (glMatrixMode GL_MODELVIEW)
   ;; (glLoadIdentity)
   ;; (gluLookAt 6 9 6
   ;;    1 3 0
   ;;    0 1 0)

   ;; ; show lighting point
   ;; (let*((ss ms (clock))
   ;;       (x 0) ;(* 3 (sin (+ ss (/ ms 1000)))))
   ;;       (y 4) ;(+ 4 (* 3 (sin (/ (+ ss (/ ms 1000)) 8)))))
   ;;       (z 3));(* 3 (cos (+ ss (/ ms 1000))))))
   ;;    (glDisable GL_LIGHTING)
   ;;    (glPointSize 5)
   ;;    (glBegin GL_POINTS)
   ;;    (glColor3f #xff/255 #xbf/255 0)
   ;;    (glVertex3f x y z)
   ;;    (glEnd)

   ;;    (glEnable GL_LIGHTING)
   ;;    (glLightfv GL_LIGHT0 GL_POSITION (list x y z 0)))

   ;; (DrawScene)

))