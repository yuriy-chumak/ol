#!/usr/bin/ol

(import (lib gl))
(gl:set-window-title "4. Shadow mapping")
;(gl:set-window-size 854 480)

(import (OpenGL version-1-3))
(import (OpenGL ARB depth_texture))
(import (OpenGL ARB shadow))
;(import (OpenGL ARB multitexture))

(unless (and ARB_depth_texture ARB_shadow)
   (begin
      (print "ARB_depth_texture and ARB_shadow extensions required.")
      (halt 1)))

(import (lib math))
(define (make-mat4x4) (map (lambda (_) (inexact 0)) (iota 16)))

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
;; (glMatrixMode GL_MODELVIEW)
;; (glLoadIdentity)

;; (glClearColor 0 0 0 0)
;; (glColor4f 1 1 1 1)
(glShadeModel GL_SMOOTH)
(glHint GL_PERSPECTIVE_CORRECTION_HINT GL_NICEST)

;; ; depth states
;; (glClearDepth 1)
;; (glDepthFunc GL_LEQUAL)
(glEnable GL_DEPTH_TEST)

(glEnable GL_LIGHTING)
(glLightModelf GL_LIGHT_MODEL_TWO_SIDE GL_TRUE)
(glEnable GL_LIGHT0)
;(glLightfv GL_LIGHT0 GL_POSITION '(0 3 0  1))
(glLightfv GL_LIGHT0 GL_AMBIENT '(0.2 0.2 0.2 1)) ; white * 0.2
(glLightfv GL_LIGHT0 GL_DIFFUSE '(0.2 0.2 0.2 1)) ; white * 0.2
(glLightfv GL_LIGHT0 GL_SPECULAR '(0 0 0 1)) ; black

;? (glEnable GL_NORMALIZE)

(glColorMaterial GL_FRONT GL_AMBIENT_AND_DIFFUSE)
;(glMaterialf GL_FRONT GL_SHININESS 16)
(glEnable GL_COLOR_MATERIAL)

(glEnable GL_CULL_FACE)

;; ; We use glScale when drawing the scene (? maybe not required)
(glEnable GL_NORMALIZE)

; Create the shadow map texture
(define shadowMapTexture (box 0))
(glGenTextures 1 shadowMapTexture)
(define shadowMapTexture (unbox shadowMapTexture))
(print "shadowMapTexture: " shadowMapTexture)

(define shadowMapSize 512)

(glBindTexture GL_TEXTURE_2D shadowMapTexture)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)

(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_COMPARE_MODE_ARB GL_COMPARE_R_TO_TEXTURE_ARB)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_COMPARE_FUNC_ARB GL_LEQUAL)

(glTexGeni GL_S GL_TEXTURE_GEN_MODE GL_EYE_LINEAR)
(glTexGeni GL_T GL_TEXTURE_GEN_MODE GL_EYE_LINEAR)
(glTexGeni GL_R GL_TEXTURE_GEN_MODE GL_EYE_LINEAR)
(glTexGeni GL_Q GL_TEXTURE_GEN_MODE GL_EYE_LINEAR)

(glTexImage2D GL_TEXTURE_2D 0 GL_DEPTH_COMPONENT shadowMapSize shadowMapSize 0
   GL_DEPTH_COMPONENT GL_UNSIGNED_BYTE NULL)

(define (DrawScene)
   ; floor
   (glColor3f 0 1 0)
   (glBegin GL_QUADS)
      (glNormal3f 0 1 0)
      (glVertex3f  5 0 -5)
      (glVertex3f -5 0 -5)
      (glVertex3f -5 0  5)
      (glVertex3f  5 0  5)
   (glEnd)

   ; show model
   (for-each (lambda (o)
         (let ((diffuse-color (get (cdr o) 'kd #f)))
            (glColor3fv diffuse-color))
         (glCallList (car o)))
      obj)
)

(define (skip l n) ; util
   (unless (zero? n)
      (skip (cdr l) (- n 1))
      l))

(define mv (make-mat4x4))
(define pr (make-mat4x4))

; draw
(gl:set-renderer (lambda ()
   (let*((ss ms (clock))
         (x (* 3 (sin (+ ss (/ ms 1000)))))
         (y (+ 8 (* 1 (sin (/ (+ ss (/ ms 1000)) 8)))))
         (z (* 3 (cos (+ ss (/ ms 1000))))))

   ; First pass - from light's point of view
   ;#|
   ; (render-to-shadow-map):
   ; //Use viewport the same size as the shadow map
   (glPushAttrib GL_VIEWPORT_BIT)
   (glViewport 0 0 shadowMapSize shadowMapSize)
   (glClear GL_DEPTH_BUFFER_BIT)

   (glColorMask GL_FALSE GL_FALSE GL_FALSE GL_FALSE)

   (glEnable GL_POLYGON_OFFSET_FILL)
   (glPolygonOffset 4 4)

   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity)
   (gluPerspective 90.0 1 1 30)
   (gluLookAt x y z ; lightPosition
              0 0 0
              0 1 0)

   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)

   (glGetFloatv GL_MODELVIEW_MATRIX mv)
   (glGetFloatv GL_PROJECTION_MATRIX pr)
 
   ; //Draw the scene
   (glDisable GL_TEXTURE_2D)
   (glDisable GL_LIGHTING)
   (DrawScene)
 
   ; //Read the depth buffer into the shadow map texture
   (glBindTexture GL_TEXTURE_2D shadowMapTexture)
   (glCopyTexImage2D GL_TEXTURE_2D 0 GL_DEPTH_COMPONENT 0 0 shadowMapSize shadowMapSize 0)
 
   ; //restore states
   (glDisable GL_POLYGON_OFFSET_FILL)
   (glColorMask GL_TRUE GL_TRUE GL_TRUE GL_TRUE)
   (glPopAttrib)
   #|

   ; DEBUG:
   (glClear GL_DEPTH_BUFFER_BIT)
   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity)
   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)

   (glEnable GL_TEXTURE_2D)
   (glBindTexture GL_TEXTURE_2D shadowMapTexture)

   (glColor3f 1 1 1)
   (glBegin GL_QUADS)
      (glTexCoord2f  0  0)
      (glVertex2f   -1 -1)
      (glTexCoord2f  1  0)
      (glVertex2f    1 -1)
      (glTexCoord2f  1  1)
      (glVertex2f    1  1)
      (glTexCoord2f  0  1)
      (glVertex2f   -1  1)
   (glEnd)
   ; end of DEBUG
   #||#

   ; let's start draw scene with shadows:
   (glClear GL_DEPTH_BUFFER_BIT)
   (glClear GL_COLOR_BUFFER_BIT)

   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity)
   (gluPerspective 45.0 (/ 854 480) 1 60)

   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (gluLookAt 6 9 6 ;cameraPosition
            1 3 0 ; 1 3 0
            0 1 0)

   ;#|
   ;(glActiveTextureARB GL_TEXTURE1_ARB)
   (glBindTexture GL_TEXTURE_2D shadowMapTexture)

   (glEnable GL_TEXTURE_2D)
   (glEnable GL_TEXTURE_GEN_S)
   (glEnable GL_TEXTURE_GEN_T)
   (glEnable GL_TEXTURE_GEN_R)
   (glEnable GL_TEXTURE_GEN_Q)

   (glTexGenfv GL_S GL_EYE_PLANE '(1 0 0 0))
   (glTexGenfv GL_T GL_EYE_PLANE '(0 1 0 0))
   (glTexGenfv GL_R GL_EYE_PLANE '(0 0 1 0))
   (glTexGenfv GL_Q GL_EYE_PLANE '(0 0 0 1))

   (glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_MODULATE)

   (glMatrixMode GL_TEXTURE)
   (glPushMatrix)

   (glLoadIdentity)
   (glTranslatef 0.5 0.5 0.5)
   (glScalef 0.5 0.5 0.5)
   (glMultMatrixf pr)
   (glMultMatrixf mv)

   ;(glActiveTextureARB GL_TEXTURE_0_ARB)

   ; draw
   ;#|
   ;(glDisable GL_TEXTURE_2D)
   (glEnable GL_LIGHTING)
   (glLightfv GL_LIGHT0 GL_POSITION (list x y z 1))

   (DrawScene)

   (glMatrixMode GL_TEXTURE)
   (glPopMatrix)

   ; draw the point
   (glDisable GL_TEXTURE_2D)
   (glDisable GL_LIGHTING)
   (glPointSize 5)
   (glBegin GL_POINTS)
   (glColor3f #xff/255 #xbf/255 0)
   (glVertex3f x y z)
   (glEnd)

   #||#

)))