#!/usr/bin/env ol

(import (lib gl-2))
(import (otus ffi))
(gl:set-window-title "3. Drawing model from file")

(import (lib math))

; load the model
(import (owl parse))
(import (file wavefront obj))
(import (file wavefront mtl))

(define (syntax-fail pos info lst)
   (list 'fail info '()))

(define obj-filename "obj/Palm_Tree.obj")
(define object (parse wavefront-obj-parser (file->list obj-filename) obj-filename #true #empty))
   
(define mtl-filename (string-append "obj/" (get object 'mtllib "")))
(define materials (parse wavefront-mtl-parser (file->list mtl-filename) mtl-filename #t #empty))

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

(glShadeModel GL_SMOOTH)
(glClearColor 0.3 0.3 0.3 1)

(glMatrixMode GL_PROJECTION)
(glLoadIdentity)
(gluPerspective 45 (/ 854 480) 0.1 100)

(glEnable GL_DEPTH_TEST)

(glEnable GL_LIGHTING)
(glLightModelf GL_LIGHT_MODEL_TWO_SIDE GL_TRUE)
(glEnable GL_NORMALIZE)

(glEnable GL_LIGHT0)

(glEnable GL_LIGHTING)
(glLightfv GL_LIGHT0 GL_POSITION '(7.0 7.0 7.0 0.0))

; draw
(gl:set-renderer (lambda (mouse)
   (glClear (vm:ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (gluLookAt 6 9 6
      1 3 0
      0 1 0)

   ; show lighting point
   (glDisable GL_LIGHTING)
   (let*((ss ms (clock))
         (x (* 3 (sin (+ ss (/ ms 1000)))))
         (y (+ 4 (* 3 (sin (/ (+ ss (/ ms 1000)) 8)))))
         (z (* 3 (cos (+ ss (/ ms 1000))))))
      (glPointSize 5)
      (glBegin GL_POINTS)
      (glColor3f #xff/255 #xbf/255 0)
      (glVertex3f x y z)
      (glEnd)
      
      (glLightfv GL_LIGHT0 GL_POSITION (list x y z 1)))
      (glEnable GL_LIGHTING)
      

   ; show model
   (for-each (lambda (o)
         (let ((diffuse-color (get (cdr o) 'kd #f)))
            (glLightfv GL_LIGHT0 GL_DIFFUSE diffuse-color))
         (glCallList (car o)))
      obj)))