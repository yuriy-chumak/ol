(define-library (scene)
(import
   (otus lisp)
   (owl parse)
   (file wavefront obj)
   (file wavefront mtl)
   (OpenGL version-2-1))

(export
   prepare-models
   draw-geometry)
(begin

(define resources "resources/Medieval Village Pack - Dec 2020/Buildings/OBJ/")

; загрузить нужные модели
(define (prepare-models . filenames)
(fold (lambda (models filename)
         (define obj-filename (string-append resources filename ".obj"))
         (print "Loading object file " obj-filename "...")
         (define obj (parse wavefront-obj-parser (file->bytestream obj-filename) obj-filename #t #empty))
         ; Load a materials
         (define mtl-filename (string-append resources (obj 'mtllib "")))
         (print "Loading materials file " mtl-filename "...")
         (define mtl (parse wavefront-mtl-parser (file->bytestream mtl-filename) mtl-filename #t #empty))

         ; materials
         (define materials
            (fold (lambda (ff material)
                     (print "  Found material " (material 'name))
                     (put ff (string->symbol (material 'name)) material))
               {}
               mtl))
         
         ; precompile
         (define vertices (list->vector (obj 'v #null)))
         (define normals (list->vector (obj 'vn '(1 2 3))))

         ; put all objects into dictionary
         (fold (lambda (ff o)
                  (define name (o 'name))
                  (define facegroups (o 'facegroups {}))
                  (define index (glGenLists (length facegroups)))

                  (print "  Found object " name)
                  (put ff (string->symbol name)
                     (reverse
                     (fold (lambda (o group i)
                              (let*((mtl (car group))
                                    (material (materials (string->symbol mtl))))
                                 (glNewList i GL_COMPILE)
                                 (glBegin GL_TRIANGLES)

                                 ; https://compgraphics.info/OpenGL/lighting/materials.php
                                 (glMaterialfv GL_FRONT_AND_BACK GL_AMBIENT_AND_DIFFUSE (material 'kd [0.8 0.8 0.8 1]))
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
                                          (glVertex3fv (ref vertices c)) ))
                                    (cdr group))
                                 (glEnd)
                                 (glEndList)
                                 (cons i o)))
                           #null
                           facegroups
                           (iota (length facegroups) index)))))

            models
            (obj 'o)))
            
   {}
   filenames))

(define (draw-geometry scene models)
   (for-each (lambda (entity)
         (define model (entity 'model))

         (glPushMatrix)
         ; special case - rotate a blades of
         (define (starts-with? string prefix)
            (and (<= (string-length prefix) (string-length string))
                  (string-eq? prefix (substring string 0 (string-length prefix)))))

         (let ((xyz (entity 'location)))
            (glTranslatef (ref xyz 1) (ref xyz 2) (ref xyz 3)))
         (let ((ypr (entity 'rotation))) ; blender rotation mode is "XYZ": yaw, pitch, roll (рыскание, тангаж, крен)
            (glRotatef (ref ypr 1) 1 0 0)
            (glRotatef (ref ypr 2) 0 1 0)
            (glRotatef (ref ypr 3) 0 0 1))

         (when (starts-with? model "Mill_Blades_Cube.")
            (glTranslatef 0 0 +3.1247)
            (let*((ss ms (uncons (syscall 96) #f))
                  (r (/ (mod (floor (* (+ ss (/ ms 1000000)) 700)) 36000) 100)))
               (glRotatef r 0 1 0))
            (glTranslatef 0 0 -3.1247))

         (for-each glCallList
            (models (string->symbol model)))
         (glPopMatrix))
      scene))

))