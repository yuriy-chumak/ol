(define-library (scene)
(import
   (otus lisp)
   (owl parse)
   (file wavefront obj)
   (file wavefront mtl)
   (otus blobs)
   (OpenGL version-2-1)
   (lib soil))

(export
   file->string
   prepare-models
   draw-geometry
   draw-lightbulbs)
(begin

   (define resources "resources/Models/FurniturePack/OBJ/")
   (define filenames (list ; a list of used models from resources folder
      ; bridges
      "deskCorner"
      "floorFull" "paneling"
      "wall"

      ; workplace
      "laptop" "computerScreen"
      "computerKeyboard" "computerMouse"
      "chairDesk"
      "tableCross" "tableCoffee"

      "cabinetTelevision" "cabinetTelevisionDoors"
      "kitchenCabinetUpperCorner"

      "bedDouble" "bathroomMirror"
      "ceilingFan"

   ))

   ; загрузить нужные модели
   (define (prepare-models filename)
      (fold (lambda (models model)
         (vector-apply model
            (lambda (mtl objects vertices normals texcoords)
               ; materials
               (define materials
                  (fold (lambda (ff material)
                           (put ff (string->symbol (ref material 1)) material))
                     {}
                     (map (lambda (material)
                              (define map_kd (ref material 3))
                              (vector
                                 (ref material 1)
                                 (ref material 2)
                                 (if map_kd (SOIL_load_OGL_texture map_kd SOIL_LOAD_RGBA SOIL_CREATE_NEW_ID 0))))
                        mtl)))

               (glActiveTexture GL_TEXTURE0) ; reset texture unit

               ; compile and put all objects into dictionary
               (fold (lambda (ff o)
                        (define name (ref o 1))
                        (define facegroups (ref o 2))
                        (define index (glGenLists (length facegroups)))

                        (print "compiling model " name "...")

                        (put ff (string->symbol name)
                           (reverse
                           (fold (lambda (o group i)
                                    (let*((mtl (car group))
                                          (material (materials (string->symbol mtl))))
                                       (glNewList i GL_COMPILE)

                                       ; https://compgraphics.info/OpenGL/lighting/materials.php
                                       ;(glMaterialfv GL_FRONT_AND_BACK GL_AMBIENT_AND_DIFFUSE (ref material 2)) ; diffuse
                                       (glColor4fv (ref material 2))
                                       (glBindTexture GL_TEXTURE_2D (ref material 3))

                                       (glBegin GL_TRIANGLES)
                                       (for-each (lambda (face)
                                             (for-each (lambda (face)
                                                   (vector-apply face (lambda (xyz uv n)
                                                      ; wavefront obj uv is [-1..+1], opengl uv - [0 .. +1]
                                                      (glTexCoord2fv (vector-map (lambda (x) (/ (+ x 1) 2))
                                                                        (ref texcoords uv)))
                                                      (glNormal3fv (ref normals n))
                                                      (glVertex3fv (ref vertices xyz)) )) )
                                                face))
                                          (cdr group))
                                       (glEnd)
                                       (glEndList)
                                       (cons i o)))
                                 #null
                                 facegroups
                                 (iota (length facegroups) index)))))
                  models objects))))
      {}
      (or ;load a precompiled models file or compile new one
         (fasl-load filename #false)
         (let ((models (fold (lambda (models filename)
                  (define obj-filename (string-append resources filename ".obj"))
                  (print "Loading object file " obj-filename "...")
                  (define obj (parse wavefront-obj-parser (file->bytestream obj-filename) obj-filename #t #empty))
                  ; Load a materials
                  (define mtl-filename (string-append resources (obj 'mtllib "")))
                  (print "Loading materials file " mtl-filename "...")
                  (define mtl (parse wavefront-mtl-parser (file->bytestream mtl-filename) mtl-filename #t #empty))

                  ; precompile
                  (define vertices (list->vector (obj 'v #null)))
                  (define normals (list->vector (obj 'vn #null)))
                  (define texcoords (list->vector (obj 'vt #null)))

                  (cons [
                        (map (lambda (material) ; materials
                              (vector
                                 (material 'name)
                                 (material 'kd)
                                 (material 'map_kd)))
                           mtl)
                        ; objects
                        (map (lambda (object)
                              (vector
                                 (object 'name)
                                 (object 'facegroups)))
                           (obj 'o))
                        vertices
                        normals
                        texcoords ]
                     models))
               '()
               filenames)))
            (fasl-save models filename)
            models))))

   (define (compile-model-geometry model)
      #f
   )

   (define (compile-model-textured model)
      #f
   )

   ; ----------------------------
   (define quadric (gluNewQuadric))

   (define (draw-geometry objects models)
      ;; (define program '(0))
      ;; (glGetIntegerv GL_CURRENT_PROGRAM program)
      ;; (define my_WorldMatrix (map inexact (iota 16)))
      ;; (define my_WorldMatrixLocation (glGetUniformLocation (car program) "my_WorldMatrix"))

      (for-each (lambda (entity)
            (define model (entity 'model))

            (glActiveTexture GL_TEXTURE7) ; temporary buffer for matrix math
            (glMatrixMode GL_TEXTURE)
            (glLoadIdentity) ; let's prepare my_WorldMatrix
            (let ((xyz (entity 'location)))
               (glTranslatef (ref xyz 1) (ref xyz 2) (ref xyz 3)))
            (let ((ypr (entity 'rotation))) ; blender rotation mode is "XYZ": yaw, pitch, roll (рыскание, тангаж, крен)
               (glRotatef (ref ypr 3) 0 0 1)
               (glRotatef (ref ypr 2) 0 1 0)
               (glRotatef (ref ypr 1) 1 0 0))
            (for-each glCallList
               (models (string->symbol model))))
         objects))

   ; Draw a light bulbs
   (define (draw-lightbulbs Lights)
      (glUseProgram 0)

      (glMatrixMode GL_MODELVIEW)
      (for-each (lambda (light i)
            ; show only "point" light sources
            (when (eq? (ref (light 'position) 4) 1)
               (glColor3fv (light 'color))
               (glPushMatrix)
               (glTranslatef (ref (light 'position) 1)
                           (ref (light 'position) 2)
                           (ref (light 'position) 3))
               (gluSphere quadric 0.2 16 8)
               (glPopMatrix)))
         Lights
         (iota (length Lights))))

   (define (file->string path)
      (define vec (file->bytestream path))
      (if vec (bytes->string vec)
      else
         (error "Unable to load: " path)))

))
