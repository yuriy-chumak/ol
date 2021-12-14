(define-library (scene)
(import
   (otus lisp)
   (owl parse)
   (file wavefront obj)
   (file wavefront mtl)
   (otus blobs)
   (OpenGL version-2-1))

(export
   file->string
   prepare-models
   draw-geometry)
(begin

(define resources "resources/Ultimate Modular Sci-Fi - Feb 2021/OBJ/")
(define filenames (list ; a list of used models from resources folder
   "Column_1" "Column_2" "Column_3" "Column_Slim"
   ; floor
   "FloorTile_Basic" "FloorTile_Basic2" "FloorTile_Corner" "FloorTile_Double_Hallway" "FloorTile_Empty"
   "FloorTile_InnerCorner" "FloorTile_Side"
   ; walls
   "Wall_1" "Wall_2" "Wall_3" "Wall_4" "Wall_5" "Wall_Empty"
   "Window_Wall_SideA" "Window_Wall_SideB"
   "ThreeWindows_Wall_SideA" "ThreeWindows_Wall_SideB"
   "LongWindow_Wall_SideA" "LongWindow_Wall_SideB"
   ; walls with door
   "DoorDouble_Wall_SideA" "DoorDouble_Wall_SideB"
   "DoorSingle_Wall_SideA" "DoorSingle_Wall_SideB"
   "DoorDoubleLong_Wall_SideA"
   "DoorSingleLong_Wall_SideA"
   ; stairs
   "Staircase"
   ; ..
   "Props_Base" "Props_Crate"
   "Props_Teleporter_1"
   "Props_Shelf" "Props_Shelf_Tall"
   "Props_Computer" "Props_ComputerSmall"
   "Pipes"))

; загрузить нужные модели
(define (prepare-models filename)
   (fold (lambda (models model)
      (vector-apply model
         (lambda (mtl objects vertices normals)
            ; materials
            (define materials
               (fold (lambda (ff material)
                        (put ff (string->symbol (ref material 1)) material))
                  {}
                  mtl))
            
            ; put all objects into dictionary
            (fold (lambda (ff o)
                     (define name (ref o 1))
                     (define facegroups (ref o 2))
                     (define index (glGenLists (length facegroups)))

                     (put ff (string->symbol name)
                        (reverse
                        (fold (lambda (o group i)
                                 (let*((mtl (car group))
                                       (material (materials (string->symbol mtl))))
                                    (glNewList i GL_COMPILE)
                                    (glBegin GL_TRIANGLES)

                                    ; https://compgraphics.info/OpenGL/lighting/materials.php
                                    ;(glMaterialfv GL_FRONT_AND_BACK GL_AMBIENT_AND_DIFFUSE (ref material 2)) ; diffuse
                                    (glColor4fv (ref material 2))
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
               (define normals (list->vector (obj 'vn '(1 2 3))))

               (cons [
                  (map (lambda (material) ; materials
                        (vector
                           (material 'name)
                           (material 'kd)))
                     mtl)
                  ; objects
                  (map (lambda (object)
                        (vector
                           (object 'name)
                           (object 'facegroups)))
                     (obj 'o))
                  vertices normals ]
                  models))
            '()
            filenames)))
         (fasl-save models filename)
         models))))

;; (define (prepare-models . filenames)
;; (fold (lambda (models filename)
;;    (print "1")
;;          (define obj-filename (string-append resources filename ".obj"))
;;          (print "Loading object file " obj-filename "...")
;;          (define obj (parse wavefront-obj-parser (file->bytestream obj-filename) obj-filename #t #empty))
;;          ; Load a materials
;;          (define mtl-filename (string-append resources (obj 'mtllib "")))
;;          (print "Loading materials file " mtl-filename "...")
;;          (define mtl (parse wavefront-mtl-parser (file->bytestream mtl-filename) mtl-filename #t #empty))

;;          ; materials
;;          (define materials
;;             (fold (lambda (ff material)
;;                      (print "  Found material " (material 'name))
;;                      (put ff (string->symbol (material 'name)) material))
;;                {}
;;                mtl))
         
;;          ; precompile
;;          (define vertices (list->vector (obj 'v #null)))
;;          (define normals (list->vector (obj 'vn '(1 2 3))))

;;          ; put all objects into dictionary
;;          (fold (lambda (ff o)
;;                   (define name (o 'name))
;;                   (define facegroups (o 'facegroups {}))
;;                   (define index (glGenLists (length facegroups)))

;;                   (print "  Found object " name)
;;                   (put ff (string->symbol name)
;;                      (reverse
;;                      (fold (lambda (o group i)
;;                               (let*((mtl (car group))
;;                                     (material (materials (string->symbol mtl))))
;;                                  (glNewList i GL_COMPILE)
;;                                  (glBegin GL_TRIANGLES)

;;                                  ; https://compgraphics.info/OpenGL/lighting/materials.php
;;                                  (glMaterialfv GL_FRONT_AND_BACK GL_AMBIENT_AND_DIFFUSE (material 'kd [0.8 0.8 0.8 1]))
;;                                  (for-each (lambda (face)
;;                                        (let ((a (ref (ref face 1) 1)) ; vertices
;;                                              (b (ref (ref face 2) 1))
;;                                              (c (ref (ref face 3) 1))

;;                                              (r (ref (ref face 1) 3)) ; normals
;;                                              (s (ref (ref face 2) 3))
;;                                              (t (ref (ref face 3) 3)))
;;                                           (glNormal3fv (ref normals r))
;;                                           (glVertex3fv (ref vertices a))
;;                                           (glNormal3fv (ref normals s))
;;                                           (glVertex3fv (ref vertices b))
;;                                           (glNormal3fv (ref normals t))
;;                                           (glVertex3fv (ref vertices c)) ))
;;                                     (cdr group))
;;                                  (glEnd)
;;                                  (glEndList)
;;                                  (cons i o)))
;;                            #null
;;                            facegroups
;;                            (iota (length facegroups) index)))))

;;             models
;;             (obj 'o)))
            
;;    {}
;;    filenames))

(define (draw-geometry scene models)
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
            (glRotatef (ref ypr 1) 1 0 0)
            (glRotatef (ref ypr 2) 0 1 0)
            (glRotatef (ref ypr 3) 0 0 1))
         ;; (glGetFloatv GL_TEXTURE_MATRIX my_WorldMatrix)
         ;; (glUniformMatrix4fv my_WorldMatrixLocation 1 GL_FALSE my_WorldMatrix)

         ; for each shadow:
         ;(glLoadIdentity)
         ;; (let ((xyz (entity 'location)))
         ;;    (glTranslatef (ref xyz 1) (ref xyz 2) (ref xyz 3)))
         ;; (let ((ypr (entity 'rotation))) ; blender rotation mode is "XYZ": yaw, pitch, roll (рыскание, тангаж, крен)
         ;;    (glRotatef (ref ypr 1) 1 0 0)
         ;;    (glRotatef (ref ypr 2) 0 1 0)
         ;;    (glRotatef (ref ypr 3) 0 0 1))

         ;; ; ...
         ;; (glMatrixMode GL_MODELVIEW)
         ;; (glPushMatrix)

         ;; (let ((xyz (entity 'location)))
         ;;    (glTranslatef (ref xyz 1) (ref xyz 2) (ref xyz 3)))
         ;; (let ((ypr (entity 'rotation))) ; blender rotation mode is "XYZ": yaw, pitch, roll (рыскание, тангаж, крен)
         ;;    (glRotatef (ref ypr 1) 1 0 0)
         ;;    (glRotatef (ref ypr 2) 0 1 0)
         ;;    (glRotatef (ref ypr 3) 0 0 1))

         ;; ; special case - rotate a blades of
         ;; (define (starts-with? string prefix)
         ;;    (and (<= (string-length prefix) (string-length string))
         ;;          (string-eq? prefix (substring string 0 (string-length prefix)))))
         ;; (when (starts-with? model "Mill_Blades_Cube.")
         ;;    (glTranslatef 0 0 +3.1247)
         ;;    (let*((ss ms (uncons (syscall 96) #f))
         ;;          (r (/ (mod (floor (* (+ ss (/ ms 1000000)) 700)) 36000) 100)))
         ;;       (glRotatef r 0 1 0))
         ;;    (glTranslatef 0 0 -3.1247))

         (for-each glCallList
            (models (string->symbol model)))
         (glPopMatrix))
      (scene 'Objects)))

(define (file->string path)
   (bytes->string
      (blob-iter
         (let ((vec (file->bytevector path)))
            (if vec vec
               (error "Unable to load: " path))))))

))
