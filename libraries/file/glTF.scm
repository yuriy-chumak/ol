(define-library (file glTF)
   (export
      read-glTF-file

      ARRAY_BUFFER
      ELEMENT_ARRAY_BUFFER
   )
   (import
      (scheme base)
      (file json)
      (owl ff) (owl string)
      (lib soil) ; image loading library
      (OpenGL 3.0))

(begin
   (import (owl io)) ;temp
   (define (print-array title array) ;temp
      (print title)
      (vector-for-each (lambda (a)
            (print "   " a))
         array))

   (import (prefix (otus base64) base64:))
   (setq vref vector-ref)

   ; read and compile glTF file
   (define (read-glTF-file filename)
      (define glTF (read-json-file "scene.gltf"))

      ; binary buffers
      (define buffers (vector-map (lambda (buffer)
            (define source (buffer 'uri))
            ; todo: assert (output size == "byteLength") ?
            (cond
               ((m/^data:application\/octet-stream;base64,/ source)
                  (make-bytevector (base64:decode (substring source 37))))
               (else
                  (file->bytevector source))))
         (glTF 'buffers)))

      ; vertex buffer objects
      (define bufferViews (vector-map (lambda (bufferView)
            (define VBO (box 0))
            (glGenBuffers 1 VBO)
            (glBindBuffer (bufferView 'target) (unbox VBO))

            (define buffer (vref buffers (bufferView 'buffer)))
            (define offset (bufferView 'byteOffset 0))
            (glBufferData (bufferView 'target) (bufferView 'byteLength)
               (cons type-vptr (cons buffer offset)) ; trick: &buffer[offset]
               GL_STATIC_DRAW)

            (glBindBuffer (bufferView 'target) 0)
            (put bufferView 'vbo (unbox VBO)))
         (glTF 'bufferViews)))
      ;(print-array "bufferViews:" bufferViews)

      ; accessors
      (define accessors (glTF 'accessors))
      (define (accessor-type->GL at)
         (cond
            ((string-eq? at "SCALAR") 1)
            ((string-eq? at "VEC2") 2)
            ((string-eq? at "VEC3") 3)
            ((string-eq? at "VEC4") 4)
            ((string-eq? at "MAT2") 4)
            ((string-eq? at "MAT3") 9)
            ((string-eq? at "MAT4") 16) ))
      ;(print-array "accessors:" accessors)

      ; images
      (define images (vector-map (lambda (image)
            (put image 'texture
               ; todo: support base64
               (SOIL_load_OGL_texture (image 'uri) SOIL_LOAD_RGBA SOIL_CREATE_NEW_ID 0)))
               ; todo: glBindTexture + glTexParameteri...
         (glTF 'images)))

      ; materials
      ; todo: put id into material
      (define materials (vector-map (lambda (material id)
            (define alphaMode (material 'alphaMode #f))
            (put (if alphaMode
                    (put material 'alphaMode (string->symbol alphaMode))
                    material) 'id id))
         (glTF 'materials)
         (make-vector (iota (size (glTF 'materials))))))


      ; meshes (vertex array objects for every primitive)
      (define meshes (vector-map (lambda (mesh)
            (put mesh 'primitives
            (vector-map (lambda (primitive)
                  (define VAO '(0))
                  (glGenVertexArrays 1 VAO)
                  (glBindVertexArray (unbox VAO))

                  (define attributes (primitive 'attributes))
                  ; NVidia's built-in vertex attribute names:
                  ; 0 gl_Vertex
                  ;   1 vertex weights 0..3 (WeightARB, VertexWeightEXT)
                  ; 2 gl_Normal
                  ; 3 gl_Color
                  ; 4 gl_SecondaryColor
                  ; 5 gl_FogCoord
                  ; 8 gl_MultiTexCoord0 .. 15 gl_MultiTexCoord7

                  ; Vertex Position
                  (let ((position (attributes 'POSITION #f))) ;VEC3
                  (when position
                     (define accessor (vref accessors position))
                     (define bufferView (vref bufferViews (accessor 'bufferView)))
                     (define gl_Position 0) ; todo: glGetAttribLocation(shaderProgram, "position")

                     (glBindBuffer GL_ARRAY_BUFFER (bufferView 'vbo))
                     (glVertexAttribPointer gl_Position
                        ; number of components per vertex
                        (accessor-type->GL (accessor 'type))
                        ; data type of each component
                        (accessor 'componentType) ; direct mapped to GL_FLOAT etc. constants
                        1 ;(accessor 'normalized 0) ; direct mapping to GL_TRUE..GL_FALSE
                        ; stride & offset
                        (bufferView 'byteStride 0)
                        (accessor 'byteOffset 0))
                     (glEnableVertexAttribArray gl_Position)
                  ))

                  ; Vertex Normal
                  (let ((normal (attributes 'NORMAL #f)))
                  (when normal
                     (define accessor (vref accessors normal))
                     (define bufferView (vref bufferViews (accessor 'bufferView)))
                     (define gl_Normal 2) ; todo: glGetAttribLocation(shaderProgram, "normal")

                     (glBindBuffer GL_ARRAY_BUFFER (bufferView 'vbo))
                     (glVertexAttribPointer gl_Normal
                        ; number of components per vertex
                        (accessor-type->GL (accessor 'type))
                        ; data type of each component
                        (accessor 'componentType) ; direct mapped to GL_FLOAT etc. constants
                        (accessor 'normalized #f) ; direct mapping to GL_TRUE..GL_FALSE
                        ; stride & offset
                        (bufferView 'byteStride 0)
                        (accessor 'byteOffset 0))
                     (glEnableVertexAttribArray gl_Normal)
                  ))
                  ; todo: TANGENT, .. TEXCOORD_n, COLOR_0 .. COLOR_n, JOINTS.., WEIGHTS..

                  ; Texture Coordinates
                  (let ((texcoord (attributes 'TEXCOORD_0 #f)))
                  (when texcoord
                     (define accessor (vref accessors texcoord))
                     (define bufferView (vref bufferViews (accessor 'bufferView)))
                     (define gl_MultiTexCoord0 8)

                     (glBindBuffer GL_ARRAY_BUFFER (bufferView 'vbo))
                     ;; (glTexCoordPointer
                     ;;    (accessor-type->GL (accessor 'type))
                     ;;    (accessor 'componentType) ; direct mapped to GL_FLOAT etc. constants
                     ;;    (accessor 'normalized #f) ; direct mapping to GL_TRUE..GL_FALSE
                     ;;    (accessor 'byteOffset 0))
                     (glVertexAttribPointer gl_MultiTexCoord0
                        ; number of components per vertex
                        (accessor-type->GL (accessor 'type))
                        ; data type of each component
                        (accessor 'componentType)
                        (accessor 'normalized #f)
                        ; stride & offset
                        (bufferView 'byteStride 0)
                        (accessor 'byteOffset 0))
                     (glEnableVertexAttribArray gl_MultiTexCoord0)))

                  (glBindVertexArray 0)
                  (put primitive 'vao (unbox VAO)))
               (mesh 'primitives))))
         (glTF 'meshes)))
      ;(print-array "meshes:" meshes)

      (ff-replace glTF {
         'buffers #false ; free buffer data, reduce memory usage
         'bufferViews bufferViews
         'meshes meshes
         'images images
         'materials materials
      }))

   (define ARRAY_BUFFER 34962)
   (define ELEMENT_ARRAY_BUFFER 34963)
))
