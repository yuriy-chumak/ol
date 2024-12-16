(define-library (file glTF)
   (export
      read-glTF-file
      glTF-parser

      ARRAY_BUFFER
      ELEMENT_ARRAY_BUFFER
   )
   (import
      (scheme base)
      (file json)
      (owl ff) (owl string)
      (lib soil) ; image loading library
      (owl parse)
      (OpenGL 3.0)) ; todo: 2.1 and extesions

(begin
   (setq vref vector-ref)

   (import (owl io))
   ;; (define (print-array title array) ;temp
   ;;    (print title)
   ;;    (vector-for-each (lambda (a)
   ;;          (print "   " a))
   ;;       array))
   ;; (import (only (otus lisp) typename))

   (import (prefix (otus base64) base64:))

   (define skip32 (let-parse* (
         (b0 byte) (b1 byte) (b2 byte) (b3 byte))
      0))
   (define (viota . args)
      (list->vector (apply iota args)))

   (define glTF-parser
      (either
         ; binary glTF
         (let-parse* (
               ; 12-byte header
               (header (word "glTF" #t))
               (version skip32)
               (flen skip32)
               ; Chunk 0 (JSON)
               (clen skip32)
               (ctype (word "JSON" #t))
               (json json-parser)
               (spaces (greedy* (imm #\space)))
               ; Chunk 1 (BIN)
               (skips (lazy+ byte))
               (ctype (word "BIN\0" #t))
               (bin (greedy+ byte)) )
            ; replace buffers[0] with buffer data
            ; assert "(size buffers) == 1"
            (put json 'buffers (vector-map (lambda (buffer)
                  (if (buffer 'uri #f)
                     buffer
                     (put buffer 'buffer
                        (make-bytevector bin))))
               (json 'buffers))))
         ; text glTF
         json-parser))
         


   ; read and compile glTF file
   (define (read-glTF-file filename)
      (define glTF (parse glTF-parser (force (file->bytestream filename)) #f))

      ; binary buffers
      (define buffers (vector-map (lambda (buffer)
            (define source (buffer 'uri #f))
            (if source
               (cond
                  ((m/^data:application\/octet-stream;base64,/ source)
                     (make-bytevector (base64:decode (substring source 37))))
                  (else
                     (file->bytevector source)))
            else
               (buffer 'buffer)))
         (glTF 'buffers)))

      ; buffer objects (a subset of the buffer)
      ; https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#reference-bufferview
      (define bufferViews (vector-map (lambda (bufferView id)
            ; TODO: don't load image buffer data into vbo
            (define VBO (box 0))
            (glGenBuffers 1 VBO)

            (define target (or
               (bufferView 'target #f) ; we know the buffer purpose
               (call/cc (lambda (claim)
                  ; is this buffer will be index buffer?
                  (vector-for-each (lambda (mesh)
                        (vector-for-each (lambda (primitive)
                              (if (eq? (primitive 'indices #f) id)
                                 (claim GL_ELEMENT_ARRAY_BUFFER)))
                           (mesh 'primitives [])))
                     (glTF 'meshes))
                  ; todo: skip image buffer data
                  ; no, use as array buffer
                  GL_ARRAY_BUFFER))))

            (glBindBuffer target (unbox VBO))

            (define buffer (vref buffers (bufferView 'buffer)))
            (define offset (bufferView 'byteOffset 0))
            (glBufferData target (bufferView 'byteLength)
               (cons type-vptr (cons buffer offset)) ; is &buffer[offset] for fft-any
               GL_STATIC_DRAW)

            (glBindBuffer target 0)
            (put bufferView 'vbo (unbox VBO)))
         (glTF 'bufferViews)
         (list->vector (iota (size (glTF 'bufferViews)))) ))

      ; accessors
      ; https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#reference-accessor
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
      ;; (print-array "accessors:" accessors)

      ; images
      (define images (vector-map (lambda (image)
            (define uri (image 'uri #f))
            (define id
               (if uri
                  ; todo: support base64
                  (SOIL_load_OGL_texture uri
                     SOIL_LOAD_RGBA SOIL_CREATE_NEW_ID 0)
               else
                  (define bufferView (vref bufferViews (image 'bufferView)))
                  (define buffer (vref buffers (bufferView 'buffer)))
                  (define byteOffset (bufferView 'byteOffset))
                  (define byteLength (bufferView 'byteLength))
                  (SOIL_load_OGL_texture_from_memory
                     (cons buffer byteOffset) byteLength ; &buffer[byteOffset]
                     SOIL_LOAD_RGBA SOIL_CREATE_NEW_ID 0)))
            ; TODO: setup texture parameters
            ;; (glBindTexture GL_TEXTURE_2D id)
            ;; (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
            ;; (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT)
            ;; ;; (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
            ;; ;; (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
            ;; (glBindTexture GL_TEXTURE_2D 0)

            (put image 'texture id))

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
                        (accessor 'normalized 0) ; direct mapping to GL_TRUE..GL_FALSE
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

      (ff-replace glTF {
         'buffers #false ; free binary buffer data, reduce memory usage
         'bufferViews bufferViews
         'meshes meshes
         'images images
         'materials materials
      }))

   (define ARRAY_BUFFER 34962)
   (define ELEMENT_ARRAY_BUFFER 34963)
))
