#!/usr/bin/env ol
(setq vref vector-ref)
;; (setq fft-float* (fft* fft-float))

(import (lib gl-3))
(gl:set-window-title "glTF")

; init
(glShadeModel GL_SMOOTH)
(glClearColor 0.11 0.11 0.11 1)
(glEnable GL_DEPTH_TEST)

(import (file glTF))
(define glTF (read-glTF-file "scene.gltf"))

(define (print-array title array)
   (print title)
   (vector-for-each (lambda (a)
         (print "   " a))
      array))

; binary buffers
(import (prefix (otus base64) base64:))
(define buffers (vector-map (lambda (buffer)
      (define source (buffer 'uri))
      ; todo: assert (output size == "byteLength") ?
      (cond
         ((m/^data:application\/octet-stream;base64,/ source)
            (make-bytevector (base64:decode (substring source 37))))
         (else
            (file->bytevector source))))
   (glTF 'buffers)))
(print-array "buffers:" buffers)

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
(print-array "bufferViews:" bufferViews)
(define buffers #f) ; free buffer data, reduce memory usage

; accessors
(define accessors (glTF 'accessors))
(print-array "accessors:" accessors)

; meshes (vertex array objects for every primitive)
(define meshes (vector-map (lambda (mesh)
      (put mesh 'primitives
      (vector-map (lambda (primitive)
            (define VAO '(0))
            (glGenVertexArrays 1 VAO)
            (glBindVertexArray (unbox VAO))

            (define position ((primitive 'attributes) 'POSITION)) ;VEC3
            (when position
               (define accessor (vref accessors position))
               (define bufferView (vref bufferViews (accessor 'bufferView)))
               (define gl_Position 0) ; todo: glGetAttribLocation(shaderProgram, "position")

               (glBindBuffer GL_ARRAY_BUFFER (bufferView 'vbo))
               (glVertexAttribPointer gl_Position
                  ; number of components per vertex
                  (let ((atype (accessor 'type)))
                     (cond
                        ((string-eq? atype "SCALAR") 1)
                        ((string-eq? atype "VEC2") 2)
                        ((string-eq? atype "VEC3") 3)
                        ((string-eq? atype "VEC4") 4)
                        ;... MATRIX...
                     ))
                  ; data type of each component
                  (accessor 'componentType) ; direct mapped to GL_FLOAT etc. constants
                  (accessor 'normalized #f) ; direct mapping to GL_TRUE..GL_FALSE
                  ; stride & offset
                  (bufferView 'byteStride 0)
                  (accessor 'byteOffset 0))
               (glEnableVertexAttribArray gl_Position)) ; todo: get index from shader?
            ; todo: NORMAL, TANGENT, TEXCOORD_0 .. TEXCOORD_n, COLOR_0 .. COLOR_n, JOINTS.., WEIGHTS..

            (glBindVertexArray 0)
            (put primitive 'vao (unbox VAO)))
         (mesh 'primitives))))
   (glTF 'meshes)))
(print-array "meshes:" meshes)

;; ;; ; images
;; ;; (define images (vector-map (lambda (image)
;; ;;       (define source (image 'uri))
;; ;;       ; todo: load image using SOIL
;; ;;       #true)
;; ;;    (glTF 'images)))
;; ;; (print images)


(define nodes (glTF 'nodes))

; ---------------------------
(define po (gl:create-program
"#version 120 // OpenGL 2.1
	void main() {
		gl_Position = ftransform();
	}"
"#version 120 // OpenGL 2.1
   uniform float time;
   uniform vec2 dimensions;
   void main(void) {
      vec2  p = 7.*(2.*gl_FragCoord.xy-dimensions.xy)/dimensions.y;
      float m1 = sin(length(p)*0.3-time*0.3);
      float m2 = sin(0.3*(length(p)*0.3-time*0.3));
      float c1 = 0.012/abs(length(mod(p,2.0*m1)-m1)-0.3);
      float c2 = 0.012/abs(length(mod(p,2.0*m2)-m2)-0.3);
      gl_FragColor = vec4(vec3(1.,2.,8.)*c1+vec3(8.,2.,1.)*c2, 1.);
   }"))
(glUseProgram po)

; draw
(import (lib GLU))
(gl:set-renderer (lambda ()
   (glClear (bor GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   ;; (glMatrixMode GL_PROJECTION)
   ;; (glLoadIdentity)
   ;; (gluPerspective 45 (/ (gl:get-window-width) (gl:get-window-height)) 0.01 1000)

   ;; (glMatrixMode GL_MODELVIEW)
   ;; (glLoadIdentity)
   ;; (gluLookAt 0 0 0
   ;;    100 100 100
   ;;    0 1 0)

   (glUseProgram po)
   (glUniform1f (glGetUniformLocation po "time") (/ (mod (time-ms) 1000000) #i1000))
   (glUniform2f (glGetUniformLocation po "dimensions")
      (gl:get-window-width) (gl:get-window-height))

   (let walk ((i 0))
      (define node (vref nodes i))
      (glPushMatrix)
      ;glMultMatrix if has matrix
      (when (node 'mesh #f)
         (define mesh (vref meshes (node 'mesh)))
         (vector-for-each (lambda (primitive)
               (glBindVertexArray (primitive 'vao))

               (define indices (primitive 'indices #f))
               (when indices
                  (define accessor (vref accessors indices))
                  (define bufferView (vref bufferViews (accessor 'bufferView)))

                  (glBindBuffer GL_ELEMENT_ARRAY_BUFFER (bufferView 'vbo))
                  (glDrawElements
                     (case (mesh 'mode 4)
                        (0 GL_POINTS)
                        (1 GL_LINES)
                        (2 GL_LINE_LOOP)
                        (3 GL_LINE_STRIP)
                        (4 GL_TRIANGLES)
                        (5 GL_TRIANGLE_STRIP)
                        (6 GL_TRIANGLE_FAN))
                     (accessor 'count)
                     (accessor 'componentType)
                     ;; (case (accessor 'componentType)
                     ;;    (GL_BYTE  GL_UNSIGNED_BYTE)
                     ;;    (GL_UNSIGNED_BYTE  GL_UNSIGNED_BYTE)
                     ;;    ...
                     (accessor 'byteOffset 0)))

               (glBindVertexArray 0))
            (mesh 'primitives [])))
      ; visit children
      (vector-for-each walk (node 'children []))
      (glPopMatrix))
))