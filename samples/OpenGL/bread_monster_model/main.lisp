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

(define nodes (glTF 'nodes))
(define meshes (glTF 'meshes))
(define bufferViews (glTF 'bufferViews))
(define accessors (glTF 'accessors))

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
(import (owl math fp))
(import (lib GLU))
(gl:set-renderer (lambda ()
   (glClear (bor GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity)
   (gluPerspective 45 (/ (gl:get-window-width) (gl:get-window-height)) 0.01 1000)

   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)

   (define x (/ (mod (time-ms) 62831) #i10000))
   (gluLookAt
      (* 30 (fsin x)) 2 (* 30 (fcos x))
      0 0 0
      0 1 0)

   (glUseProgram po)
   (glUniform1f (glGetUniformLocation po "time") (/ (mod (time-ms) 1000000) #i1000))
   (glUniform2f (glGetUniformLocation po "dimensions")
      (gl:get-window-width) (gl:get-window-height))

   (let walk ((i 0))
      (define node (vref nodes i))
      (glPushMatrix)
      (when (node 'matrix #f)
         (define m (node 'matrix))
         ;; (define mat [
         ;;    (vref m 0) (vref m 4) (vref m 8) (vref m 12)
         ;;    (vref m 1) (vref m 5) (vref m 9) (vref m 13)
         ;;    (vref m 2) (vref m 6) (vref m 10) (vref m 14)
         ;;    (vref m 3) (vref m 7) (vref m 11) (vref m 15)
         ;; ])
         (glMultMatrixf m))
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
                     (accessor 'byteOffset 0)))

               (glBindVertexArray 0))
            (mesh 'primitives [])))
      ; visit children
      (vector-for-each walk (node 'children []))
      (glPopMatrix))
))