#!/usr/bin/env ol

;; initialize OpenGL
(import (lib gl))
(gl:set-window-title "8. Lighting")
(import (OpenGL version-2-1))
; todo: splash screen

(import (scene))

; load (and create if no one) a models cache
(define models (prepare-models "cache.bin"))
(print "compiled models:\n" models)

;; load a scene
(import (file json))
(define scene (read-json-file "scene1.json"))

;; shaders
(define po (gl:CreateProgram
"#version 120 // OpenGL 2.1
   void main() {
      gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
   }"
"#version 120 // OpenGL 2.1
   void main() {
      // nothing to do
   }"))

;; -----------------------
; https://learnopengl.com/Getting-started/Coordinate-Systems
(define shadowed (gl:CreateProgram ; todo: add "#define" to the shaders aas part of language
   ;; (file->string "light.vs")
   ;; (file->string "light.fs")))
   (file->string "shaders/8.lighting.vs")
   (file->string "shaders/8.lighting.fs")))

(define justdraw (gl:CreateProgram
"#version 120 // OpenGL 2.1
   varying 
   void main() {
      gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
      gl_TexCoord[0] = gl_MultiTexCoord0;
   }"
"#version 120 // OpenGL 2.1
   uniform sampler2D shadow;
   void main() {
      gl_FragColor = texture2D(shadow, gl_TexCoord[0].st);
   }"))

;; render buffer
(import (OpenGL EXT framebuffer_object))

(define depth-map '(0))
(glGenTextures (length depth-map) depth-map)
(print "depth-map: " depth-map)
(glBindTexture GL_TEXTURE_2D (car depth-map))
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
(glTexImage2D GL_TEXTURE_2D 0 GL_DEPTH_COMPONENT 1024 1024 0 GL_DEPTH_COMPONENT GL_FLOAT 0)
(glBindTexture GL_TEXTURE_2D 0)

(define depth-fbo '(0))
(glGenFramebuffers (length depth-fbo) depth-fbo)
(print "depth-fbo: " depth-fbo)
(glBindFramebuffer GL_FRAMEBUFFER (car depth-fbo))
(glFramebufferTexture2D GL_FRAMEBUFFER GL_DEPTH_ATTACHMENT GL_TEXTURE_2D (car depth-map) 0)
(glDrawBuffer GL_NONE)
(glReadBuffer GL_NONE)
(glBindFramebuffer GL_FRAMEBUFFER 0)

;; draw

(import (lib math))
(import (owl math fp))

; настройки
(glShadeModel GL_SMOOTH)
(glClearColor 0.2 0.2 0.2 1)

(glEnable GL_DEPTH_TEST)

;(glEnable GL_NORMALIZE)
;; (glEnable GL_COLOR_MATERIAL)
;; (glColorMaterial GL_FRONT_AND_BACK GL_DIFFUSE)

;; (glLightModelf GL_LIGHT_MODEL_TWO_SIDE GL_TRUE)

;; освещение сцены
(glEnable GL_LIGHTING)

(define lights (vector->list (scene 'Lights)))
(print "lights: " lights)

(glLightModelfv GL_LIGHT_MODEL_AMBIENT '(0.1 0.1 0.1 1))
; set lights specular colors
(for-each (lambda (i)
      (glEnable (+ GL_LIGHT0 i))
      (glLightfv (+ GL_LIGHT0 i) GL_AMBIENT '(1.0 1.0 1.0 1))
      (glLightfv (+ GL_LIGHT0 i) GL_DIFFUSE '(1.0 1.0 1.0 1))
      (glLightfv (+ GL_LIGHT0 i) GL_SPECULAR '(1.0 1.0 1.0 1))
      ; GL_EMISSION
      ; GL_SHININESS
      ; 
      )
   (iota (length lights)))


; draw
(gl:set-renderer (lambda (mouse)
(let*((ss ms (clock))
      (ticks (/ (+ ss (/ ms 1000)) 2)))

   '(begin ; calculate the shadow texture
      (glViewport 0 0 1024 1024)
      (glBindFramebuffer GL_FRAMEBUFFER (car depth-fbo))
      (glClear GL_DEPTH_BUFFER_BIT)
      (glUseProgram po)

      (glMatrixMode GL_PROJECTION)
      (glLoadIdentity)
      ; lightSpaceMatrix is a (lightProjection * lightView) matrix
      (glOrtho -20 20 -20 20 0 50) ; gl_Projection is a light projection matrix
      ;(gluPerspective 45 1.0 0.1 100)
      (glMatrixMode GL_MODELVIEW)
      (glLoadIdentity)
      (vector-apply ((cadr lights) 'position)
         (lambda (x y z w)
            (gluLookAt x y z ; gl_ModelView is a light space matrix
               0 0 0
               0 0 1)))

      (glCullFace GL_FRONT)
      (draw-geometry scene models)
      (glCullFace GL_BACK)
      (glBindFramebuffer GL_FRAMEBUFFER 0))

   ;; let's draw a scene
   (glCullFace GL_BACK)
   (when #true
      (glViewport 0 0 (gl:get-window-width) (gl:get-window-height))
      (glClearColor 0 0 0 1)
      ;(glClear GL_DEPTH_BUFFER_BIT)
      (glClear (vm:ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

      (glUseProgram shadowed)

      ;; prepare shadow matrices (and define light positions)
      (glEnable GL_LIGHTING)
      (for-each (lambda (light i)
         (vector-apply (light 'position)
            (lambda (x y z w)
               ; let's generate light transform matrix into texture matrix
               ;; (glActiveTexture (+ GL_TEXTURE0 i))
               ;; (glMatrixMode GL_TEXTURE)
               ;; (glLoadIdentity)
               ;; (glOrtho -20 20 -20 20 0 50) ; gl_Projection is a light projection matrix
               ;; (gluLookAt x y z ; gl_ModelView is a light space matrix
               ;;    0 0 0
               ;;    0 0 1)

               ;(glEnable (+ GL_LIGHT0 i))
               (glLightfv (+ GL_LIGHT0 i) GL_POSITION (list x y z w)))))
         lights
         (iota (length lights)))

      ; Камера
      (define camera (ref (scene 'Cameras) 1))

      (glMatrixMode GL_PROJECTION)
      (glLoadIdentity)
      (gluPerspective (camera 'angle) (/ (gl:get-window-width) (gl:get-window-height)) 0.1 100) ; (camera 'clip_start) (camera 'clip_end)

      ; setup a camera
      (begin
         (define location (camera 'location))
         (define target (camera 'target))
         ;; (define location [0 21.2 15.6])
         ;; (define target [0 0 0])

         (glMatrixMode GL_MODELVIEW)
         (glLoadIdentity)
         (gluLookAt
            (ref location 1) (ref location 2) (ref location 3)
            (ref target 1) (ref target 2) (ref target 3)
            0 0 1))

      ;; setup shadow textures:
      ;; (glEnable GL_TEXTURE_2D)
      ;; (glActiveTexture GL_TEXTURE0)
      ;; (glBindTexture GL_TEXTURE_2D (car depth-fbo))
      ;; (glUniform1i (glGetUniformLocation shadowed "shadow") 0) ; 0-th texture unit

      (draw-geometry scene models))
      
      ;; (glUseProgram 0)
      ;; (glPointSize 5)
      ;; (glBegin GL_POINTS)
      ;;    (glColor3f #xff/255 #xbf/255 0)
      ;;    (glVertex3f x y z)
      ;; (glEnd)
      ;; (glBegin GL_LINES)
      ;;    (glVertex3f x y z)
      ;;    (glVertex3f x y 0)
      ;; (glEnd)

   (when #false
      (glViewport 0 0 (/ (gl:get-window-width) 2) (/ (gl:get-window-height) 2))
      (glClearColor 1 0 0 1)
      ;(glClear (vm:ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
      (glClear GL_DEPTH_BUFFER_BIT)

      ;; ; set and show lighting point
      ;; (glDisable GL_LIGHTING)
      ;; (let*((ss ms (clock))
      ;;       (x (- (* 7 (sin (+ ss (/ ms 1000)))) 3))
      ;;       (y (- (* 7 (cos (+ ss (/ ms 1000)))) 3))
      ;;       (z 5))
      ;;    (glPointSize 5)
      ;;    (glBegin GL_POINTS)
      ;;    (glColor3f #xff/255 #xbf/255 0)
      ;;    (glVertex3f x y z)
      ;;    (glEnd)
         
      ;;    (glLightfv GL_LIGHT0 GL_POSITION (list x y z w)))
      ;; (glEnable GL_LIGHTING)
      ;; (glDisable GL_LIGHTING)

      ;; draw a texture
      (glBindFramebuffer GL_FRAMEBUFFER 0)
      (glUseProgram justdraw)


      (glMatrixMode GL_PROJECTION)
      (glLoadIdentity)
      (glMatrixMode GL_MODELVIEW)
      (glLoadIdentity)
      (glOrtho 0 1 0 1 0 1)

      (glEnable GL_TEXTURE_2D)
      (glBindTexture GL_TEXTURE_2D (car depth-fbo))
      (glUniform1i (glGetUniformLocation justdraw "shadow") 0) ; 0-th texture unit

      (glBegin GL_QUADS)
         (glColor3f 1 1 1)

         (glTexCoord2f 0 0)
         (glVertex2f 0 0)
         (glTexCoord2f 1 0)
         (glVertex2f 1 0)
         (glTexCoord2f 1 1)
         (glVertex2f 1 1)
         (glTexCoord2f 0 1)
         (glVertex2f 0 1)
      (glEnd))

   (glClearColor 0 0 1 1)
)))
