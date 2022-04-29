#!/usr/bin/env ol

; initialize OpenGL
(import (lib gl-2))
(gl:set-window-title "7.texturing.lisp")
(import (scheme dynamic-bindings))

; gl global init
(glShadeModel GL_SMOOTH)
(glEnable GL_DEPTH_TEST)

(glEnable GL_CULL_FACE); GL_BACK

; scene
(import (scene))

;(define textures (load-texures models))
(import (lib soil))

; load (and create if no one) a models cache
(define models (prepare-models "cache.bin"))
(define geometry (compile-triangles models))

; load a scene
(import (file json))
(define scene (read-json-file "scene.json"))

; scene lights
(define Lights (vector->list (scene 'Lights)))
;(print "Lights: " Lights)

; scene objects
(define Objects (vector->list (scene 'Objects)))
;(print "Objects: " Objects)

; rotating ceiling fan
(define (ceilingFan? entity) (string-eq? (entity 'name "") "ceilingFan"))
(define ceilingFan (make-parameter (car (keep ceilingFan? Objects))))
(define Objects (remove ceilingFan? Objects))

;; render buffer
(import (OpenGL EXT framebuffer_object))

(define TEXW 1024)
(define TEXH 1024)

; texture2d
(define texture '(0))
(glGenTextures (length texture) texture)
(print "texture: " texture)
(glBindTexture GL_TEXTURE_2D (car texture))
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
(glTexImage2D GL_TEXTURE_2D 0 GL_RGBA TEXW TEXH 0 GL_RGBA GL_UNSIGNED_BYTE 0)
(glBindTexture GL_TEXTURE_2D 0)

(define framebuffer '(0))
(glGenFramebuffers (length framebuffer) framebuffer)
(print "framebuffer: " framebuffer)
(glBindFramebuffer GL_FRAMEBUFFER (car framebuffer))
(glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D (car texture) 0)

; we have to create hardware depth buffer if we want to use a depth
(define depthrenderbuffer '(0))
(glGenRenderbuffers (length depthrenderbuffer) depthrenderbuffer)
(glBindRenderbuffer GL_RENDERBUFFER (car depthrenderbuffer))
(glRenderbufferStorage GL_RENDERBUFFER GL_DEPTH_COMPONENT TEXW TEXH)
(glFramebufferRenderbuffer GL_FRAMEBUFFER GL_DEPTH_ATTACHMENT GL_RENDERBUFFER (car depthrenderbuffer))
(glBindFramebuffer GL_FRAMEBUFFER 0)

; texture producer shader program
(define texturer (gl:create-program
"#version 120 // OpenGL 2.1
   #define gl_ModelMatrix gl_TextureMatrix[7] //project specific model matrix
   #define gl_WorldViewProjectionMatrix gl_ModelViewProjectionMatrix

   void main() {
      gl_Position = gl_WorldViewProjectionMatrix * gl_ModelMatrix * gl_Vertex;
      gl_FrontColor = gl_Color;
      gl_TexCoord[0] = gl_MultiTexCoord0;
   }"
"#version 120 // OpenGL 2.1
   uniform sampler2D tex0;
   void main() {
      gl_FragColor = gl_Color * texture2D(tex0, gl_TexCoord[0].st);
   }"))

; just apply texture program
(define draw-texture (gl:create-program
"#version 120 // OpenGL 2.1
   void main() {
      gl_Position = ftransform();
      gl_TexCoord[0] = gl_MultiTexCoord0;
   }"
"#version 120 // OpenGL 2.1
   uniform sampler2D tex0;
   void main() {
      gl_FragColor = texture2D(tex0, gl_TexCoord[0].st);
   }"))

; draw
(gl:set-renderer (lambda ()
   (glViewport 0 0 TEXW TEXH)
   (glBindFramebuffer GL_FRAMEBUFFER (car framebuffer))

   (glClearColor 0 0 0 1)
   (glClear (vm:ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   ;; rotate ceilingFan
   (rotate ceilingFan 0.3)

   (glUseProgram texturer)

   ; camera setup
   (begin
      (define Camera (ref (scene 'Cameras) 1))

      (glMatrixMode GL_PROJECTION)
      (glLoadIdentity)
      (gluPerspective
         (Camera 'angle)
         (/ (gl:get-window-width) (gl:get-window-height))
         (Camera 'clip_start) (Camera 'clip_end))

      (define target (vector->list (Camera 'target)))
      (define location (vector->list (Camera 'location)))
      (define up (vector->list [0 0 1]))

      (glMatrixMode GL_MODELVIEW)
      (glLoadIdentity)
      (apply gluLookAt (append location target up)))

   ; draw a geometry with colors
   (glActiveTexture GL_TEXTURE0) ; light matrix from the sun
   (glUniform1i (glGetUniformLocation texturer "tex0") 0)

   (draw-geometry (cons (ceilingFan) Objects) geometry)

   ; Draw a light bulbs
   (draw-lightbulbs Lights)

   ; Draw a result texture (normals)
   (glBindFramebuffer GL_FRAMEBUFFER 0)
   (glUseProgram draw-texture)

   (glViewport 0 0 (gl:get-window-width) (gl:get-window-height))
   (glClearColor 0 0 0 1)
   (glClear (vm:ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity)
   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (glOrtho 0 1 0 1 0 1)

   (glEnable GL_TEXTURE_2D)
   (glActiveTexture GL_TEXTURE0)
   (glBindTexture GL_TEXTURE_2D (car texture))

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
   (glEnd)

))
