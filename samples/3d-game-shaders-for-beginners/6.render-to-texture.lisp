#!/usr/bin/env ol

;; initialize OpenGL
(import (lib gl2))
(gl:set-window-title "6.render-to-texture.lisp")

(import (scene))

; load (and create if no one) a models cache
(define models (prepare-models "cache.bin"))

;; load a scene
(import (file json))
(define scene (read-json-file "scene1.json"))

;; init
(glShadeModel GL_SMOOTH)
(glEnable GL_DEPTH_TEST)

;(glEnable GL_CULL_FACE)
;(glCullFace GL_BACK)

; create glsl shader program
(define normals (gl:create-program
"#version 120 // OpenGL 2.1
   #define gl_WorldMatrix gl_TextureMatrix[7]
   varying vec3 normal;
   void main() {
      gl_Position = gl_ModelViewProjectionMatrix * gl_WorldMatrix * gl_Vertex;
      normal = gl_Normal * 0.5 + vec3(0.5, 0.5, 0.5);
   }"
"#version 120 // OpenGL 2.1
   varying vec3 normal;
   void main() {
      gl_FragColor = vec4(normalize(normal), 1.0);
   }"))

(define draw-texture (gl:create-program
"#version 120 // OpenGL 2.1
   void main() {
      gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
      gl_TexCoord[0] = gl_MultiTexCoord0;
   }"
"#version 120 // OpenGL 2.1
   uniform sampler2D tex0;
   void main() {
      gl_FragColor = texture2D(tex0, gl_TexCoord[0].st);
   }"))


;; render buffer
(import (OpenGL EXT framebuffer_object))

(define framebuffer '(0))
(glGenFramebuffers (length framebuffer) framebuffer)
(print "framebuffer: " framebuffer)

(define texture '(0))
(glGenTextures (length texture) texture)
(print "texture: " texture)

(define TEXW 1024)
(define TEXH 1024)

(glBindTexture GL_TEXTURE_2D (car texture))
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
(glTexImage2D GL_TEXTURE_2D 0 GL_RGBA TEXW TEXH 0 GL_RGBA GL_UNSIGNED_BYTE 0)
(glBindTexture GL_TEXTURE_2D 0)

(glBindFramebuffer GL_FRAMEBUFFER (car framebuffer))
(glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D (car texture) 0)

; we have to create depth buffer if we want to use a depth
(define depthrenderbuffer '(0))
(glGenRenderbuffers (length depthrenderbuffer) depthrenderbuffer)
(glBindRenderbuffer GL_RENDERBUFFER (car depthrenderbuffer))
(glRenderbufferStorage GL_RENDERBUFFER GL_DEPTH_COMPONENT TEXW TEXH)
(glFramebufferRenderbuffer GL_FRAMEBUFFER GL_DEPTH_ATTACHMENT GL_RENDERBUFFER (car depthrenderbuffer))

; draw
(gl:set-renderer (lambda (mouse)
   (glViewport 0 0 TEXW TEXH)
   (glBindFramebuffer GL_FRAMEBUFFER (car framebuffer))

   (glClearColor 0 0 0 1)
   (glClear (vm:ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
   (glUseProgram normals)

   ; Camera setup
   (begin
      (define camera (ref (scene 'Cameras) 1))

      (define angle (camera 'angle))
      (define location (camera 'location))
      (define target (camera 'target))

      (glMatrixMode GL_PROJECTION)
      (glLoadIdentity)
      (gluPerspective angle (/ (gl:get-window-width) (gl:get-window-height)) 0.1 100) ; (camera 'clip_start) (camera 'clip_end)

      (glMatrixMode GL_MODELVIEW)
      (glLoadIdentity)
      (gluLookAt
         (ref location 1) (ref location 2) (ref location 3)
         (ref target 1) (ref target 2) (ref target 3)
         0 0 1))

   ; draw a geometry
   (draw-geometry scene models)

   ; draw a texture:
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
