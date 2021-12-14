#!/usr/bin/env ol

;; initialize OpenGL
(import (lib gl))
(gl:set-window-title "6. render-to-texture")
(import (OpenGL version-2-1))
; todo: splash screen

(import (scene))

; load (and create if no one) a models cache
(define models (prepare-models "cache.bin"))
(print "compiled models:\n" models)

;; load a scene
(import (file json))
(define scene (read-json-file "scene1.json"))

(define normals (gl:CreateProgram
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

;; let's find a sun
(define sun (car (filter (lambda (light) (string-ci=? (light 'type) "SUN"))
   (vector->list (scene 'Lights)))))
(print "sun:" sun)

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

; init
(glShadeModel GL_SMOOTH)
(glEnable GL_DEPTH_TEST)

; draw
(gl:set-renderer (lambda (mouse)
   (glViewport 0 0 TEXW TEXH)
   (glBindFramebuffer GL_FRAMEBUFFER (car framebuffer))

   (glClearColor 0 0 0 1)
   (glClear (vm:ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
   (glUseProgram normals)

   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity)
   (glOrtho -20 20 -20 20 -20 20)

   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (gluLookAt
      (ref (sun 'position) 1) ; x
      (ref (sun 'position) 2) ; y
      (ref (sun 'position) 3) ; z
      0 0 0 ; sun is directed light
      0 0 1) ; up is 'z'

   ; set and show lighting point
   (draw-geometry scene models)

   (glBindFramebuffer GL_FRAMEBUFFER 0)
   (glUseProgram 0)

   (glViewport 0 0 (gl:get-window-width) (gl:get-window-height))
   (glClearColor 0 0 0 1)
   (glClear (vm:ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity)
   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (glOrtho 0 1 0 1 0 1)

   (glEnable GL_TEXTURE_2D)
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
