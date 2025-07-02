#!/usr/bin/env ol

(import (lib gl))
(import (OpenGL 2.1))
(import (lib GLU))

(import (OpenGL ARB framebuffer_object))
(define framebuffers [0 0 0 0 0 0])
(glGenFramebuffers (size framebuffers) framebuffers)

(define textures [0 0 0 0 0 0])
(glGenTextures (size textures) textures)

(for-each (lambda (fbo tex)
      (glBindTexture GL_TEXTURE_2D tex)
      (glTexImage2D GL_TEXTURE_2D 0 GL_RGB 256 256 0 GL_RGB GL_UNSIGNED_BYTE NULL)
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
      (glBindTexture GL_TEXTURE_2D 0)
      
      (glBindFramebuffer GL_FRAMEBUFFER fbo)
      (glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D tex 0))
   framebuffers
   textures)

(glBindTexture GL_TEXTURE_2D 0)
(glBindFramebuffer GL_FRAMEBUFFER 0)

; texturing shaders
(define programs (vector-map (lambda (i)
      (gl:create-program
         "#version 120 // OpenGL 2.1
            void main() {
               gl_Position = gl_Vertex;
            }"
         (file->string (string-append "program" (number->string i) ".glsl"))))
   [1 2 3 4 5 6]))

; common code
(import (scheme inexact))
,load "cube.lisp"

; draw
(gl:set-renderer (lambda (mouse)
   (define aspect (/ (gl:get-window-width) (gl:get-window-height)))
   (glClearColor 0.1 0.1 0.1 1)
   (glClear (bor GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   ; render textures
   (glViewport 0 0 255 255)
   (glDisable GL_TEXTURE_2D)
   (glDisable GL_DEPTH_TEST)
   (for-each (lambda (fbo shader)
         (glBindFramebuffer GL_FRAMEBUFFER fbo)
         (glUseProgram shader)
         (glUniform1f (glGetUniformLocation shader "time") (/ (mod (time-ms) 1000000) #i1000))
         (glUniform2f (glGetUniformLocation shader "dimensions") 255 255)
         (glBegin GL_QUADS)
            (glVertex2f -1.0 +1.0)
            (glVertex2f +1.0 +1.0)
            (glVertex2f +1.0 -1.0)
            (glVertex2f -1.0 -1.0)
         (glEnd))
      framebuffers
      programs)
   (glBindFramebuffer GL_FRAMEBUFFER 0)
   (glUseProgram 0)

   ; main rendering program
   (glViewport 0 0 (gl:get-window-width) (gl:get-window-height))
   (glEnable GL_DEPTH_TEST)

   ; classical projection matrix
   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity)
   (gluPerspective 45 aspect 0.1 1000)

   (define Y -1.2)
   (define R -3)

   ; classical modelview matrix
   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (gluLookAt 0 0 0
      0 Y R
      0 1 0)

   ; world transformation
   (define t (/ (mod (time-ms) 6283) #i1000))
   (glTranslatef 0 -2.1 -5)
   (glRotatef (* t 360/3.14) 0 1 0)

   ; draw cube with color and texture
   (glEnable GL_TEXTURE_2D)
   (glColor3f 1 1 1)
   (cube:draw)

))
