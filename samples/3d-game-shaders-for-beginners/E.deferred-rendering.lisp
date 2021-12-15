#!/usr/bin/env ol

;; initialize OpenGL
(import (lib gl))
(gl:set-window-title "E. Deferred Rendering")
(import (OpenGL version-2-1))

(import (scene))

; load (and create if no one) a models cache
(define models (prepare-models "cache.bin"))
(print "compiled models:\n" models)

;; load a scene
(import (file json))
(define scene (read-json-file "scene1.json"))

;; let's find a sun
(define sun (car (filter (lambda (light) (string-ci=? (light 'type) "SUN"))
   (vector->list (scene 'Lights)))))
(print "sun:" sun)

;; shaders
(define just-depth (gl:CreateProgram
"#version 120 // OpenGL 2.1
   #define gl_WorldMatrix gl_TextureMatrix[7]
   void main() {
      gl_Position = gl_ProjectionMatrix * gl_ModelViewMatrix * gl_WorldMatrix * gl_Vertex;
   }"
"#version 120 // OpenGL 2.1
   void main() {
      // nothing to do
   }"))

;; -----------------------
; https://learnopengl.com/Getting-started/Coordinate-Systems
(define forward-program (gl:CreateProgram
   (file->string "shaders/E.deferred-rendering.vs")
   (file->string "shaders/E.deferred-rendering.fs")))

(define justdraw (gl:CreateProgram
"#version 120 // OpenGL 2.1
   void main() {
      gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
      gl_TexCoord[0] = gl_MultiTexCoord0;
   }"
"#version 120 // OpenGL 2.1
   uniform sampler2D shadowMap;
   void main() {
      gl_FragColor = texture2D(shadowMap, gl_TexCoord[0].st);
   }"))

(define deferred-shadow (gl:CreateProgram
"#version 120 // OpenGL 2.1
   void main() {
      gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
      gl_TexCoord[0] = gl_MultiTexCoord0;
   }"
"#version 120 // OpenGL 2.1
   uniform sampler2D colorMap;
   uniform sampler2D shadowMap;
   uniform sampler2D fragPosLightMap;
   void main() {
      // restore vector values
      vec4 color = texture2D(colorMap, gl_TexCoord[0].st);
      vec4 fragPosLightSpace = texture2D(fragPosLightMap, gl_TexCoord[0].st);

	   // 1. simple shadow calculation
	   vec3 projCoords = fragPosLightSpace.xyz;

	   // get closest depth value from light's perspective (using [0,1] range fragPosLight as coords)
	   float closestDepth = texture2D(shadowMap, projCoords.xy).r;
	   // get depth of current fragment from light's perspective
	   float currentDepth = projCoords.z;
	   // check whether current frag pos is in shadow
	   float bias = 0.005;
	   float shadow = (currentDepth - bias) > closestDepth ? 0.4 : 1.0;

      // 2. PCF
      shadow = 0.0;
      vec2 texelSize = 1.0 / vec2(1024);
      for(int x = -1; x <= 1; x++) {
         for(int y = -1; y <= 1; y++) {
            float pcfDepth = texture2D(shadowMap, projCoords.xy + vec2(x, y) * texelSize).r;
            shadow += (currentDepth - bias) > pcfDepth ? 0.4 : 1.0;
         }
      }
      shadow /= 9.0;

	   gl_FragColor = color * shadow;
   }"))
; todo: float bias = max(0.05 * (1.0 - dot(normal, lightDir)), 0.005);  

;; depth buffer
(import (OpenGL EXT framebuffer_object))
(import (OpenGL ARB draw_buffers))

(define TEXW 1024)
(define TEXH 1024)

(define depth-map '(0))
(glGenTextures (length depth-map) depth-map)
(print "depth-map: " depth-map)
(glBindTexture GL_TEXTURE_2D (car depth-map))
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
(glTexImage2D GL_TEXTURE_2D 0 GL_DEPTH_COMPONENT TEXW TEXH 0 GL_DEPTH_COMPONENT GL_FLOAT 0)
(glBindTexture GL_TEXTURE_2D 0)

(define depth-fbo '(0))
(glGenFramebuffers (length depth-fbo) depth-fbo)
(print "depth-fbo: " depth-fbo)
(glBindFramebuffer GL_FRAMEBUFFER (car depth-fbo))
(glFramebufferTexture2D GL_FRAMEBUFFER GL_DEPTH_ATTACHMENT GL_TEXTURE_2D (car depth-map) 0)
(glDrawBuffer GL_NONE)
(glReadBuffer GL_NONE)
(glBindFramebuffer GL_FRAMEBUFFER 0)

;; deferred rendering textures:
(define forward-fbo '(0))
(glGenFramebuffers (length forward-fbo) forward-fbo)
(print "forward-fbo: " forward-fbo)

(define TEXW 1024)
(define TEXH 1024)

(define color-textures '(0 0))
(glGenTextures (length color-textures) color-textures)
(print "color-textures: " color-textures)

; color
(glBindTexture GL_TEXTURE_2D (car color-textures))
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
(glTexImage2D GL_TEXTURE_2D 0 GL_RGBA TEXW TEXH 0 GL_RGBA GL_UNSIGNED_BYTE 0)
(glBindTexture GL_TEXTURE_2D 0)

; vector
(glBindTexture GL_TEXTURE_2D (cadr color-textures))
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
(glTexImage2D GL_TEXTURE_2D 0 GL_RGB16 TEXW TEXH 0 GL_RGB GL_FLOAT 0)
(glBindTexture GL_TEXTURE_2D 0)


(glBindFramebuffer GL_FRAMEBUFFER (car forward-fbo))
(glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D (lref color-textures 0) 0)
(glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT1 GL_TEXTURE_2D (lref color-textures 1) 0)
(glDrawBuffers 2 (list GL_COLOR_ATTACHMENT0 GL_COLOR_ATTACHMENT1))

; and we have to create depth buffer if we want to use a depth
(define depthrenderbuffer '(0))
(glGenRenderbuffers (length depthrenderbuffer) depthrenderbuffer)
(glBindRenderbuffer GL_RENDERBUFFER (car depthrenderbuffer))
(glRenderbufferStorage GL_RENDERBUFFER GL_DEPTH_COMPONENT TEXW TEXH)
(glFramebufferRenderbuffer GL_FRAMEBUFFER GL_DEPTH_ATTACHMENT GL_RENDERBUFFER (car depthrenderbuffer))

; TBD.

;; draw

(import (lib math))
(import (owl math fp))

; настройки
(glShadeModel GL_SMOOTH)
(glEnable GL_DEPTH_TEST)

;; освещение сцены
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

   ;; calculate the shadow texture
   (begin
      (glViewport 0 0 TEXW TEXH)
      (glBindFramebuffer GL_FRAMEBUFFER (car depth-fbo))

      (glClearColor 0 0 0 1)
      (glClear GL_DEPTH_BUFFER_BIT)

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

      (glEnable GL_CULL_FACE)
      (glCullFace GL_FRONT)
      (glUseProgram just-depth)
      (draw-geometry scene models))

   (glDisable GL_CULL_FACE)
   (glCullFace GL_BACK)

   ;; let's draw a scene
   (begin
      (glBindFramebuffer GL_FRAMEBUFFER (car forward-fbo))
      (glViewport 0 0 TEXW TEXH)
      (glClearColor 1 0 0 1)
      (glClear (vm:ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

      (glUseProgram forward-program)

      ; Camera setup (should be before Lights setup)
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

      ; Lights setup
      (for-each (lambda (light i)
            (glLightfv (+ GL_LIGHT0 i) GL_POSITION (light 'position)))
         lights
         (iota (length lights)))

      ; Shadow setup
      (glActiveTexture GL_TEXTURE2) ; light matrix from the sun
      (glMatrixMode GL_TEXTURE)
      (glLoadIdentity) ; let's prepare my_WorldMatrix
      (glOrtho -20 20 -20 20 -20 20)
      (gluLookAt
         (ref (sun 'position) 1) ; x
         (ref (sun 'position) 2) ; y
         (ref (sun 'position) 3) ; z
         0 0 0 ; sun is directed light
         0 0 1) ; up is 'z'
      
      (glActiveTexture GL_TEXTURE0) ; light matrix from the sun
      (glBindTexture GL_TEXTURE_2D (car depth-fbo))

      (draw-geometry scene models))

   ;; Calculate and draw a final image
   (begin
      (glBindFramebuffer GL_FRAMEBUFFER 0)
      (glUseProgram deferred-shadow)

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
      (glBindTexture GL_TEXTURE_2D (car color-textures))
      (glUniform1i (glGetUniformLocation deferred-shadow "colorMap") 0)
      (glActiveTexture GL_TEXTURE1)
      (glBindTexture GL_TEXTURE_2D (car depth-map))
      (glUniform1i (glGetUniformLocation deferred-shadow "shadowMap") 1)
      (glActiveTexture GL_TEXTURE2)
      (glBindTexture GL_TEXTURE_2D (cadr color-textures))
      (glUniform1i (glGetUniformLocation deferred-shadow "fragPosLightMap") 2)

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

   '(begin
      (glBindFramebuffer GL_FRAMEBUFFER 0)
      (glUseProgram justdraw)

      (glViewport 0 0 (div (gl:get-window-width) 2) (div (gl:get-window-height) 2))
      (glClearColor 0 0 0 1)
      (glClear (vm:ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

      (glMatrixMode GL_PROJECTION)
      (glLoadIdentity)
      (glMatrixMode GL_MODELVIEW)
      (glLoadIdentity)
      (glOrtho 0 1 0 1 0 1)

      (glEnable GL_TEXTURE_2D)
      (glActiveTexture GL_TEXTURE0)
      (glBindTexture GL_TEXTURE_2D (car color-textures))

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
))
