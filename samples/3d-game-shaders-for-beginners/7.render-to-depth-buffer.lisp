#!/usr/bin/env ol

;; initialize OpenGL
(import (lib gl))
(gl:set-window-title "7. Render to the Depth Buffer")
(import (OpenGL version-2-1))
; todo: splash screen

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

(define aprogram (gl:CreateProgram
"#version 120 // OpenGL 2.1
   #define gl_WorldMatrix gl_TextureMatrix[7]
   varying vec4 fragPosLightSpace;
   void main() {
      gl_Position = gl_ProjectionMatrix * gl_ModelViewMatrix * gl_WorldMatrix * gl_Vertex;
      gl_FrontColor = gl_Color;

      fragPosLightSpace = gl_TextureMatrix[0] * gl_WorldMatrix * gl_Vertex; // position from the sun view point
   }"
"#version 120 // OpenGL 2.1
   uniform sampler2D shadow;
   varying vec4 fragPosLightSpace;
   void main() {
      // perform perspective divide
      vec3 projCoords = fragPosLightSpace.xyz / fragPosLightSpace.w;
      // transform to [0,1] range
      projCoords = projCoords * 0.5 + 0.5;
      // get closest depth value from light's perspective (using [0,1] range fragPosLight as coords)
      float closestDepth = texture2D(shadow, projCoords.xy).r;
      // get depth of current fragment from light's perspective
      float currentDepth = projCoords.z;
      // check whether current frag pos is in shadow
      float bias = 0.005;
      float shadow = (currentDepth - bias) > closestDepth ? 0.4 : 1.0;

      gl_FragColor = vec4(gl_Color.rgb * shadow, 1.0);
   }"))


;; render buffer
(import (OpenGL EXT framebuffer_object))

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

;; init

(glShadeModel GL_SMOOTH)
(glEnable GL_DEPTH_TEST)

; draw
(gl:set-renderer (lambda (mouse)
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

   (glUseProgram just-depth)
   (glDisable GL_CULL_FACE)
   (draw-geometry scene models)

   (glBindFramebuffer GL_FRAMEBUFFER 0)
   (glViewport 0 0 (gl:get-window-width) (gl:get-window-height))
   (glClearColor 0.2 0.2 0.2 1)
   (glClear (vm:ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   (glUseProgram aprogram)
   (glEnable GL_CULL_FACE)

   (if #T
   then
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

      (glActiveTexture GL_TEXTURE0) ;light matrix from the step above:
      (glMatrixMode GL_TEXTURE)
      (glLoadIdentity) ; let's prepare my_WorldMatrix
      (glOrtho -20 20 -20 20 -20 20)
      (gluLookAt
         (ref (sun 'position) 1) ; x
         (ref (sun 'position) 2) ; y
         (ref (sun 'position) 3) ; z
         0 0 0 ; sun is directed light
         0 0 1) ; up is 'z'
      
      (glBindTexture GL_TEXTURE_2D (car depth-fbo))

      ; Draw a geometry with colors
      (draw-geometry scene models)

   else
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
      (glActiveTexture GL_TEXTURE0)
      (glBindTexture GL_TEXTURE_2D (car depth-fbo))

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
