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
(define vertex-shader "#version 120 // OpenGL 2.1
   void main() {
      gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
   }")
(define fragment-shader "#version 120 // OpenGL 2.1
   void main() {
      // nothing to do
   }")

(define po (gl:CreateProgram vertex-shader fragment-shader))

;; -----------------------
; https://learnopengl.com/Getting-started/Coordinate-Systems
(define shadowed (gl:CreateProgram
"#version 120 // OpenGL 2.1
   varying vec4 FragPosLightSpace;
   uniform mat4 my_matrix;
   varying float diffuseIntensity;
   varying float specularIntensity;
   void main() {
      gl_Position = gl_ProjectionMatrix * gl_ModelViewMatrix * gl_Vertex;

      vec4 vertexPosition = gl_ModelViewMatrix * gl_Vertex;
      vec3 lightDirection = gl_LightSource[0].position.xyz - vertexPosition.xyz * gl_LightSource[0].position.w;

      vec3 normal = normalize(gl_Normal);

      vec3 unitLightDirection = normalize(lightDirection);
      vec3 eyeDirection       = normalize(-vertexPosition.xyz);
      vec3 reflectedDirection = normalize(-reflect(unitLightDirection, normal));

      diffuseIntensity  = dot(normal, unitLightDirection);
      specularIntensity = max(dot(reflectedDirection, eyeDirection), 0);

      // seen from light:
      FragPosLightSpace = gl_TextureMatrix[0] * vec4(gl_Vertex.xyz, 1.0);

      gl_FrontColor = gl_Color;
   }"
"#version 120 // OpenGL 2.1
   uniform sampler2D shadow;
   varying vec4 FragPosLightSpace;
   varying float diffuseIntensity;
   varying float specularIntensity;
   void main() {
      vec3 projCoords = FragPosLightSpace.xyz / FragPosLightSpace.w;
      projCoords = projCoords * 0.5 + 0.5;
      float currentDepth = projCoords.z;
      float closestDepth = texture2D(shadow, projCoords.st).r;

      float bias = 0.005;
//      // float bias = max(0.05 * (1.0 - dot(normal, lightDir)), 0.005);
//      float shdw = currentDepth - bias > closestDepth  ? 1.0 : 0.0;

      float shdw = 0.0;
      vec2 texelSize = 1.0 / vec2(1024, 1024); //textureSize(shadow, 0);
      for (int x = -1; x <= 1; ++x)
      {
         for (int y = -1; y <= 1; ++y)
         {
            float pcfDepth = texture2D(shadow, projCoords.st + vec2(x, y) * texelSize).r;
            shdw += currentDepth - bias > pcfDepth ? 1.0 : 0.0;
         }
      }
      shdw /= 9.0;

      gl_FragColor = vec4(gl_Color.rgb * diffuseIntensity + gl_Color.rgb * (1 - shdw) * (diffuseIntensity + specularIntensity), 1.0);
   }"))

(define justdraw (gl:CreateProgram
"#version 120 // OpenGL 2.1
   varying 
   void main() {
      gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
      gl_TexCoord[0]=gl_MultiTexCoord0;
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

;; (glEnable GL_LIGHTING)
;; (glLightModelf GL_LIGHT_MODEL_TWO_SIDE GL_TRUE)
;; (glEnable GL_LIGHT0)

; draw
(gl:set-renderer (lambda (mouse)
(let*((ss ms (clock))
      (ticks (/ (+ ss (/ ms 1000)) 2)))

   (define x (* 15 (sin ticks)))
   (define y (* 15 (cos ticks)))
   (define z 8)

   ;; (define x 15)
   ;; (define y 15)
   ;; (define z 15)

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
   (gluLookAt x y z ; gl_ModelView is a light space matrix
      0 0 0
      0 0 1)

   (glLightfv GL_LIGHT0 GL_POSITION (list x y z 1)) ; не надо

   (glCullFace GL_FRONT)
   (draw-geometry scene models)
   (glCullFace GL_BACK)
   (glBindFramebuffer GL_FRAMEBUFFER 0)

   ;; let's draw a scene
   (when #true
      (glViewport 0 0 (gl:get-window-width) (gl:get-window-height))
      (glClearColor 0 0 0 1)
      (glClear GL_DEPTH_BUFFER_BIT)
      (glClear (vm:ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

      (glUseProgram shadowed)

      ;; prepare shadow matrix:
      (begin
         (glLightfv GL_LIGHT0 GL_POSITION (list x y z 1))
         ; let's generate light transform matrix into texture matrix
         (glActiveTexture GL_TEXTURE0)
         (glMatrixMode GL_TEXTURE)
         (glLoadIdentity)
         (glOrtho -20 20 -20 20 0 50) ; gl_Projection is a light projection matrix
         ;(gluPerspective 45 1.0 0.1 100)
         (gluLookAt x y z ; gl_ModelView is a light space matrix
            0 0 0
            0 0 1))

      (glMatrixMode GL_PROJECTION)
      (glLoadIdentity)
      ;(glOrtho -20 20 -20 20 0 50) ; gl_Projection is a light projection matrix
      (gluPerspective 45 (/ (gl:get-window-width) (gl:get-window-height)) 0.1 100)

      (glMatrixMode GL_MODELVIEW)
      (glLoadIdentity)
      (gluLookAt -17 -17 15
         0 0 0
         0 0 1)

      (glEnable GL_TEXTURE_2D)
      (glActiveTexture GL_TEXTURE0)
      (glBindTexture GL_TEXTURE_2D (car depth-fbo))
      (glUniform1i (glGetUniformLocation shadowed "shadow") 0) ; 0-th texture unit

      (draw-geometry scene models)
      
      (glUseProgram 0)
      (glPointSize 5)
      (glBegin GL_POINTS)
         (glColor3f #xff/255 #xbf/255 0)
         (glVertex3f x y z)
      (glEnd)
      (glBegin GL_LINES)
         (glVertex3f x y z)
         (glVertex3f x y 0)
      (glEnd) )

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
         
      ;;    (glLightfv GL_LIGHT0 GL_POSITION (list x y z 1)))
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
