#!/usr/bin/env ol

; initialize OpenGL
(import (lib gl-2))
(gl:set-window-title "7.render-to-depth-buffer")

(import (scene))
(import (scheme inexact))

; load (and create if no one) a models cache
(define models (prepare-models "cache.bin"))
(define geometry (compile-triangles models))

; load a scene
(import (file json))
(define scene (read-json-file "scene.json"))

; let's find a sun
(define sun (car (filter (lambda (light) (string-eq? (light 'type) "SUN"))
   (vector->list (scene 'Lights)))))
(print "sun:" sun)

; scene objects
(define Objects (scene 'Objects))
(print "Objects: " Objects)

; simply draw a 2d texture shader
(define texture2d (gl:create-program
"#version 120 // OpenGL 2.1
   void main() {
      gl_Position = ftransform();
      gl_TexCoord[0] = gl_MultiTexCoord0;
   }"
"#version 120 // OpenGL 2.1
   uniform sampler2D shadow;
   void main() {
      gl_FragColor = texture2D(shadow, gl_TexCoord[0].st);
   }"))

; simply draw a cube texture shader
(define textureCube (gl:create-program
"#version 120 // OpenGL 2.1
   void main() {
      gl_Position = ftransform();
      gl_TexCoord[0] = gl_MultiTexCoord0;
   }"
"#version 120 // OpenGL 2.1
   uniform samplerCube shadow;
   void main() {
      gl_FragColor = textureCube(shadow, gl_TexCoord[0].stp);
   }"))


; framebuffers
(import (OpenGL EXT framebuffer_object))

(define TEXW 1024)
(define TEXH 1024)

; depth 2d map framebuffer
(define depthmap2d '(0))
(glGenTextures (length depthmap2d) depthmap2d)
(print "depthmap2d: " depthmap2d)
(glBindTexture GL_TEXTURE_2D (car depthmap2d))
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
(glTexImage2D GL_TEXTURE_2D 0 GL_DEPTH_COMPONENT TEXW TEXH 0 GL_DEPTH_COMPONENT GL_FLOAT 0)
(glBindTexture GL_TEXTURE_2D 0)

(define depthfbo2d '(0))
(glGenFramebuffers (length depthfbo2d) depthfbo2d)
(print "depthfbo2d: " depthfbo2d)
(glBindFramebuffer GL_FRAMEBUFFER (car depthfbo2d))
(glFramebufferTexture2D GL_FRAMEBUFFER GL_DEPTH_ATTACHMENT GL_TEXTURE_2D (car depthmap2d) 0)
(glDrawBuffer GL_NONE)
(glReadBuffer GL_NONE)
(glBindFramebuffer GL_FRAMEBUFFER 0)

(define depth2d (gl:create-program
"#version 120 // OpenGL 2.1
   #define gl_WorldMatrix gl_TextureMatrix[7]
   void main() {
      gl_Position = gl_ProjectionMatrix * gl_ModelViewMatrix * gl_WorldMatrix * gl_Vertex;
   }"
"#version 120 // OpenGL 2.1
   void main() {
      // nothing to do
   }"))

; textureCube
(import (only (OpenGL EXT geometry_shader4) glFramebufferTexture))

; depth cubemap framebuffer
;; кубическая карта теней
;; https://web.archive.org/web/20200911125435/https://habr.com/ru/post/354208/
(define depthmapCube '(0))
(glGenTextures (length depthmapCube) depthmapCube)
(print "depthmapCube: " depthmapCube)

(glBindTexture GL_TEXTURE_CUBE_MAP (car depthmapCube))
(for-each (lambda (side)
      (glTexImage2D side 0 GL_DEPTH_COMPONENT
                    TEXW TEXH 0 GL_DEPTH_COMPONENT GL_FLOAT NULL))
   (iota 6 GL_TEXTURE_CUBE_MAP_POSITIVE_X))

(glTexParameteri GL_TEXTURE_CUBE_MAP GL_TEXTURE_MAG_FILTER GL_NEAREST)
(glTexParameteri GL_TEXTURE_CUBE_MAP GL_TEXTURE_MIN_FILTER GL_NEAREST)
(glTexParameteri GL_TEXTURE_CUBE_MAP GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
(glTexParameteri GL_TEXTURE_CUBE_MAP GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
(glTexParameteri GL_TEXTURE_CUBE_MAP GL_TEXTURE_WRAP_R GL_CLAMP_TO_EDGE)  

(define depthfboCube '(0))
(glGenFramebuffers (length depthfboCube) depthfboCube)
(print "depthfboCube: " depthfboCube)
(glBindFramebuffer GL_FRAMEBUFFER (car depthfboCube))

(glFramebufferTexture GL_FRAMEBUFFER GL_DEPTH_ATTACHMENT (car depthmapCube) 0)
(glDrawBuffer GL_NONE)
(glReadBuffer GL_NONE)
(glBindFramebuffer GL_FRAMEBUFFER 0)

(define depthCube (gl:create-program
;; todo: логарифмический буфер глубины
;; https://outerra.blogspot.com/2012/11/maximizing-depth-buffer-range-and.html
"#version 120 // OpenGL 2.1
   #define gl_ModelMatrix gl_TextureMatrix[7]
   void main() {
      gl_Position = gl_ModelMatrix * gl_Vertex;
   }"

GL_TRIANGLES GL_TRIANGLE_STRIP 18
"#version 120 // OpenGL 2.1
   #extension GL_EXT_geometry_shader4 : enable
   void main()
   {
      for (int face = 0; face < 6; ++face)
      {
         // встроенная переменная, определяющая в какую грань кубической карты идет рендер
         gl_Layer = face; 
         for (int i = 0; i < 3; ++i) { // цикл по всем вершинам треугольника
            gl_Position = gl_ProjectionMatrix * gl_TextureMatrix[face] * gl_PositionIn[i];
            EmitVertex();
         }
         EndPrimitive();
      }
   }"

"#version 120 // OpenGL 2.1
   void main() {
      // do nothing
   }"
))

(define a 1) ; size of cube
(define a0 (* -3 a))
(define a1 (+ a0 a a))
(define a2 (+ a1 a a))
(define a3 (+ a2 a a))
(define +a a)
(define -a (- 0 a))

(define b (sqrt 3))
(define +b b)
(define -b (- 0 b))

(define pnttxr [
   [+b -b +b]
   [+b +b +b]
   [-b +b +b]
   [-b -b +b]
   [+b -b -b]
   [+b +b -b]
   [-b +b -b]
   [-b -b -b]
])

(define pntver [
   [ (+ a1 a) (+ a0 a -a) 0 ]
   [ (+ a1 a) (+ a0 a +a) 0 ]
   [ (- a1 a) (+ a0 a +a) 0 ]
   [ (- a1 a) (+ a0 a -a) 0 ]

   [ (+ a1 a) (+ a2 a -a) 0 ]
   [ (+ a1 a) (+ a2 a +a) 0 ]
   [ (- a1 a) (+ a2 a +a) 0 ]
   [ (- a1 a) (+ a2 a -a) 0 ]

   [ (+ a0 a) (+ a1 a -a) 0 ]
   [ (+ a0 a) (+ a1 a +a) 0 ]
   [ (- a0 a) (+ a1 a +a) 0 ]
   [ (- a0 a) (+ a1 a -a) 0 ]

   [ (+ a1 a) (+ a1 a -a) 0 ]
   [ (+ a1 a) (+ a1 a +a) 0 ]
   [ (- a1 a) (+ a1 a +a) 0 ]
   [ (- a1 a) (+ a1 a -a) 0 ]

   [ (+ a2 a) (+ a1 a -a) 0 ]
   [ (+ a2 a) (+ a1 a +a) 0 ]
   [ (- a2 a) (+ a1 a +a) 0 ]
   [ (- a2 a) (+ a1 a -a) 0 ]

   [ (+ a3 a) (+ a1 a -a) 0 ]
   [ (+ a3 a) (+ a1 a +a) 0 ]
   [ (- a3 a) (+ a1 a +a) 0 ]
   [ (- a3 a) (+ a1 a -a) 0 ]
])

(define tabtxr [
   4 0 3 7    ;; D
   1 5 6 2    ;; U
   3 2 6 7    ;; W
   0 1 2 3    ;; N
   4 5 1 0    ;; E
   7 6 5 4    ;; S
])
(define tabver [
   0  1  2  3
   4  5  6  7
   8  9 10 11
   12 13 14 15
   16 17 18 19
   20 21 22 23
])

;; init

(glShadeModel GL_SMOOTH)
(glEnable GL_DEPTH_TEST)
(glEnable GL_CULL_FACE); GL_BACK

; draw
(gl:set-renderer (lambda ()
   ; render sun depth buffer
   (glViewport 0 0 TEXW TEXH)
   (glBindFramebuffer GL_FRAMEBUFFER (unbox depthfbo2d))
   (glUseProgram depth2d)

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

   (glDisable GL_CULL_FACE)
   (draw-geometry (scene 'Objects) geometry)

   (glBindFramebuffer GL_FRAMEBUFFER 0)
   (glViewport 0 0 (gl:get-window-width) (gl:get-window-height))
   (glClearColor 0.2 0.2 0.2 1)
   (glClear (vm:ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   (glBindFramebuffer GL_FRAMEBUFFER 0)

   ; -----------------------------------------------------------
   ; render lightbulb depth buffer
   (let*((ss ms (clock))
         (ticks (/ (+ ss (/ ms 1000)) 1)))

   (define light {
      'type "POINT"
      'color [1 1 1]
      'position [
         (* 5 (sin (/ ticks 20)))
         (* 5 (cos (/ ticks 20)))
         1
         1] ; 1 for POINT light
   })

   (glViewport 0 0 TEXW TEXH)
   (glBindFramebuffer GL_FRAMEBUFFER (unbox depthfboCube))
   (glClear GL_DEPTH_BUFFER_BIT)

   ;; матрица проецирования у нас одна для всех сторон кубической карты
   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity)
   ;; про z-near и z-far
   ;; https://habr.com/ru/post/252771/#comment_8325185
   (gluPerspective 90.0 (/ TEXW TEXH) 1 100)

   (glMatrixMode GL_MODELVIEW) ; в шейдере не используется
   (glLoadIdentity)

   (define position (light 'position))
   (for-each (lambda (i atx aty atz upx upy upz)
         (glActiveTexture (+ GL_TEXTURE0 i))
         (glMatrixMode GL_TEXTURE)
         (glLoadIdentity)
         (gluLookAt
               (ref position 1)         (ref position 2)         (ref position 3)
            (+ (ref position 1) atx) (+ (ref position 2) aty) (+ (ref position 3) atz)
            upx upy upz))
      '(0  1  2  3  4  5) ; i
      '(1 -1  0  0  0  0) ; at..
      '(0  0  1 -1  0  0)
      '(0  0  0  0  1 -1)
      '(0  0  0  0  0  0) ; up..
      '(-1 -1 0  0 -1 -1)
      '(0  0  1 -1  0  0))

   (glUseProgram depthCube)

   (glCullFace GL_FRONT)
   (draw-geometry (scene 'Objects) geometry)
   (glCullFace GL_BACK)

   (glUseProgram 0)
   (glBindFramebuffer GL_FRAMEBUFFER 0))
   ; ---------------------

   ; draw sun depth texture
   (glViewport 0 0 (gl:get-window-width) (gl:get-window-height))
   (glClearColor 0 0 0 1)
   (glClear (vm:ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity)
   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (glOrtho 0 2 0 1 0 1)

   (glUseProgram texture2d)

   (glEnable GL_TEXTURE_2D)
   (glActiveTexture GL_TEXTURE0)
   (glUniform1i (glGetUniformLocation texture2d "shadow") 0)
   (glBindTexture GL_TEXTURE_2D (car depthmap2d))

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

   ; draw light bulb depth texture
   (glUseProgram textureCube)

   (glEnable GL_TEXTURE_CUBE_MAP)

   (glActiveTexture GL_TEXTURE0)
   (glUniform1i (glGetUniformLocation textureCube "shadow") 0)
   (glBindTexture GL_TEXTURE_CUBE_MAP (car depthmapCube))

   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (glOrtho -15 5 -4 4 -1 1)

   (glColor3f 1 1 1)
   (glBegin GL_QUADS)
   (for-each (lambda (i)
         (define j (ref tabtxr (+ i 1)))
         (glTexCoord3fv (ref pnttxr (+ j 1)))
         (define l (ref tabver (+ i 1)))
         (glVertex3fv (ref pntver (+ l 1))))
      (iota 24 23 -1))
   (glEnd)

))
