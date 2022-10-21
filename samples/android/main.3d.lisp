#!/usr/bin/env ol
(import (lib gl)) ; TODO: уменьшить количество подгружаемого кода

; ----------
; todo: draw "splash" image
; todo: рисовать в обычном окошке, не в полном!
; и уже тогда, когда все ресурсы подтянуться - только тогда перейти в полноэкранный режим
; (это надо сделать перед самым стартом рендерера)
; таким образом у нас повисит перед глазами окошко (именно что окошко) со сплешем
; и уже после этого игра запустит 3д
(print "splash")
; ----------

; ---------------------------
; load other data.
(import (lib gl-2)
   (scheme inexact))
(import (lib GLU))

; -- image loading --
;; (define background
;;    (SOIL_load_OGL_texture "/sdcard/ol/media/field-of-view.jpg" SOIL_LOAD_RGBA SOIL_CREATE_NEW_ID 0))
(import (lib soil))
(define background
   (let ((file (file->bytevector "media/wood.jpg")))
      (SOIL_load_OGL_texture_from_memory file (size file) SOIL_LOAD_RGBA SOIL_CREATE_NEW_ID SOIL_FLAG_MIPMAPS)))

;; TODO: переделать все для GL_OVR_multiview
;; README: https://arm-software.github.io/opengl-es-sdk-for-android/multiview.html
;; usage sample: /home/uri/Workspace/oculus/ovr_sdk_mobile_1.29.0/VrSamples/VrCubeWorld_NativeActivity/Src/VrCubeWorld_NativeActivity.c

(gl:set-window-size 1280 720)

; OpenGL Scene Settings
(define RL_CULL_DISTANCE_NEAR 0.01)
(define RL_CULL_DISTANCE_FAR 1000.0)
(define DEG2RAD (/ #i3.14159265358979323846 #i180.0))

(define FOVY 80.0)   ; найдено экспериментально
(define ASPECT (/ (/ (gl:get-window-width) 2) (gl:get-window-height)))

(define TEXW 2048)
(define W (/ TEXW 2))
(define TEXH 2048)

; ---------------------------------------------------
(import (config))
(define interpupillary-distance #i0.07) ; межзрачковое расстояние, meters
(define eye-positions (cons
   (list (/ interpupillary-distance -2) #i0.075 #i0.045)
   (list (/ interpupillary-distance +2) #i0.075 #i0.045)))

(define chroma-ab-correction [#i0.996 #i-0.004 #i1.014 0])

; screen size in pixels
(define hResolution (gl:get-window-width))
(define vResolution (gl:get-window-height))


; LoadVrStereoConfig:
; Compute aspect ratio

; Compute lens parameters
(define lensShift (/ (- (/ (ref vr-screen-size 1) 4) (/ lens-separation-distance 2)) (ref vr-screen-size 2)))

(define leftLensCenter    [(+ 1/4 lensShift) 1/2])
(define rightLensCenter   [(- 3/4 lensShift) 1/2])
(define leftScreenCenter  [   1/4            1/2])
(define rightScreenCenter [   3/4            1/2])

; Compute distortion scale parameters
; NOTE: To get lens max radius, lensShift must be normalized to [-1..1]
(define lensRadius (abs (- -1 (* -4 lensShift))))
(define lensRadiusSq (* lensRadius lensRadius))
(define distortionScale (+
      (ref lens-distortion-values 1)
   (* (ref lens-distortion-values 2) lensRadiusSq)
   (* (ref lens-distortion-values 2) lensRadiusSq lensRadiusSq)
   (* (ref lens-distortion-values 2) lensRadiusSq lensRadiusSq lensRadiusSq)))
(define normScreenWidth 1/2)
(define normScreenHeight 1)

(define scaleIn [(/ 2 normScreenWidth)
                 (/ 2 normScreenHeight ASPECT)])
(define scale [(/ (* normScreenWidth 1/2) distortionScale)
               (/ (* normScreenHeight 1/2 ASPECT) distortionScale)])

; Distortion shader
(define distortion (gl:create-program
"#version 120 // OpenGL 2.1
   void main() {
      gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex; //ftransform();
      gl_TexCoord[0] = gl_MultiTexCoord0;
   }"
"#version 120 // OpenGL 2.1
   uniform sampler2D bitmap;

   #define LEFT 0
   #define RIGHT 1

   uniform vec2 leftLensCenter;
   uniform vec2 rightLensCenter;
   uniform vec2 leftScreenCenter;
   uniform vec2 rightScreenCenter;

   uniform vec2 scale;
   uniform vec2 scaleIn;
   uniform vec4 deviceWarpParam;
   uniform vec4 chromaAbParam;

   void main() {
      // Compute lens distortion
      vec2 lensCenter = gl_TexCoord[0].x < 0.5 ? leftLensCenter : rightLensCenter;
      vec2 screenCenter = gl_TexCoord[0].x < 0.5 ? leftScreenCenter : rightScreenCenter;
      vec2 theta = (gl_TexCoord[0].xy - lensCenter) * scaleIn;

      float rSq = theta.x*theta.x + theta.y*theta.y;
      vec2 theta1 = theta*(deviceWarpParam.x + deviceWarpParam.y*rSq + deviceWarpParam.z*rSq*rSq + deviceWarpParam.w*rSq*rSq*rSq);
      vec2 thetaBlue = theta1*(chromaAbParam.z + chromaAbParam.w*rSq);

      vec2 tcBlue = lensCenter + scale*thetaBlue;

      // temp
      //tcBlue = lensCenter + scale*theta;
      //gl_FragColor = texture2D(bitmap, tcBlue);

      if (any(bvec2(
            clamp(tcBlue, screenCenter - vec2(0.25, 0.5), screenCenter + vec2(0.25, 0.5)) - tcBlue)))
      {
         // Set black fragment for everything outside the lens border
         gl_FragColor = vec4(0, 0, 0, 1.0);
      }
      else {
#if 0 // временно отключим дисторшен
         gl_FragColor = texture2D(bitmap, tcBlue);
#else
         // Compute color chroma aberration
         float blue = texture2D(bitmap, tcBlue).b;

         vec2 tcGreen = lensCenter + scale*theta1;
         float green = texture2D(bitmap, tcGreen).g;

         vec2 thetaRed = theta1*(chromaAbParam.x + chromaAbParam.y*rSq);
         vec2 tcRed = lensCenter + scale*thetaRed;
         float red = texture2D(bitmap, tcRed).r;

         gl_FragColor = vec4(red, green, blue, 1.0);
#endif
      }
   }"
))

; set shader values
(glUseProgram distortion)
(glUniform2fv (glGetUniformLocation distortion "leftLensCenter") 1 leftLensCenter)
(glUniform2fv (glGetUniformLocation distortion "rightLensCenter") 1 rightLensCenter)
(glUniform2fv (glGetUniformLocation distortion "leftScreenCenter") 1 leftScreenCenter)
(glUniform2fv (glGetUniformLocation distortion "rightScreenCenter") 1 rightScreenCenter)
(glUniform2fv (glGetUniformLocation distortion "scale") 1 scale)
(glUniform2fv (glGetUniformLocation distortion "scaleIn") 1 scaleIn)
(glUniform4fv (glGetUniformLocation distortion "deviceWarpParam") 1 lens-distortion-values)
(glUniform4fv (glGetUniformLocation distortion "chromaAbParam") 1 chroma-ab-correction)

(print "leftLensCenter: " (vector-map inexact leftLensCenter))
; ----------------------------------------------------
; Create render target texture
(import (OpenGL ARB framebuffer_object))

; texture2d
(define texture '(0))
(glGenTextures (length texture) texture)

(glBindTexture GL_TEXTURE_2D (car texture))
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
(glTexImage2D GL_TEXTURE_2D 0 GL_RGBA TEXW TEXH 0 GL_RGBA GL_UNSIGNED_BYTE #f)
(glBindTexture GL_TEXTURE_2D 0)

; framebuffer
(define framebuffer '(0))
(glGenFramebuffers (length framebuffer) framebuffer)
(glBindFramebuffer GL_FRAMEBUFFER (car framebuffer))
(glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D (car texture) 0)
; depthbuffer
;  we have to create hardware depth buffer if we want to use a depth
(define depthrenderbuffer '(0))
(glGenRenderbuffers (length depthrenderbuffer) depthrenderbuffer)
(glBindRenderbuffer GL_RENDERBUFFER (car depthrenderbuffer))
(glRenderbufferStorage GL_RENDERBUFFER GL_DEPTH_COMPONENT TEXW TEXH)
(glFramebufferRenderbuffer GL_FRAMEBUFFER GL_DEPTH_ATTACHMENT GL_RENDERBUFFER (car depthrenderbuffer))
(glBindFramebuffer GL_FRAMEBUFFER 0)

; camera
(define position '(0 0  0))
(define target   '(0 0 -1)) ; вперед...
(define up       '(0 1  0))
; perspective projection

(define (draw-cube)
   (glDisable GL_TEXTURE_2D)
   ; нарисуем много линий
   ;(glBegin GL_TRIANGLE_STRIP)
   (glBegin GL_LINE_STRIP)
      (for-each (lambda (x y z  r g b)
            (glColor3f r g b)
            (glVertex3f x y z))
         '(-1  1 -1  1  1  1  1 -1 -1 -1 -1  1 -1  1) ;x
         '( 1  1 -1 -1 -1  1  1  1  1 -1 -1 -1  1  1) ;y
         '( 1  1  1  1 -1  1 -1  1 -1  1 -1 -1 -1 -1) ;z

         '( 1  1  1  1  1  1  1  0  0  0  0  0  0  0)
         '( 1  1  1  0  0  0  0  1  1  1  1  0  0  0)
         '( 1  0  1  0  1  0  1  0  1  0  1  0  1  0) )
   (glEnd))

(define (draw-plane)
   (glEnable GL_TEXTURE_2D)
   (glBegin GL_QUADS)

   (glTexCoord2f 0 0)
   (glVertex3f -1 -1 0)

   (glTexCoord2f 0 1)
   (glVertex3f -1 +1 0)

   (glTexCoord2f 1 1)
   (glVertex3f +1 +1 0)

   (glTexCoord2f 1 0)
   (glVertex3f +1 -1 0)

   (glEnd))


(import (scheme srfi-27))
(define planes (map (lambda (i)
      [
         ; xyz
            (- (* 10 (random-real)) 5)
            (- (* 10 (random-real)) 5)
            (- -10 (* 10 i))
         ; rotation
         (random-real) (random-real) (random-real) ])
   (iota 50)))
(define (draw-planes delta)
   (for-each (lambda (plane)
         (glPushMatrix)
         (glTranslatef (ref plane 1) (ref plane 2) (ref plane 3))
         (glRotatef delta (ref plane 4) (ref plane 5) (ref plane 6))
         (draw-plane)

         (glPopMatrix))
      planes))


; init
(glShadeModel GL_SMOOTH)

(define started (let*((ss ms (clock))) (cons ss ms)))

(gl:set-renderer (lambda ()
   (define now (let*((ss ms (clock))) (cons ss ms)))
   ;(define delta (* 0.5 (sin (/ (+ (* 1000 (- (car now) (car started))) (- (cdr now) (cdr started))) 500))))
   (define delta (let*((ss ms (clock)))
         (+ (* (mod ss 1000) 1000) ms)))
   ;; (define target (list
   ;;    0 0 (let*((ss ms (clock)))
   ;;                (if (> (mod ss 20) 10) -1 -3))))

   ;; (define (draw)
   ;;    (glPushMatrix)
   ;;    (glTranslatef 0 0 -10)
   ;;    (draw-plane)
   ;;    (glPopMatrix)

   ;;    (glPushMatrix)
   ;;    (glScalef 2 2 1)
   ;;    (glTranslatef 0 -1 -12)
   ;;    (draw-plane)
   ;;    (glPopMatrix)

   ;;    ;; (glColor3f 0.2 0.5 0.2)
   ;;    ;; (glDisable GL_TEXTURE_2D)
   ;;    ;; ; нарисуем много линий
   ;;    ;; (glBegin GL_TRIANGLES)
   ;;    ;;    (glColor3f 1 0 0)
   ;;    ;;    (glVertex2f -0.6 -0.6)

   ;;    ;;    (glColor3f 0 1 0)
   ;;    ;;    (glVertex2f +0.6 -0.6)

   ;;    ;;    (glColor3f 0 0 1)
   ;;    ;;    (glVertex2f -0.0 +0.7)
   ;;    ;; (glEnd)
   ;;    (glFlush))

   (glUseProgram 0)
   (glBindFramebuffer GL_FRAMEBUFFER (car framebuffer))
   (glClearColor 0 0.2 0.2 1)
   (glEnable GL_DEPTH_TEST)

   (glViewport 0 0 TEXW TEXH)
   (glClear (vm:ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   ;; (glMatrixMode GL_PROJECTION)
   ;; (glLoadIdentity)

   ;; (define top (* RL_CULL_DISTANCE_NEAR (tan (* fovy 0.5 DEG2RAD))))
   ;; (define right (* top aspect))
   ;; (glFrustum (- right) right (- top) top RL_CULL_DISTANCE_NEAR RL_CULL_DISTANCE_FAR)

   ;; (glMatrixMode GL_MODELVIEW)
   ;; (glLoadIdentity)
   ;; (apply gluLookAt (append position target up))

   (glBindTexture GL_TEXTURE_2D background)

   ; left eye
   (glViewport 0 0 W TEXH)

   ; камера влияет только на проецирование
   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity)
   (gluPerspective FOVY ASPECT RL_CULL_DISTANCE_NEAR RL_CULL_DISTANCE_FAR)

   ; а вод моделвью - 
   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   ; todo: векторное произведение верха и вперед
   (apply gluLookAt (append 
      (map + position (car eye-positions)) target up))

   ;; (glRotatef (/ delta 30) 0 0 1)
   (draw-planes (/ delta 30))

   ; right eye
   (glViewport W 0 W TEXH)

   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity)
   (gluPerspective FOVY ASPECT RL_CULL_DISTANCE_NEAR RL_CULL_DISTANCE_FAR)

   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (apply gluLookAt (append 
      (map + position (cdr eye-positions)) target up))

   ;; (glRotatef (/ delta 30) 0 0 1)
   (draw-planes (/ delta 30))

   ; ...
   ; ----------------------------------------------------------
   ; draw to the screen:
   (glUseProgram distortion)
   (glBindFramebuffer GL_FRAMEBUFFER 0)
   (glClearColor 0 1 0 1)
   (glDisable GL_DEPTH_TEST)

   (glViewport 0 0 (gl:get-window-width) (gl:get-window-height))
   (glClear (vm:ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   ; выведем текстуру на весь экран
   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity)
   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (glOrtho 0 1 0 1 0 1)

   (glEnable GL_TEXTURE_2D)
   (glActiveTexture GL_TEXTURE0)
   (glUniform1i (glGetUniformLocation distortion "bitmap") 0)
   (glBindTexture GL_TEXTURE_2D (car texture))
   ;(glBindTexture GL_TEXTURE_2D background)

   (glColor3f 1 1 1)
   (glBegin GL_QUADS)
      (glTexCoord2f 0 0)
      (glVertex2f 0 0)
      (glTexCoord2f 1 0)
      (glVertex2f 1 0)
      (glTexCoord2f 1 1)
      (glVertex2f 1 1)
      (glTexCoord2f 0 1)
      (glVertex2f 0 1)
   (glEnd)
   (glFlush)


   (when #f
      ;; нарисуем картинку на весь экран!
      (glUseProgram 0)
      (glBindFramebuffer GL_FRAMEBUFFER 0)
      (glDisable GL_DEPTH_TEST)

      (glViewport 0 0 (gl:get-window-width) (gl:get-window-height))

      ; выведем текстуру на весь экран
      (glMatrixMode GL_PROJECTION)
      (glLoadIdentity)
      (glMatrixMode GL_MODELVIEW)
      (glLoadIdentity)
      (glOrtho 0 1 0 1 0 1)

      (glEnable GL_TEXTURE_2D)
      (glActiveTexture GL_TEXTURE0)
      (glBindTexture GL_TEXTURE_2D background)

      (glColor3f 1 1 1)
      (glBegin GL_QUADS)
         (glTexCoord2f 0 0)
         (glVertex2f 0 0)
         (glTexCoord2f 1 0)
         (glVertex2f 1 0)
         (glTexCoord2f 1 1)
         (glVertex2f 1 1)
         (glTexCoord2f 0 1)
         (glVertex2f 0 1)
      (glEnd)
      (glFlush))

))
