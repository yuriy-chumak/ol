#!/usr/bin/env ol
(import (lib gl-2)
   (scheme inexact))

(import (lib soil))
;; (define background
;;    (SOIL_load_OGL_texture "/sdcard/ol/media/field-of-view.jpg" SOIL_LOAD_RGBA SOIL_CREATE_NEW_ID 0))
(define background
   (let ((file (file->bytevector "media/wood.jpg")))
      (SOIL_load_OGL_texture_from_memory file (size file) SOIL_LOAD_RGBA SOIL_CREATE_NEW_ID 0)))

;; TODO: переделать все для GL_OVR_multiview
;; README: https://arm-software.github.io/opengl-es-sdk-for-android/multiview.html
;; usage sample: /home/uri/Workspace/oculus/ovr_sdk_mobile_1.29.0/VrSamples/VrCubeWorld_NativeActivity/Src/VrCubeWorld_NativeActivity.c

; Oculus Go Screen:
; 2560x1440 (538ppi)
;     4,758364312 inch x 2,676579926 inch
;     0,120862454 cm   x 0,06798513
; 1 inch = 0.0254 meters (or divide by 39.37)
; http://www.sitesinvr.com/viewer/settings.htm
(gl:set-window-size 1280 720)

; screen size in pixels
(define hResolution (gl:get-window-width))
(define vResolution (gl:get-window-height))

(define RL_CULL_DISTANCE_NEAR 0.01)
(define RL_CULL_DISTANCE_FAR 1000.0)
(define DEG2RAD (/ #i3.14159265358979323846 #i180.0))

(define hScreenSize #i0.12086245) ; meters
(define vScreenSize #i0.06798513) ; meters
(define lensSeparationDistance #i0.0604) ; почти точно половина ширины

;(define eyeToScreenDistance #i0.011) ; fov = 2atan(vScreenSize/2/eyeToScreenDistance)
(define fovy 80.0)   ; найдено экспериментально



(define interpupillaryDistance #i0.07) ; межзрачковое расстояние


                            ;[#i1.0 #i0.22 #i0.24 0])
(define lensDistortionValues [#i1.0 #i0.11 #i0.26 0])
;(define lensDistortionValues [#i1.0 #i0.0 #i0.0 0])
(define chromaAbCorrection [#i0.996 #i-0.004 #i1.014 0])

; LoadVrStereoConfig:
; Compute aspect ratio
(define aspect (/ (/ hResolution 2) vResolution))

; Compute lens parameters
(define lensShift (/ (- (/ hScreenSize 4) (/ lensSeparationDistance 2)) hScreenSize))

(define leftLensCenter    [(+ 1/4 lensShift) 1/2])
(define rightLensCenter   [(- 3/4 lensShift) 1/2])
(define leftScreenCenter  [   1/4            1/2])
(define rightScreenCenter [   3/4            1/2])

; Compute distortion scale parameters
; NOTE: To get lens max radius, lensShift must be normalized to [-1..1]
(define lensRadius (abs (- -1 (* -4 lensShift))))
(define lensRadiusSq (* lensRadius lensRadius))
(define distortionScale (+
      (ref lensDistortionValues 1)
   (* (ref lensDistortionValues 2) lensRadiusSq)
   (* (ref lensDistortionValues 2) lensRadiusSq lensRadiusSq)
   (* (ref lensDistortionValues 2) lensRadiusSq lensRadiusSq lensRadiusSq)))
(define normScreenWidth 1/2)
(define normScreenHeight 1)

(define scaleIn [(/ 2 normScreenWidth)
                 (/ 2 normScreenHeight aspect)])
(define scale [(/ (* normScreenWidth 1/2) distortionScale)
               (/ (* normScreenHeight 1/2 aspect) distortionScale)])

; Fovy is normally computed with: 2*atan2f(device.vScreenSize, 2*device.eyeToScreenDistance)
; ...but with lens distortion it is increased (see Oculus SDK Documentation)
;; (define fovy 90.0); (* 2 (atan (* vScreenSize 1/2) eyeToScreenDistance))) ; Really need distortionScale?
;; (print "fovy: " fovy)

;; ; Compute camera projection matrices
;; (define projection [
;;    '(#i1 #i0 #i0 #i0
;;      #i0 #i1 #i0 #i0
;;      #i0 #i0 #i1 #i0
;;      #i0 #i0 #i0 #i1)
;;    '(#i1 #i0 #i0 #i0
;;      #i0 #i1 #i0 #i0
;;      #i0 #i0 #i1 #i0
;;      #i0 #i0 #i0 #i1) ])
(define projOffset (* lensShift 4))

;; (glMatrixMode GL_PROJECTION)
;; (glLoadIdentity) ; left
;; (gluPerspective fovy aspect RL_CULL_DISTANCE_NEAR RL_CULL_DISTANCE_FAR)
;; (glTranslatef projOffset 0 0)
;; (glGetDoublev GL_PROJECTION_MATRIX (ref projection 1))

;; (glLoadIdentity) ; right
;; (gluPerspective fovy aspect RL_CULL_DISTANCE_NEAR RL_CULL_DISTANCE_FAR)
;; (glTranslatef (- projOffset) 0 0)
;; (glGetDoublev GL_PROJECTION_MATRIX (ref projection 2))

(define viewOffset [
   '(#i1 #i0 #i0 (* interpupillaryDistance #i-0.5)
     #i0 #i1 #i0 #i0.075
     #i0 #i0 #i1 #i0.045
     #i0 #i0 #i0 #i1.000)
   '(#i1 #i0 #i0 (* interpupillaryDistance #i+0.5)
     #i0 #i1 #i0 #i0.075
     #i0 #i0 #i1 #i0.045
     #i0 #i0 #i0 #i1.000) ])

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
         gl_FragColor = vec4(0.3, 0.3, 0.3, 1.0);
      }
      else {
#if 1 // временно отключим дисторшен
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
(glUniform4fv (glGetUniformLocation distortion "deviceWarpParam") 1 lensDistortionValues)
(glUniform4fv (glGetUniformLocation distortion "chromaAbParam") 1 chromaAbCorrection)

(print "leftLensCenter: " (vector-map inexact leftLensCenter))
; ----------------------------------------------------
; Create render target texture
(import (OpenGL ARB framebuffer_object))

(define TEXW (gl:get-window-width))
(define TEXH (gl:get-window-height))
; texture2d
(define texture '(0))
(glGenTextures (length texture) texture)

(glBindTexture GL_TEXTURE_2D (car texture))
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
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
(define position '(0 0 0))
(define target   '(0 0 -1)) ; вперед...
(define up       '(0 1 0))
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


; init
(glShadeModel GL_SMOOTH)

(define started (let*((ss ms (clock))) (cons ss ms)))

(gl:set-renderer (lambda ()
   (define now (let*((ss ms (clock))) (cons ss ms)))
   ;(define delta (* 0.5 (sin (/ (+ (* 1000 (- (car now) (car started))) (- (cdr now) (cdr started))) 500))))
   (define delta (let*((ss ms (clock)))
         (+ (* (mod ss 1000) 1000) ms)))

   (define (draw)
      (glPushMatrix)
      (glTranslatef 0 0 -10)
      (draw-plane)
      (glPopMatrix)

      (glPushMatrix)
      (glScalef 2 2 1)
      (glTranslatef 0 -1 -12)
      (draw-plane)
      (glPopMatrix)

      ;; (glColor3f 0.2 0.5 0.2)
      ;; (glDisable GL_TEXTURE_2D)
      ;; ; нарисуем много линий
      ;; (glBegin GL_TRIANGLES)
      ;;    (glColor3f 1 0 0)
      ;;    (glVertex2f -0.6 -0.6)

      ;;    (glColor3f 0 1 0)
      ;;    (glVertex2f +0.6 -0.6)

      ;;    (glColor3f 0 0 1)
      ;;    (glVertex2f -0.0 +0.7)
      ;; (glEnd)
      (glFlush))

   (glUseProgram 0)
   (glBindFramebuffer GL_FRAMEBUFFER (car framebuffer))
   (glClearColor 0 0.2 0.2 1)
   (glEnable GL_DEPTH_TEST)

   (glViewport 0 0 (gl:get-window-width) (gl:get-window-height))
   (glClear (vm:ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   (define W (/ (gl:get-window-width) 2))

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
   (glViewport 0 0 W (gl:get-window-height))

   ; камера влияет только на проецирование
   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity) ; left
   (gluPerspective fovy aspect RL_CULL_DISTANCE_NEAR RL_CULL_DISTANCE_FAR)
   ;; (glTranslatef (- projOffset) 0 0)

   ; временно по иксу туда-сюда
   (define xyz (list (/ interpupillaryDistance 2) 0 0))

   ; а вод моделвью - 
   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   ; todo: векторное произведение верха и вперед
   ;; (apply gluLookAt (append position target up))
   (apply gluLookAt (append 
      (map - position xyz) target up))

   ;; (glRotatef (/ delta 30) 0 0 1)
   (draw)

   ; right eye
   (glViewport W 0 W (gl:get-window-height))

   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity) ; left
   (gluPerspective fovy aspect RL_CULL_DISTANCE_NEAR RL_CULL_DISTANCE_FAR)
   ;; (glTranslatef (+ projOffset) 0 0)
   ;; (glLoadIdentity)
   ;; (glLoadMatrixf (ref projection 2))

   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   ;; (apply gluLookAt (append position target up))
   (apply gluLookAt (append 
      (map + position xyz) target up))

   ;; (glRotatef (/ delta 30) 0 0 1)
   (draw)

      ; ...
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
