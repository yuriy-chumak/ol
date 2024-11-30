#!/usr/bin/env ol

(import (lib gl-2))
(import (lib gl-2 vr))
(gl:enable-vr 'Oculus)

(import (lib soil))
(define (file->bytevector path)
   (define (sys:read fd maxlen)
      (syscall 0 fd maxlen))
   (define stat (syscall 4 (c-string path)))
   (when stat
      (let*((port (open-binary-input-file path))
            (file (sys:read port (ref stat 8))))
         (close-port port)
      file)))

,load "cube.lisp"

(define program (gl:create-program
"#version 120 // OpenGL 2.1

varying vec2 st;
void main()
{
   gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
   gl_FrontColor = gl_Color;
   st = gl_MultiTexCoord0.st;
}"

"#version 120 // OpenGL 2.1
uniform sampler2D tex0;

varying vec2 st;
void main()
{
   gl_FragColor = texture2D(tex0, st);
}"))

; init
(glEnable GL_BLEND)
(glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
(glEnable GL_DEPTH_TEST)

(glClearColor 0 0 0 1.0)

; draw
(import (lib GLU))
(define old (time-ms))
(gl:set-renderer (lambda (mouse options)
   (glClear (vm:ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   (define FOVY (if (options 'eye #f) 90.0 45.0)) ;; typical VR FOV is 90, otherwise 45
   (define ASPECT (/ (gl:get-window-width) (gl:get-window-height)))

   ; --------------------------
   ; projection matrix
   (glMatrixMode GL_PROJECTION)
   ;non-vr projection matrix (todo: use own math)
   (define projection-matrix [
      #i1 #i0 #i0 #i0
      #i0 #i1 #i0 #i0
      #i0 #i0 #i1 #i0
      #i0 #i0 #i0 #i1
   ])

   (glLoadIdentity)
   (gluPerspective FOVY ASPECT 0.1 1000)
   (glGetFloatv GL_PROJECTION_MATRIX projection-matrix)
   ; try to get vr matrix if we use vr mode
   (glGetFloatv VR_PROJECTION_MATRIX projection-matrix)
   ; and set the final matrix
   (glLoadMatrixf projection-matrix)

   ; --------------------------
   ; projection matrix
   (glMatrixMode GL_MODELVIEW)
   ;non-vr modelview matrix
   (define modelview-matrix [
      #i1 #i0 #i0 #i0
      #i0 #i1 #i0 #i0
      #i0 #i0 #i1 #i0
      #i0 #i0 #i0 #i1
   ])
   (glLoadIdentity)
   (define Y -2)
   (define R -5)
   (gluLookAt 0 0 0  ; eye
              0 Y R  ; center
              0 1 0) ; up
   (glGetFloatv GL_MODELVIEW_MATRIX modelview-matrix)
   ; try to get vr matrix if we use vr mode
   (glGetFloatv VR_MODELVIEW_MATRIX modelview-matrix)
   ; and set the final matrix
   (glLoadMatrixf modelview-matrix)

   (define t (/ (mod (time-ms) 6283) #i1000))
   (glTranslatef 0 -3 -8)
   (glRotatef (* t 360/3.14) 0 1 0)
   (cube:draw)
))
