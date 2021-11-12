#!/usr/bin/env ol

;; initialize OpenGL
(import (lib gl))
(gl:set-window-title "4. reference-frames.lisp")
(import (OpenGL version-2-1))
; todo: splash screen

;; load and compile all models
(import (scene))

; загрузить нужные модели
(define models (prepare-models "Mill"))
(print models)

;; load a scene
(import (file json))
(define scene (read-json-file "scene1.json"))

;; draw

(import (lib math))
(import (owl math fp))

; настройки
(glShadeModel GL_SMOOTH)
(glClearColor 0.3 0.3 0.3 1)

(glMatrixMode GL_PROJECTION)
(glLoadIdentity)
(gluPerspective 45 (/ 854 480) 0.1 100)

(glEnable GL_DEPTH_TEST)

(glEnable GL_LIGHTING)
(glLightModelf GL_LIGHT_MODEL_TWO_SIDE GL_TRUE)
(glEnable GL_NORMALIZE)

(glEnable GL_LIGHT0)

(glEnable GL_LIGHTING)
(glLightfv GL_LIGHT0 GL_POSITION '(7.0 7.0 7.0 0.0))

; draw
(gl:set-renderer (lambda (mouse)
   (glClear (vm:ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (gluLookAt -5 -9 5
      0 0 2
      0 0 1)

   ; set and show lighting point
   (glDisable GL_LIGHTING)
   (let*((ss ms (clock))
         (x (* 5 (sin (+ ss (/ ms 1000)))))
         (y (* 5 (cos (+ ss (/ ms 1000))))) ;(+ 1 (* 3 (sin (/ (+ ss (/ ms 1000)) 8)))))
         (z 2)) ;(* 3 (cos (+ ss (/ ms 1000))))))
      (glPointSize 5)
      (glBegin GL_POINTS)
      (glColor3f #xff/255 #xbf/255 0)
      (glVertex3f x y z)
      (glEnd)
      
      (glLightfv GL_LIGHT0 GL_POSITION (list x y z 1)))
   (glEnable GL_LIGHTING)
      
   (draw-geometry scene models)
))
