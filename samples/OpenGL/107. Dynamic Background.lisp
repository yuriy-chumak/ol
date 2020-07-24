#!/usr/bin/ol
(import (lib gl))
(gl:set-window-title "7. Dynamic Background")

(import (OpenGL version-1-1))
(import (lib soil))
(import (lib gl console))
(import (lib keyboard))

; init
(glShadeModel GL_SMOOTH)
(glClearColor 0.1 0.1 0.1 1)

(glEnable GL_BLEND)
(glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)

(glEnable GL_TEXTURE_2D)

(define root "resources/Glitch parallex BG sampler/GA9NRNJSB792OG4 Akaki Cape-shows depth/")
(define background (list
   (SOIL_load_OGL_texture (string-append root "Gradient (z -6).png") SOIL_LOAD_RGBA SOIL_CREATE_NEW_ID 0)
   (SOIL_load_OGL_texture (string-append root "mtn (z -4).png") SOIL_LOAD_RGBA SOIL_CREATE_NEW_ID 0)
   (SOIL_load_OGL_texture (string-append root "sky (z -3).png") SOIL_LOAD_RGBA SOIL_CREATE_NEW_ID 0)
   (SOIL_load_OGL_texture (string-append root "bg_2 (z -2).png") SOIL_LOAD_RGBA SOIL_CREATE_NEW_ID 0)
   (SOIL_load_OGL_texture (string-append root "bg_1 (z -1).png") SOIL_LOAD_RGBA SOIL_CREATE_NEW_ID 0)
   (SOIL_load_OGL_texture (string-append root "middleground (z 0).png") SOIL_LOAD_RGBA SOIL_CREATE_NEW_ID 0)
   (SOIL_load_OGL_texture (string-append root "middleplus (z 1).png") SOIL_LOAD_RGBA SOIL_CREATE_NEW_ID 0)
))

(define scale 2618/571) ; magic "average aspect ratio" number
(define window (list 0 0))
(gl:set-expose-handler (lambda (x y width height)
   (set-ref! window 1 width)
   (set-ref! window 2 height)
   (glViewport x y width height)))

(define left (box 0)) ; left border position
(define speed 240) ; magic "slowdown" number

; onboard helper text
(define helper (create-window 3 3 20 4))

(set-window-writer helper (lambda (print)
   (print WHITE "Press [left arrow] or [right arrow] to move the picture.")
))
; draw
(gl:set-renderer (lambda (mouse)
   (glClear GL_COLOR_BUFFER_BIT)
   (glLoadIdentity)
   (glOrtho 0 1 0 1 0 1)

   (glEnable GL_BLEND)
   (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
   (glEnable GL_TEXTURE_2D)
   (glColor3f 1 1 1)

   (define width (ref window 1))
   (define height (ref window 2))

   (for-each (lambda (texture multiplier)
         (define x (* (/ (unbox left) speed) multiplier))

         (glBindTexture GL_TEXTURE_2D texture)
         (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
         (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
         (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
         (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT)
          
         (glBegin GL_QUADS)
            (for-each (lambda (xy st)
                  (glTexCoord2f (car st) (cdr st))
                  (glVertex2f (car xy) (cdr xy)))
               '((0 . 0) (1 . 0) (1 . 1) (0 . 1))
               `((,x . 1) (,(+ x (/ width height scale)) . 1) (,(+ x (/ width height scale)) . 0) (,x . 0)))
         (glEnd))
      background
      '(1/6 1/4 1/3 1/2 1/1 1/1 2/1))

   (render-windows)
))

(gl:set-keyboard-handler (lambda (key)
   ;; (print "key: " key)
   (case key
      (113
         (set-car! left (- (car left) 1)))
      (114
         (set-car! left (+ (car left) 1)))
      (9
         (halt 1))))) ; esc - quit
