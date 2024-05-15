#!/usr/bin/env ol

; let's create image buffer
(import (scheme inexact))
(define plasma
   (fold append #null
      (map (lambda (y)
            (map (lambda (x)
                  (let ((value (/
                           (+ (sin (/ y 4))
                              (sin (/ (+ x y) 8))
                              (sin (/ (sqrt (+ (* x x) (* y y))) 8))
                              4) 8)))
                     value))
               (iota 256)))
         (iota 256))))

; render
(import (lib gl-1))
(gl:set-window-size 256 256)

(glBindTexture GL_TEXTURE_2D 0)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
(glTexImage2D GL_TEXTURE_2D 0 GL_LUMINANCE
   256 256
   0 GL_LUMINANCE GL_FLOAT (cons (fft* fft-float) plasma))

(glEnable GL_TEXTURE_2D)

(gl:set-renderer (lambda (_)
   (glClear GL_COLOR_BUFFER_BIT)
   (glBegin GL_QUADS)
      (glTexCoord2f 0 0)
      (glVertex2f -1 -1)
      (glTexCoord2f 0 1)
      (glVertex2f -1 1)
      (glTexCoord2f 1 1)
      (glVertex2f 1 1)
      (glTexCoord2f 1 0)
      (glVertex2f 1 -1)
   (glEnd)))
