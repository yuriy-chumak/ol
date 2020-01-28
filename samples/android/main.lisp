#!/usr/bin/ol

; -----------------------------------------------------------------
; зададим конфигурацию графического окна
(define-library (lib gl config)
(export config) (import (otus lisp))
(begin
   (define config (pairs->ff `(
      ; размеры окна в знакоместах:
      ; напомню, что мы используем фиксированный шрифт размера 9*16
      (width . ,(* 80 9))
      (height . ,(* 16 25)))))))

(import (lib gl))
(import (otus random!))

; Cross-OS trick (use different opengl's for different OSes):
(define-library (lib gl common)
   (import (scheme core))
   (export version)
   (cond-expand
      (Android
         (import (OpenGL ES version-1-1))
         (export (exports (OpenGL ES version-1-1))))
      (else
         (import (OpenGL version-1-1))
         (export (exports (OpenGL version-1-1)))))
   (begin (define version "1.0")))
(import (lib gl common))

; рендерер:
(gl:set-renderer (lambda (mouse)
   (glClearColor 0.2 0.2 0.2 1)
   (glClear GL_COLOR_BUFFER_BIT)

   (glClear GL_COLOR_BUFFER_BIT)

   (glColor3f 0.2 0.5 0.2)
   (glBegin GL_TRIANGLES)
      (glColor3f 1 0 0)
      (glVertex2f -0.6 -0.6)

      (glColor3f 0 1 0)
      (glVertex2f +0.6 -0.6)

      (glColor3f 0 0 1)
      (glVertex2f -0.0 +0.7)
   (glEnd)
))
