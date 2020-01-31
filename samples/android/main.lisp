#!/usr/bin/ol

; ----------------------------------
; зададим размеры графического окна
(define-library (lib gl config)
(export config) (import (otus lisp))
(begin
   (define config {
      ; напомню, что мы используем фиксированный шрифт размера 9*16
      'width  (* 1  9 80)      ; 80 знакомест в ширину
      'height (* 1 16 25)      ; 25 знакомест в высоту
      'scale  32               ; шкала увеличения
   })))
(import (lib gl config))

; --------------------------------
; .. и тут же его (окно) создадим
(import (lib gl))
(gl:set-window-title "The House of the Rising Sun")

; -------------------------------------------------
; а теперь нарисуем сплеш и можем спокойно грузить уровень
(import (otus ffi))
(import (lib soil))

(import (OpenGL version-1-1))

(glOrtho 0 1 1 0 0 1)

; поищем наш файл в папках
(define id
   (let ((file (file->bytevector "splash.png")))
      (SOIL_load_OGL_texture_from_memory file (size file) SOIL_LOAD_RGBA SOIL_CREATE_NEW_ID 0)))
(glEnable GL_TEXTURE_2D)
(glBindTexture GL_TEXTURE_2D id)
(glBegin GL_TRIANGLES)
   ; рисуем на весь экран квадратик с текстурой
   (for-each (lambda (xy)
         (glTexCoord2f (car xy) (cdr xy))
         (glVertex2f (car xy) (cdr xy)))
      '((0 . 0) (1 . 0) (1 . 1)
        (0 . 0) (1 . 1) (0 . 1)))
(glEnd)
;(glDisable GL_TEXTURE_2D)
(gl:SwapBuffers (interact 'opengl ['get 'context])) ; todo: make a function
;(glDeleteTextures 1 (list id)) ; и спокойно удалим сплеш текстуру

(print "ok.")
; ----------------------------------------------------------
; включим для windows VSYNC,
; для linux он включен по умолчанию 
;
(define-library (---)
(export enable-vsync)
(import (scheme core))
(cond-expand
   (Windows
      (import (OpenGL WGL EXT swap_control))
      (begin
         (define (enable-vsync)
            (if WGL_EXT_swap_control
               (wglSwapIntervalEXT 1))))) ; enable vsync
   (else
      (begin
         (define (enable-vsync) #false)))))
(import (---))
(enable-vsync)

(gl:set-renderer (lambda (mouse)
   (glClearColor 0.4 0.0 0.0 1)
   (glClear GL_COLOR_BUFFER_BIT)

   (glBindTexture GL_TEXTURE_2D id)
   (glBegin GL_TRIANGLES)
      (for-each (lambda (xy)
            (glTexCoord2f (car xy) (cdr xy))
            (glVertex2f (car xy) (cdr xy)))
         '((0 . 0) (1 . 0) (1 . 1)
           (0 . 0) (1 . 1) (0 . 1)))
   (glEnd)
))