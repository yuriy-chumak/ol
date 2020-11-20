#!/usr/bin/ol
(import (otus ffi))

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

(import (lib gl))
(import (OpenGL version-1-1))  ; will use simplest opengl for now

;; (gl:set-window-title "The House of the Rising Sun")

; -------------------------------------------------
; а теперь нарисуем сплеш и можем спокойно грузить уровень
(import (otus random!))
(import (lib soil))

; поищем наш файл в папках
(define id
   (let ((file (file->bytevector "splash.png")))
      (SOIL_load_OGL_texture_from_memory file (size file) SOIL_LOAD_RGBA SOIL_CREATE_NEW_ID 0)))
(glEnable GL_TEXTURE_2D)
(glBindTexture GL_TEXTURE_2D id)
(glOrtho 0 1 1 0 0 1)
(glBegin GL_TRIANGLES)
   ; рисуем на весь экран квадратик с текстурой
   (for-each (lambda (xy)
         (glTexCoord2f (car xy) (cdr xy))
         (glVertex2f (car xy) (cdr xy)))
		'((0 . 0) (0 . 1) (1 . 1)
		  (0 . 0) (1 . 1) (1 . 0)))
(glEnd)
(glFinish)

;; ;; (gl:SwapBuffers (interact 'opengl ['get 'context])) ; todo: make a function
;; ;; (glDeleteTextures 1 (list id)) ; и спокойно удалим сплеш текстуру

;; ; ----------------------------------------------------------
;; ; включим для windows VSYNC,
;; ; для linux он включен по умолчанию 
;; ;
;; ;; (define-library (---)
;; ;; (export enable-vsync)
;; ;; (import (scheme core))
;; ;; (cond-expand
;; ;;    (Windows
;; ;;       (import (OpenGL WGL EXT swap_control))
;; ;;       (begin
;; ;;          (define (enable-vsync)
;; ;;             (if WGL_EXT_swap_control
;; ;;                (wglSwapIntervalEXT 1))))) ; enable vsync
;; ;;    (else
;; ;;       (begin
;; ;;          (define (enable-vsync) #false)))))
;; ;; (import (---))
;; ;; (enable-vsync)

(gl:set-renderer (lambda (mouse)
   (glClearColor (/ (rand! 256) 256) (/ (rand! 256) 256) (/ (rand! 256) 256) 1)
   (glClear GL_COLOR_BUFFER_BIT)

	(glColor3f 1 1 1)
   (glBegin GL_TRIANGLES)
      ; рисуем на весь экран квадратик с текстурой
		(for-each (lambda (xy)
				(glTexCoord2f (car xy) (cdr xy))
				(glVertex2f (car xy) (cdr xy)))
			'((0 . 0) (0 . 1) (1 . 1)
			  (0 . 0) (1 . 1) (1 . 0)))
   (glEnd)
   ;; ;; (glDisable GL_TEXTURE_2D)

	(glFinish)))