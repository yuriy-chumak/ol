#!/usr/bin/env ol

(import (otus ffi))
(import (lib sdl2))

; test:
; ***************************************************
(if (< (SDL_Init SDL_INIT_VIDEO) 0)
   (begin
      (print "Unable to Init SDL: " (SDL_GetError))
      (halt 1)))

;; (unless (eq? (IMG_Init IMG_INIT_PNG) IMG_INIT_PNG)
;;    (print "Unable to init SDL png image support: " (SDL_GetError))
;;    (halt 1))

(define window (SDL_CreateWindow "Create SDL2 Window sample"
   SDL_WINDOWPOS_UNDEFINED SDL_WINDOWPOS_UNDEFINED
   854 480 SDL_WINDOW_OPENGL))

(define context (SDL_GL_CreateContext window))
(print "context: " context)

;; (define renderer (SDL_CreateRenderer window -1 (bor SDL_RENDERER_ACCELERATED SDL_RENDERER_PRESENTVSYNC)))

(define picture
   (append
      (fold append #null
         (map (lambda (y)
               (fold append #null
               (map (lambda (x)
                     (list 1.0 0.0 0.0 1.0))
                  (iota 256))))
            (iota 128)))
      (fold append #null
         (map (lambda (y)
               (fold append #null
               (map (lambda (x)
                     (list 0.0 1.0 0.0 1.0))
                  (iota 256))))
            (iota 128)))))

(import (OpenGL version-1-2))

(define GL_RGBA16F #x881A)

(glBindTexture GL_TEXTURE_2D 0)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
(glTexImage2D GL_TEXTURE_2D 0 GL_RGBA16F
   256 256
   0 GL_RGBA GL_FLOAT (cons (fft* fft-float) picture))


(call/cc (lambda (break)
   (let loop ()
      (let ((event (make-SDL_Event)))
         (let event-loop ()
            (unless (eq? (SDL_PollEvent event) 0)
               (let ((type (bytevector->int32 event 0)))
                  (case type
                     (SDL_QUIT (break))
                     (else
                        #f))
                  (event-loop)))))

      (glViewport 0 0 854 480)
      (glClearColor 1 0 1 1)
      (glClear GL_COLOR_BUFFER_BIT)

      (glEnable GL_TEXTURE_2D)
      (glBegin GL_QUADS)
         (glTexCoord2f 0 0)
         (glVertex2f -1 -1)
         (glTexCoord2f 0 1)
         (glVertex2f -1 1)
         (glTexCoord2f 1 1)
         (glVertex2f 1 1)
         (glTexCoord2f 1 0)
         (glVertex2f 1 -1)
      (glEnd)

      (SDL_GL_SwapWindow window)
      (loop))))


;(SDL_DestroyRenderer renderer)
;(SDL_DestroyWindow window)
;(SDL_Quit)

(print "done.")
