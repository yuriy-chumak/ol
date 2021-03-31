#!/usr/bin/ol

(import (otus ffi))
(import (lib sdl2))

; test:
; ***************************************************
(if (< (SDL_Init SDL_INIT_VIDEO) 0)
   (begin
      (print "Unable to Init SDL: " (SDL_GetError))
      (halt 1)))

(unless (eq? (IMG_Init IMG_INIT_PNG) IMG_INIT_PNG)
   (print "Unable to init SDL png image support: " (SDL_GetError))
   (halt 1))

(define window (SDL_CreateWindow "Create SDL2 Window sample"
   SDL_WINDOWPOS_UNDEFINED SDL_WINDOWPOS_UNDEFINED
   854 480 SDL_WINDOW_SHOWN))

(define renderer (SDL_CreateRenderer window -1 (bor SDL_RENDERER_ACCELERATED SDL_RENDERER_PRESENTVSYNC)))

(define picture (IMG_Load "SDL_logo.png"))
(define texture (SDL_CreateTextureFromSurface renderer picture))
(SDL_FreeSurface picture)

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
      (SDL_RenderClear renderer)
      (SDL_RenderCopy renderer texture #f #f)
      (SDL_RenderPresent renderer)
      (loop))))


;(SDL_DestroyRenderer renderer)
;(SDL_DestroyWindow window)
;(SDL_Quit)

(print "done.")
