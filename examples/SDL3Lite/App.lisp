#!/usr/bin/env ol

(import (lib sdl3lite))

(define WINDOW_WIDTH  640)
(define WINDOW_HEIGTH 480)

(import (scheme dynamic-bindings))
(define Window (make-parameter #f))
(define Renderer (make-parameter #f))

(define (SDL_AppInit appstate argc argv)
   (call/cc (lambda (return)
      (unless (SDL_Init SDL_INIT_VIDEO)
         (SDL_Log "Init error: %s\n" (SDL_GetError))
         (return SDL_APP_FAILURE))
   
      (define window (SDL_CreateWindow "Renderer" WINDOW_WIDTH WINDOW_HEIGTH SDL_WINDOW_OPENGL))
      (unless window
         (SDL_Log "Create window error: %s\n" (SDL_GetError))
         (return SDL_APP_FAILURE))
      (Window window)

      (define renderer (SDL_CreateRenderer window #f))
      (unless renderer
         (SDL_Log "Create renderer error: %s\n" (SDL_GetError))
         (return SDL_APP_FAILURE))
      (Renderer renderer)

      SDL_APP_CONTINUE
)))

(define (SDL_AppEvent appstate event)
   (if (= (SDL_Event->type event) SDL_EVENT_QUIT)
      SDL_APP_SUCCESS
      SDL_APP_CONTINUE))

(define (SDL_AppIterate appstate)
   (define renderer (Renderer))
   (SDL_RenderClear renderer)
   (SDL_RenderPresent renderer)

   SDL_APP_CONTINUE)

(define (SDL_AppQuit appstate result)
   (SDL_DestroyRenderer (Renderer))
   (SDL_DestroyWindow (Window))

   (SDL_Quit))

; -- main ---------
,load "SDL_main.scm"
