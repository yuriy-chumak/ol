#!/usr/bin/env ol

(import (lib sdl3lite))
(import (OpenGL 1.2))

; -- main ----------------
(define WINDOW_WIDTH  640)
(define WINDOW_HEIGTH 480)

(unless (SDL_Init SDL_INIT_VIDEO)
   (SDL_Log "Init error: %s\n" (SDL_GetError))
   (exit 1))

(define window (SDL_CreateWindow "OpenGL1" WINDOW_WIDTH WINDOW_HEIGTH SDL_WINDOW_OPENGL))
(unless window
   (SDL_Log "Create window error: %s\n" (SDL_GetError))
   (exit 1))

(define context (SDL_GL_CreateContext window))
(unless context
   (SDL_Log "Create context error: %s\n" (SDL_GetError))
   (exit 1))

(let loop ()
   (define event (make-SDL_Event))
   (define done (let loop ((done #false))
      (if (= (SDL_PollEvent event) 0)
         done
         (loop (or done (= (SDL_Event->type event) SDL_EVENT_QUIT))))))

   (glClear GL_DEPTH_BUFFER_BIT)

   (glBegin GL_POLYGON)
   (glColor3f 1.0 0.0 0.0)  (glVertex3f -0.6 -0.75 0.5)
   (glColor3f 0.0 1.0 0.0)  (glVertex3f  0.6 -0.75 0.0)
   (glColor3f 0.0 0.0 1.0)  (glVertex3f  0.0  0.75 0.0)
   (glEnd)

   (SDL_GL_SwapWindow window)
   (unless done (loop)))

(SDL_GL_DestroyContext context)
(SDL_DestroyWindow window)
(SDL_Quit)
