#!/usr/bin/env ol

(import (lib sdl3lite))
(import (OpenGL 1.2))

(define (display)
   (glClear GL_COLOR_BUFFER_BIT)

   (glColor3f 1.0 1.0 1.0)
   (glBegin GL_LINES)

   (for-each (lambda (i)
         (glVertex3f i 0  2.5)
         (glVertex3f i 0 -2.5)
         (glVertex3f  2.5 0 i)
         (glVertex3f -2.5 0 i) )
      (lrange -2.5 0.25 2.51))
   (glEnd)

   (glBegin GL_TRIANGLE_STRIP)
   (glColor3f 1.0 1.0 1.0)  (glVertex3f  0.0 2.0  0.0)
   (glColor3f 1.0 0.0 0.0)  (glVertex3f -1.0 0.0  1.0)
   (glColor3f 0.0 1.0 0.0)  (glVertex3f  1.0 0.0  1.0)
   (glColor3f 0.0 0.0 1.0)  (glVertex3f  0.0 0.0 -1.4)
   (glColor3f 1.0 1.0 1.0)  (glVertex3f  0.0 2.0  0.0)
   (glColor3f 1.0 0.0 0.0)  (glVertex3f -1.0 0.0  1.0)
   (glEnd)
)

(define (init)
   (glClearColor 0.1 0.39 0.88 1)
   (glColor3f 1.0 1.0 1.0)

   (glEnable GL_CULL_FACE)
   (glCullFace GL_BACK)

   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity)
   (glFrustum -2 2 -1.5 1.5 1 40)

   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (glTranslatef 0 0 -3)
   (glRotatef 50 1 0 0)
   (glRotatef 70 0 1 0)
)

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

(init)

(let loop ()
   (define event (make-SDL_Event))
   (define done (let loop ((done #false))
      (if (= (SDL_PollEvent event) 0)
         done
         (loop (or done (= (SDL_Event->type event) SDL_EVENT_QUIT))))))
   (display)
   (SDL_GL_SwapWindow window)
   (unless done (loop)))

(print "ok")
