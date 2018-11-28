#!/usr/bin/ol

(define x '(0 1 2 3 4 5 6 7 8 9))
(define y '(2.7 2.8 31.4 38.1 58.0 76.2 100.5 130.0 149.3 180.0))


; render
(import (lib gl))
(import (OpenGL version-1-1))
(glOrtho 0 10 0 200 0 1)

(gl:set-renderer (lambda (mouse)
   (glClear GL_COLOR_BUFFER_BIT)
   (glColor3f 0 1 0)
   (glBegin GL_LINE_STRIP)
      (map (lambda (x y)
            (glVertex2f x y))
         x y)
   (glEnd)
   #null))
