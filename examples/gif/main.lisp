#!/usr/bin/env ol
(import (lib gl))
(gl:set-window-title "Gif Decoder")

(import (OpenGL 1.1))
(import (lib soil))

; init
(glShadeModel GL_SMOOTH)
(glClearColor 0.1 0.1 0.1 1)

(import (file gif))
(define id 0)

; draw
(gl:set-renderer (lambda ()
   (glClearColor 0.2 0.2 0.2 1)
   (glClear GL_COLOR_BUFFER_BIT)

   (glEnable GL_TEXTURE_2D)

   (glBindTexture GL_TEXTURE_2D id)
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)

   (glColor3f 1 1 1)
   (glBegin GL_QUADS)
      (glTexCoord2f 0 0)
      (glVertex2f -1  1)
      (glTexCoord2f 0 1)
      (glVertex2f -1 -1)
      (glTexCoord2f 1 1)
      (glVertex2f  1 -1)
      (glTexCoord2f 1 0)
      (glVertex2f  1  1)
   (glEnd)

))

(call/cc (lambda (return)
(let loop ((n 0))
   (define-values (filename name)
      (case n
         (0 (values "splash.gif" "splash.gif"))
         (1 (values "green-256.gif" "green [256 x 256]"))
         (else
            (return (gl:set-window-title "no more images")))))
   (gl:set-window-title (string-append "Loading " name "..."))
   (define gif (read-gif-stream (file->bytestream filename)))

   (define colors (gif->rgb24 gif))
   (glTexImage2D GL_TEXTURE_2D 0 GL_RGB
      (gif 'width) (gif 'height)
      0 GL_RGB GL_UNSIGNED_BYTE colors)
   (gl:set-window-title name)

   (wait 3000)
   (loop (+ n 1)))))
