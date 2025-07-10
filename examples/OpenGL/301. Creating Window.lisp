#!/usr/bin/env ol
(import (lib gl 3.1))
(gl:set-window-title "1. Creating an OpenGL 3.1 Window")

; let's check context version
(define major (box 0))
(define minor (box 0))
(glGetIntegerv GL_MAJOR_VERSION major)
(glGetIntegerv GL_MINOR_VERSION minor)
(print "Context version: " (unbox major) "." (unbox minor))
(print "OpenGL version: " (glGetString GL_VERSION))

; init
(glClearColor 0.3 0.3 0.3 1)

; draw loop
(gl:set-renderer (lambda ()
   (glClear GL_COLOR_BUFFER_BIT)))
