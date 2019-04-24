#!/usr/bin/ol
(import (lib gl))
(gl:set-window-title "1. Creating an OpenGL 3.0 Window")

(gl:set-context-version 3 0) ; use OpenGL version 3.0
(import (OpenGL version-3-0))

; let's check context version
(define major (box 0))
(define minor (box 0))
(glGetIntegerv GL_MAJOR_VERSION major)
(glGetIntegerv GL_MINOR_VERSION minor)
(print "Context version: " (unbox major) "." (unbox minor))
(print "OpenGL version: " (glGetString GL_VERSION))

; init
(glShadeModel GL_SMOOTH)
(glClearColor 0.3 0.3 0.3 1)

; draw loop
(gl:set-renderer (lambda (mouse)
   (glClear GL_COLOR_BUFFER_BIT)))
