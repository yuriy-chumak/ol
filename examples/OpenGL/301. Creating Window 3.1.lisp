#!/usr/bin/env ol

; create OpenGL window
(import (lib gl))
(gl:set-window-title "1. Creating an OpenGL 3.1 Window")

; upgrade context to 3.1
(import (lib gl 3 context))
(gl:set-context-version 3 1)

; import OpenGL functions
(import (OpenGL 3.1))

; let's check context version
(define major (box 0))
(define minor (box 0))
(glGetIntegerv GL_MAJOR_VERSION major)
(glGetIntegerv GL_MINOR_VERSION minor)
(print "Context version: " (unbox major) "." (unbox minor))
(print "OpenGL version: " (glGetString GL_VERSION))

; global init
(glClearColor 0.3 0.3 0.3 1)

; render pass
(gl:set-renderer (lambda ()
   (glClear GL_COLOR_BUFFER_BIT) ))
