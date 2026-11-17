#!/usr/bin/env ol

; create OpenGL window and import OpenGL functions
(import (lib gl 3.1))
(gl:set-window-title "1. Creating an OpenGL 3.1 Window")

; let's check context version
(print "  OpenGL version: " (glGetString GL_VERSION))

(define major (box 0))
(define minor (box 0))
(glGetIntegerv GL_MAJOR_VERSION major)
(glGetIntegerv GL_MINOR_VERSION minor)
(print "  Context version: " (unbox major) "." (unbox minor))

; global init
(glClearColor 0.3 0.3 0.3 1)

; render loop
(gl:set-renderer (lambda ()
   (glClear GL_COLOR_BUFFER_BIT) ))
