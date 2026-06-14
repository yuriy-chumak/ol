#!/usr/bin/env ol

; AMD and Intel cards (mesa drivers):
(import (scheme process-context))
(set-environment-variable! "MESA_GL_VERSION_OVERRIDE" "3.2")
; 3.2 - for Core Profile
; 3.2COMPAT - for Compatibility Profile
; 3.0FC - for Forward Compatible

; NVidia cards:
(import (lib gl config))
(config 'set 'exact-context #true)

; create OpenGL window with exact profile
(import (lib gl 3.2))
(gl:set-window-title "1. Creating an OpenGL 3.2 exact Window")

; let's check context version
(define major (box 0))
(define minor (box 0))
(glGetIntegerv GL_MAJOR_VERSION major)
(glGetIntegerv GL_MINOR_VERSION minor)
(print "Context version: " (unbox major) "." (unbox minor))
(print "OpenGL version: " (glGetString GL_VERSION))

; global init
; no glShadeModel in 3.2 core profile
(glClearColor 0.3 0.3 0.3 1)

; render pass
(gl:set-renderer (lambda ()
   (glClear GL_COLOR_BUFFER_BIT) ))
