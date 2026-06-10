#!/usr/bin/env ol

(syscall 1014 "MESA_GL_VERSION_OVERRIDE\0" "3.2") ; AMD and Intel cards

; update OpenGL config
(import (lib gl config))
(config 'set 'exact-context #true) ; NVidia cards

(import (lib gl 3.2))
(gl:set-window-title "1. Creating an OpenGL 3.2 Window")

; let's check context version
(define major (box 0))
(define minor (box 0))
(glGetIntegerv GL_MAJOR_VERSION major)
(glGetIntegerv GL_MINOR_VERSION minor)
(print "Context version: " (unbox major) "." (unbox minor))
(print "OpenGL version: " (glGetString GL_VERSION))

; init
; no glShadeModel in 3.2 core profile
(glClearColor 0.3 0.3 0.3 1)

; draw loop
(gl:set-renderer (lambda ()
   (glClear GL_COLOR_BUFFER_BIT)))
