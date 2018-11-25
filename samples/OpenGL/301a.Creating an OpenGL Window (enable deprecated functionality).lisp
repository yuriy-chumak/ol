#!/usr/bin/ol
(import (lib gl))
(gl:set-window-title "1. Creating an OpenGL 3.0 Window")

; change OpenGL config
(define-library (OpenGL config)
   (import (otus lisp))
   (export config)
   (begin
      (define config (put #empty 'GL_VERSION_1_2_DEPRECATED #true))
))

(gl:set-context-version 3 2) ; use OpenGL version 3.0
(import (OpenGL version-3-2))

; let's check context version
(define major (box 0))
(define minor (box 0))
(glGetIntegerv GL_MAJOR_VERSION major)
(glGetIntegerv GL_MINOR_VERSION minor)
(print "Context version: " (unbox major) "." (unbox minor))
(print "OpenGL version: " (glGetString GL_VERSION))

; init
(glShadeModel GL_SMOOTH)
(glClearColor 0.11 0.11 0.11 1)

(print "enabled deprecated 1.2:")
(print "   GL_VERSION_1_2_DEPRECATED: " GL_VERSION_1_2_DEPRECATED)
(print "   GL_LIGHT_MODEL_COLOR_CONTROL: " GL_LIGHT_MODEL_COLOR_CONTROL)
(print "still disabled 2.0:")
(print "   GL_VERSION_2_0_DEPRECATED: " GL_VERSION_2_0_DEPRECATED)
(print "   GL_POINT_SPRITE: " GL_POINT_SPRITE)

; draw loop
(gl:set-renderer (lambda (mouse)
   (glClear GL_COLOR_BUFFER_BIT)))
