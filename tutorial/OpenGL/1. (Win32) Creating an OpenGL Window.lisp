#!/usr/bin/ol

(import (OpenGL version-1-0)
   (lib winapi))

(define width 640)
(define height 480)

(define OR (lambda list (fold bor 0 list)))

;(main)
(define window (CreateWindowEx
   (OR WS_EX_APPWINDOW WS_EX_WINDOWEDGE) (c-string "#32770") ; #32770 is for system classname for DIALOG
   (c-string "1. Creating an OpenGL Window")
   (OR WS_OVERLAPPEDWINDOW WS_CLIPSIBLINGS WS_CLIPCHILDREN)
   0 0 width height ; x y width height
   #false ; no parent window
   #false ; no menu
   #false ; instance
   #false))

; PIXELFORMATDESCRIPTOR
(define pfd (vm:new-raw-object type-vector-raw '(#x28 00  1  00  #x25 00 00 00 00 #x10 00 00 00 00 00 00
                                               00 00 00 00 00 00 00 #x10 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00)))
(define hDC (GetDC window))
(define PixelFormat (ChoosePixelFormat hDC pfd))
(print "PixelFormat = " PixelFormat)
(print "SetPixelFormat = "
   (SetPixelFormat hDC PixelFormat pfd))

(define hRC (wglCreateContext hDC))

;(init)
(wglMakeCurrent hDC hRC)

(print "OpenGL version: " (glGetString GL_VERSION))
(print "OpenGL vendor: " (glGetString GL_VENDOR))
(print "OpenGL renderer: " (glGetString GL_RENDERER))

(glShadeModel GL_SMOOTH)
(glClearColor 0.11 0.11 0.11 1)

(wglMakeCurrent #f #f)

;(show)
(ShowWindow window SW_SHOW)
(SetForegroundWindow window)
(SetFocus window)

;,quit;(loop) ;UINT - unsigned int
(call/cc (lambda (break)
(let ((MSG (vm:new-raw-object type-vector-raw 48)))

   (let loop ()
      (let process-events ()
         (if (eq? 1 (PeekMessage MSG #f 0 0 PM_REMOVE))
            (begin
               (TranslateMessage MSG)
               (DispatchMessage MSG)
               (process-events))))
   
      (wglMakeCurrent hDC hRC)
      (glClear GL_COLOR_BUFFER_BIT)
   
      (SwapBuffers hDC)
      (wglMakeCurrent #f #f)

      (loop)))))

;(done)
(wglDeleteContext hRC)
(ReleaseDC window hDC)
(DestroyWindow window)

(print "Ok.")
