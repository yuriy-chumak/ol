#!/usr/bin/ol

(import (OpenGL version-1-0)
   (lib winapi))

(define width 640)
(define height 480)

(define OR (lambda list (fold bor 0 list)))

;(main)
(define window (CreateWindowEx
   (OR WS_EX_APPWINDOW WS_EX_WINDOWEDGE) "#32770" "OL OpenGL Sample 0" ; #32770 is for system classname for DIALOG
;    0 "#32770" "OL OpenGL Sample 0" ; #32770 is for system classname for DIALOG
   (OR WS_OVERLAPPEDWINDOW WS_CLIPSIBLINGS WS_CLIPCHILDREN)
   0 0 width height ; x y width height
   null ; no parent window
   null ; no menu
   null ; instance
   null)) ; todo: override as '(INTEGER . 0)

; PIXELFORMATDESCRIPTOR
(define pfd (raw type-vector-raw '(#x28 00  1  00  #x25 00 00 00 00 #x10 00 00 00 00 00 00
                                                   00 00 00 00 00 00 00 #x10 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00)))                        
(define hDC (GetDC window))
(define PixelFormat (ChoosePixelFormat hDC pfd))
(print "PixelFormat = " PixelFormat)
(print "SetPixelFormat = "
(SetPixelFormat hDC PixelFormat pfd))

(define hRC (wglCreateContext hDC))

;(init)
(print "wglMakeCurrent = "
(wglMakeCurrent hDC hRC))

(print "OpenGL version: " (glGetString GL_VERSION))
(print "OpenGL vendor: " (glGetString GL_VENDOR)) 
(print "OpenGL renderer: " (glGetString GL_RENDERER))

(glShadeModel GL_SMOOTH)
(glClearColor 0.11 0.11 0.11 1)

(glMatrixMode GL_PROJECTION)
(glLoadIdentity)
(gluPerspective 45 (/ width height) 0.1 100)

(glEnable GL_DEPTH_TEST)

(wglMakeCurrent '() '())

;(show)
(ShowWindow window SW_SHOW)
(SetForegroundWindow window)
(SetFocus window)

;(loop)
(let ((MSG (raw type-vector-raw (repeat 0 28))))
(let loop ((x 1) (dx 0.02) (y 3) (dy 0.03))
   (let process-events ()
      (if (= 1 (PeekMessage MSG '() 0 0 PM_REMOVE))
         (begin
            (TranslateMessage MSG)
            (DispatchMessage MSG)
            (process-events))))

   (wglMakeCurrent hDC hRC)
   (glClear (fx:or GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (gluLookAt x y 5
      0 0 0
      0 1 0)

   (glBegin GL_LINES)
      ; Ox
      (glColor3f 1 0 0)
      (glVertex3f 0 0 0)
      (glVertex3f 2 0 0)
         (glVertex3f 2 0 0)
         (glVertex3f 1.9 0.1 0)
         (glVertex3f 2 0 0)
         (glVertex3f 1.9 0 0.1)
      ; Oy
      (glColor3f 0 1 0)
      (glVertex3f 0 0 0)
      (glVertex3f 0 2 0)
         (glVertex3f 0 2 0)
         (glVertex3f 0.1 1.9 0)
         (glVertex3f 0 2 0)
         (glVertex3f 0 1.9 0.1)
      ; Oz
      (glColor3f 0 0 1)
      (glVertex3f 0 0 0)
      (glVertex3f 0 0 2)
         (glVertex3f 0 0 2)
         (glVertex3f 0.1 0 1.9)
         (glVertex3f 0 0 2)
         (glVertex3f 0 0.1 1.9)
   (glEnd)

   (glBegin GL_QUADS)
      ; front
      (glColor3f 0.7 0.7 0.7)
      (glVertex3f  1 -1 -1)
      (glVertex3f  1  1 -1)
      (glVertex3f -1  1 -1)
      (glVertex3f -1 -1 -1)

      ; back
      (glColor3f 0.9 0.8 0.5)
      (glVertex3f  1 -1  1)
      (glVertex3f  1  1  1)
      (glVertex3f -1  1  1)
      (glVertex3f -1 -1  1)

      ; right
      (glColor3f 0.7 0.2 0.2)
      (glVertex3f  1 -1 -1)
      (glVertex3f  1  1 -1)
      (glVertex3f  1  1  1)
      (glVertex3f  1 -1  1)

      ; left
      (glColor3f 0.2 0.2 0.7)
      (glVertex3f -1 -1 -1)
      (glVertex3f -1  1 -1)
      (glVertex3f -1  1  1)
      (glVertex3f -1 -1  1)

      ; top
      (glColor3f 0.7 0.7 0.2)
      (glVertex3f  1  1  1)
      (glVertex3f  1  1 -1)
      (glVertex3f -1  1 -1)
      (glVertex3f -1  1  1)

      ; bottom
      (glColor3f 0.2 0.7 0.7)
      (glVertex3f  1 -1  1)
      (glVertex3f  1 -1 -1)
      (glVertex3f -1 -1 -1)
      (glVertex3f -1 -1  1)

   (glEnd)

   (SwapBuffers hDC)
   (wglMakeCurrent '() '())

   (let ((nx (if (or (> x 2) (< x -2)) (- dx) dx))
         (ny (if (or (> y 4) (< y -4)) (- dy) dy)))
      (loop (+ x nx) nx (+ y ny) ny))))

;(done)
(wglDeleteContext hRC)
(ReleaseDC window hDC)
(DestroyWindow window)

(print "Ok.")
