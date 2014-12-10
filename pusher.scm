; OL
(define *USE_GLBEGIN* 1)

;  (define isCompiled (list->byte-vector '(0 0 0 0)))
;  (sys-prim 1033 isCompiled #false #false)
(import (owl pinvoke))
(import (lib windows))
(import (OpenGL version-1-2))

; вспомогательный макрос для собрать в кучку все bor
(define OR (lambda list (fold bor 0 list)))
(define (make-byte-vector n elem)
   (list->byte-vector (repeat elem n)))

(define width 1280)
(define height 720)
(define window (CreateWindowEx
    (OR WS_EX_APPWINDOW WS_EX_WINDOWEDGE) "#32770" "OL OpenGL Sample 1" ; #32770 is for system classname for DIALOG
    (OR WS_OVERLAPPEDWINDOW WS_CLIPSIBLINGS WS_CLIPCHILDREN)
    0 0 width height ; x y width height
    0 ; no parent window
    0 ; no menu
    0 ; instance
    null)) ; todo: override as '(INTEGER . 0)

; PIXELFORMATDESCRIPTOR
(define pfd (list->byte-vector '(#x28 00  1  00  #x25 00 00 00 00 #x10 00 00 00 00 00 00
                                                   00 00 00 00 00 00 00 #x10 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00)))
(define hDC (GetDC window))
(define PixelFormat (ChoosePixelFormat hDC pfd))
  (print "PixelFormat = " PixelFormat)
  (print "SetPixelFormat = "
(SetPixelFormat hDC PixelFormat pfd))
(define hRC (wglCreateContext hDC))

  (print "wglMakeCurrent = "
(wglMakeCurrent hDC hRC))

  (print "OpenGL version: " (glGetString GL_VERSION))
  (print "OpenGL vendor: " (glGetString GL_VENDOR))
  (print "OpenGL renderer: " (glGetString GL_RENDERER))


; real code
  ; https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml
; проверка, что все запустилось.
(define (msgbox)
  (if (= (MessageBox 0 "Please, press OK for test pass!" (c-string "load-library test") (bor MB_OKCANCEL MB_ICONASTERISK)) IDOK)
    (print "OK")
    (print "CANCEL")))
;(msgbox)

; в момент импорта сделать все нужные привязки
(ShowWindow window SW_SHOW)
(SetForegroundWindow window)
(SetFocus window)

; ResizeGLScene
(glViewport 0 0 width height)

(glMatrixMode GL_PROJECTION)
(glLoadIdentity)

(glMatrixMode GL_MODELVIEW)
(glLoadIdentity)

(glShadeModel GL_SMOOTH)
(print "glHint = "
(glHint GL_PERSPECTIVE_CORRECTION_HINT GL_NICEST))

(glClearColor 0 0 1 1)

;(define vertexPositions (list->byte-vector '(
;;        (glVertex2i 2 0)
;  00 00 #x00 #x40    0 0 #x00 #x00    0 0 0 0    00 00 #x80 #x3F
;;        (glVertex2i 1 2)
;  00 00 #x80 #x3F    0 0 #x00 #x40    0 0 0 0    00 00 #x80 #x3F
;;        (glVertex2i 0 0)
;  00 00 #x00 #x00    0 0 #x00 #x00    0 0 0 0    00 00 #x80 #x3F
;)))

;(define MSG (make-byte-vector 28 0)) ; sizeof(MSG)=28
(define MSG "1234567890123456789012345678") ; sizeof(MSG)=28
;(call/cc (lambda (return)
(define (cycle)   ;MSG
  (if (= 1 (PeekMessage MSG 0 0 0 PM_REMOVE))
    (begin
      ; тут можно обработать сообщения к окну, если надо.
      ; Например, такое:
;       (print (ref MSG 0) "." (ref MSG 1) "." (ref MSG 2) "." (ref MSG 3) "-" (ref MSG 4) "." (ref MSG 5) "." (ref MSG 6) "." (ref MSG 7))
       (let ((message (+ (refb MSG 4) (* (refb MSG 5) 256))))
         (if (= message WM_LBUTTONDOWN)
           (print "WM_LBUTTONDOWN")
         (if (= message WM_SIZE)
           (print "WM_SIZE")
         (if (= message WM_WINDOWPOSCHANGED)
           (print "WM_WINDOWPOSCHANGED")))))
;          (let ((w (+ (ref MSG 12) (* (ref MSG 13) 256)))
;                (h (+ (ref MSG 14) (* (ref MSG 15) 256))))
;             (print "w: " w ", h: " h))))
      (TranslateMessage MSG)
      (DispatchMessage MSG))
      
    (begin ; DrawGLScene
       (glClear GL_COLOR_BUFFER_BIT)
       
       (glColor3f 0 1 0)
       (glBegin GL_TRIANGLES)
         (glVertex2f -1 -1)
         (glVertex2f +1 -1)
         (glVertex2f -1 +1)
       (glEnd)
;       (glBegin GL_TRIANGLE_STRIP)
;         (glVertex2f -1 -1)
;         (glVertex2f +1 -1)
;         (glVertex2f -1 +1)
;         (glVertex2f +1 +1)
;       (glEnd)

       (SwapBuffers hDC)))
  (if (= (GetAsyncKeyState 27) 0) (cycle)))
(cycle)

; KillGLWindow
(wglMakeCurrent 0 0)
(wglDeleteContext hRC)
(ReleaseDC window hDC)
(DestroyWindow window)

(print "@")
