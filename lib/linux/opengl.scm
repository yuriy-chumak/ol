#!/usr/bin/ol
(define-library (lib linux opengl)
   (import
      (r5rs base) (owl io) (owl primop) (owl tuple) (owl string)
      (owl pinvoke) (owl list) (owl math) (owl vector) (owl list-extra) (owl ff)
      (owl interop)

      (lib x11)
      (OpenGL version-1-0))

   (export
      (exports (OpenGL version-1-0))
      gl:run
   )

(begin
; ==========================================================================
(fork-server 'opengl (lambda ()
(let this ((context #f) (renderer #f) (userdata #f))
   (let ((envelope (check-mail)))
   ; обработчик команд серверу opengl
   (if envelope
      (let* ((sender message envelope))
         (tuple-case message
            ; создать окно для рендеринга, проинициализировать в нем opengl контекст
            ((new-GLwindow title width height)
               (let*((display (XOpenDisplay 0))
                     (screen  (XDefaultScreen display))
                     (window  (XCreateSimpleWindow display (XRootWindow display screen)
                        0 0 width height 1
                        (XBlackPixel display screen) (XWhitePixel display screen))))
                  (XSelectInput display window ExposureMask)
                  (XMapWindow display window)
                  (XStoreName display window (c-string title))
               (let*((vi (glXChooseVisual display screen
                              (raw type-vector-raw '(
                                 4 0 0 0 ; GLX_RGBA
                                 5 0 0 0  1 0 0 0 ; GLX_DOUBLEBUFFER
                                 8 0 0 0  1 0 0 0 ; GLX_RED_SIZE
                                 9 0 0 0  1 0 0 0 ; GLX_GREEN_SIZE
                                10 0 0 0  1 0 0 0 ; GLX_BLUE_SIZE
                                12 0 0 0  1 0 0 0 ; GLX_DEPTH_SIZE

                                 0 0 0 0)))); None
                     (cx (glXCreateContext display vi 0 1)))
               (glXMakeCurrent display window cx)
               (print "OpenGL version: " (glGetString GL_VERSION))
               (print "OpenGL vendor: " (glGetString GL_VENDOR)) 
               (print "OpenGL renderer: " (glGetString GL_RENDERER))
               (glXMakeCurrent display null null)

               (this (tuple display window cx) renderer userdata))))
            ((set-renderer function)
               (this context function userdata))
            ((set-userdata function)
               (glXMakeCurrent (ref context 1) (ref context 2) (ref context 3))
               (let ((userdata (function)))
               (glXMakeCurrent (ref context 1) null null)
               (this context renderer userdata)))
            (else
               (this context renderer userdata))))
   ; главный рисовальный цикл
   (if context
      (let*((XEvent (raw type-vector-raw (repeat 0 192)))
            (display (ref context 1))
            (window  (ref context 2))
            (cx      (ref context 3)))
         (let process-events ()
            (if (> (XPending display) 0)
               (begin
               (XNextEvent display XEvent)
               (process-events))))

         (glXMakeCurrent display window cx)
         (let ((userdata (if renderer (apply renderer userdata))))
         (glXSwapBuffers display window)
         (glXMakeCurrent display null null)

         (this context renderer userdata)))

   ; ничего не делаем
   (begin
      (syscall 1022 0 #false #false) ;set ticker value
      (this context renderer userdata))))))))

;(define (gl:create-window title width height)
;(let ((window create-gl-window 0 0 width height))
;   (interact 'opengl (tuple 'set-main-window window))
;   window))
   

;(define (gl:create-window title)
;   (mail 'opengl (tuple 'create-window title 640 480)))
;(define (gl:set-renderer renderer)
;   (mail 'opengl (tuple 'set-renderer renderer)))
;(define (gl:do-main-loop)
;   (interact 'opengl (tuple #f)))

(define (gl:run title width height init renderer)
   (mail 'opengl (tuple 'new-GLwindow title width height))
   (mail 'opengl (tuple 'set-userdata init))
   (mail 'opengl (tuple 'set-renderer renderer))
   (interact 'opengl (tuple #f)))
))