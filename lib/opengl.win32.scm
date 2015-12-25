#!/usr/bin/ol
;(define-library (lib opengl)
;   (export
;      (exports (OpenGL version-1-2))
;
;      GenTextures ; byte-vector (number)
;
;      ; todo: move to the (lib windows)
;      create-window
;      destroy-window
;      load-bmp
;
;      main-game-loop
;   )

   (import
      (r5rs base) (owl io) (owl primop) (owl tuple) (owl string)
      (owl pinvoke) (owl list) (owl math) (owl vector) (owl list-extra) (owl ff)
      (owl interop)

      (lib x11) (lib winapi)
      (OpenGL version-1-0))

;(begin
;(define uname (syscall 63 0 0 0)) ; internal
;(define WIN32 (string-eq? (car uname) "Windows"))

;(import (lib winapi))
;(import (lib x11))

; пара служебных функций

;(define OR (lambda list (fold bor 0 list)))
;(define (make-byte-vector n elem)
;   (list->byte-vector (repeat elem n)))
;(define (get-int16 byte-vector offset)
;   (+ (refb byte-vector offset)
;      (* (refb byte-vector (+ offset 1)) 256)))
;(define ?? (lambda (function arguments) (if function (apply function arguments) arguments)))


; ==========================================================================
;(define (GenTextures count)
;   (let ((id (list->byte-vector (repeat 0 count))))
;      (glGenTextures count id)
;      id))


(define (create-gl-window x y width height)
(let ((uname (syscall 63 #f #f #f)))
   (cond
      ((string-ci=? (ref uname 1) "Windows")
         (let*((window (CreateWindowEx
                           (OR WS_EX_APPWINDOW WS_EX_WINDOWEDGE) "#32770" "OL OpenGL Sample 0" ; #32770 is for system classname for DIALOG
                           (OR WS_OVERLAPPEDWINDOW WS_CLIPSIBLINGS WS_CLIPCHILDREN)
                           0 0 width height ; x y width height
                           null ; no parent window
                           null ; no menu
                           null ; instance
                           null)))
            window))

      ((string-ci=? (ref uname 1) "Linux")
         (let*((display (XOpenDisplay 0))
               (screen (XDefaultScreen display))
               (window (XCreateSimpleWindow display (XRootWindow display screen)
                           0 0 width height 1
                           (XBlackPixel display screen) (XWhitePixel display screen))))
            (XSelectInput display window ExposureMask)
            (XMapWindow display window)

            window))

      ;"HP-UX"
      ;"SunOS"
      ;"Darwin"
      ;"FreeBSD"
      ;"CYGWIN_NT-5.2-WOW64"
      ;"MINGW32_NT-5.2"
      ;...
      (else
         (runtime-error "Unknown platform")))))

(define (create-gl-context window)
   (print "create-gl-context-linux"))

   ;(XSelectInput dpy win (OR ExposureMask KeyPressMask))
   ;(XMapWindow dpy win)

(define (delete-gl-context context)
   (print "delete-gl-context"))


; обработчик команд
(define (command-processor args)
   (tuple-case message
      ; задать окно для рендеринга, проинициализировать в нем opengl контекст 
      ((set-main-window new-window) ; создать окно
         (let ((window (getf args 'window)))
         ; если у нас уже есть окно - удалим его и освободим ресурсы
            (if window (delete-gl-context context)))
         (if new-window
            (let* ((window new-window)
                   (context (create-gl-context window)))
               (mail sender window)
               (put (put args
                  'window window)
                  'context context))
               ; else
            (let* ((window #f))
               (mail sender 'ok)
               (put (put args
                  'window window)
                  'context '(#f.#f)))))
            
      ; простые сеттеры и геттеры:
      ; задать функцию рендерера, swap-buffers будет происходить автоматически
      ((register-renderer renderer)
         (put args 'renderer renderer))
      ; set user data
      ((set-userdata userdata)
         (put args 'userdata userdata))
      ((set-keyboard keyboard)
         (put args 'reyboard keyboard))
;      ((set-mouse mouse)
;         (this window context  ss ms  userdata  renderer  keyboard mouse))
      ; error on invalid command
      (else
         (runtime-error "Unknown opengl server request" message)
         args)))

(define (message-loop-processor args)
   (let ((XEvent (raw type-vector-raw (repeat 0 192))))
      (XNextEvent dpy XEvent)
      (if (= (refb XEvent 0) 12)
         (let ((renderer (getf args 'renderer)))
            (if renderer (renderer))))))


; главный цикл оконной работы:
;(define (main-loop)
(fork-server 'opengl (lambda ()
; обработчики
(define (msg-processor this MSG  window context  ss ms  userdata  renderer  keyboard mouse)
; тут можно обработать сообщения к окну, если надо.
; Например, такое:
   (begin ; атомарный обработчик-диспетчер
      (TranslateMessage MSG)
      (DispatchMessage MSG))

   (case (+ (refb MSG 4) (* (refb MSG 5) 256))
      (WM_LBUTTONDOWN ;WM_SIZING
         (print "WM_LBUTTONDOWN")
         (if mouse
            (let* ((x (get-int16 MSG 12))
                   (y (get-int16 MSG 14))
                   (_ (print "x: " x ", y:" y))
                   (userdata (mouse userdata #t #f x y)))
               (this window context  ss ms  userdata  renderer  keyboard mouse))
            (this window hDC hRC  ss ms  userdata  renderer  keyboard mouse)))
      (WM_PAINT
         (print "paint")
         (let ((rect (list->byte-vector (list 0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0))))
            (GetClientRect window rect)
            (glViewport 0 0 (get-int16 rect #x8) (get-int16 rect #xC)))
         (this window hDC hRC  ss ms  userdata  renderer  keyboard mouse))
      (WM_KEYDOWN
         (print "WM_KEYDOWN: " (refb MSG 8) "-" (refb MSG 9))
         (if keyboard
            (let* ((userdata (keyboard userdata (refb MSG 8))))
               (print "new userdata: " userdata)
               (this window hDC hRC  ss ms  userdata  renderer  keyboard mouse))
            (this window hDC hRC  ss ms  userdata  renderer  keyboard mouse)))
      (else
         (this window hDC hRC  ss ms  userdata  renderer  keyboard mouse))))

(define (opengl-processor this  window hDC hRC  ss ms  userdata  renderer  keyboard mouse)
(let* ((ss2 ms2 (clock))
       (dms (mod (+ 1000 (- ms2 ms)) 1000))) ; todo: add seconds here too
   (if (and window renderer)
      (if (> dms 0)
         (let* ((userdata (renderer dms userdata)))
            (SwapBuffers hDC)
            (this window hDC hRC  ss2 ms2  userdata  renderer  keyboard mouse))
         ;else
         (this window hDC hRC  ss ms  userdata  renderer  keyboard mouse))
     ;else
      (begin
         (wait 1)
         (this window hDC hRC  ss2 ms2  userdata  renderer  keyboard mouse)))))

; внутренние переменные
(let* ((MSG "1234567890123456789012345678") ; sizeof(MSG)=28
       (ss ms (clock)))
;; главный оконный цикл фреймворка
(let this ((window #f) (context '(().())  (ss ss) (ms ms)  (userdata #empty) (renderer #f) (keyboard #f) (mouse #f))
; обработаем команды фреймворка
(let ((envelope (check-mail)))
   (if envelope
      (let* ((sender message envelope))
         ;(print "opengl server got message " message)
         (if (tuple? message)
            (tuple-processor this sender message  window hDC hRC  ss ms  userdata  renderer  keyboard mouse)
            ;else
            (this window hDC hRC  ss ms  userdata  renderer  keyboard mouse)))
   (if (= 1 (PeekMessage MSG 0 0 0 PM_REMOVE))
      (msg-processor this MSG  window hDC hRC  ss ms  userdata  renderer  keyboard mouse)
   ; else
   (opengl-processor this window hDC hRC  ss ms  userdata  renderer  keyboard mouse))))))))

;(fork-server 'opengl main-loop)

; нужные функции
;(define (create-window title width height)
;
;(define (delete handle) #false)

(define (main-game-loop predicate)
   (if (predicate) (begin
      ;(_yield)
      (wait 5)
      (main-game-loop predicate))))
  
(define (gl:create-window title width height)
(let ((window create-gl-window 0 0 width height))
   (interact 'opengl (tuple 'set-main-window window))
   window))

;(define (destroy-window window)
;   (interact 'opengl (tuple 'set-main-window #false))
;   (DestroyWindow window))
   
;))
