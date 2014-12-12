(define-library (lib opengl)
   (export
      (exports (OpenGL version-1-0))

      ; todo: move to the (lib windows)
      create-window
      destroy-window

      main-game-loop
   )

   (import
     (owl defmac) (owl io) (owl primop) (owl error) (owl tuple) (owl string)
     (owl pinvoke) (owl list) (owl math) (owl vector) (owl list-extra)
     (lib windows)
     (owl interop)

     (OpenGL version-1-0))
  
(begin
; пара служебных функций

(define OR (lambda list (fold bor 0 list)))
(define (make-byte-vector n elem)
   (list->byte-vector (repeat elem n)))
;(define ?? (lambda (function arguments) (if function (apply function arguments) arguments)))

; главный цикл оконной работы:
;(define (main-loop)
(fork-server 'opengl (lambda ()
; внутренние переменные
(let* ((MSG "1234567890123456789012345678") ; sizeof(MSG)=28
       (ss ms (clock)))
;; главный оконный цикл фреймворка
(let this ((window #f) (hDC #f) (hRC #f)  (ss ss) (ms ms)  (userdata #f) (renderer #f) (renderer-args #f))
   ; обработаем команды фреймворка
   (let ((mail (check-mail)))
      (if mail
         (let* ((sender message mail))
            ;(print "opengl server got message " message)
            (if (tuple? message)
               (tuple-case message
                  ; задать окно для рендеринга, проинициализировать в нем opengl контекст 
                  ((set-main-window new-window) ; создать окно
                     (if window (begin
                        (wglMakeCurrent hDC 0)
                        (if hRC
                           (wglDeleteContext hRC))
                        (if hDC
                           (ReleaseDC window hDC))))
                     (if new-window
                        (let* ((window new-window)
                               (pfd (list->byte-vector '(#x28 00  1 00  #x25 00 00 00
                                                         00 #x10 00 00 00 00 00 00
                                                         00 00 00 00 00 00 00 #x10 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00)))
                               (hDC (GetDC window)))
                        (let* ((PixelFormat (ChoosePixelFormat hDC pfd)))
                           (print "Pixel Format: " PixelFormat)
                           (print "    options: " (if (> (bor (ref pfd 4) 4) 0) "PFD_DRAW_TO_WINDOW " "")
                                                  (if (> (bor (ref pfd 4) 1) 0) "PFD_DOUBLEBUFFER " "")
                                                  (if (> (bor (ref pfd 4) 32) 0) "PFD_SUPPORT_OPENGL " ""))
                           (print "    color bits: " (refb pfd 9))
                           (print "    depth bits: " (refb pfd 25))
                           (print "SetPixelFormat: " (if (=
                           (SetPixelFormat hDC PixelFormat pfd) 1) "Ok" "Failed")))
                        (let* ((hRC (wglCreateContext hDC)))
                           (print "Make GL context Current: " (if (=
                           (wglMakeCurrent hDC hRC) 1) "Ok" "Failed"))
                           (print)
                           (print "OpenGL version: " (glGetString GL_VERSION))
                           (print "OpenGL vendor: " (glGetString GL_VENDOR))
                           (print "OpenGL renderer: " (glGetString GL_RENDERER))
                        
                           (ShowWindow window SW_SHOW)
                           (SetForegroundWindow window)
                           (SetFocus window)

                           (this window hDC hRC  ss ms  userdata  renderer renderer-args)))
                        ; else
                        (this #false #f #f  ss ms  userdata  renderer renderer-args)))
                        
                  ; простые сеттеры и геттеры:
                  ; задать функцию рендерера, swap-buffers будет происходить автоматически
                  ((register-renderer renderer renderer-args)
                     (this window hDC hRC  ss ms  userdata  renderer renderer-args))
                  ; set user data
                  ((set-userdata userdata)
                     (this window hDC hRC  ss ms  userdata  renderer renderer-args))
                  ; error on invalid command
                  (else
                     (print "Unknown opengl server request: " message)
                     (this window hDC hRC  ss ms  userdata  renderer renderer-args)))))))
   ; если окно уже есть - обработаем окно
   (if window (begin
      (if (= 1 (PeekMessage MSG 0 0 0 PM_REMOVE))
         (begin
         ; тут можно обработать сообщения к окну, если надо.
         ; Например, такое:
            (case (+ (refb MSG 4) (* (refb MSG 5) 256))
               (WM_LBUTTONDOWN ;WM_SIZING
                  (print "Left mouse button pressed")))
;               (let ((rect (list->byte-vector '(0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0))))
;                  (GetClientRect window rect)
;                  (let ((wh (cons (+ (refb rect 8) (* (refb rect 9) 256)) (+ (refb rect 12) (* (refb rect 13) 256)))))
;                     (print "new size: " (car wh) " x " (cdr wh))
;                     (glViewport 0 0 (car wh) (cdr wh))))))
;          (let ((w (+ (ref MSG 12) (* (ref MSG 13) 256)))
;                (h (+ (ref MSG 14) (* (ref MSG 15) 256))))
;             (print "w: " w ", h: " h))))
            (TranslateMessage MSG)
            (DispatchMessage MSG))
         ; no windows system events - let's draw
         (begin
            (let* ((ss2 ms2 (clock))
                   (dms (mod (+ 1000 (- ms2 ms)) 1000))) ; todo: add seconds here too
               (if (> dms 0)
                  (if renderer
                     (let* ((result (apply renderer (append (list dms userdata) renderer-args)))
                            (userdata (car result))
                            (renderer-args (cdr result)))   ;(apply renderer (append (list dms userdata) renderer-args))))
                        (print "userdata: " userdata)
;                        (print "renderer-args: " renderer-args)
                        (SwapBuffers hDC)
                        (this window hDC hRC  ss2 ms2  userdata  renderer renderer-args))))
                  ; no renderer
               (this window hDC hRC  ss ms  userdata  renderer renderer-args))))))


   ; если окна не было, то доберемся сюда
   (_yield)
   (let* ((ss ms (clock))) ; обновим часы
      (this window hDC hRC  ss ms  userdata  renderer renderer-args))))))

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
  
(define (create-window title width height)
   (let* ((window (CreateWindowEx
                     (OR WS_EX_APPWINDOW WS_EX_WINDOWEDGE) "#32770" title ; #32770 is for system classname for DIALOG
                     (OR WS_OVERLAPPEDWINDOW WS_CLIPSIBLINGS WS_CLIPCHILDREN)
                     0 0 width height ; x y width height
                     0 ; no parent window
                     0 ; no menu
                     0 ; instance
                     null)))

      window))

(define (destroy-window window)
   (DestroyWindow window))
))