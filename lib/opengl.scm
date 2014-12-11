(define-library (lib opengl)
   (export
      (exports (OpenGL version-1-0))

;      create-window
;      delete
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
(let ((MSG "1234567890123456789012345678") ; sizeof(MSG)=28
      )
;; главный оконный цикл фреймворка
(let this ((window #f) (hDC #f) (renderer #f) (renderer-values #f))
   ; обработаем команды фреймворка
   (let ((mail (check-mail)))
      (if mail
         (let* ((sender message mail))
            (if (tuple? message)
               (tuple-case message
                  ((create title width height) ; создать окно
                     (let* ((window (CreateWindowEx
                                       (OR WS_EX_APPWINDOW WS_EX_WINDOWEDGE) "#32770" title ; #32770 is for system classname for DIALOG
                                       (OR WS_OVERLAPPEDWINDOW WS_CLIPSIBLINGS WS_CLIPCHILDREN)
                                       0 0 width height ; x y width height
                                       0 ; no parent window
                                       0 ; no menu
                                       0 ; instance
                                       null))
                            (pfd (list->byte-vector '(#x28 00  1  00  #x25 00 00 00 00 #x10 00 00 00 00 00 00
                                                                        00 00 00 00 00 00 00 #x10 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00)))
                            (hDC (GetDC window))
                            (PixelFormat (ChoosePixelFormat hDC pfd))
                            (_ (SetPixelFormat hDC PixelFormat pfd))
                            (hRC (wglCreateContext hDC))
                            (_ (wglMakeCurrent hDC hRC))
                            ;(dimensions
                            ;   (let ((rect (list->byte-vector '(0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0))))
                            ;      (GetClientRect window rect)
                            ;      (cons (+ (refb rect 8) (* (refb rect 9) 256)) (+ (refb rect 12) (* (refb rect 13) 256)))))

                           ) ; todo: override as '(INTEGER . 0)
                        (print "OpenGL version: " (glGetString GL_VERSION))
                        (print "OpenGL vendor: " (glGetString GL_VENDOR))
                        (print "OpenGL renderer: " (glGetString GL_RENDERER))

                        (ShowWindow window SW_SHOW)
                        (SetForegroundWindow window)
                        (SetFocus window)

                        (this window hDC renderer renderer-values)))
                  ((register-renderer renderer renderer-values)
                     (this window hDC renderer renderer-values))
              ))
;                     (print "create the window: " title " " width " " height))))
            (print message))
         (_yield)))
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
            (glClear GL_COLOR_BUFFER_BIT)
;            (let ((renderer-values (?? renderer renderer-values)))
            (let ((renderer-values (if renderer (apply renderer renderer-values) renderer-values)))
;                ((
               (SwapBuffers hDC)
               (this window hDC renderer renderer-values))))))

   ; если окна не было, то доберемся сюда
   (this window hDC renderer renderer-values)))))

;(fork-server 'opengl main-loop)

; нужные функции
;(define (create-window title width height)
;
;(define (delete handle) #false)

))