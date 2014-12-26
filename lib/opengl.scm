(define-library (lib opengl)
   (export
      (exports (OpenGL version-1-2))
      
      GenTextures ; byte-vector (number)

      ; todo: move to the (lib windows)
      create-window
      destroy-window
      load-bmp

      main-game-loop
   )

   (import
     (owl defmac) (owl io) (owl primop) (owl error) (owl tuple) (owl string)
     (owl pinvoke) (owl list) (owl math) (owl vector) (owl list-extra) (owl ff)
     (lib windows)
     (owl interop)

     (OpenGL version-1-2))
  
(begin
; пара служебных функций

(define OR (lambda list (fold bor 0 list)))
(define (make-byte-vector n elem)
   (list->byte-vector (repeat elem n)))
(define (get-int16 byte-vector offset)
   (+ (refb byte-vector offset)
      (* (refb byte-vector (+ offset 1)) 256)))
;(define ?? (lambda (function arguments) (if function (apply function arguments) arguments)))


; ==========================================================================
(define (GenTextures count)
   (let ((id (list->byte-vector (repeat 0 count))))
      (glGenTextures count id)
      id))



; главный цикл оконной работы:
;(define (main-loop)
(fork-server 'opengl (lambda ()
; обработчики
(define (tuple-processor this  sender message  window hDC hRC  ss ms  userdata  renderer renderer-args  keyboard)
   (tuple-case message
      ; задать окно для рендеринга, проинициализировать в нем opengl контекст 
      ((set-main-window new-window) ; создать окно
         ; если у нас уже есть окно - удалим его и освободим ресурсы
         (if window (begin
            (wglMakeCurrent hDC 0)
            (if hRC
               (wglDeleteContext hRC))
            (if hDC
               (ReleaseDC window hDC))))
         (if new-window
            (let* ((window new-window)
                   (pfd (list->byte-vector '(#x28 00  1 00  #x25 00 00 00 ; size, version, flags
                                                            00 #x10 00 00 00 00 00 00 00 00 ; pixelType, colorBits, r,g,b,a bits+shift
                                                            00 00 00 00 00 #x10 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00))) ; accum, depth, etc.
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

               (mail sender window)
               (this window hDC hRC  ss ms  userdata  renderer renderer-args  keyboard)))
            ; else
            (begin
               (mail sender 'ok)
               (this #false #f #f  ss ms  userdata  renderer renderer-args  keyboard))))
            
      ; простые сеттеры и геттеры:
      ; задать функцию рендерера, swap-buffers будет происходить автоматически
      ((register-renderer renderer renderer-args)
         (this window hDC hRC  ss ms  userdata  renderer renderer-args  keyboard))
      ; set user data
      ((set-userdata userdata)
         (this window hDC hRC  ss ms  userdata  renderer renderer-args  keyboard))
      ((set-keyboard keyboard)
         (this window hDC hRC  ss ms  userdata  renderer renderer-args  keyboard))
      ; error on invalid command
      (else
         (print "Unknown opengl server request: " message)
         (this window hDC hRC  ss ms  userdata  renderer renderer-args  keyboard))))


(define (msg-processor this MSG  window hDC hRC  ss ms  userdata  renderer renderer-args  keyboard)
; тут можно обработать сообщения к окну, если надо.
; Например, такое:
   (begin ; атомарный обработчик-диспетчер
      (TranslateMessage MSG)
      (DispatchMessage MSG))

   (case (+ (refb MSG 4) (* (refb MSG 5) 256))
      (WM_LBUTTONDOWN ;WM_SIZING
         (print "Left mouse button pressed")
         (this window hDC hRC  ss ms  userdata  renderer renderer-args  keyboard))
      (WM_PAINT
         (print "paint")
         (let ((rect (list->byte-vector (list 0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0))))
            (GetClientRect window rect)
            (glViewport 0 0 (get-int16 rect #x8) (get-int16 rect #xC))) 
         (this window hDC hRC  ss ms  userdata  renderer renderer-args  keyboard))
      (WM_KEYDOWN
         (print "WM_KEYDOWN: " (refb MSG 8) "-" (refb MSG 9))
         (if keyboard
            (let* ((userdata (keyboard userdata (refb MSG 8))))
               (print "new userdata: " userdata)
               (this window hDC hRC  ss ms  userdata  renderer renderer-args  keyboard))
            (this window hDC hRC  ss ms  userdata  renderer renderer-args  keyboard)))
      (else
         (this window hDC hRC  ss ms  userdata  renderer renderer-args  keyboard))))

(define (opengl-processor this  window hDC hRC  ss ms  userdata  renderer renderer-args  keyboard)
(let* ((ss2 ms2 (clock))
       (dms (mod (+ 1000 (- ms2 ms)) 1000))) ; todo: add seconds here too
   (if (and window renderer)
      (if (> dms 0)
         (let* ((result (apply renderer (append (list dms userdata) renderer-args)))
                (userdata (car result))
                (renderer-args (cdr result)))   ;(apply renderer (append (list dms userdata) renderer-args))))
            (SwapBuffers hDC)
            (this window hDC hRC  ss2 ms2  userdata  renderer renderer-args  keyboard))
         ;else
         (this window hDC hRC  ss ms  userdata  renderer renderer-args  keyboard))
     ;else
     (begin
        (wait 1)
        (this window hDC hRC  ss2 ms2  userdata  renderer renderer-args  keyboard)))))

; внутренние переменные
(let* ((MSG "1234567890123456789012345678") ; sizeof(MSG)=28
       (ss ms (clock)))
;; главный оконный цикл фреймворка
(let this ((window #f) (hDC #f) (hRC #f)  (ss ss) (ms ms)  (userdata #empty) (renderer #f) (renderer-args #f)  (keyboard #f))
; обработаем команды фреймворка
(let ((envelope (check-mail)))
   (if envelope
      (let* ((sender message envelope))
         (print "sender: " sender)
         ;(print "opengl server got message " message)
         (if (tuple? message)
            (tuple-processor this sender message  window hDC hRC  ss ms  userdata  renderer renderer-args  keyboard)
            ;else
            (this window hDC hRC  ss ms  userdata  renderer renderer-args  keyboard)))
   (if (= 1 (PeekMessage MSG 0 0 0 PM_REMOVE))
      (msg-processor this MSG  window hDC hRC  ss ms  userdata  renderer renderer-args  keyboard)
   ; else
   (opengl-processor this window hDC hRC  ss ms  userdata  renderer renderer-args  keyboard))))))))
   

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

      (interact 'opengl (tuple 'set-main-window window))
      window))

(define (destroy-window window)
   (interact 'opengl (tuple 'set-main-window #false))
   (DestroyWindow window))
   
(define (load-bmp filename)
   (let* ((in (open-input-file filename))
          (BITMAPFILEHEADER (get-block in (+ 2 4  2 2  4)))
          (BITMAPINFOHEADER (get-block in (+ 4  4 4  2 2  4 4  4 4 4 4)))
          (size (- (get-int16 BITMAPFILEHEADER 2)
                   (vec-len BITMAPFILEHEADER)
                   (vec-len BITMAPINFOHEADER)))
          (width (get-int16 BITMAPINFOHEADER 4))
          (height (get-int16 BITMAPINFOHEADER 8))
          (bitcount (get-int16 BITMAPINFOHEADER 14))
          (data (get-block in size))
          (close-port in))
      (print "loaded " filename)
      (print "    width: " width)
      (print "    height: " height)
      (print "    bitcount: " bitcount)

   (let ((id (GenTextures 1)))
      (print "id: " (refb id 0))

      (glBindTexture GL_TEXTURE_2D (refb id 0))
      (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA width height 0
         (case bitcount
           (24 GL_BGR)
           (32 GL_BGRA))
         GL_UNSIGNED_BYTE data)
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)

      (list->ff (list
        (cons 'id   (refb id 0))
        (cons 'width  width)
        (cons 'height height))))))

(define (sprite filename) #false)

))
