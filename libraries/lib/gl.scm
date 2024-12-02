(define-library (lib gl)
   (version 1.0)
   (license MIT/LGPL3)
   (description "otus-lisp gl library")
(import
   (otus lisp)
   (otus case-apply)
   (lib gl config)
   (OpenGL platform))

(export
   gl:set-window-title gl:set-window-size
   gl:set-renderer ; OpenGL rendering function
   gl:set-prerenderer ; OpenGL pre-rendering function
   gl:set-calculator ; Math and Phys calculations

   ; event handlers
   gl:set-mouse-handler
   gl:set-keyboard-handler
   gl:set-gamepad-handler

   gl:set-resize-handler

   ; getters
   gl:window-dimensions ; variable
   gl:get-window-width gl:get-window-height ; functions

   ; additional functions
   gl:hide-cursor
   gl:redisplay ; (swap buffers)

   gl:force-render
   gl:swap-buffers

   ; * internal use (with gl3 and gl4)
   native:enable-context native:disable-context
   native:swap-buffers
   hook:exit)

   ; todo:
   ;gl:finish ; if renderer exists - wait for window close, else just glFinish

(begin
   (define WIDTH  (get config 'width  854))
   (define HEIGHT (get config 'height 480))

   ; assume that window size can not be large than 16777215 for x32 build
   ;                                  and 72057594037927935 for x64 build.
   (define STATE [0 0 WIDTH HEIGHT]) ; x y width height

   ; public getters
   (define gl:window-dimensions STATE)

   (define (gl:get-window-width)
      (ref STATE 3))
   (define (gl:get-window-height)
      (ref STATE 4))

   (define GL_VR_HINT #x12001)
   (define GL_VIEWPORT #x0BA2)
)

(begin
   (setq glGetIntegerv (GL_LIBRARY fft-void "glGetIntegerv" fft-int (fft& fft-int)))
)
; -=( native functions )=-------------------------------------
(cond-expand
   (Android (include "lib/gl/Android.lisp"))
   (Linux (include "lib/gl/Linux.lisp"))
   (Emscripten (include "lib/gl/WebGL.lisp"))
   (Windows (include "lib/gl/Windows.lisp"))
   (else (runtime-error "Unsupported platform" *uname*)))

; -=( opengl coroutine )=-------------------------------------
(cond-expand
   ((or Emscripten) ; special case
      (begin
         (actor 'opengl (lambda ()
         (let loop ((dictionary {
               ; defaults:
               'resize-handler (lambda (w h) (glViewport 0 0 w h))
         }))
            (let*((envelope (wait-mail))
                  (sender msg envelope))
               (case msg
                  ; low level interface:
                  (['set key value]
                     (loop (put dictionary key value)))
                  (['get key]
                     (mail sender (dictionary key #false))
                     (loop dictionary))
                  (['debug]
                     (mail sender dictionary)
                     (loop dictionary))

                  (['atexit]
                     ;; (unless (get dictionary 'renderer #f)
                     ;;    ; рендерера нет, значит оновим буфер
                     ;;    (gl:SwapBuffers (get dictionary 'context #f)))
                     ;;    ; рендерер есть, но режим интерактивный? тогда вернем управление юзеру
                     ;;    ;(if *interactive* ;(or (zero? (length (command-line))) (string-eq? (car (command-line)) "-"))
                     ;;    ;   (mail sender 'ok)))
                     (loop (put dictionary 'customer sender)))

                  (else
                     (runtime-error "Unknown opengl server command " msg)
                     (loop dictionary))
               )))))))
   (else
      (begin
         ; smart call to renderer
         (define (render this)
            (define renderer (this 'renderer #f))
            (when renderer
               (define mouse (os:GetMousePos (this 'context #f)))
               (define (draw eye)
                  (case-apply renderer
                     (list 0)
                     (list 1 mouse)
                     (list 2 mouse {
                        'mouse mouse
                        'eye eye
                        ; TBD:
                        'option1 #true
                        'option2 #false
                     })))

               (if (this 'vr-mode) ;; VR mode
               then
                  (glHint GL_VR_HINT 1) ; включает использование VR матриц
                  ((this 'vr-begin))
                  (for-each (lambda (eye)
                        ((this 'vr-eye) eye)  ; prepare rendering through the eye
                        ; mirror actual viewport to the window:
                        (glGetIntegerv GL_VIEWPORT gl:window-dimensions)
                        (draw eye)
                        ((this 'vr-finish)))  ; rendering through eye is finished
                     '(1 2)) ; left eye = 1, right eye = 2
                  ((this 'vr-end)) ; 
                  (glHint GL_VR_HINT 0)
                  (native:swap-buffers (this 'context []))
               else
                  (draw #f) ;; regular mode
                  (native:swap-buffers (this 'context []))
               )))

         ; main OpenGL actor
         (actor 'opengl (lambda ()
         (let loop ((this {
               ; defaults
               'resize-handler (lambda (w h) (glViewport 0 0 w h))
               'vr-mode #false
               'autorender setup:autorender-mode
         }))
         (cond
            ((check-mail) => (lambda (envelope)
               (let*((sender msg envelope))
                  (case msg
                     ; low level interface:
                     ('this
                        (mail sender this)
                        (loop this))

                     (['set key value]
                        (loop (put this key value)))
                     (['get key]
                        (mail sender (this key #false))
                        (loop this))
                     (['debug] ; todo: remove
                        (mail sender this)
                        (loop this))

                     (['atexit]  ; wait for OpenGL window closing (just no answer for interact)
                        ;; (unless (get this 'renderer #f)
                        ;;    ; если рендерера нет, просто оновим буфер
                        ;;    (gl:SwapBuffers (get this 'context #f)))
                        ;;    ; рендерер есть, но режим интерактивный? тогда вернем управление юзеру
                        ;;    ;(if *interactive* ;(or (zero? (length (command-line))) (string-eq? (car (command-line)) "-"))
                        ;;    ;   (mail sender 'ok)))
                        (loop (put this 'customer sender)))

                     ; setters
                     (['set-window-title title]
                        (os:SetWindowTitle (this 'context #f) title)
                        (loop this))

                     (['set-window-size width height]
                        (os:SetWindowSize (this 'context #f) width height)
                        (glViewport 0 0 width height)
                        (loop this))

                     (['set-resize-handler resize-handler]
                        (if resize-handler ; force fire resize function
                           (resize-handler (ref STATE 3) (ref STATE 4)))
                        (loop (put this 'resize-handler resize-handler)))

                     ; events
                     (['resize width height]
                        (set-ref! STATE 3 width)  ; cache current window dimensions
                        (set-ref! STATE 4 height)

                        (let ((resize-handler (this 'resize-handler #f)))
                           (if resize-handler (resize-handler width height)))
                        (loop this))

                     (else
                        (runtime-error "Unknown opengl server command " msg)
                        (loop this))))))

            ; блок непосредственно рабочего цикла окна
            ((this 'context #f) => (lambda (context)
               (let ((prerenderer (this 'prerenderer #f))
                     (calculator (this 'calculator #f)))
                  ; 1. обработаем сообщения (todo: не более чем N за раз)
                  (native:process-events context (lambda (event)
                     (case event
                        (['quit] (halt 0))
                        (['mouse button x y]
                           ((this 'mouse-handler (lambda (b x y) #f)) button x y))
                        (['keyboard key pressed]
                           (define handler (this 'keyboard-handler #f))
                           (case (arity handler)
                              (1 (if pressed (handler key)))
                              (2 (handler key pressed))))
                        (['gamepad event key value]
                           ((this 'gamepad-handler (lambda (e k v) #f)) event key value))
                        (else
                           #false))))
                  ; 2. вычисляем мир
                  (if calculator
                     (calculator))
                  ; 3. и нарисуем его
                  (if prerenderer
                     (prerenderer))
                  ; 4. 
                  (when (this 'autorender #f)
                     (render this))

                  ; 4. done
                  (sleep 0)
                  (loop this))))
            (else
               (sleep 1)
               (loop this)))))))))

(begin

; -=( main )=--------------------------
; force window creation.
(let ((context (native:create-context "Ol: OpenGL Window")))
   (if context
      (mail 'opengl ['set 'context context])
      (runtime-error "Can't create OpenGL context")))

(define (gl:redisplay)
   (native:swap-buffers
      (await (mail 'opengl ['get 'context]))))

(define (gl:force-render)
   (render (await (mail 'opengl 'this))))

(define (gl:swap-buffers)
   (native:swap-buffers
      (await (mail 'opengl ['get 'context]))))

; ----------------------------------------------------------
; just change a function
(define (gl:set-renderer renderer)
   (mail 'opengl ['set 'renderer renderer]))

(define (gl:set-prerenderer renderer)
   (mail 'opengl ['set 'prerenderer renderer]))

(define (gl:set-calculator calculator)
   (mail 'opengl ['set 'calculator calculator]))

; do some thing on change
(define (gl:set-window-title title)
   (mail 'opengl ['set-window-title title]))

(define (gl:set-window-size width height)
   (mail 'opengl ['set-window-size width height]))

(define hook:exit (lambda args
   (await (mail 'opengl ['atexit]))))

; -----------------------------
;; (define gl:Color (case-lambda
;;    ((r g b)
;;       (glColor3f r g b))))

(define (gl:hide-cursor)
   (os:HideCursor (await (mail 'opengl ['get 'context]))))

(define (gl:set-mouse-handler handler)
   (mail 'opengl ['set 'mouse-handler handler]))

(define (gl:set-keyboard-handler handler)
   (mail 'opengl ['set 'keyboard-handler handler]))

(define (gl:set-gamepad-handler handler)
   (mail 'opengl ['set 'gamepad-handler handler]))

(define (gl:set-resize-handler handler)
   (mail 'opengl ['set-resize-handler handler]))

))
