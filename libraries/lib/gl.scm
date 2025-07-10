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

   gl:splash-screen

   ; * internal use (with gl3 and gl4)
   native:enable-context native:disable-context
   native:swap-buffers
   hook:exit
   
   (exports (OpenGL platform)))

   ; todo:
   ;gl:finish ; if renderer exists - wait for window close, else just glFinish

; -=( internal constants )=------------
(begin
   (define WIDTH  (config 'width  854))
   (define HEIGHT (config 'height 480))

   ; assume that window size can not be large than
   ; 16777215 for x32 build, and
   ; 72057594037927935 for x64 build.
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

; -=( native functions )=-------------------------------------
(begin
   (setq glGetIntegerv (GL_LIBRARY fft-void "glGetIntegerv" fft-int (fft& fft-int)))
)
(cond-expand
   (Android (include "lib/gl/Android.lisp"))
   (Linux (include "lib/gl/Linux.lisp"))
   (Emscripten (include "lib/gl/WebGL.lisp"))
   (Windows (include "lib/gl/Windows.lisp"))
   (else
      (runtime-error "Unsupported platform!!" (uname))))

; -=( opengl coroutine )=-------------------------------------
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
                  ((this 'vr-eye) eye) ; prepare rendering through the eye
                  ; mirror actual viewport to the window:
                  (glGetIntegerv GL_VIEWPORT gl:window-dimensions)
                  (draw eye)
                  ((this 'vr-flush)))  ; rendering through eye is finished
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
         'resize-handler (lambda (w h) (glViewport 0 0 w h)) ;todo: change window size
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
                  ((this 'resize-handler) width height)
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
         ; if we still have no context
         (sleep 1)
         (loop this))))))

   ; --------
   ; public function
   (define (gl:redisplay)
      (native:swap-buffers
         (await (mail 'opengl ['get 'context]))))

   (define (gl:force-render)
      (render (await (mail 'opengl 'this))))

   (define (gl:swap-buffers)
      (native:swap-buffers
         (await (mail 'opengl ['get 'context]))))

   ; ----------------------------------------------------------
   ; just call a function
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

   ; 
   (define (gl:splash-screen width height colors)
      ; load minimal function set
      (define glEnable      (GL_LIBRARY GLvoid "glEnable" GLenum))
         (define GL_TEXTURE_2D #x0DE1)
      (define glDisable     (GL_LIBRARY GLvoid "glDisable" GLenum))
      ;; (setq glGenTextures (GL_LIBRARY GLvoid "glGenTextures" GLsizei GLuint&))
      ;; (setq glDeleteTextures (GL_LIBRARY GLvoid "glDeleteTextures" GLsizei GLuint*))
      (define glBindTexture (GL_LIBRARY GLvoid "glBindTexture" GLenum GLuint))
      (define glTexParameteri (GL_LIBRARY GLvoid "glTexParameteri" GLenum GLenum GLint))
         (define GL_TEXTURE_MAG_FILTER #x2800)
         (define GL_TEXTURE_MIN_FILTER #x2801)
         (define GL_LINEAR #x2601)  (define GL_NEAREST #x2600)
      (define glTexImage2D  (GL_LIBRARY GLvoid "glTexImage2D" GLenum GLint GLint GLsizei GLsizei GLint GLenum GLenum fft-any))
         (define GL_RGB #x1907)
         (define GL_UNSIGNED_BYTE #x1401)
      (define glBegin       (GL_LIBRARY GLvoid "glBegin" GLenum))
         (define GL_QUADS #x0007)
      (define glEnd         (GL_LIBRARY GLvoid "glEnd"))
      (define glVertex2f    (GL_LIBRARY GLvoid "glVertex2f" GLfloat GLfloat))
      (define glTexCoord2f  (GL_LIBRARY GLvoid "glTexCoord2f" GLfloat GLfloat))

      ; upload texture
      ;; (glGenTextures 1 id)
      (glBindTexture GL_TEXTURE_2D 0) ;; (car id))
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
      (glTexImage2D GL_TEXTURE_2D 0 GL_RGB
         width height
         0 GL_RGB GL_UNSIGNED_BYTE colors)

      ; draw textured quad
      (glEnable GL_TEXTURE_2D)
      ;; (glColor3f 1 1 1)
      (glBegin GL_QUADS)
         (glTexCoord2f 0 0)
         (glVertex2f -1  1)
         (glTexCoord2f 0 1)
         (glVertex2f -1 -1)
         (glTexCoord2f 1 1)
         (glVertex2f  1 -1)
         (glTexCoord2f 1 0)
         (glVertex2f  1  1)
      (glEnd)

      ; show it
      (gl:redisplay)

      ; done.
      ;; (glDeleteTextures 1 id)
      (glDisable GL_TEXTURE_2D)
   )

   ; -=( main )=--------------------------
   ; do a window creation:
   (let ((context (native:create-context "Ol: OpenGL Window")))
      (if context
         (mail 'opengl ['set 'context context])
         (runtime-error "Can't create OpenGL context")))

))
