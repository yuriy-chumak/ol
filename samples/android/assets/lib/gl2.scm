(define-library (lib gl2)
   (import
      (otus lisp)
      (otus ffi))

   (export
      gl:set-renderer

      *atexit*)
   (import
      (lib gl config)
      (EGL version-1-1))
(begin

(define WIDTH  (get config 'width  854))
(define HEIGHT (get config 'height 480))
(define CONFIG (list->ff '( ; todo: move to config
   (red   .  8)
   (green .  8)
   (blue  .  8)
   (depth . 24)
)))


; =============================================
; automation
(fork-server 'opengl (lambda ()
(let this ((dictionary #empty))
(cond
   ; блок обработки сообщений
   ((check-mail) => (lambda (e) ; can be (and (eq? something 0) (check-mail)) =>
      (let*((sender msg e))
         ;(print "envelope: " envelope)
         (tuple-case msg
            ; low level interface:
            ((set key value)
               (this (put dictionary key value)))
            ((get key)
               (mail sender (get dictionary key #false))
               (this dictionary))
            ((debug)
               (mail sender dictionary)
               (this dictionary))

            ((finish)  ; wait for OpenGL window closing (just no answer for interact)
               ;(glFinish)

               (unless (get dictionary 'renderer #f)
                  ; рендерера нет, значит оновим буфер
                  (let ((context (get dictionary 'context #false)))
                     (if context
                        (eglSwapBuffers (ref context 1)(ref context 2))))
                  ; рендерер есть, но режим интерактивный? тогда вернем управление юзеру
                  (if (or (zero? (length *vm-args*)) (string-eq? (car *vm-args*) "-"))
                     (mail sender 'ok)))
               (this (put dictionary 'customer sender)))

            ; context
            ((set-context context)
               (this (put dictionary 'context context)))
            ((get-context)
               (mail sender (get dictionary 'context #f))
               (this dictionary))

            ; renderer
            ((set-renderer renderer)
               (this (put dictionary 'renderer renderer)))
            ((get-renderer)
               (mail sender (get dictionary 'renderer #f))
               (this dictionary))

            (else
               (print-to stderr "Unknown opengl server command " msg)
               (this dictionary))))))

   ; блок непосредственно рабочего цикла окна
   (else
      ; обработаем сообщения (todo: не более чем N за раз)
      (let ((context (get dictionary 'context #f)))
         (if context ; todo: добавить обработку кнопок
            #true))

      ; вызовем рисовалку (есть есть)
      (let ((renderer (get dictionary 'renderer #f)))
         (if renderer (begin
            (renderer #false) ; no mouse for android
            (let ((context (get dictionary 'context #false)))
               (if context
                  (eglSwapBuffers (ref context 1)(ref context 2)))))))

         ; done.
         (sleep 1)
         (this dictionary))))))


; -=( main )=--------------------------
; force window creation.
(define (gl:create-context)
   (let ((self (load-dynamic-library #f))
         (android (load-dynamic-library "libandroid.so")))
   (let ((ANativeWindow_setBuffersGeometry (android fft-void "ANativeWindow_setBuffersGeometry" fft-void* fft-int fft-int fft-int)))
      (define display (eglGetDisplay EGL_DEFAULT_DISPLAY))

      (define major '(0)) (define minor '(0))
      (eglInitialize display major minor)
      (print "EGL version: " (car major) "." (car minor))

      (define attribs (list
	        EGL_SURFACE_TYPE EGL_WINDOW_BIT
	        EGL_BLUE_SIZE 8
	        EGL_GREEN_SIZE 8
	        EGL_RED_SIZE 8
	        EGL_NONE))
      (define config (make-vptr))
      (define numConfigs '(0))
      (eglChooseConfig display attribs config 1 numConfigs)

      (define format '(0))
      (eglGetConfigAttrib display config EGL_NATIVE_VISUAL_ID format)
      (print "format: " format)

      (define window (syscall 1002 #f #f #f))
      (ANativeWindow_setBuffersGeometry window 0 0 (car format))

      (define surface (eglCreateWindowSurface display config window #f))
      (define context (eglCreateContext display config #false #false))

      (eglMakeCurrent display surface surface context)

      (define width '(0))
      (define height '(0))
      (eglQuerySurface display surface EGL_WIDTH width)
      (eglQuerySurface display surface EGL_HEIGHT height)
      (print "window: " width "x" height)

      (mail 'opengl (tuple 'set-context (tuple display surface window context))))))

; окно у нас уже есть, создадим и активируем контекст
(gl:create-context)

; -----------------------------
(define (gl:set-renderer renderer)
   (mail 'opengl (tuple 'set-renderer renderer)))

(define (gl:finish)
   (interact 'opengl (tuple 'finish)))

(define (gl:swap-buffers)
   (let ((context (interact 'opengl (tuple 'get 'context))))
      (eglSwapBuffers (ref context 1)(ref context 2))))

(define *atexit* gl:finish)
))