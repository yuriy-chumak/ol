(define-library (lib gl console)
   (import (otus lisp)
      (lib gl)
      (OpenGL version-1-4)
      (lib freetype))

   (export
      move-to
      write-at

      set-color
      BLACK BLUE GREEN CYAN RED MAGENTA BROWN GRAY YELLOW WHITE
      LIGHTGRAY LIGHTBLUE LIGHTGREEN LIGHTCYAN LIGHTRED LIGHTMAGENTA

      create-window
      set-window-writer
      set-window-background

      render-windows
      windows-make-selection
   )
   ; ...

(begin
   (setq cursor '(0 . 0)) ; текущее положение курсора, пока не используется
   (setq fcolor '(0)) ; текущий цвет, тоже пока не используется
   (setq config:cell-size '(9 . 16)) ; размер знакоместа, пока не используется
   (setq drawing-area (tuple 0 0 0 0))

   ; let's preparte the font
   (define ft (make-FT_Library))
   (setq error (FT_Init_FreeType ft))
   (unless (eq? error 0)
      (runtime-error "Can't access freetype library" error))

   ; load font
   (define face (make-FT_Face))
   (setq error (FT_New_Face ft (c-string "fonts/Anonymous Pro.ttf") 0 face))
   (unless (eq? error 0)
      (runtime-error "Can't load Anonymous Pro font" error))

   ; slot for character bitmaps
   (define slot (face->glyph face))

   (setq error (FT_Set_Pixel_Sizes face 0 16))

   (define (set-color rgb)
      (glPixelMapusv GL_PIXEL_MAP_I_TO_R 256 (repeat (lref rgb 0) 256))
      (glPixelMapusv GL_PIXEL_MAP_I_TO_G 256 (repeat (lref rgb 1) 256))
      (glPixelMapusv GL_PIXEL_MAP_I_TO_B 256 (repeat (lref rgb 2) 256))
      (glPixelMapusv GL_PIXEL_MAP_I_TO_A 256 (iota 256 0 256)))

   ; яркостные компоненты палитры
   (setq q 21845) (setq a 45690) (setq f 65535)
   ; full CGA 16-color palette
   (define BLACK (list 0 0 0))
   (define BLUE (list 0 0 a))
   (define GREEN (list 0 a 0))
   (define CYAN (list 0 a a))
   (define RED (list a 0 0))
   (define MAGENTA (list a 0 a))
   (define BROWN (list a q 0))
   (define GRAY (list a a a))
   (define YELLOW (list f f q))
   (define WHITE (list f f f))
   (define LIGHTGRAY (list a a a))
   (define LIGHTBLUE (list q q f))
   (define LIGHTGREEN (list q f q))
   (define LIGHTCYAN (list q f f))
   (define LIGHTRED (list f q q))
   (define LIGHTMAGENTA (list f q f))

   (define (move-to x y)
      (set-car! cursor x)
      (set-cdr! cursor y))

   (define (number->string n)
      (list->string (render-number n null 10)))

   ; internal function
   (define (write . text)
      (glPixelStorei GL_UNPACK_ALIGNMENT 1)
      (glPixelZoom 1 -1)
      (define viewport '(0 0 0 0))
      (glGetIntegerv GL_VIEWPORT viewport)
      (for-each (lambda (text)
            (define (echo text)
               (let do ((x (+ (ref drawing-area 1) (car cursor)))
                        (y (+ (ref drawing-area 2) (cdr cursor)))
                        (text (string->runes text)))
                  (if (null? text)
                     (move-to (- x (ref drawing-area 1)) (- y (ref drawing-area 2))) ; закончили печатать, переместим курсор
                     ; иначе отрендерим символ
                     (let ((char (car text)))
                        (case char
                           (#\space
                              (do (+ x 1) y (cdr text)))
                           (#\newline
                              (do (ref drawing-area 1) (+ y 1) (cdr text)))
                           (else
                              (FT_Load_Char face char FT_LOAD_RENDER)
                              (let ((bitmap (glyph->bitmap slot)))
                                 (glWindowPos2iv (list
                                    (+ (* x 9) (ref bitmap 1))
                                    (- (- (lref viewport 3) (* y 16)) (- 16 (ref bitmap 3) 4)))) ; 4 - поправочный коэффициент в вертикальном позиционировании
                                 (glDrawPixels (ref bitmap 2) (ref bitmap 4) GL_COLOR_INDEX GL_UNSIGNED_BYTE (ref bitmap 5))
                                 (do (+ x 1) y (cdr text)))))))))

            (cond
               ; строка - текст
               ((string? text)
                  (echo text))
               ((number? text)
                  (echo (number->string text)))
               ; список - цвет
               ((list? text)
                  (set-color text))
               ((number? text)
                  ; TODO: вывести на экран число
                  #false)
               
               ((symbol? text) #t) ; игнорировать, символы обратавывает другая функция
               
               (else
                  #false)))
         text))

   (define (write-at x y text)
      (move-to x y)
      (write text))

   ; internal function:
   ; возвращает привязанный к знакоместу символ.
   ; привязки смотреть в описании функции write
   (define (make-selection windows X Y)
      (call/cc (lambda (return)
         (let ((write (lambda text
                  (define selection (list #f))
                  (for-each (lambda (text)
                        (cond
                           ((string? text)
                              (let do ((x (+ (ref drawing-area 1) (car cursor)))
                                       (y (+ (ref drawing-area 2) (cdr cursor)))
                                       (text (string->runes text)))
                                 (if (null? text)
                                    (move-to (- x (ref drawing-area 1)) (- y (ref drawing-area 2))) ; закончили печатать, переместим курсор
                                    ; иначе отрендерим символ
                                    (let ((char (car text)))
                                       (case char
                                          (#\space
                                             (if (and (eq? x X) (eq? y Y)) ; если мы в позиции мыши
                                                (return (car selection)))
                                             (do (+ x 1) y (cdr text)))
                                          (#\newline
                                             (do (ref drawing-area 1) (+ y 1) (cdr text)))
                                          (else
                                             (if (and (eq? x X) (eq? y Y)) ; если мы в позиции мыши
                                                (return (car selection)))
                                             (do (+ x 1) y (cdr text))))))))
                           ((symbol? text)
                              (set-car! selection text))
                           ((eq? text #false)
                              (set-car! selection #false))
                           (else
                              #false)))
                     text))))
            (let loop ((ff (ff-iter windows)))
               (unless (null? ff)
                  (let*((window (cdar ff))
                        (writer (if window (ref window 5))))
                     (if writer
                        (let ((vp '(0 0 0 0)))
                           (set-ref! drawing-area 1 (ref window 1))
                           (set-ref! drawing-area 2 (ref window 2))
                           ; а тут проведем тест на попадание мышки в выводимое знакоместо
                           (move-to 0 0)
                           ;(print window " - " X ", " Y)
                           (writer write)
                     ))
                     (loop (force (cdr ff))))))))))

   (fork-server 'windows (lambda ()
      (let this ((itself #empty))
         (let*((envelope (wait-mail))
               (sender msg envelope))
            (tuple-case msg
               ; low level interaction interface
               ((set key data)
                  (let ((itself (put itself key data)))
                     (this itself)))
               ((get key)
                  (mail sender (get itself key #false))
                  (this itself))
               ((debug)
                  (mail sender itself)
                  (this itself))

               ((create x y width height)
                  (let*((id (get itself 'id 1))
                        (itself (put itself 'id (+ id 1)))
                        (writer #false) ; 5
                        (background #false) ; 6
                        (itself (put itself id (tuple x y width height writer background))))
                     (mail sender id)
                     (this itself)))
               ((set-window-writer id writer)
                  (let*((window (get itself id #false))
                        (itself (unless window itself
                           (put itself id (set-ref window 5 writer)))))
                     (this itself)))
               ((set-window-background id color)
                  (let*((window (get itself id #false))
                        (itself (unless window itself
                           (put itself id (set-ref window 6 color)))))
                     (this itself)))

               ((draw)
                  (glEnable GL_BLEND)
                  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
                  (glEnable GL_SCISSOR_TEST)
                  ; цикл по всем окнам
                  (let loop ((ff (ff-iter itself)))
                     (unless (null? ff)
                        (let*((window (cdar ff))
                              (writer (if window (ref window 5)))
                              (background (if window (ref window 6))))
                           (if writer
                              (let ((vp '(0 0 0 0))) ; оригинальный вьюпорт
                                 (glGetIntegerv GL_VIEWPORT vp) ; save viewport
                                 (if background (begin
                                    (glScissor
                                       (* (ref window 1) (car config:cell-size))
                                       (- (lref vp 3)
                                          (* (ref window 2) (cdr config:cell-size))
                                          (* (ref window 4) (cdr config:cell-size)))
                                       (* (ref window 3) (car config:cell-size))
                                       (* (ref window 4) (cdr config:cell-size)))
                                    (glClearColor (lref background 0) (lref background 1) (lref background 2) 1)
                                    (glClear GL_COLOR_BUFFER_BIT)))
                                 (glViewport (lref vp 0) (lref vp 1) (lref vp 2) (lref vp 3)) ; restore viewport

                                 (set-ref! drawing-area 1 (ref window 1))
                                 (set-ref! drawing-area 2 (ref window 2))
                                 ; и наконец выведем на экран текст
                                 (move-to 0 0)
                                 (writer write)))
                           (loop (force (cdr ff))))))
                  (glDisable GL_SCISSOR_TEST)
                  (mail sender 'ok)
                  (this itself))

               ((make-selection x y)
                  (mail sender (make-selection itself
                     (floor (/ x (car config:cell-size)))
                     (floor (/ y (cdr config:cell-size)))))
                  (this itself))

               (else
                  (print-to stderr "Unknown windows command: " msg)
                  (this itself)))))))

   (define (create-window x y width height)
      (interact 'windows (tuple 'create x y width height)))

   (define (set-window-writer id writer)
      (mail 'windows (tuple 'set-window-writer id writer)))
   
   (define (set-window-background id color)
      (mail 'windows (tuple 'set-window-background id color)))

   (define (render-windows)
      (interact 'windows (tuple 'draw)))

   (define (windows-make-selection x y)
      (interact 'windows (tuple 'make-selection x y)))
))