(define-library (lib gl console)
   (import (otus lisp)
      (lib gl)
      (lib soil))

   (cond-expand
      (Android
         (import
            (OpenGL ES version-1-1))
         (begin
            (setq FONT "/sdcard/Anonymous_Pro_Minus.ttf")
            (define glClearTexImage #false)))
      (else
         (import
            (OpenGL version-1-4))
            ;(OpenGL ARB clear_texture))
         (begin
            (setq FONT "fonts/Anonymous Pro.ttf"))))

   (export
      move-to ; x y, передвинуть курсор в координаты x,y внутри текущего окна

      set-color ; '(R G B), установить текущий цвет в терминах RGB палитры
      BLACK BLUE GREEN CYAN RED MAGENTA BROWN GRAY YELLOW WHITE
      LIGHTGRAY LIGHTBLUE LIGHTGREEN LIGHTCYAN LIGHTRED LIGHTMAGENTA

      create-window ; x y width height, создать новое окно
      destroy-window ; window, удалить созданное окно
      show-window hide-window
      set-window-background set-window-border
      set-window-writer ; (lambda (w) ...), задать обработчик контента окна

      render-windows ; глобальная функция отрисовки всех окон
      windows-make-selection ; вернуть привязанную к знакоместу метаинформацию
   )

   (import
      (OpenGL SGIS generate_mipmap))
   ; ...

(begin
   (setq cursor '(0 . 0)) ; текущее положение курсора
   ;(setq fcolor '(0)) ; текущий цвет, тоже пока не используется
   (setq drawing-area #(0 0 0 0))

   ; скомпилируем текстурный атлас
   (define atlas (box 0))
   (glGenTextures 1 atlas)
   (glBindTexture GL_TEXTURE_2D (unbox atlas))

   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR_MIPMAP_LINEAR)
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR_MIPMAP_LINEAR)
   (glTexParameteri GL_TEXTURE_2D GL_GENERATE_MIPMAP_SGIS GL_TRUE)

   ; задаем символы, которые будут в нашем атласе
   ; please read samples/OpenGL/fonts/README.md

   ; словарь буква -> глиф на текстуре
   (import (file json))
   (import (owl parse))
   (define font (read-json-file "fonts/Anonymous Pro.json"))
   (define font (put font 'characters
      (ff-fold (lambda (ff key value)
                  (put ff (string-ref (symbol->string key) 0) value))
         #empty
         (get font 'characters #empty))))

   (define width (box 0))
   (define height (box 0))
   (define channels (box 0))
   (let ((file (file->bytevector (font 'file))))
      (define image (SOIL_load_image_from_memory file (size file) width height channels SOIL_LOAD_AUTO)) ;SOIL_LOAD_RGBA
      (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA (unbox width) (unbox height) 0 GL_RGBA GL_UNSIGNED_BYTE image)
      (SOIL_free_image_data image))

   ; font precompilation:
   (define charset
      (ff-map (lambda (key glyph) [
            ; texture coordinates
            (inexact (/    (glyph 'x)                  (font 'width)))
            (inexact (/    (glyph 'y)                  (font 'height)))
            (inexact (/ (+ (glyph 'x) (glyph 'width) ) (font 'width)))
            (inexact (/ (+ (glyph 'y) (glyph 'height)) (font 'height)))
            
            ; glyph coordinates
            (glyph 'originX)
            (glyph 'originY)
            (- (glyph 'originX) (glyph 'width))
            (- (glyph 'originY) (glyph 'height))
            ])
         (font 'characters)))

   (define font-size (font 'size 16))
   (define font-width (floor (* font-size 9/16)))
   ; atlas creation finished

   ; -----------------------------
   (define (set-color rgb)
      (apply glColor3f rgb))

   ; яркостные компоненты палитры
   (setq q #x55/255) (setq a #xAA/255) (setq f #xFF/255)
   ; full CGA 16-color palette
   (define BLACK (list 0 0 0))
   (define BLUE (list 0 0 a))
   (define GREEN (list 0 a 0))
   (define CYAN (list 0 a a))
   (define RED (list a 0 0))
   (define MAGENTA (list a 0 a))
   (define BROWN (list a q 0))
   (define GRAY (list q q q))
   (define YELLOW (list f f q))
   (define WHITE (list f f f))
   (define LIGHTGRAY (list a a a))
   (define LIGHTBLUE (list q q f))
   (define LIGHTGREEN (list q f q))
   (define LIGHTCYAN (list q f f))
   (define LIGHTRED (list f q q))
   (define LIGHTMAGENTA (list f q f))

   ; установить новую позицию курсора в окне
   (define (move-to x y)
      (set-car! cursor x)
      (set-cdr! cursor y))

   ; internal function
   (define (write . text)
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
                        (let ((tc (charset char #false))
                              (x (+ (* x font-width) 2))
                              (y (+ (* y font-size) font-size -12)))
                           (if tc (vector-apply tc
                              (lambda (l t r b  dx dy xw yh)
                                 (let*((lx (- x dx))
                                       (ty (- y dy))
                                       (rx (- x xw))
                                       (by (- y yh)))
                                    (glTexCoord2f l t)
                                    (glVertex2f lx ty)
                                    (glTexCoord2f r t)
                                    (glVertex2f rx ty)
                                    (glTexCoord2f r b)
                                    (glVertex2f rx by)

                                    (glTexCoord2f l t)
                                    (glVertex2f lx ty)
                                    (glTexCoord2f r b)
                                    (glVertex2f rx by)
                                    (glTexCoord2f l b)
                                    (glVertex2f lx by))))))
                        (do (+ x 1) y (cdr text))))))))

      (for-each (lambda (text)
            (cond
               ; строка - текст
               ((string? text)
                  (echo text))
               ((number? text)
                  (echo (number->string text)))
               ; список - цвет
               ((list? text)
                  (set-color text))

               ((symbol? text) #t) ; игнорировать, символы обратавывает другая функция
               ((function? text) #t) ; аналогично

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
      (define selection (list #f))
      (call/cc (lambda (return)
         (define (test text)
            (let do ((x (+ (ref drawing-area 1) (car cursor)))
                     (y (+ (ref drawing-area 2) (cdr cursor)))
                     (text (string->runes text)))
               (if (null? text)
                  (move-to (- x (ref drawing-area 1)) (- y (ref drawing-area 2))) ; закончили печатать, переместим курсор
                  ; иначе отрендерим символ
                  (let ((char (car text)))
                     (case char
                        (#\space
                           (when (and (eq? x X) (eq? y Y)) ; если мы в позиции мыши
                              (return (car selection)))
                           (do (+ x 1) y (cdr text)))
                        (#\newline
                           (do (ref drawing-area 1) (+ y 1) (cdr text)))
                        (else
                           (when (and (eq? x X) (eq? y Y)) ; если мы в позиции мыши
                              (return (car selection)))
                           (do (+ x 1) y (cdr text))))))))

         (let ((write (lambda text
                  (set-car! selection #false)
                  (for-each (lambda (text)
                        (cond
                           ((string? text)
                              (test text))
                           ((number? text)
                              (test (number->string text)))
                           ((symbol? text)
                              (set-car! selection text))
                           ((function? text)
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
                     (when writer
                        (set-ref! drawing-area 1 (ref window 1))
                        (set-ref! drawing-area 2 (ref window 2))
                        ; а тут проведем тест на попадание мышки в выводимое знакоместо
                        (move-to 0 0)
                        (writer write))
                     (loop (force (cdr ff))))))))))


   ; сопрограмма - главный обработчик графической консоли
   (coroutine 'windows (lambda ()
      (let this ((itself #empty))
         (let*((envelope (wait-mail))
               (sender msg envelope))
            (case msg
               ; low level interaction interface
               (['set key value]
                  (this        (put itself key value)))
               (['get key]
                  (mail sender (get itself key #false))
                  (this itself))
               (['debug]
                  (mail sender itself)
                  (this itself))

               (['create x y width height]
                  (let*((id (get itself 'id 1))
                        (itself (put itself 'id (+ id 1)))
                        (writer #false) ; 5
                        (background #false) ; 6
                        (border #false) ; 7
                        (visible #true) ; 8
                        (itself (put itself id [x y width height writer background border visible])))
                     (mail sender id)
                     (this itself)))
               (['destroy window]
                  (this (del itself window)))
               (['set-window-writer id writer]
                  (let*((window (get itself id #false))
                        (itself (if (not window) itself
                           (put itself id (set-ref window 5 writer)))))
                     (this itself)))
               (['set-window-background id color]
                  (let*((window (get itself id #false))
                        (itself (if (not window) itself
                           (put itself id (set-ref window 6 color)))))
                     (this itself)))
               (['set-window-border id color]
                  (let*((window (get itself id #false))
                        (itself (if (not window) itself
                           (put itself id (set-ref window 7 color)))))
                     (this itself)))
               (['set-window-visibility id visibility]
                  (let*((window (get itself id #false))
                        (itself (if (not window) itself
                           (put itself id (set-ref window 8 visibility)))))
                     (this itself)))

               (['draw]
                  (glEnable GL_BLEND)
                  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)

                  (glPushMatrix)
                  (glLoadIdentity)
                  (glOrtho 0 (* 80 font-width) (* 25 font-size) 0 0 1) ; размер нашей консольки
                  ; todo: высчитывать ее динамически

                  (glEnable GL_TEXTURE_2D)
                  (glBindTexture GL_TEXTURE_2D (car atlas))

                  ; цикл по всем окнам
                  (let loop ((ff (ff-iter itself)))
                     (unless (null? ff)
                        (let ((window (cdar ff)))
                           (if (and (vector? window) (ref window 8)) (vector-apply window
                              (lambda (x y width height writer background border visible)
                                 (set-ref! drawing-area 1 (ref window 1))
                                 (set-ref! drawing-area 2 (ref window 2))
                                 (move-to 0 0) ; курсор в начало окна, полюбому

                                 ; отрисовка фона окна
                                 (if background (begin
                                    (glDisable GL_TEXTURE_2D)
                                    (apply glColor3f background)
                                    (glBegin GL_TRIANGLES)
                                    (let ((x (ref window 1))
                                          (y (ref window 2))
                                          (w (ref window 3))
                                          (h (ref window 4)))
                                       (glVertex2f x y)
                                       (glVertex2f x (+ y h))
                                       (glVertex2f (+ x w) (+ y h))

                                       (glVertex2f x y)
                                       (glVertex2f (+ x w) (+ y h))
                                       (glVertex2f (+ x w) y))
                                    (glEnd)
                                    (glEnable GL_TEXTURE_2D)))

                                 ; содержание окна
                                 (if writer (begin
                                    (apply glColor3f WHITE) ; сбросим цвет на дефолтный
                                    (glBegin GL_TRIANGLES)
                                       (writer write)
                                    (glEnd)))

                                 ; рамка
                                 (if border (begin
                                    (glDisable GL_TEXTURE_2D)
                                    (apply glColor3f border)
                                    (glBegin GL_LINE_LOOP)
                                    (glVertex2f (*    x        font-width) (*    y         font-size))
                                    (glVertex2f (* (+ x width) font-width) (*    y         font-size))
                                    (glVertex2f (* (+ x width) font-width) (* (+ y height) font-size))
                                    (glVertex2f (*    x        font-width) (* (+ y height) font-size))
                                    (glEnd)
                                    (glEnable GL_TEXTURE_2D)))

                                 #true)))
                           (loop (force (cdr ff))))))

                  (glPopMatrix)
                  (glDisable GL_TEXTURE_2D)
                  (glDisable GL_BLEND)

                  (mail sender 'ok)
                  (this itself))

               (['make-selection x y]
                  (mail sender (make-selection itself
                     (floor (/ (* x 80) (ref gl:window-dimensions 3)))
                     (floor (/ (* y 25) (ref gl:window-dimensions 4)))))
                  (this itself))

               (else
                  (print-to stderr "Unknown windows command: " msg)
                  (this itself)))))))

   (define (create-window x y width height)
      (await (mail 'windows ['create x y width height])))
   (define (destroy-window window)
      (mail 'windows ['destroy window]))

   (define (show-window id)
      (mail 'windows ['set-window-visibility id #true]))
   (define (hide-window id)
      (mail 'windows ['set-window-visibility id #false]))

   (define (set-window-writer id writer)
      (mail 'windows ['set-window-writer id writer]))

   (define (set-window-background id color)
      (mail 'windows ['set-window-background id color]))

   (define (set-window-border id color)
      (mail 'windows ['set-window-border id color]))

   (define (render-windows)
      (await (mail 'windows ['draw])))

   (define (windows-make-selection x y)
      (await (mail 'windows ['make-selection x y])))
))