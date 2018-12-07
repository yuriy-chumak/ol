(define-library (lib gl console)
   (import (otus lisp)
      (lib gl)
;      (lib soil)
      ;; (OpenGL ARB clear_texture)
      (lib freetype))

   (cond-expand
      (Android
         (import (OpenGL ES version-1-1))
         (begin
            (setq FONT "/sdcard/fonts/Anonymous Pro Minus.ttf")))
      (else
         (import (OpenGL version-1-4))
         (begin
            (setq FONT "fonts/Anonymous Pro.ttf"))))

   (export
      move-to ; x y, передвинуть курсор в координаты x,y внутри текущего окна

      set-color ; '(R G B), установить екущий цвет в терминах RGB палитры
      BLACK BLUE GREEN CYAN RED MAGENTA BROWN GRAY YELLOW WHITE
      LIGHTGRAY LIGHTBLUE LIGHTGREEN LIGHTCYAN LIGHTRED LIGHTMAGENTA

      create-window ; x y width height, создать новое окно
      set-window-background set-window-border
      set-window-writer ; (lambda (w) ...), задать обработчик контента окна

      render-windows ; глобальная функция отрисовки всех окон
      windows-make-selection ; вернуть привязанную к знакоместу метаинформацию
   )
   ; ...

(begin
   (setq cursor '(0 . 0)) ; текущее положение курсора
   ;(setq fcolor '(0)) ; текущий цвет, тоже пока не используется
   (setq config:cell-size '(9 . 16)) ; размер знакоместа, пока не используется
   (setq drawing-area (tuple 0 0 0 0))

   ; let's preparte the font
   (define ft (make-FT_Library))
   (setq error (FT_Init_FreeType ft))
   (unless (eq? error 0)
      (runtime-error "Can't access freetype library" error))

   ; load font
   (define face (make-FT_Face))
   (setq error (FT_New_Face ft (c-string FONT) 0 face))
   (unless (eq? error 0)
      (runtime-error "Can't load Anonymous Pro font" error))

   ; slot for character bitmaps
   (define slot (face->glyph face))

   ; скомпилируем текстурный атлас
   (define atlas '(0))
   (glGenTextures 1 atlas)
   (glBindTexture GL_TEXTURE_2D (car atlas))
   (glPixelStorei GL_UNPACK_ALIGNMENT 1)
   ;; (glPixelStorei GL_PACK_ALIGNMENT 1)

   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)

   ; создадим текстуру
   (glTexImage2D GL_TEXTURE_2D 0 GL_ALPHA 320 512 0 GL_ALPHA GL_UNSIGNED_BYTE #f)

   ; здесь мы модифицируем поведение glTexSubImage2D так, что бы точки шрифта всегда были засвечены
   ;(glPixelTransferf GL_RED_BIAS 1)
   ;(glPixelTransferf GL_GREEN_BIAS 1)
   ;(glPixelTransferf GL_BLUE_BIAS 1)
   ; почистим нашу текстуру, так как может попасть мусор (не надо?)
   ;; (if glClearTexImage
   ;;    (glClearTexImage (car atlas) 0 GL_LUMINANCE_ALPHA GL_UNSIGNED_BYTE (bytevector 250)))

   ; задаем символы, которые будут в нашем атласе
   ; словарь буква -> текстура
   (define charset
   (let ((symbols (fold append #null (list
            (iota (- 127 #\space) #\space)
            ; additional latin capital, todo.
            (string->runes "АБВГҐДЕЁЄЖЗИІЇЙКЛМНОПРСТУЎФХЦЧШЩЪЫЬЭЮЯ")
            (string->runes "абвгґдеёєжзиіїйклмнопрстуўфхцчшщъыьэюя")))))
      ; пускай знакоместо будет 16 в высоту (что дает нам 32 строки) и 9(10) в ширину (32 колонки) - итого, 1024 символов; 1 лишняя точка для того, чтобы символы не накладывались

      ; зададим размер символов
      (FT_Set_Pixel_Sizes face 0 (cdr config:cell-size))

      (list->ff
         (map (lambda (char i)
               (FT_Load_Char face char FT_LOAD_RENDER)
               (let ((x (+ (* 10 (mod i 32)) 1)) ; 113
                     (y (+ (* 16 (div i 32)) -3))
                     (bitmap (glyph->bitmap slot)))
                  (glTexSubImage2D GL_TEXTURE_2D 0
                     (+ x (ref bitmap 1)) ; x
                     (+ y (- 16 (ref bitmap 3))) ; y
                     (ref bitmap 2) (ref bitmap 4) ; width, height
                     GL_ALPHA GL_UNSIGNED_BYTE (ref bitmap 5)) ; was GL_ALPHA
                  (cons char (tuple (/ (mod i 32) 32) (/ (div i 32) 32)))))
            symbols
            (iota (length symbols) 0)))))

   ;(glPixelTransferf GL_RED_BIAS 0)
   ;(glPixelTransferf GL_GREEN_BIAS 0)
   ;(glPixelTransferf GL_BLUE_BIAS 0)
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

   (define (move-to x y)
      (set-car! cursor x)
      (set-cdr! cursor y))

   (define (number->string n)
      (list->string (render-number n null 10)))

   ; internal function
   (define (write . text)
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
                              (let ((uv (getf charset char)))
                                 (if uv
                                    (let ((u (ref uv 1))
                                          (v (ref uv 2)))
                                       (glTexCoord2f u (+ v 1/32))
                                       (glVertex2f x (+ y 1))
                                       (glTexCoord2f (+ u 9/320) (+ v 1/32))
                                       (glVertex2f (+ x 1) (+ y 1))
                                       (glTexCoord2f (+ u 9/320) v)
                                       (glVertex2f (+ x 1) y)

                                       (glTexCoord2f u (+ v 1/32))
                                       (glVertex2f x (+ y 1))
                                       (glTexCoord2f (+ u 9/320) v)
                                       (glVertex2f (+ x 1) y)
                                       (glTexCoord2f u v)
                                       (glVertex2f x y))))
                              (do (+ x 1) y (cdr text))))))))

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
                     (if writer
                        (let ((vp '(0 0 0 0)))
                           (set-ref! drawing-area 1 (ref window 1))
                           (set-ref! drawing-area 2 (ref window 2))
                           ; а тут проведем тест на попадание мышки в выводимое знакоместо
                           (move-to 0 0)
                           (writer write)
                     ))
                     (loop (force (cdr ff))))))))))


   ; сопрограмма - главный обработчик графической консоли
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
                        (border #false) ; 7
                        (itself (put itself id (tuple x y width height writer background border))))
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
               ((set-window-border id color)
                  (let*((window (get itself id #false))
                        (itself (unless window itself
                           (put itself id (set-ref window 7 color)))))
                     (this itself)))

               ((draw)
                  (glEnable GL_BLEND)
                  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)

                  (glLoadIdentity)
                  (glOrtho 0 80 25 0 0 1)

                  (glEnable GL_TEXTURE_2D)
                  (glBindTexture GL_TEXTURE_2D (car atlas))

                  ; цикл по всем окнам
                  (let loop ((ff (ff-iter itself)))
                     (unless (null? ff)
                        (let ((window (cdar ff)))
                           (if (tuple? window) (tuple-apply window
                              (lambda (x y width height writer background border)
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
                                    (glVertex2f x y)
                                    (glVertex2f (+ x width 1/9) y)
                                    (glVertex2f (+ x width 1/9) (+ y height 1/16))
                                    (glVertex2f x (+ y height 1/16))
                                    (glEnd)
                                    (glEnable GL_TEXTURE_2D)))

                                 #true)))
                           (loop (force (cdr ff))))))

                  (glDisable GL_TEXTURE_2D)
                  (glDisable GL_BLEND)
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

   (define (set-window-border id color)
      (mail 'windows (tuple 'set-window-border id color)))

   (define (render-windows)
      (interact 'windows (tuple 'draw)))

   (define (windows-make-selection x y)
      (interact 'windows (tuple 'make-selection x y)))
))