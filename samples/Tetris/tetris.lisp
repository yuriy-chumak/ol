#!/usr/bin/env ol
(import (lib gl)
   (lib rlutil)
   (scheme misc) (otus random!))

(gl:set-window-title "Tetris")
(import (OpenGL version-1-0))

; -- фигуры -----------------------
(define схеми (pairs->ff `(
   (палка . (
      (#x66 #xff #xff) ; цвет
      .( ; занимаемое пространство (4 положения)
         ((0 . 0) (1 . 0) (2 . 0) (3 . 0))
         ((0 . 0) (0 . 1) (0 . 2) (0 . 3))
         ((0 . 0) (1 . 0) (2 . 0) (3 . 0))
         ((0 . 0) (0 . 1) (0 . 2) (0 . 3)))))
   (гак . (
      (#x00 #x99 #x99)
      .(
         ((0 . 0) (1 . 0) (2 . 0) (0 . 1))
         ((0 . 0) (0 . 1) (0 . 2) (1 . 2))
         ((0 . 1) (1 . 1) (2 . 1) (2 . 0))
         ((0 . 0) (1 . 0) (1 . 1) (1 . 2)))))
   (антигак . (
      (#xff #x66 #x00)
      .(
         ((1 . 0) (1 . 1) (1 . 2) (0 . 2))
         ((0 . 0) (0 . 1) (1 . 1) (2 . 1))
         ((1 . 0) (0 . 0) (0  .1) (0 . 2))
         ((0 . 0) (1 . 0) (2 . 0) (2 . 1)))))
   (куб . (
      (#xff #xff #x00)
      .(
         ((0 . 0) (1 . 0) (1 . 1) (0 . 1))
         ((0 . 0) (1 . 0) (1 . 1) (0 . 1))
         ((0 . 0) (1 . 0) (1 . 1) (0 . 1))
         ((0 . 0) (1 . 0) (1 . 1) (0 . 1)))))
   (вуж . (
      (#x00 #xff #x00)
      .(
         ((0 . 0) (1 . 0) (1 . 1) (2 . 1))
         ((1 . 0) (1 . 1) (0 . 1) (0 . 2))
         ((0 . 0) (1 . 0) (1 . 1) (2 . 1))
         ((1 . 0) (1 . 1) (0 . 1) (0 . 2)))))
   (антивуж . (
      (#xff #x00 #x00)
      .(
         ((0 . 1) (1 . 1) (1 . 0) (2 . 0))
         ((0 . 0) (0 . 1) (1 . 1) (1 . 2))
         ((0 . 1) (1 . 1) (1 . 0) (2 . 0))
         ((0 . 0) (0 . 1) (1 . 1) (1 . 2)))))
   (торт . (
      (#x40 #x40 #x40)
      .(
         ((0 . 0) (1 . 0) (2 . 0) (1 . 1))
         ((0 . 0) (0 . 1) (0 . 2) (1 . 1))
         ((0 . 1) (1 . 1) (2 . 1) (1 . 0))
         ((0 . 1) (1 . 0) (1 . 1) (1 . 2)))))
)))

(define (фон-цеглинки name)
   (let ((fig (getf схеми name)))
      (if fig
         (car fig))))
(define (план-цеглинки name rotation)
   (let ((fig (getf схеми name)))
      (if fig
         (list-ref (cdr fig) rotation))))

(define (ширина-цеглинки name rotation)
   (apply max (map car (план-цеглинки name rotation))))
(define (висота-цеглинки name rotation)
   (apply max (map cdr (план-цеглинки name rotation))))


;  в стакане у нас пусть лежат имена фигур (1-7)
(define склянка (list->vector (map (lambda (row)
   (list->vector (repeat #f 10)))
   (iota 20))))
; поточна (що падаэ) цеглинка
(define цеглинка [#f '(#f . #f) #f])

; что в стакане по координатам (x,y) / начиная с нуля /?
(define (зчитати x y)
   (ref (ref склянка (+ y 1)) (+ x 1)))
; записать что-то в стакан по координатам (x,y) / начиная с нуля /?
(define (вписати що x y)
   (set-ref! (ref склянка (+ y 1)) (+ x 1) що))

(define програв '(#f))
; -- utils ------------------------
(define (box x y)
   (glVertex2f x y)
   (glVertex2f (+ x 1) y)
   (glVertex2f (+ x 1) (+ y 1))
   (glVertex2f x (+ y 1)))
(define (color r g b)
   (glColor3f (/ r #xff) (/ g #xff) (/ b #xff)))

; -- logic ------------
(define (створити-нову-цеглинку!) ; текущая фигура
   (let ((name (list-ref (keys схеми) (rand! (length (keys схеми)))))
         (rotation (rand! 4)))
      (set-ref! цеглинка 1 name)
      (set-ref! цеглинка 3 rotation)
      (set-car! (ref цеглинка 2) (rand! (- 10 (ширина-цеглинки name rotation))))
      (set-cdr! (ref цеглинка 2) (- 19 (висота-цеглинки name rotation)))))

(створити-нову-цеглинку!)

; отрисовать фигуру
(define (покласти-цеглинку-в-склянку)
(let ((name (ref цеглинка 1))
      (rotation (ref цеглинка 3))
      (position (ref цеглинка 2)))
   (for-each (lambda (xy)
         (вписати name
            (+ (car position) (car xy))
            (+ (cdr position) (cdr xy))))
      (план-цеглинки name rotation))))

(define (забрати-цеглинку-з-склянки)
(let ((name (ref цеглинка 1))
      (rotation (ref цеглинка 3))
      (position (ref цеглинка 2)))
   (for-each (lambda (xy)
         (вписати #false
            (+ (car position) (car xy))
            (+ (cdr position) (cdr xy))))
      (план-цеглинки name rotation))))

; temp:
;(set-ref! цеглинка 1 'куб)
;(set-car! (ref цеглинка 2) 0)
;(set-cdr! (ref цеглинка 2) 18)
(покласти-цеглинку-в-склянку)

(define (намалювати-склянку)
   (glBegin GL_QUADS)
   (for-each (lambda (y)
      (for-each (lambda (x)
         (let ((фон (фон-цеглинки (зчитати x y))))
            (if фон
            (begin
               (color (car фон) (cadr фон) (caddr фон))
               (box x y)))))
         (iota 10))) (iota 20))
   (glEnd))

(define (ticks)
(let* ((ss ms (clock)))
   (+ (* ss 10)
      (floor (/ ms 100)))))

; -- main -------------------------
; 640 x 480
(gl:set-window-size 640 480)
(glViewport 0 0 640 480)

; init
(glShadeModel GL_SMOOTH)
(glClearColor 0.11 0.11 0.11 1)
(glOrtho -9 +19  -1 +21  -1 1)

(gl:set-userdata (ticks))

; draw
(gl:set-renderer (lambda (mouse)
   (glClear GL_COLOR_BUFFER_BIT)

   (let ((ss (gl:get-userdata))
         (s2 (ticks)))
      (unless (or (eq? ss s2) (car програв))
         ; 0. удалить текущую фигуру из стакана
         (забрати-цеглинку-з-склянки)

         ; todo: проверить есть ли заполненные полоски в стакане, и если есть - удалить
         ; проверить, можно ли фигурку сдвинуть туда, куда хочет юзер
         (if (zero? (mod s2 3)) ; 3 раза в секунду или пробел
         (let ((left (key-pressed #xff51))
               (right (key-pressed #xff53))
               (up (key-pressed #xff52)))
            (let ((shift
                     (cond
                        (left -1)
                        (right 1)
                        (else #f)))
                  (x (car (ref цеглинка 2)))
                  (y (cdr (ref цеглинка 2))))
               ; проверяем сдвиг
               (if shift
                  (if
                     (let ((схема (план-цеглинки (ref цеглинка 1) (ref цеглинка 3))))
                        (fold (lambda (q xy)
                           (and q
                              (let ((x (+ x (car xy) shift))
                                    (y (+ y (cdr xy))))
;                                    (print x ", " y)
                                 (and (>= x 0) (< x 10) (eq? (зчитати x y) #false)))))
                           #true схема))
                     (set-car! (ref цеглинка 2) (+ x shift))))
               ; проверка поворота фигуры
               (if up
                  (if
                     (let ((схема (план-цеглинки (ref цеглинка 1) (mod (+ (ref цеглинка 3) 3) 4))))
                        (fold (lambda (q xy)
                              (and q
                              (let ((x (+ x (car xy)))
                                    (y (+ y (cdr xy))))
                                 (and (>= x 0) (< x 10) (>= y 0) (eq? (зчитати x y) #false)))))
                           #true схема))
                     (let ((rotation (mod (+ (ref цеглинка 3) 3) 4)))
                        (print "rotation: " rotation)
                        (set-ref! цеглинка 3 rotation)))))))

         ; 2. проверить, можно ли опустить текущую фигуру вниз, да - опустить
         (if (or (zero? (mod s2 5)) (key-pressed #x0020)) ; 2 раза в секунду или пробел
         (let ((x (car (ref цеглинка 2)))
               (y (cdr (ref цеглинка 2))))
         (if
            (let ((схема (план-цеглинки (ref цеглинка 1) (ref цеглинка 3))))
               (fold (lambda (q xy)
                  (and q
                     (let ((x (+ x (car xy)))
                           (y (+ y (cdr xy))))
                        (and (> y 0) (eq? (зчитати x (- y 1)) #false))))) ; y>0 и внизу пусто
                  #true схема))
            ; then впадемо вниз на 1
            (set-cdr! (ref цеглинка 2) (- y 1))
            ; else створимо нову цеглинку
            (begin
               ; спочатку треба повернути цю
               (покласти-цеглинку-в-склянку)
               ; i от тепер можна познищувати заповненi полоски!

               (let ((нова-склянка
                        (let loop ((l #null) (f (vector->list склянка)))
                           (if (null? f) l
                              (if (has? (vector->list (car f)) #f)
                                 (loop (cons (car f) l) (cdr f))
                                 (loop l (cdr f)))))))
                  (unless (eq? (length нова-склянка) 20)
                     ; ну что ж, есть строчки на удаление
                     (let ((нова-склянка (list->vector (append (reverse нова-склянка) (repeat (list->vector (repeat #false 10)) (- 20 (length нова-склянка)))))))
                        (for-each (lambda (y)
                              (for-each (lambda (x)
                                    (set-ref! (ref склянка y) x (ref (ref нова-склянка y) x)))
                                 (iota 10 1)))
                           (iota 20 1)))))

               (створити-нову-цеглинку!)))))
            
         ; для каждой 

         ; 3. а если нет - добавить новую (если нельзя добавить - конец игры)
         (let ((x (car (ref цеглинка 2)))
               (y (cdr (ref цеглинка 2))))
         (unless
               (let ((схема (план-цеглинки (ref цеглинка 1) (ref цеглинка 3))))
                  (fold (lambda (q xy)
                           (and q
                              (let ((x (+ x (car xy)))
                                    (y (+ y (cdr xy))))
                                 (eq? (зчитати x y) #false))))
                     #true схема))
            (set-car! програв #true)))
         (покласти-цеглинку-в-склянку))
   (намалювати-склянку)
   (if (car програв)
      (begin
         (color #xFF 0 0)
         (glBegin GL_QUADS)
            (box -7 15)            (box -4 15)

                 (box -6 13) (box -5 13)
            (box -7 12)            (box -4 12)
         (glEnd)
   ;;       ; намалювати грусний смайл
         #t
   ;;       ;...
       ))

   ; сiточка
   (glColor3f 0.2 0.5 0.2)
   (glBegin GL_LINES)
      (map (lambda (y)
            (glVertex2f 0 y)
            (glVertex2f 10 y))
         (iota 21))

      (map (lambda (x)
            (glVertex2f x 0)
            (glVertex2f x 20))
         (iota 11))
   (glEnd)

   (gl:set-userdata s2))))
