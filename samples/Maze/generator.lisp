#!/usr/bin/ol
(import (otus random!))
(define (randint a b) (+ a (rand! (- b a -1)))) ; a <= x <= b

(import (lib rlutil))

(define WIDTH 127) ; should be odd
(define HEIGHT 49) ; should be odd
(define max_rooms 19)
(define random_connections 20)


(define width WIDTH)
(define height HEIGHT)
(define room_margin 5)
(define min_room_width 5)
(define max_room_width 35)
(define min_room_height 5)
(define max_room_height 15)

;; (define max_room_x 10)
;; (define min_room_x 5)
;; (define max_room_x 10)
;; (define rooms_overlap #false)
;; (define random_connections 1)
;; (define random_spurs 3)

(define (shuffle! o) ; перемешивалка векторов
   (for-each (lambda (i)
         (let ((a (ref o i))
               (j (+ 1 (rand! i))))
            (set-ref! o i (ref o j))
            (set-ref! o j a)))
      (reverse (iota (size o) 1)))
   o)


; make the random room
(define (make-room unused)
   (let*((w (randint min_room_width max_room_width))
         (h (randint min_room_height max_room_height))
         (x (randint room_margin (- width w room_margin)))
         (y (randint room_margin (- height h room_margin))))
      [
         (- x (modulo x 2) -1)
         (- y (modulo y 2) -1)
         (- w (modulo w 2) -1)
         (- h (modulo h 2) -1)]))

; пустой уровень (заполненный скальными породами)
(define level (map
   (lambda (?)
      (make-bytevector WIDTH #\#))
   (iota HEIGHT)))

; нагенеринуем несколько комнат
(define rooms
   (map make-room (iota max_rooms)))

; и сразу добавим их на карту, конечно же
(for-each (lambda (room)
            (let ((x (ref room 1)) (y (ref room 2))
                  (w (ref room 3)) (h (ref room 4)))
               (for-each (lambda (y)
                     (for-each (lambda (x)
                           (set-ref! (lref level y) x #\space))
                        (iota w x)))
                  (iota h y))))
   rooms)

; добавим каждой комнате по одному "выходу" (или по несколько), естественно, рандомно
; с них и будем строить "междукомнатный "лабиринт
; сервисные функции:
(define (granite? x y)
   (if (and (<= 0 x) (<= 0 y))
      (eq? (ref (lref level y) x) #\#)))
(define (wall? x y)
   (if (eq? (ref (lref level y) x) #\2)
      (print-to stderr x "-" y ": " (has? '(#\# #\< #\^ #\v #\>) (ref (lref level y) x))))
   (if (and (<= 0 x) (<= 0 y))
      (has? '(#\# #\< #\^ #\v #\>) (ref (lref level y) x))))

;(define (wall? x y)
;   (if (and (<= 0 x) (<= 0 y))
;      (not (eq? (ref (lref level y) x) #\space))))
;                         left      top       right     bottom
(define neighbors ['(-2 . 0) '(0 . -2) '(+2 . 0) '(0 . +2)])

; источники коридоров:
(define origins (apply append
   (map (lambda (room)
            (let ((x (ref room 1)) (y (ref room 2))
                  (w (ref room 3)) (h (ref room 4)))
               (map (lambda (r)
                        (let*((x y
                                 (case (rand! 4)
                                    (0 (values x (+ y (rand! h))))
                                    (1 (values (+ x (rand! w)) y))
                                    (2 (values (+ x w -1) (+ y (rand! h))))
                                    (3 (values (+ x (rand! w)) (+ y h -1))))))
                           (cons (- x (modulo x 2) -1)
                                 (- y (modulo y 2) -1))))
                     (iota (rand! random_connections)))))  ; предполагаемое количeство выходов из комнаты
      rooms)))

; пробъем дырки в стенах - выходы из комнаты
(for-each (lambda (xy)
      (let* ((x y xy))
         ; todo: рандомно вставить вместо дыры - дверь
         (if (wall? (+ x 1) y)
            (set-ref! (lref level y) (+ x 1) #\space))
         (if (wall? x (+ y 1))
            (set-ref! (lref level (+ y 1)) x #\space))
         (if (wall? (- x 1) y)
            (set-ref! (lref level y) (- x 1) #\space))
         (if (wall? x (- y 1))
            (set-ref! (lref level (- y 1)) x #\space))))
   origins)

; в промежутках между комнатами построим лабиринт
(for-each (lambda (xy)
   (let* ((x y xy))
      ; запустим генерацию лабиринта
      (let loop ((x x) (y y))
         (set-ref! (lref level y) x #\space)

         (for-each (lambda (neighbor)
               (if (granite? (+ x (car neighbor)) (+ y (cdr neighbor)))
                  (begin
                     (set-ref! (lref level (+ y (/ (cdr neighbor) 2))) (+ x (/ (car neighbor) 2)) #\space) ; пробъем стену
                     (loop (+ x (car neighbor)) (+ y (cdr neighbor)))
                     )))
            (vector->list (shuffle! neighbors)))
         ; дополнительный шаг - заложим проход, если это тупик
         (case (+ (if (wall? (+ x 1) y) #b0001 0) ; right
                  (if (wall? x (+ y 1)) #b0010 0) ; bottom
                  (if (wall? (- x 1) y) #b0100 0) ; left
                  (if (wall? x (- y 1)) #b1000 0)); top
            (#b1110
               (set-ref! (lref level y) (+ x 1) #\<)
               (set-ref! (lref level y) x #\<))
            (#b1101
               (set-ref! (lref level (+ y 1)) x #\v)
               (set-ref! (lref level y) x #\v))
            (#b1011
               (set-ref! (lref level y) (- x 1) #\>)
               (set-ref! (lref level y) x #\>))
            (#b0111
               (set-ref! (lref level (- y 1)) x #\^)
               (set-ref! (lref level y) x #\^)))
      )))
origins)

; финальная полировка подземелья
(for-each (lambda (line)
      (for-each (lambda (x)
            (unless (has? '(#\space #\. #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8) (ref line x))
               (set-ref! line x #\#)))
         (iota WIDTH)))
   level)

; show the map
(cls)
(for-each (lambda (v) (print (vm:cast v type-string))) level)
,quit

