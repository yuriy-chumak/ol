#!/usr/bin/ol
(import (otus random!))
(define (randint a b) (+ a (rand! (- b a -1)))) ; a <= x <= b

(import (lib rlutil))

(define WIDTH 127) ; should be odd
(define HEIGHT 49) ; should be odd
(define max_rooms 7)


(define width WIDTH)
(define height HEIGHT)
(define room_margin 5)
(define min_room_width 5)
(define max_room_width 15)
(define min_room_height 5)
(define max_room_height 15)

(define max_room_x 10)
(define min_room_x 5)
(define max_room_x 10)
(define rooms_overlap #false)
(define random_connections 1)
(define random_spurs 3)


; make the random room
(define (make-room unused)
   (let*((w (randint min_room_width max_room_width))
         (h (randint min_room_height max_room_height))
         (x (randint room_margin (- width w 1)))
         (y (randint room_margin (- height h 1))))
      (tuple x y w h)))

; пустой уровень (заполненный скальными породами)
(define level (map
   (lambda (?)
      (make-bytevector WIDTH #\#))
   (iota HEIGHT)))

; нагененинуем несколько комнат
(define rooms
   (map make-room (iota max_rooms)))

; и сразу отрисуем их на карте, конечно же
(for-each (lambda (room)
            (let ((x (ref room 1))
                  (y (ref room 2))
                  (w (ref room 3))
                  (h (ref room 4)))
               (for-each (lambda (y)
                     (for-each (lambda (x)
                           (set-ref! (lref level y) x #\space))
                        (iota w x)))
                  (iota h y))))
   rooms)

; лабиринт!
(define (unvisited? x y)
   (if (and (< 0 x WIDTH) (< 0 y HEIGHT))
      (eq? (ref (lref level y) x) #\#)))
;                         left      top       right     bottom
(define neighbors (tuple '(-2 . 0) '(0 . -2) '(+2 . 0) '(0 . +2)))
(define (shuffle! o)
   (for-each (lambda (i)
         (let ((a (ref o i))
               (j (+ 1 (rand! i))))
            (set-ref! o i (ref o j))
            (set-ref! o j a)))
      (reverse (iota (size o) 1)))
   o)

(for-each (lambda (y)
      (for-each (lambda (x)
            (if (unvisited? x y)
               (let loop ((x x) (y y))
                  
                  ; make current cell "visited"
                  (set-ref! (lref level y) x #\.)
                  (for-each (lambda (neighbor)
                        (if (unvisited? (+ x (car neighbor)) (+ y (cdr neighbor)))
                           (begin
                              (set-ref! (lref level (+ y (/ (cdr neighbor) 2))) (+ x (/ (car neighbor) 2)) #\.)
                              (loop (+ x (car neighbor)) (+ y (cdr neighbor))))))
                     (tuple->list (shuffle! neighbors))))))
         (iota (floor (/ WIDTH 2)) 1 2)))
   (iota (floor (/ HEIGHT 2)) 1 2))
   ; assert this cell is visited

; добавим каждой комнате по одному "выходу" (или по несколько)
; естественно, рандомно
(for-each (lambda (room)
            (let ((x (ref room 1)) (y (ref room 2))
                  (w (ref room 3)) (h (ref room 4)))
               (for-each (lambda (r)
                     (case (rand! 4)
                        (0 ; left
                           (set-ref! (lref level (+ y (rand! h))) (- x 1) #\space))
                        (1 ; top
                           (set-ref! (lref level (- y 1)) (+ x (rand! w)) #\space))
                        (2 ; right
                           (set-ref! (lref level (+ y (rand! h))) (+ x w) #\space))
                        (3 ; bottom
                           (set-ref! (lref level (+ y h)) (+ x (rand! w)) #\space))
                        (else
                           #false)))
                  (iota (rand! 4)))))
   rooms)

; уберем тупики
; todo


;; (for-each (lambda (y)
;;       (for-each (lambda (x)
;;             ; если отсюда можно начать строить лабиринт - надо построить


;;             (set-ref! (lref level y) x #\.))
;;          (iota w x)))
;;    (iota h y))))



; show the map
(cls)
(for-each (lambda (v) (print (vm:cast v type-string))) level)


,quit
; generate the level with rooms and corridors
(define (gen_level)
   (define rooms
      (map make-room (iota (* max_rooms 2))))




   ;(print "rooms: " rooms)
   (define joints #null)
      ;; (let loop ((joints #null) (rooms rooms))
      ;;    (if (null? (cdr rooms))
      ;;       joints
      ;;       (let ((joint (join_rooms (car rooms) (cadr rooms) 'either)))
      ;;          (loop (cons joint joints)
      ;;             (cdr rooms))))))

   (cons rooms joints))

(define (gen_tiles_level)
   ;...
   #false
)

;
(define rooms (gen_level))

(define corridors
   (let loop ((rooms (car rooms)) (corridors #null))
      (if (null? (cdr rooms))
         rooms
         (loop (cdr rooms) (cons (make-corridor (car rooms) (cadr rooms)) corridors)))))
(print "corridors: " corridors)


; debug output:

; make level
(define level (map
   (lambda (?)
      (list->string (repeat #\# WIDTH)))
   (iota HEIGHT)))

(for-each (lambda (room)
            (let ((x (lref room 0))
                  (y (lref room 1))
                  (w (lref room 2))
                  (h (lref room 3)))
               (for-each (lambda (y)
                     (for-each (lambda (x)
                           (set-ref! (lref level y) x #\.))
                        (iota w x)))
                  (iota h y))))
   (car rooms))

(cls)
(for-each print level)
(locate 1 1)
,quit
(for-each (lambda (joint)
            (let loop ((from joint))
               (unless (null? (cdr from))
                  (let ((a (car from))
                        (b (cadr from)))
                     (cond
                        ((eq? (car a) (car b)) ; вертикальная
                           (let ((y1 (min (cdr a) (cdr b)))
                                 (y2 (max (cdr a) (cdr b))))
                              (for-each (lambda (y)
                                          (locate (car a) y)
                                          (display "#"))
                                 (iota (- y2 y1) y1))))
                        ((eq? (cdr a) (cdr b)) ; горизонтальная
                           (let ((x1 (min (car a) (car b)))
                                 (x2 (max (car a) (car b))))
                              (for-each (lambda (x)
                                          (locate x (cdr a))
                                          (display "#"))
                                 (iota (- x2 x1) x1))))
                        (else
                           (print "unknown corridor")))
                     (loop (cdr from))))))
   (cdr rooms))
            ;; (locate (list-ref room 0) (list-ref room 1))
            ;; (for-each (lambda (?) (display "#")) (iota (list-ref room 2)))
            ;; (for-each (lambda (y)
            ;;       (locate (list-ref room 0) (+ (list-ref room 1) y))
            ;;       (display "#")
            ;;       (for-each (lambda (?) (display " ")) (iota (- (list-ref room 2) 2)))
            ;;       (display "#"))
            ;;    (iota (- (list-ref room 3) 2) 1))
            ;; (locate (list-ref room 0) (+ (list-ref room 1) (list-ref room 3) -1))
            ;; (for-each (lambda (?) (display "#")) (iota (list-ref room 2))))

#|
|#