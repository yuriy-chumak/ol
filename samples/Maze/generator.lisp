#!/usr/bin/ol
(import (otus random!))
(define (randint a b) (+ a (rand! (- b a -1)))) ; a <= x <= b

(define WIDTH 64)
(define HEIGHT 64)

(define width WIDTH)
(define height HEIGHT)
(define max_rooms 15)
(define min_room_xy 5)
(define max_room_xy 10)
(define rooms_overlap #false)
(define random_connections 1)
(define random_spurs 3)
(define tiles (list->ff '((stone . " ") (floor . ".") (wall . "#"))))
(define level (map (lambda (?) (repeat 'stone WIDTH)) (iota HEIGHT)))
(define room_list '())
(define corridor_list '())
(define tiles_level '())

(define (gen_room)
   (let*((w (randint min_room_xy max_room_xy))
         (h (randint min_room_xy max_room_xy))
         (x (randint 1 (- width w 1)))
         (y (randint 1 (- height h 1))))
      (list x y w h)))
(define (room_overlapping room room_list)
   (apply (lambda (x y w h)
      (call/cc (lambda (return)
         (for-each (lambda (r)
               (if (and
                     (< x (+ (list-ref r 0) (list-ref r 2)))
                     (< (list-ref r 0) (+ x w))
                     (< y (+ (list-ref r 1) (list-ref r 3)))
                     (< (list-ref r 1) (+ y h)))
                  (return #true)))
            room_list)
         #false)))
      room))

(define (corridor_between_points x1 y1 x2 y2 join_type) ; default join_type = 'either
   #false
)

(define (join_rooms room1 room2 join_type) ; default join_type = 'either
   #false
)

(define (gen_level)
   (define max_iters (* max_rooms 5))
   (define room_list
      (fold (lambda (room_list a)
               (if (>= (length room_list) max_rooms)
                  room_list
                  (let ((tmp_room (gen_room)))
                     (if (or
                           rooms_overlap
                           (null? room_list)
                           (not (room_overlapping tmp_room room_list)))
                        (cons tmp_room room_list)
                        room_list))))
         #null (iota max_iters)))

   ; ...
   #false
   room_list
)

(define (gen_tiles_level)
   ;...
   #false
)

;
(define rooms
   (gen_level))

(print rooms)
(import (lib rlutil))
(for-each (lambda (room)
            (locate (list-ref room 0) (list-ref room 1))
            (for-each (lambda (?) (display "#")) (iota (list-ref room 2)))
            (for-each (lambda (y)
                  (locate (list-ref room 0) (+ (list-ref room 1) y))
                  (display "#")
                  (for-each (lambda (?) (display " ")) (iota (- (list-ref room 2) 2)))
                  (display "#"))
               (iota (- (list-ref room 3) 2) 1))
            (locate (list-ref room 0) (+ (list-ref room 1) (list-ref room 3) -1))
            (for-each (lambda (?) (display "#")) (iota (list-ref room 2))))
   rooms)
