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

(define (make-room unused)
   (let*((w (randint min_room_xy max_room_xy))
         (h (randint min_room_xy max_room_xy))
         (x (randint 1 (- width w 1)))
         (y (randint 1 (- height h 1))))
      (list x y w h)))

;; (define (corridor_between_points x1 y1 x2 y2 join_type) ; default join_type = 'either
;;    ;(print "corridor_between_points: " x1 " " y1 " - " x2 " " y2 " : " join_type)
;;    (if (or (= x1 x2) (= y1 y2))
;;       (list (cons x1 x2) (cons y1 y2))
;;       (let ((join (if (eq? join_type 'either)
;;                      (cond
;;                         ((not (null? (intersect '(0 1) (list x1 x2 y1 y2))))
;;                            'bottom)
;;                         ((not (null? (intersect (list (- width 1) (- width 2)) (list x1 x2))))
;;                            'top)
;;                         ((not (null? (intersect (list (- height 1) (- height 2)) (list y1 y2))))
;;                            'top)
;;                         (else
;;                            (list-ref (list 'top 'bottom) (rand! 2))))
;;                      join_type)))
;;          (cond
;;             ((eq? join 'top)
;;                (list (cons x1 y1) (cons x1 y2) (cons x2 y2)))
;;             ((eq? join 'bottom)
;;                (list (cons x1 y1) (cons x2 y1) (cons x2 y2)))
;;             (else
;;                (print "error corridor creation"))))))

;; (define (join_rooms room1 room2 join_type) ; default join_type = 'either
;;    ; resort rooms
;;    (let*((room1 room2 (if (> (list-ref room1 0) (list-ref room2 0)) (values room2 room1) (values room1 room2))))
;;       (let ((x1 (lref room1 0))
;;             (y1 (lref room1 1))
;;             (w1 (lref room1 2))
;;             (h1 (lref room1 3))

;;             (x2 (lref room2 0))
;;             (y2 (lref room2 1))
;;             (w2 (lref room2 2))
;;             (h2 (lref room2 3)))

;;       (let ((x1_2 (+ x1 w1 -1))
;;             (y1_2 (+ y1 h1 -1))
;;             (x2_2 (+ x2 w2 -1))
;;             (y2_2 (+ y2 h2 -1)))
;;          (cond
;;             ; overlapping on x
;;             ((and
;;                (< x1 (+ x2 w2))
;;                (< x2 (+ x1 w1)))
;;                (let*((jx1 (randint x2 x1_2))
;;                      (jx2 jx1)
;;                      (tmp (sort < (list y1 y2 y1_2 y2_2)))
;;                      (jy1 (+ (lref tmp 1) 1))
;;                      (jy2 (- (lref tmp 2) 1)))
;;                   (corridor_between_points jx1 jy1 jx2 jy2 'either)))
;;             ; overlapping on y
;;             ((and
;;                (< y1 (+ y2 h2))
;;                (< y2 (+ y1 h1)))
;;                (let*((jy1 (if (> y2 y1)
;;                                  (randint y2 y1_2)
;;                                  (randint y1 y2_2)))
;;                      (jy2 jy1)
;;                      (tmp (sort < (list x1 x2 x1_2 x2_2)))
;;                      (jx1 (+ (lref tmp 1) 1))
;;                      (jx2 (- (lref tmp 2) 1)))
;;                   (corridor_between_points jx1 jy1 jx2 jy2 'either)))
;;             ; no overlap
;;             (else
;;                (let ((join (if (eq? join_type 'either)
;;                               (list-ref (list 'top 'bottom) (rand! 2))
;;                               join_type)))
;;                   (cond
;;                      ((eq? join 'top)
;;                         (if (> y2 y1)
;;                            (let ((jx1 (+ x1_2 1))
;;                                  (jy1 (randint y1 y1_2))
;;                                  (jx2 (randint x2 x2_2))
;;                                  (jy2 (- y2 1)))
;;                               (corridor_between_points jx1 jy1 jx2 jy2 'bottom))
;;                            (let ((jx1 (randint x1 x1_2))
;;                                  (jy1 (- y1 1))
;;                                  (jx2 (- x2 1))
;;                                  (jy2 (randint y2 y2_2)))
;;                               (corridor_between_points jx1 jy1 jx2 jy2 'top))))
;;                      ((eq? join 'bottom)
;;                         (if (> y2 y1)
;;                            (let ((jx1 (randint x1 x1_2))
;;                                  (jy1 (+ y1_2 1))
;;                                  (jx2 (- x2 1))
;;                                  (jy2 (randint x2 x2_2)))
;;                               (corridor_between_points jx1 jy1 jx2 jy2 'top))
;;                            (let ((jx1 (+ x1_2 1))
;;                                  (jy1 (randint y1 y1_2))
;;                                  (jx2 (randint x2 x2_2))
;;                                  (jy2 (+ y2_2 1)))
;;                               (corridor_between_points jx1 jy1 jx2 jy2 'bottom))))
;;                      (else
;;                         (print "invalid direction"))))))))))

; generate the level with rooms and corridors
(define (gen_level)
   (define rooms
      (map make-room (iota (* max_rooms 5))))
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

; debug output:
(import (lib rlutil))

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