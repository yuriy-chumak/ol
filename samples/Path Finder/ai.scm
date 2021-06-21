(define-library (ai)
(import (otus lisp)
   (scheme dynamic-bindings))

(export
   unhash
   new-creature)

(begin

   (define unique-id (make-parameter 1))
   (define (generate-unique-id)
      (let ((id (unique-id)))
         (unique-id (+ id 1))
         id))

;; ;; ; ---------------------------------------------------------------------------
;; ;; ; assert (start point is free)
(define (horizontal-bresenham x1 y1  x2 y2  dk at)
; assert обе точки не внутри стен
; Проверка более горизонтальных чем вертикальных линий
; 1. выровнять стартовую (слева) точку вправо
; 3. проверить первый куб (слева от стартовой) (через индикатор y = -1)
; 4. в цикле проверять правый куб для точки (и левый, если перешли через координатную прямую - по у)
(let* ((x (+ 1 (floor x1)))
       (y (+ y1 (* (- x x1) dk))))
(let loop ((x x) (y y) (oldy -1)  (n (- (ceil x2) x)))
   (if (= n 0)
      #t
   (if (and
          (not (= y oldy)) ; если пересекли линию стен
          (> (at (- x 1) (floor y)) 0))
      #f
   (if (> (at x (floor y)) 0)
      #f
   (loop (+ x 1) (+ y dk) y (- n 1))))))))


(define (vertical-bresenham x1 y1  x2 y2  dk at)
(let* ((y (+ 1 (floor y1)))
       (x (+ x1 (* (- y y1) dk))))
(let loop ((x x) (y y) (oldx -1)  (n (- (ceil y2) y)))
   (if (= n 0)
      #t
   (if (and
          (not (= x oldx)) ; если пересекли линию стен
          (> (at (floor x) (- y 1)) 0))
      #f
   (if (> (at (floor x) y) 0)
      #f
   (loop (+ x dk) (+ y 1) y (- n 1))))))))

(define (is-point-can-see-point x1 y1  x2 y2  at)
; подразумевается, что начальная и конечная точки НЕ в стене
(let ((dx (- x2 x1))
      (dy (- y2 y1)))
   (if (> (abs dx) (abs dy)) ; горизонтальный
      (if (> dx 0)
         (horizontal-bresenham x1 y1  x2 y2  (/ dy dx) at)
         (horizontal-bresenham x2 y2  x1 y1  (/ dy dx) at))
      (if (> dy 0)
         (vertical-bresenham x1 y1  x2 y2  (/ dx dy) at)
         (vertical-bresenham x2 y2  x1 y1  (/ dx dy) at)))))

; не обрабатываем карты больше 2^16 размером
(define (hash x y)
   (+ (<< x 16) y))
(define (unhash key)
   (values (>> key 16)
           (mod key 65536)))
           ;(band key (<< 1 16))))


(define (new-creature)
(let* ((id (generate-unique-id))
       (creature (fork-server id (lambda ()
   ; x, y - положение AI
   (let this ((fov {}) (x 1) (y 1)) ; пустое, очень одинокое место...
   (let* ((envelope (wait-mail))
          (sender msg envelope))
      (case msg
         (['die]
            #false) ; закончим сопрограмму

         (['set-location location]
            (print "creature: 'set-location " location)
            (this fov (car location) (cdr location)))
         (['get-location]
            (mail sender (cons x y))
            (this fov x y))
         ; двигаться
         (['move dx dy]
            (let ((cell (car (fov (hash (+ x dx) (+ y dy)) '(+ . 1)))))
               (if (eq? cell 0)
                  (this fov (+ x dx) (+ y dy))
               else
                  (this fov x y))))

         (['get-fov]
            (mail sender fov)
            (this fov x y))

         ; обновить "свою" карту мира (fov) на основе заданной карты (map)
         ; осмотреться
         (['look-around map]
               (define WIDTH (size (car map)))
               (define HEIGHT (size map))
               ; смотрим из "середины" точки
               (define X (+ x 0.5))
               (define Y (+ y 0.5))
               (define (at x y) (ref (ref map y) x))

               (this
                  (fold (lambda (ff x)
                           (fold (lambda (ff y)
                                    (if (or
                                          (is-point-can-see-point X Y    x         y      at)
                                          (is-point-can-see-point X Y (+ x 1)      y      at)
                                          (is-point-can-see-point X Y (+ x 1)   (+ y 1)   at)
                                          (is-point-can-see-point X Y    x      (+ y 1)   at)
                                          (is-point-can-see-point X Y (+ x 1/2) (+ y 1/2) at))
                                          ;; вот тут надо бы проапдейтить вейпоинты, которые зависят от этого блока (?)
                                       (put ff (hash x y) (cons (ref (ref map y) x) 600)) ; 100 - сколько ходов будем помнить
                                    else
                                       ff))
                              ff (iota HEIGHT 1)))
                     ; понемногу будем "забывать" карту
                     (ff-fold (lambda (ff key value)
                           (if (eq? (cdr value) 0)
                              ff
                           else
                              (put ff key (cons (car value) (-- (cdr value))))))
                        {} fov)
                     (iota WIDTH 1))
                  x y))

         ; построить путь к клетке (to-x to-y)
         (['A* to-x to-y]
            (let ((hash2 (lambda (xy)
                     (hash (car xy) (cdr xy))))
                  ; пуста ли клетка карты "в голове" персонажа, работает для любых координат, даже отрицательных
                  (floor? (lambda (x y)
                     ; будем считать, что отрицательные координаты (спорное решение) или неизвестные места - свободная дорога
                     (or (< x 1) (< y 1) (eq? (car (fov (hash x y) '(0 . 0))) 0)))))
            ; отправить назад результат работы алгоритма
            (mail sender
            (if (and (= x to-x) (= y to-y)) ; уже пришли
               [0 0 #empty #empty]
            else ; строим путь
               (let step1 ((n 999) ; максимальное количество шагов поиска
                           (c-list-set #empty) ; пустой "закрытый" список
                           (o-list-set {(hash x y)  [(cons x y) #f  0 0 0]})) ; "открытый" список с единственным стартовым элементом
                  ;; (print "step1 " n)
                  ;; (print "   c: " c-list-set)
                  ;; (print "   o: " o-list-set)

                  (if (eq? o-list-set #empty)
                     [0 0 #empty #empty] ; упс, не можем найти дороги
                  else
                     ; найдем клетку с минимальной стоимостью:
                     (let*((f (ff-fold (lambda (s key value)
                                          (if (< (ref value 5) (car s))
                                             (cons (ref value 5) value)
                                             s))
                                 (cons 99999 #f) o-list-set))
                           (xy (ref (cdr f) 1)) ; положение клетки с минимальным весом '(x.y)
                           ; перенесем ее из открытого в закрытый список
                           (o-list-set (del o-list-set (hash2 xy)))
                           (c-list-set (put c-list-set (hash2 xy) (cdr f))))

                        ; закончились ходы, или достигли нужной точки
                        (if (or (eq? n 0)
                                (equal? xy (cons to-x to-y)))
                           (let rev ((xy xy))
                              ; обратный проход по найденному пути, вернуть только первый шаг
                              ;  (просто в сторону предполагаемого маршрута, если ходов больше нет - спорное решение)
                              (let*((parent (ref (get c-list-set (hash2 xy) #f) 2)) ; todo: переделать
                                    (parent-of-parent (ref (get c-list-set (hash2 parent) #f) 2)))
                                 (if parent-of-parent (rev parent)
                                    (vector
                                       (- (car xy) (car parent))
                                       (- (cdr xy) (cdr parent))
                                       c-list-set
                                       o-list-set))))
                        else
                           ; 5: Проверяем все соседние клетки.
                           ;  Игнорируем те, которые находятся в закрытом списке или непроходимы
                           ;  (поверхность со стенами, водой), остальные добавляем в открытый список,
                           ;  если они там еще не находятся. Делаем выбранную клетку "родительской"
                           ;  для всех этих клеток.
                           (let*((x (car xy))
                                 (y (cdr xy))
                                 (o-list-set (fold (lambda (n v)
                                                ;; (print "n: " n ", v: " v)
                                                ;; (print (car v) "/" (cdr v))
                                                ;; (print (floor? (car v) (cdr v)))
                                                ;; (print "---")
                                                (if (and
                                                      (floor? (car v) (cdr v)) ; если туда можно передвинуться...
                                                      (eq? #f (get c-list-set (hash2 v) #f)))
                                                   (let ((G (+ (ref (get c-list-set (hash2 xy) #f) 3) 1)); G родителя + 1
                                                         ; H calculated by "Manhattan method"
                                                         ; http://www2.in.tu-clausthal.de/~zach/teaching/info_literatur/A_Star/A_star_tutorial/heuristics.htm.html
                                                         (H (* (+ (abs (- (car v) to-x))
                                                                  (abs (- (cdr v) to-y)))
                                                               2))
                                                         ; 6: Если соседняя клетка уже находится в открытом списке
                                                         (got (get o-list-set (hash2 v) #f)))

                                                      ; если эта клетка уже в списке
                                                      (if got
                                                         (if (< G (ref got 3)) ; но наш путь короче
                                                            (put n (hash2 v)  [v xy  G H (+ G H)])
                                                            ;else ничего не делаем
                                                            n)
                                                         ; else
                                                         (put n (hash2 v)  [v xy  G H (+ G H)])))
                                                   n))
                                                o-list-set (list
                                                               (cons x (- y 1))
                                                               (cons x (+ y 1))
                                                               (cons (- x 1) y)
                                                               (cons (+ x 1) y)))))
                              (step1 (- n 1) c-list-set o-list-set)))))))))
            (this fov x y))


         (else
            (print "ai error: unknown command " msg)
            (this fov x y))))) ))))
   (print "new creature " id " spawned.")
   id))

))