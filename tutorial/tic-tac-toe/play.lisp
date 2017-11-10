#!/usr/bin/ol

(define rand!
   (let* ((ss ms (clock))
          (seed (band (+ ss ms) #xffffffff))
          (seed (cons (band seed #xffffff) (>> seed 24))))
      (lambda (limit)
         (let*((next (+ (car seed) (<< (cdr seed) 24)))
               (next (+ (* next 1103515245) 12345)))
            (set-car! seed (band     next     #xffffff))
            (set-cdr! seed (band (>> next 24) #xffffff))

            (mod (mod (floor (/ next 65536)) 32768) limit)))))

; protocol:
; first -> second: game?
; second <- first: ok
; first -> second: '(x . y), where x is position by x (1, 2, 3), y - position by y (1, 2, 3)
;  or
; nothing, if win, loss or draw

; начинать ли первому?
(if (and (eq? (length *vm-args*) 2)
         (string-eq? (second *vm-args*) "go"))
   (print 'game?))

(define (turn board)
   (let loop ()
      (let ((x (+ (rand! 3) 1))
            (y (+ (rand! 3) 1)))
         (if (not (eq? (ref (ref board y) x) 0))
            (loop)
            (values x y)))))

(define (check? board key)
   (or
      ; вертикальные
      (and (eq? (ref (ref board 1) 1) key)
           (eq? (ref (ref board 2) 1) key)
           (eq? (ref (ref board 3) 1) key))
      (and (eq? (ref (ref board 1) 2) key)
           (eq? (ref (ref board 2) 2) key)
           (eq? (ref (ref board 3) 2) key))
      (and (eq? (ref (ref board 1) 3) key)
           (eq? (ref (ref board 2) 3) key)
           (eq? (ref (ref board 3) 3) key))
      ; горизонтальные
      (and (eq? (ref (ref board 1) 1) key)
           (eq? (ref (ref board 1) 2) key)
           (eq? (ref (ref board 1) 3) key))
      (and (eq? (ref (ref board 2) 1) key)
           (eq? (ref (ref board 2) 2) key)
           (eq? (ref (ref board 2) 3) key))
      (and (eq? (ref (ref board 3) 1) key)
           (eq? (ref (ref board 3) 2) key)
           (eq? (ref (ref board 3) 3) key))
      ; диагональные
      (and (eq? (ref (ref board 1) 1) key)
           (eq? (ref (ref board 2) 2) key)
           (eq? (ref (ref board 2) 3) key))
      (and (eq? (ref (ref board 1) 3) key)
           (eq? (ref (ref board 2) 2) key)
           (eq? (ref (ref board 3) 1) key))))
   
(define (win? board)
   (check? board 1))      
(define (loss? board)
   (check? board 2))
(define (draw? board)
   (not (or
      (eq? (ref (ref board 1) 1) 0)
      (eq? (ref (ref board 2) 1) 0)
      (eq? (ref (ref board 3) 1) 0)
      (eq? (ref (ref board 1) 2) 0)
      (eq? (ref (ref board 2) 2) 0)
      (eq? (ref (ref board 3) 2) 0)
      (eq? (ref (ref board 1) 3) 0)
      (eq? (ref (ref board 2) 3) 0)
      (eq? (ref (ref board 3) 3) 0))))
   
(let main ((board #false))
   (print-to stderr "current board: " board)

   (define command (read))
   (cond
      ((eq? command 'game?)
         (let ((board (tuple (tuple 0 0 0) (tuple 0 0 0) (tuple 0 0 0))))
            (print 'ok)
            (main board)))
      ((eq? command 'no)
         (print "bye."))
      ((eq? command 'ok)
         (let ((board (tuple (tuple 0 0 0) (tuple 0 0 0) (tuple 0 0 0))))
            (let*((x y (turn board)))
               (print (cons x y)) ; first turn
               (set-ref! (ref board y) x 1)
               (main board))))
      (else
         (set-ref! (ref board (cdr command)) (car command) 2)
         (print "new board: " board)

         (cond
            ((loss? board)
               (print-to stderr "loss :(")
               (main #false))
            ((draw? board)
               (print-to stderr "draw!")
               (print "game?")
               (main #false))
            (else
               (let*((x y (turn board)))
                  (print (cons x y))
                  (set-ref! (ref board y) x 1)

                  (cond
                     ((win? board)
                        (print-to stderr "win! :)")
                        (print "game?")
                        (main #false))
                     ((draw? board)
                        (print-to stderr "draw!")
                        (main #false))
                     (else
                        (main board)))))))))
