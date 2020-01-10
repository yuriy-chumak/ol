; https://www.rosettacode.org/wiki/Stack#Ol

(define stack #null)
(print "stack is: " stack)
(print "is stack empty: " (eq? stack #null))

(print "* pushing 1")
(define stack (cons 1 stack))
(print "stack is: " stack)
(print "is stack empty: " (eq? stack #null))

(print "* pushing 2")
(define stack (cons 2 stack))
(print "stack is: " stack)
(print "is stack empty: " (eq? stack #null))

(print "* pushing 3")
(define stack (cons 3 stack))
(print "stack is: " stack)
(print "is stack empty: " (eq? stack #null))

(print "* poping")
(define-values (value stack) (uncons stack #f))
(print "value: " value)
(print "stack: " stack)
(print "is stack empty: " (eq? stack #null))

(print "* poping")
(define-values (value stack) (uncons stack #f))
(print "value: " value)
(print "stack: " stack)
(print "is stack empty: " (eq? stack #null))

(print "* poping")
(define-values (value stack) (uncons stack #f))
(print "value: " value)
(print "stack: " stack)
(print "is stack empty: " (eq? stack #null))

(print "* poping")
(define-values (value stack) (uncons stack #f))
(print "value: " value)
(print "stack: " stack)
(print "is stack empty: " (eq? stack #null))

(fork-server 'stack (lambda ()
   (let this ((me '()))
      (let*((envelope (wait-mail))
            (sender msg envelope))
         (case msg
            (['empty]
               (mail sender (null? me))
               (this me))
            (['push value]
               (this (cons value me)))
            (['pop]
               (cond
                  ((null? me)
                     (mail sender #false)
                     (this me))
                  (else
                     (mail sender (car me))
                     (this (cdr me))))))))))
(define (push value)
   (mail 'stack ['push value]))
(define (pop)
   (interact 'stack ['pop]))
(define (empty)
   (interact 'stack ['empty]))

(for-each (lambda (n)
      (print "pushing " n)
      (push n))
   (iota 5 1)) ; '(1 2 3 4 5)

(let loop ()
   (print "is stack empty: " (empty))
   (unless (empty)
      (print "popping value, got " (pop))
      (loop)))
(print "done.")
