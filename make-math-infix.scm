#!/usr/bin/ol

(print "; infix notation implementation
; please do not mix math ops with arguments,
; i mean no '1+2' but '1 + 2'
(define-library (owl math infix)
   (export
      MATH:)
   (import
      (owl math))
   (begin
      (define-syntax MATH:
         (syntax-rules (+ - * /)
            ; раскрытие внешних скобок
            ((MATH: (a))
               (MATH: a))
            ((MATH: (a .x))
               (MATH: a .x))

            ;; functions
            ((MATH: f (a (unquote .x)))
               (f a (MATH: .x)))
            ((MATH: f (.x))
               (f (MATH: .x)))
")

(for-each (lambda (op)
      (print "
      ;; a " op " b
      ((MATH: a      " op " b)
         (" op " (MATH: a)      (MATH: b)))
      ((MATH: a      " op " b(.bb))
         (" op " (MATH: a)      (MATH: b(.bb))))
      ((MATH: a(.aa) " op " b)
         (" op " (MATH: a(.aa)) (MATH: b)))
      ((MATH: a(.aa) " op " b(.bb))
         (" op " (MATH: a(.aa)) (MATH: b(.bb))))"))
   '(+ - * /))


(define cost {
   '+ 1
   '- 1
   '* 2
   '/ 2
})
(for-each (lambda (op)
      (print "      ;; a " op " b ...")
      (for-each (lambda (op2)
            (print "      ;; a " op " b " op2 " c")
            (print "      ((MATH: a      " op " b      " op2 " c      .x)")
            (if (less? (cost op) (cost op2))
               (print "         (MATH:  a      " op " (b       " op2 " c      .x)))")
               (print "         (MATH: (a      " op "  b     ) " op2 " c      .x))"))
            (print "      ((MATH: a      " op " b      " op2 " c(.cc) .x)")
            (if (less? (cost op) (cost op2))
               (print "         (MATH:  a      " op " (b       " op2 " c(.cc) .x)))")
               (print "         (MATH: (a      " op "  b     ) " op2 " c(.cc) .x))"))
            (print "      ((MATH: a      " op " b(.bb) " op2 " c      .x)")
            (if (less? (cost op) (cost op2))
               (print "         (MATH:  a      " op " (b(.bb)  " op2 " c      .x)))")
               (print "         (MATH: (a      " op "  b(.bb)) " op2 " c      .x))"))
            (print "      ((MATH: a      " op " b(.bb) " op2 " c(.cc) .x)")
            (if (less? (cost op) (cost op2))
               (print "         (MATH:  a      " op " (b(.bb)  " op2 " c(.cc) .x)))")
               (print "         (MATH: (a      " op "  b(.bb)) " op2 " c(.cc) .x))"))
            (print "      ((MATH: a(.aa) " op " b      " op2 " c      .x)")
            (if (less? (cost op) (cost op2))
               (print "         (MATH:  a(.aa) " op " (b       " op2 " c      .x)))")
               (print "         (MATH: (a(.aa) " op "  b     ) " op2 " c      .x))"))
            (print "      ((MATH: a(.aa) " op " b      " op2 " c(.cc) .x)")
            (if (less? (cost op) (cost op2))
               (print "         (MATH:  a(.aa) " op " (b       " op2 " c(.cc) .x)))")
               (print "         (MATH: (a(.aa) " op "  b     ) " op2 " c(.cc) .x))"))
            (print "      ((MATH: a(.aa) " op " b(.bb) " op2 " c      .x)")
            (if (less? (cost op) (cost op2))
               (print "         (MATH:  a(.aa) " op " (b(.bb)  " op2 " c      .x)))")
               (print "         (MATH: (a(.aa) " op "  b(.bb)) " op2 " c      .x))"))
            (print "      ((MATH: a(.aa) " op " b(.bb) " op2 " c(.cc) .x)")
            (if (less? (cost op) (cost op2))
               (print "         (MATH:  a(.aa) " op " (b(.bb)  " op2 " c(.cc) .x)))")
               (print "         (MATH: (a(.aa) " op "  b(.bb)) " op2 " c(.cc) .x))")))
         '(+ - * /)))
   '(+ - * /))


(for-each (lambda (op)
      (print "
      ((MATH: (a " op " b))
         (" op " (MATH: a) (MATH: b)))"))
   '(+ - * /))

(print "
      ;; single value
      ((MATH: x)
         x)))
))")
