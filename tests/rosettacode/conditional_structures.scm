; http://www.rosettacode.org/wiki/Conditional_structures

; if-then, the simplest conditional primitive.
(if (= (* 2 2) 4) (print "if-then: equal"))
(if (= (* 2 2) 6) (print "if-then: non equal"))

; if-then-else, the full conditional 'if' primitive.
(if (= (* 2 2) 4) (print "if-then-else: equal") (print "if-then-else: non equal"))
(if (= (* 2 2) 6) (print "if-then-else: non equal") (print "if-then-else: i don't know"))

; when and unless, the simplification of if
(when (= (* 2 2) 4) (print "when: equal"))
(unless (= (* 2 2) 6) (print "unless: not equal"))

; case, the sequence of comparing values.
(case (* 2 2)
   (3
      (print "case: 3"))
   (4
      (print "case: 4"))
   ((5 6 7)
      (print "case: 5 or 6 or 7"))
   (else
      (print "case: i don't know")))

; additionally, case can select vectors with variables filling
(case (vector 'selector 1 2 3)
   (['case1 x y]
      (print "case: case1 " x ", " y))
   (['selector x y z]
      (print "case: selector " x ", " y ", " z))
   (else
      (print "case: i don't know")))

; cond, the sequnce of comparators.
(cond
   ((= (* 2 2) 4)
      (print "cond: equal"))
   ((= (* 2 2) 6)
      (print "cond: not equal"))
   (else
      (print "cond: i don't know")))

; case-lambda, selecting the lambda based on arguments count.
(define smart (case-lambda
   ((x)
      (print x ", -, -"))
   ((x y)
      (print x ", " y ", -"))
   ((x y z)
      (print x ", " y ", " z))))
(smart 1)
(smart 1 2)
(smart 1 2 3)
