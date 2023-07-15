; http://www.rosettacode.org/wiki/Conditional_structures

; if-then, the simplest conditional `if` primitive.
(if (= (* 2 2) 4)
   (print "if-then: equal"))
(if (= (* 2 2) 6)
   (print "if-then: should not be printed"))

; if-then-else, the full conditional `if` primitive.
(if (= (* 2 2) 4)
   (print "if-then-else: equal")
   (print "if-then-else: non equal"))
(if (= (* 2 2) 6)
   (print "if-then-else: equal")
   (print "if-then-else: non equal"))

; when and unless, the simplification of `if` without `begin`
(when (= (* 2 2) 4)
   (print "when: ..just do something..")
   (print "when: equal"))

(unless (= (* 2 2) 6)
   (print "unless: ..just do something..")
   (print "unless: not equal"))

; if-then-else, extended conditional `if` primitive.
(if (= (* 2 2) 4)
   (print "if-then-else*: equal")
else
   (print "if-then-else*: ..just do something..")
   (print "if-then-else*: non equal"))

(if (= (* 2 2) 4)
then
   (print "if-then-else*: ..just do something..")
   (print "if-then-else*: equal")
else
   (print "if-then-else*: ..just do something..")
   (print "if-then-else*: non equal"))

(if (= (* 2 2) 4) ; same as `when`
then
   (print "if-then-else*: ..just do something..")
   (print "if-then-else*: equal"))


; case, the sequence of comparing values.
(case (* 2 2)
   (3 ; exact number
      (print "case: 3"))
   (4 ; exact number
      (print "case: 4"))
   ((5 6 7) ; list of numbers
      (print "case: 5 or 6 or 7"))
   (else
      (print "case: i don't know")))

; extended case with usable else
(case (* 2 2)
   (3 ; exact number
      (print "case: 3"))
   (else => (lambda (num)
      (print "case: real value is " num))))

(case (* 2 2)
   (3 ; exact number
      (print "case: 3"))
   (else is num
      (print "case: real value is " num)))

; extended case with vectors
(case ['selector 1 2 3]
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
