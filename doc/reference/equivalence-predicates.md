Equivalence predicates
======================

# eqv?
`(eqv? obj1 obj2)`, *procedure*

```scheme
(eqv? 'a 'a)                 ==> #t
(eqv? 'a 'b)                 ==> #f
(eqv? 2 2)                   ==> #t
(eqv? 2 2.0)                 ==> #f
(eqv? '() '())               ==> #t
(eqv? 100000000 100000000)   ==> #t
(eqv? 0.0 +nan.0)            ==> #f
(eqv? (cons 1 2) (cons 1 2)) ==> #f
(eqv? (lambda () 1)
      (lambda () 2))         ==> #f
(let ((p (lambda (x) x)))
    (eqv? p p))              ==> #t
(eqv? #f 'nil)               ==> #f
```

# eq?
`(eq? obj1 obj2)`, *primop*

```scheme
```

# equal?
`(equal? obj1 obj2)`, *procedure*

```scheme
```
