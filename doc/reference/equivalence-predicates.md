Equivalence predicates
======================

A predicate is a procedure that always returns a boolean value (#true or #false).
An equivalence predicate is the computational analogue of a mathematical equivalence relation; it is symmetric, reflexive, and transitive.

[eq?](#eq), [eqv?](#eqv), [equal?](#equal)

# eq?
`(eq? obj1 obj2)`, *primop*

Returns #true if *obj1* and *obj2* are definitely the same object or same values.

```scheme
(eq? 'a 'a)                  ==>  #true  ; you can eq? symbols,
(eq? "a" "a")                ==>  #false ; but not strings
(eq? 'a 'b)                  ==>  #false
(eq? "" "")                  ==>  #false

(eq? #false #false)          ==>  #true  ; same values
(eq? '() '())                ==>  #true  ; same values: '() is #null, #null eq #null
(eq? '(a) '(a))              ==>  #false
(let ((q '(a)))
   (eq? q q))                ==>  #true
(eq? 2 2)                    ==>  #true  ; same values
(eq? 222222222222222222222
     222222222222222222222   ==>  #false ; long integers are objects, not values
(eq? #\A #\A)                ==>  #true
(eq? car car)                ==>  #true

(eq? (lambda (x) x)
     (lambda (x) x))         ==>  #true
(eq? (lambda (x) x)
     (lambda (y) y))         ==>  #true  ; code optimizer reuses the same existing functions

```

# eqv?
`(eqv? obj1 obj2)`, *procedure*

The eqv? procedure defines a useful equivalence relation on objects.
Briefly, it returns #true if *obj1* and *obj2* are normally regarded as the same object.


```scheme
(eqv? 'a 'a)                 ==>  #true
(eqv? 'a 'b)                 ==>  #false

(eqv? 2 2)                   ==>  #true
(eqv? 2 2.0)                 ==>  #true  ; 2.0 is exact in Ol
(eqv? 2 #i2.0)               ==>  #false
(eqv? 1 #i1)                 ==>  #false
(eqv? 100000 100000)         ==>  #true
(eq? 222222222222222222222
     222222222222222222222   ==>  #true
(eqv? 0.33 0.33)             ==>  #true
(eqv? 7/3 14/6)              ==>  #true
(eqv? 2+3i 2+3i)             ==>  #true

(eqv? 0.0 +nan.0)            ==>  #false
(eqv? +nan.0 +nan.0)         ==>  #true

(eqv? '() '())               ==>  #true
(eqv? #false '())            ==>  #false
(eqv? "" "")                 ==>  #false
(eqv? '#() '#())             ==>  #false
(eqv? '(a) '(a))             ==>  #false
(let ((q '(a)))
   (eqv? q q))               ==>  #true
(eqv? "a" "a")               ==>  #false
(eqv? #\a #\A)               ==>  #false
(eqv? '(b) (cdr '(a b)))     ==>  #false

(eqv? (cons 1 2) (cons 1 2)) ==>  #false

(eqv? (lambda () 1)
      (lambda () 2))         ==>  #false
(eqv? (lambda (x) x)
      (lambda (x) x))        ==>  #true
(eqv? (lambda (x) x)
      (lambda (y) y))        ==>  #true
(letrec (
      (f (lambda () (if (eqv? f g) 'both 'f)))
      (g (lambda () (if (eqv? f g) 'both 'g))))
   (eqv? f g))               ==>  #false
(let ((p (lambda (x) x)))
    (eqv? p p))              ==>  #true
```

# equal?
`(equal? obj1 obj2)`, *procedure*

The equal? procedure, when applied to pairs, vectors, strings and bytevectors, recursively compares them, returning #true when the unfoldings of its arguments into (possibly
infinite) trees are equal (in the sense of equal?) as ordered trees, and #false otherwise.

```scheme
(equal? "" "")                 ==>  #true
(equal? '#() '#())             ==>  #true
(equal? '(a) '(a))             ==>  #true
(equal? "a" "a")               ==>  #true
(equal? #\a #\A)               ==>  #false
(equal? #\A #\A)               ==>  #true
(equal? (cons 1 2) (cons 1 2)) ==>  #true
(equal? '(1 (2 (3 4)) 5)
        '(1 (2 (3 4)) 5))      ==>  #true
(equal? { 'name "Yuriy"
          'phone 33172299 }
        { 'name "Yuriy"
          'phone 33172299 })   ==>  #true
(equal? [1 2 3] [1 2 3])       ==>  #true
(equal? (bytevector "123")
        (string "123))         ==>  #false

(equal? (append '(1 2) '((7)))
       '(1 2 (7)))             ==>  #true
```
