Pairs and Lists
===============
A *pair* (sometimes called a *dotted pair*) is a record structure with two fields called the car and cdr fields (for historical reasons).
Pairs are created by the procedure `cons`. The car and cdr fields are accessed by the procedures `car` and `cdr`.

Pairs are used primarily to represent lists. A *list* can be defined recursively as either the empty list or a pair whose cdr is a list.

## TOC

[cons](#cons), [pair?](#pair), [car](#car), [cdr](#cdr),  
[list](#list), [make-list](#make-list), [list-copy](#list-copy), [#null](#null), [null?](#null-1), [cons*](#cons-1), [list?](#list-1),  
[caar](#caar--cddr), [cadr](#caar--cddr), [cdar](#caar--cddr), [cddr](#caar--cddr),  
[caaar](#caar--cddr), [caadr](#caar--cddr), [cadar](#caar--cddr), [caddr](#caar--cddr), [cdaar](#caar--cddr), [cdadr](#caar--cddr), [cddar](#caar--cddr), [cdddr](#caar--cddr),  
[caaaar](#caar--cddr), [caaadr](#caar--cddr), [caadar](#caar--cddr), [caaddr](#caar--cddr), [cadaar](#caar--cddr), [cadadr](#caar--cddr), [caddar](#caar--cddr), [cadddr](#caar--cddr), [cdaaar](#caar--cddr), [cdaadr](#caar--cddr), [cdadar](#caar--cddr), [cdaddr](#caar--cddr), [cddaar](#caar--cddr), [cddadr](#caar--cddr), [cdddar](#caar--cddr), [cddddr](#caar--cddr),  
[length](#length), [list-ref (lref)](#list-ref-lref), [list-tail](#list-tail),  
[repeat](#repeat), [iota](#iota), [lrange](#lrange), [append](#append), [reverse](#reverse),  
[take](#take), [drop](#drop),  
[memq](#memq), [memv](#memv), [member](#member), [assq](#assq), [assv](#assv), [assoc](#assoc),  
[map](#map), [fold](#fold), [foldr](#foldr)

### Non functional features (very limited, use with care!)
[set-car!](#set-car), [set-cdr!](#set-cdr), [list-set!](#list-set)

# cons
`(cons obj1 obj2)`, *primop*

Returns a newly allocated pair whose car is *obj1* and whose cdr is *obj2*.
The pair is guaranteed to be different (in the sense of `eq?` and `eqv?`) from every existing object.

```scheme
(cons 1 2)                    ==> '(1 . 2)
(cons 'a 3)                   ==> '(a . 3)
(cons 1 '())                  ==> '(1)
(cons '() 1)                  ==> '(() . 1)
(cons '(a) '(b c d))          ==> '((a) b c d)
(cons "a" '(b c))             ==> '("a" b c)
(cons '(a b) 'c)              ==> '((a b) . c)
```

# pair?
`(pair? obj)`, *procedure*

Returns #true if *obj* is a pair, otherwise returns #false.

```scheme
(pair? 1)                     ==>  #false
(pair? '(a . b))              ==>  #true
(pair? '(a b c))              ==>  #true
(pair? '())                   ==>  #false
(pair? #(a b))                ==>  #false
(pair? '{a b})                ==>  #false
(pair? #true)                 ==>  #false
(pair? 17/121)                ==>  #false
```

# car
`(car pair)`, *primop*

Returns the contents of the car field of *pair*.
Note that it is an error to take the car of the not a pair.

```scheme
(car (cons 1 2))              ==>  1
(car (cons 'a 3))             ==> 'a
(car (cons 1 '()))            ==>  1
(car (cons '() 1))            ==> '()
(car (cons '(a) '(b c d)))    ==> '(a)
(car (cons "a" '(b c)))       ==>  "a"
(car (cons '(a b) 'c))        ==> '(a b)
(car '(1 2 3 4 5))            ==>  1
```

# cdr
`(cdr pair)`, *primop*

Returns the contents of the cdr field of *pair*.
Note that it is an error to take the cdr of the empty list.

```scheme
(cdr (cons 1 2))              ==>  2
(cdr (cons 'a 3))             ==>  3
(cdr (cons 1 '()))            ==> '()
(cdr (cons '() 1))            ==>  1
(cdr (cons '(a) '(b c d)))    ==> '(b c d)
(cdr (cons "a" '(b c)))       ==> '(b c)
(cdr (cons '(a b) 'c))        ==> 'c
(cdr '(1 2 3 4 5))            ==> '(2 3 4 5)
```

# list
`(list obj ...)`, *macro*

Returns a newly allocated *list* of its arguments.

```scheme
(list 1 2 3 4 5 6 7)          ==> '(1 2 3 4 5 6 7)
(list 'a (+ 3 4) 'c)          ==> '(a 7 c)
(list)                        ==> '()
(list (list 1 2) (list 3 4))  ==> '((1 2) (3 4))
```

# make-list
`(make-list k)`, *procedure*  
`(make-list k fill)`, *procedure*

Returns a newly allocated list of *k* elements.
If a second argument is given, then each element is initialized to *fill*. Otherwise the initial contents of each element is #false.

```scheme
(make-list 4)     ==> '(#false #false #false #false)
(make-list 7 3)   ==> '(3 3 3 3 3 3 3)
(make-list 0)     ==> '()
```

# list-copy
`(list-copy obj)`, *procedure*

Returns a newly allocated shallow copy of the given *obj* if it is a list.
Only the pairs themselves are copied; the cars of the result are the same (in the sense of `eqv?`) as the cars of *list*.

```scheme
(list-copy '())               ==> '()
(list-copy '(1 2 (3) 4))      ==> '(1 2 (3) 4)
(let ((l '(1 2)))
   (eq? (list-copy l) l))     ==>  #false

(let*((x '("a" ("b" "c") "d"))
      (y (list-copy x)))
   (eq? (lref x 1) (lref y 1))) ==>  #true  ; ("b" "c")
```

# #null
`#null`, *constant*

The empty list. Mainly known as *'()*.

`null` (deprecated, but still widely used) is the same as `#null`.
`#n` is a one more synonym for `#null`.

```scheme
#null  ==>  '()
null   ==>  '() ; `null` is depraceted
#n     ==>  #null
```

# null?
`(null? obj)`, *procedure*

Returns #true if *obj* is the empty list, otherwise returns #false.

```scheme
(null? 1)                     ==>  #false
(null? '(a . b))              ==>  #false
(null? '())                   ==>  #true
(null? #null)                 ==>  #true
(null? null)                  ==>  #true  ; `null` is depraceted
(null? (cdr '(1)))            ==>  #true
(null? #false)                ==>  #false
```

# cons*
`(cons* obj ...)`, *macro*

Returns a newly allocated improper(!) list of its arguments.

```scheme
(cons* 1 2)                   ==> '(1 . 2)
(cons* 'a (+ 3 4) 'c)         ==> '(a 7 . c)
(cons* 1)                     ==>  1
```

# list?
`(list? obj)`, *procedure*

Returns #true if *obj* is a proper list, otherwise returns #false.

```scheme
(list? '(1 . 2))              ==>  #false
(list? '(1 2 3))              ==>  #true
(list? '())                   ==>  #true
(list? #false)                ==>  #false
(list? (cons 1 (cons 2 '()))) ==>  #true
(list? (cons 1 2))            ==>  #false
(list? (cons 1 #null))        ==>  #true
(list? (cons* 1 2 3))         ==>  #false
```

# ca..ar ... cd..dr
`(caar list)`, *procedure*  
`(cadr list)`, *procedure*  
`(cdar list)`, *procedure*  
`(cddr list)`, *procedure*  

`(caaar list)`, *procedure*  
`(caadr list)`, *procedure*  
...  
`(cddar list)`, *procedure*  
`(cdddr list)`, *procedure*  

`(caaaar list)`, *procedure*, `(scheme cxr)`  
`(caaadr list)`, *procedure*, `(scheme cxr)`  
...  
`(cdddar list)`, *procedure*, `(scheme cxr)`  
`(cddddr list)`, *procedure*, `(scheme cxr)`

```scheme
caar   ==>  (lambda (x) (car (car x)))
cadr   ==>  (lambda (x) (car (cdr x)))
cdar   ==>  (lambda (x) (cdr (car x)))
cddr   ==>  (lambda (x) (cdr (cdr x)))

(cddadr '(1 (2 3 4 5) 6))     ==> '(4 5)
(cadadr '(1 (2 3 4 5) 6))     ==>  3
```

# length
`(length list)`, *procedure*

Returns the length of *list*.

```scheme
(length '(a b c))            ==>  3
(length '(a (b) (c d e)))    ==>  3
(length '())                 ==>  0
```

# list-ref, lref
`(list-ref list k)`, *procedure*  
`lref` is the same as `list-ref`, widely used

Returns the *k*th element of *list*.

```scheme
(list-ref '(a b c) 0)      ==> 'a
(list-ref '(a b c) 8)      ==>  #false
(list-ref '(a b c d) 2)    ==> 'c
```

# list-tail
`(list-tail list k)`, *procedure*

Returns the sublist of *list* obtained by omitting the first *k* elements.

```scheme
(list-tail '(1 2 3 4) 2)   ==> '(3 4)
(list-tail '(1 2 3) 0)     ==> '(1 2 3)
```

# repeat
`(repeat fill k)`, *procedure*

Returns a newly allocated list of *k* elements, each is initialized to *fill*.

```scheme
(repeat 7 3)               ==> '(7 7 7)
(repeat "me" 6)            ==> '("me" "me" "me" "me" "me" "me")
```

# iota
`(iota count)`, *procedure*
`(iota count start)`, *procedure*
`(iota count start step)`, *procedure*

Returns a newly allocated list with sequence of count numbers from *start* with step *step*.
The default value of *start* is a 0. The default values of *step* is a 1.

```scheme
(iota 20)                ==> '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)
(iota 5 -3)              ==> '(-3 -2 -1 0 1)
(iota 3 10 1000)         ==> '(10 1010 2010)
```

# lrange
`(lrange from step to)`, *procedure*

Returns a newly allocated list with sequence of numbers from *from* to *to* with step *step*.

```scheme
(lrange 0 1 10)          ==> '(0 1 2 3 4 5 6 7 8 9)
(lrange 1 3 10)          ==> '(1 4 7)
(lrange 5 -1 0)          ==> '(5 4 3 2 1)
```

# append
`(append list ...)`, *procedure*

Returns a list consisting of the elements of the first *list* followed by the elements of the other *lists*.

```scheme
(append '(x) '(y))                ==> '(x y)
(append '(a) '(b c d))            ==> '(a b c d)
(append '(a (b)) '((c)))          ==> '(a (b) (c))
(append '(a b) '(c . d))          ==> '(a b c . d)
(append '() 'a)                   ==> 'a
(append '(1 2 3))                 ==> '(1 2 3)
(append '(1) '(2) '(3 (4)) '(5))  ==> '(1 2 3 (4) 5)
(append '(1 2 3) 4)               ==> '(1 2 3 . 4)
(append)                          ==> '()
```

# reverse
`(reverse list)`, *procedure*

Returns a newly allocated list consisting of the elements of *list* in reverse order.

```scheme
(reverse '(a b c))                ==> '(c b a)
(reverse '(a (b c) d (e (f))))    ==> '((e (f)) d (b c) a)
```

# take
`(take list count)`, *procedure*

Returns list with first *count* elements of *list*, if *count* less or equal to list length. Otherwise returns just a copy of a list.

```scheme
(take '(1 2 3 4 5) 3)             ==> '(1 2 3)
(take '(1 2 3) 0)                 ==> '()
(take '(1 2) 777)                 ==> '(1 2)
```

# drop
`(drop list count)`, *procedure*

Returns list with last *count* elements of *list*, if *count* less or equal to list length. Otherwise returns empty list.

```scheme
(drop '(1 2 3 4 5) 3)             ==> '(4 5)
(drop '(1 2 3) 0)                 ==> '(1 2 3)
(drop '(1 2) 777)                 ==> '()
```

# memq
`(memq obj list)`, *procedure*

Returns the first sublist of *list* whose car is *obj*, where the sublists of *list* are the non-empty lists returned by (list-tail list k) for k less than the length of *list*, otherwise returns *#f*.  
The `memq` procedure uses `eq?` to compare obj with the elements of *list*.

```scheme
(memq 'a '(a b c))         ==>  '(a b c)
(memq 'b '(a b c))         ==>  '(b c)
(memq 'a '(b c d))         ==>  #false
(memq "b" '("a" "b" "c"))  ==>  #false
(memq '(a) '(b (a) c))     ==>  #false
; memq is not working for long numbers:
(memq 1000000000000000000000
   '(100 1000000000000000000000 102))  ==>  #false
; but working for short:
(memq 101 '(100 101 102))  ==>  '(101 102)
```

# memv
`(memv obj list)`, *procedure*

Returns the first sublist of *list* whose car is *obj*, where the sublists of *list* are the non-empty lists returned by (list-tail list k) for k less than the length of *list*, otherwise returns *#f*.  
The `memv` procedure uses `eqv?` to compare obj with the elements of *list*.

```scheme
(memv 'a '(a b c))         ==>  '(a b c)
(memv 'b '(a b c))         ==>  '(b c)
(memv 'a '(b c d))         ==>  #false
(memv "b" '("a" "b" "c"))  ==>  #false
(memv '(a) '(b (a) c))     ==>  #false
; memv is working for any numbers:
(memv 101 '(100 101 102))  ==>  '(101 102)
(memv 1000000000000000000000
   '(100 1000000000000000000000 102))  ==>  '(1000000000000000000000 102)
```

# member
`(member obj list)`, *procedure*  
`(member obj list compare)`, *procedure*

Returns the first sublist of *list* whose car is *obj*, where the sublists of *list* are the non-empty lists returned by (list-tail list k) for k less than the length of *list*, otherwise returns *#f*.  
The `member` procedure uses *compare* if given, and `equal?` otherwise, to compare obj with the elements of *list*.

```scheme
(member 'a '(a b c))         ==>  '(a b c)
(member 'b '(a b c))         ==>  '(b c)
(member 'a '(b c d))         ==>  #false
(member "b" '("a" "b" "c"))  ==>  '("b" "c")
(member '(a) '(b (a) c))     ==>  '((a) c)
(member 101 '(100 101 102))  ==>  '(101 102)
(member 1000000000000000000000
   '(100 1000000000000000000000 102))    ==>  '(1000000000000000000000 102)
(member "B" '("a" "b" "c"))              ==>  #false
(member "B" '("a" "b" "c") string-ci=?)  ==>  '("b" "c")
```

# assq
`(assq obj alist)`, *procedure*

Returns the first pair in *alist* whose car field is *obj*, otherwise returns *#f*.  
The `assq` procedure uses `eq?` to compare obj with the car fields of the pairs in alist.

```scheme
(assq 'a '((a 1) (b 2) (c 3)))          ==>  '(a 1)
(assq 'b '((a 1) (b 2) (c 3)))          ==>  '(b 2)
(assq 'd '((a 1) (b 2) (c 3)))          ==>  #false
(assq "b" '(("a" 1) ("b" 2) ("c" 3)))   ==>  #false
(assq '(b) '(((a) 1) ((b) 2) ((c) 3)))  ==>  #false
; assq is not working for long numbers:
(assq 1000000000000000000000
   '((2 3) (1000000000000000000000 101) (102 103)))  ==>  #false
; but working for short:
(assq 5 '((2 3) (5 7) (11 13)))         ==>  '(5 7)
```

# assv
`(assv obj alist)`, *procedure*

Returns the first pair in *alist* whose car field is *obj*, otherwise returns *#f*.  
The `assv` procedure uses `eqv?` to compare obj with the car fields of the pairs in alist.

```scheme
(assv 'a '((a 1) (b 2) (c 3)))          ==>  '(a 1)
(assv 'b '((a 1) (b 2) (c 3)))          ==>  '(b 2)
(assv 'd '((a 1) (b 2) (c 3)))          ==>  #false
(assv "b" '(("a" 1) ("b" 2) ("c" 3)))   ==>  #false
(assv '(b) '(((a) 1) ((b) 2) ((c) 3)))  ==>  #false
; assv is working for any numbers:
(assv 5 '((2 3) (5 7) (11 13)))         ==>  '(5 7)
(assv 1000000000000000000000
   '((2 3) (1000000000000000000000 101) (102 103)))  ==>  '(1000000000000000000000 101)
```

# assoc
`(assoc obj alist)`, *procedure*
`(assoc obj alist compare)`, *procedure*

Returns the first pair in *alist* whose car field is *obj*, otherwise returns *#f*.  
The `assoc` procedure uses *compare* if given, and `equal?` otherwise, to compare obj with the elements of *list*.

```scheme
(assoc 'a '((a 1) (b 2) (c 3)))          ==>  '(a 1)
(assoc 'b '((a 1) (b 2) (c 3)))          ==>  '(b 2)
(assoc 'd '((a 1) (b 2) (c 3)))          ==>  #false
(assoc "b" '(("a" 1) ("b" 2) ("c" 3)))   ==>  '("b" 2)
(assoc '(b) '(((a) 1) ((b) 2) ((c) 3)))  ==>  '((b) 2)
(assoc 5 '((2 3) (5 7) (11 13)))         ==>  '(5 7)
(assoc 1000000000000000000000 '((2 3) (1000000000000000000000 101) (102 103)))  ==>  '(1000000000000000000000 101)
(assoc "B" '(("a" 1) ("b" 2) ("c" 3)))               ==>  #false
(assoc "B" '(("a" 1) ("b" 2) ("c" 3)) string-ci=?)   ==>  '("b" 2)
```

# map
`(map handler a ...)`, *procedure*

The `map` function uses the per-element results to create a new list.

```scheme
(map - '(1 2 3))                 ==> '(-1 -2 -3)
(map sqrt '(1 4 9 16))           ==> '(1 2 3 4)
(map (lambda (n i)
        ((if (zero? (mod i 2)) + -) n))
   '(1 3 5 7 9)
   '(1 2 3 4 5))                 ==> '(-1 3 -5 7 -9)
(map (lambda (n) (/ 1 n))
   (iota 5 1 2))                 ==> '(1 1/3 1/5 1/7 1/9)
(map (lambda (i p)
        (string-append i p))
   '("peanuts" "popcorn" "crackerjack")
   '("!"       "?"       "^"))   ==> '("peanuts!" "popcorn?" "crackerjack^")
(map car '(
   (1 . 2) (3 . 4) (5 . 6)))     ==> '(1 3 5)
```

# fold
`(fold handler state a ...)`, *procedure*

Combine the elements of list(s) from left to right.

```scheme
(fold + 0 '(1 2 3))           ==>  6
(fold + 7 '(1 2 3))           ==>  13
(fold - 7 '(1 2 3))           ==>  1
(fold cons 3 '(5 6 7))        ==> '(((3 . 5) . 6) . 7)

(fold put {} '(x y z)
             '(4 5 6))        ==>  { 'x 4  'y 5  'z 6 }
(fold (lambda (f x)
         (min f x))
   100
   '(6 7 8 4 7))              ==>  4
(fold (lambda (f x y)
         (string-append f "/" x y))
   "X"
   '("a" "b" "c" "d")
   '("1" "2" "3" "4"))        ==>  "X/a1/b2/c3/d4"

; Leibniz formula for Pi,
;  1 - 1/3 + 1/5 - 1/7 + 1/9 - 1/11 + ...
> (let ((N 10000))
     (define (sign n)
        (if (zero? (mod n 2)) + -))
     (fold (lambda (f x i)
              ((sign i) f (/ #i4 x)))
        #i4
        (iota N 3 2)
        (iota N 1)))
3.14169264
```

# foldr
`(foldr handler state a ...)`, *procedure*

Combine the elements of list(s) from right to left.

```scheme
(foldr + 0 '(1 2 3))           ==>  6
(foldr + 7 '(1 2 3))           ==>  13
(foldr - 7 '(1 2 3))           ==>  -5
(foldr cons 3 '(5 6 7))        ==>  '(5 6 7 . 3)

(foldr - 9)                               ==>  9
(foldr - 9 '(1 2))                      ; === (- 1 (- 2 9))
                                          ==>  8
(foldr - 9 '(1 2) '(3 4))               ; === (- 1 3 (- 2 4 9))
                                          ==>  9
(foldr - 9 '(1 2) '(3 4) '(5 6))        ; === (- 1 3 5 (- 2 4 6 9))
                                          ==> 10
(foldr - 9 '(1 2) '(3 4) '(5 6) '(7 8)) ; === (- 1 3 5 7 (- 2 4 6 8 9))
                                          ==> 11

; Please note that the order of variables in the lambda differs than in fold!
(foldr (lambda (x f)
         (min f x))
   100
   '(6 7 8 4 7))               ==>  4
(foldr (lambda (x y f)
         (string-append f "/" x y))
   "X"
   '("a" "b" "c" "d")
   '("1" "2" "3" "4"))         ==>  "X/d4/c3/b2/a1"
   ```

Non functional features
=======================

Ol provides limited support for non-functional (in sense of [paradigm](https://en.wikipedia.org/wiki/Functional_programming)) features.
Please only use them if you know what you are doing! This can lead to unexpected behavior of your program.

# set-car!
`(set-car! pair obj)`, *procedure*

Stores obj in the car field of pair. *Obj* must be enum, symbol, or constant.

```scheme
(set-car! '(1 . 2) 8)         ==>  '(8 . 2)
(set-car! (cons 'a 'b) 'xxx)  ==>  '(xxx . b)
(set-car! '(a . #f) #t)       ==>  '(#true . #false)
```

# set-cdr!
`(set-cdr! pair obj)`, *procedure*

Stores obj in the cdr field of pair. *Obj* must be enum, symbol, or constant.

```scheme
(set-cdr! '(1 . 2) 8)         ==>  '(1 . 8)
(set-cdr! (cons 'a 'b) 'xxx)  ==>  '(a . xxx)
(set-cdr! '(a . #f) #t)       ==>  '(a . #true)
```

# list-set!
`(list-set! list k obj)`, *procedure*

Stores *obj* in element *k* of *list*. *Obj* must be enum, symbol, or constant.

```scheme
(let ((me '(1 2 3 4)))
   (list-set! me 2 77)
   me)                       ==>  '(1 2 77 4)
```

