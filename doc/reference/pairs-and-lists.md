Pairs and Lists
===============
A *pair* (sometimes called a *dotted pair*) is a record structure with two fields called the car and cdr fields (for historical reasons). Pairs are created by the procedure `cons`. The car and cdr fields are accessed by the procedures `car` and `cdr`.

Pairs are used primarily to represent lists. A *list* can be defined recursively as either the empty list or a pair whose cdr is a list.

# cons
`(cons obj1 obj2)`, *primop*

Returns a newly allocated pair whose car is obj1 and whose cdr is obj2. The pair is guaranteed to be different (in the sense of `eqv?`) from every existing object.

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

Returns *#t* if obj is a pair, otherwise returns *#f*.

```scheme
(pair? 1)                     ==>  #false
(pair? '(a . b))              ==>  #true
(pair? '(a b c))              ==>  #true
(pair? '())                   ==>  #false
(pair? #(a b))                ==>  #false
(pair? '{a b}                 ==>  #false
(pair? #true)                 ==>  #false
(pair? 17/121)                ==>  #false
```

# car
`(car pair)`, *primop*

Returns the contents of the *car* field of pair. Note that it is an error to take the car of the not a pair.

```scheme
(car (cons 1 2))              ==> 1
(car (cons 'a 3))             ==> 'a
(car (cons 1 '()))            ==> 1
(car (cons '() 1))            ==> '()
(car (cons '(a) '(b c d)))    ==> '(a)
(car (cons "a" '(b c)))       ==> "a"
(car (cons '(a b) 'c))        ==> '(a b)
```

# cdr
`(cdr pair)`, *primop*

Returns the contents of the cdr field of pair . Note that it
is an error to take the cdr of the empty list.

```scheme
(cdr (cons 1 2))              ==> 2
(cdr (cons 'a 3))             ==> 3
(cdr (cons 1 '()))            ==> '()
(cdr (cons '() 1))            ==> 1
(cdr (cons '(a) '(b c d)))    ==> '(b c d)
(cdr (cons "a" '(b c)))       ==> '(b c)
(cdr (cons '(a b) 'c))        ==> 'c
```

# list
`(list obj ...)`, *macro*

Returns a newly allocated *list* of its arguments.

```scheme
(list 'a (+ 3 4) 'c)          ==> '(a 7 c)
(list)                        ==> '()
```

# make-list
`(make-list k)`, *procedure*  
`(make-list k fill)`, *procedure*

Returns a newly allocated list of k elements. If a second argument is given, then each element is initialized to fill.
Otherwise the initial contents of each element is #false.

```scheme
(make-list 7)     ==> '(#false #false #false #false #false #false #false)
(make-list 7 3)   ==> '(3 3 3 3 3 3 3)
```

# #null
`#null`, *constant*

The empty list. Also known as *'()*.

```scheme
#null  ==>  '()
null   ==>  '() ; `null` is deprecated, but still used
```

# null?
`(null? obj)`, *procedure*

Returns *#t* if obj is the empty list, otherwise returns *#f*.

```scheme
(null? 1)                     ==>  #false
(null? '(a . b))              ==>  #false
(null? '())                   ==>  #true
(null? #null)                 ==>  #true
(null? null)                  ==>  #true ; `null` is depraceted
(null? (cdr '(1)))            ==>  #true
```

# cons*
`(cons* obj ...)`, *macro*

Returns a newly allocated *improper list* of its arguments.

```scheme
(cons* 'a (+ 3 4) 'c)         ==> '(a 7 . c)
(cons* 1)                     ==> 1
```

# list?
`(list? obj)`, *procedure*

Returns *#t* if obj is a proper list, otherwise returns *#f*.

```scheme
(list? '(1 . 2))              ==>  #false
(list? '(1 2 3))              ==>  #true
(list? '())                   ==>  #true
(list? #false)                ==>  #false
(list? (cons 1 (cons 2 '()))) ==>  #true
(list? (cons 1 2))            ==>  #false
(list? (cons* 1 2 3))         ==>  #false
```

<!-- # set-car!
`(set-car! pair obj)`, *procedure*

Dangerous! Don't use.

# set-cdr!
`(set-cdr! pair obj)`, *procedure*

Dangerous! Don't use. -->

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
caar   ==> (lambda (x) (car (car x)))
cadr   ==> (lambda (x) (car (cdr x)))
cdar   ==> (lambda (x) (cdr (car x)))
cddr   ==> (lambda (x) (cdr (cdr x)))

;(cddadr '(1 (2 3 4 5) 6))       ==> '(4 5)
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


<!-- 

(list-tail list k)
(list-ref list k)
(list-set! list k obj )

(memq obj list) procedure
(memv obj list) procedure
(member obj list) procedure
(member obj list compare)

(assq obj alist) procedure
(assv obj alist) procedure
(assoc obj alist) procedure
(assoc obj alist compare) 

(list-copy obj ) 


; https://www.youtube.com/watch?v=MOqm0qGJhpw -->
