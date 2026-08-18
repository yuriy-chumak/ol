Booleans
========

The standard boolean objects for *True* and *False* are written as `#true` and `#false`.
Alternatively, they can be written as `#t` `#T` and `#f` `#F`, respectively.

```scheme
#t       ===  #true
#T       ===  #true
#f       ===  #false
#F       ===  #false
```

In Ol, all objects are treated as *true* (❗including the empty list, unlike some other Lisp dialects❗); only `#false` counts as *false* in conditional expressions.
```scheme
(if "hello" 2)    ==>  2
(if 123     3)    ==>  3
(if '()    17)    ==> 17
(if {}      1)    ==>  1
(if +inf.0  0)    ==>  0
(if #false 22)    ==>  #false
```

*False* is returned by default for functions with no explicit return result.
```scheme
> (define (function x) (if (odd? x) "odd"))
> (function 100)
#false
```

Boolean constants evaluate to themselves, so they don't need to be quoted in programs.
```scheme
'#true   ==>  #true
'#false  ==>  #false
```

## TOC

[not](#not), [boolean?](#boolean), [boolean=?](#boolean-1)

# not
`(not obj)`, *procedure*

The *not* procedure returns `#true` if *obj* is `#false`, and returns `#false` otherwise.

```scheme
(not #t)         ==>  #false
(not 3)          ==>  #false
(not (list 3))   ==>  #false
(not '())        ==>  #false
(not (list))     ==>  #false
(not "")         ==>  #false
(not "#false")   ==>  #false
(not 12345)      ==>  #false
(not not)        ==>  #false
(not 'sym)       ==>  #false
(not #f)         ==>  #true
(not (not #f))   ==>  #false
(not (not 42))   ==>  #true
```

# boolean?
`(boolean? obj)`, *procedure*

The *boolean?* predicate returns `#true` if *obj* is either `#true` or `#false`, and returns `#false` otherwise.

```scheme
(boolean? #f)        ==>  #true
(boolean? #false)    ==>  #true
(boolean? #T)        ==>  #true
(boolean? #true)     ==>  #true
(boolean? 'true)     ==>  #false
(boolean? '#true)    ==>  #true
(boolean? "#true")   ==>  #false
(boolean? 0)         ==>  #false
(boolean? '())       ==>  #false
(boolean? #null)     ==>  #false
```

# boolean=?
`(boolean=? boolean1 boolean2 boolean3 ...)`, *procedure*

Returns `#true` if all the arguments are booleans and either all are `#true` or all are `#false`.

```scheme
(boolean=? #f #f #f #f #f 1)   ==>  #false
(boolean=? 1 #t #t #t #t #t)   ==>  #false
(boolean=? #t #t #t #t)        ==>  #true
(boolean=? #f #f #f #f)        ==>  #true
(boolean=? #f #t #f #t)        ==>  #false
(boolean=? #t #T #true)        ==>  #true
(boolean=? #t "#T" #true)      ==>  #false
(boolean=? #f #F #false)       ==>  #true
```