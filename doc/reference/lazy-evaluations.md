Lazy Evaluations
================

Lazy evaluation (or call-by-need) is an evaluation strategy which delays the evaluation of an expression until its value is needed (non-strict evaluation) and which also avoids repeated evaluations (sharing).

Lazy lists (aka streams) are like lists, but they are computed only as far as needed. You can for example define a lazy list of all integers below a million, and then proceed to run computations on them, without worrying whether you have enough memory to hold a million numbers. Lazy lists are useful in computations, where you know how something is constructed, but don't yet know how many of them will be needed, or know that you only need them one at a time and don't want to waste memory. 

A lazy list is either *#null*, a pair of a value and rest of the lazy list, or a function of zero arguments (a *thunk*) which when called will return the rest of the lazy list. Therefore, since normal lists are a subset of lazy lists, all lazy list functions can also take normal lists as arguments.

Scheme warning: recall that Ol does not have mutable data structures, so lazy lists do not cache their results!

[lcons](#lcons), [lpair?](#lpair), [lcar](#lcar), [lcdr](#lcdr),  
[liota](#liota),  
[ltake](#ltake), [lfind](#lfind),  
[lmap](#lmap),  [lfold](#lfold), [lfoldr](#lfoldr)

# lcons
`(lcons obj1 obj2)`, *procedure*

```scheme
(lcons 1 2)                     ==>  (cons 1 (lambda () 2))
(lcons 'a 3)                    ==>  (cons 'a (lambda () 3))
(lcons 1 '())                   ==>  (cons 1 (lambda () '()))
(lcons '() 1)                   ==>  (cons '() (lambda () 1))
(lcons '(a) '(b c d))           ==>  (cons '(a) (lambda () '(b c d)))
(lcons "a" '(b c))              ==>  (cons "a" (lambda () '(b c)))
(lcons '(a b) 'c)               ==>  (cons '(a b) (lambda () 'c))
```

# lpair?
`(lpair? obj)`, *procedure*

```scheme
```

# lmap
`(lmap handler )

