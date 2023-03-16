Booleans
========

The standard boolean objects for *true* and *false* are written as `#true` and `#false`.
Alternatively, they can be written as `#t` `#T` and `#f` `#F`, respectively.

All the Ol objects are treated as *true* (including empty list !), only `#false` counts as *false* in conditional expressions.

Boolean constants evaluate to themselves, so they don't need to be quoted in programs.
```scheme
#t       ==>  #true
#T       ==>  #true
#f       ==>  #false
#F       ==>  #false
'#true   ==>  #true
```

[not](#not), [boolean?](#boolean), [boolean=?](#boolean-1)

# not
`(not obj)`, *procedure*

The *not* procedure returns #true if *obj* is false, and returns #false otherwise.

```scheme
(not #t)         ==>  #false
(not 3)          ==>  #false
(not (list 3))   ==>  #false
(not #f)         ==>  #true
(not '())        ==>  #false
(not (list))     ==>  #false
(not 'sym)       ==>  #false
```

# boolean?
`(boolean? obj)`, *procedure*

The *boolean?* predicate returns #true if *obj* is either #true or #false and returns #false otherwise.

```scheme
(boolean? #f)    ==>  #true
(boolean? 0)     ==>  #false
(boolean? '())   ==>  #false
(boolean? #T)    ==>  #true
(boolean? #true) ==>  #true
(boolean? #false)==>  #true
(boolean? #null) ==>  #false
```

# boolean=?
`(boolean=? boolean1 boolean2 boolean3 ...)`, *procedure*

Returns #true if all the arguments are booleans and all are #true or all are #false.

```scheme
(boolean=? #f #f #f #f #f 1)    ==>  #false
(boolean=? 1 #t #f #t #f #f)    ==>  #false
(boolean=? #f #t #f #t)         ==>  #false
(boolean=? #t #t #t #t)         ==>  #true
(boolean=? #f #f #f #f)         ==>  #true
```