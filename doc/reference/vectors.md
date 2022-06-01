Vectors
=======

Vectors are heterogeneous structures whose elements are indexed by integers.
A vector typically occupies less space than a list of the same length, and the average time needed to access a randomly chosen element is typically less for the vector than for the list.

The first element in a vector is indexed by **zero**, and the last element is indexed by the length of the vector minus one.

Vectors are written using the `#()`, `[]`, and `vector` notations:

- `` #(0 (1 2 3) xxx "Anna")``, (r7rs-compliant style)
- `` [0 '(1 2 3) 'xxx (string-append "An" "na") [1 2]]``
- `` '[0 (1 2 3) xxx "Anna"]``
- `` `[0 (1 2 3) xxx ,(string-append "An" "na") ,[1 2]]``
- `` (vector 0 '(1 2 3) 'xxx (string-append "An" "na") [1 2])``

[vector](#vector), [make-vector](#make-vector), [vector?](#vector-1),  
[vector-length](#vector-length), [vector-ref](#vector-ref),  
[vector->list](#vector-list), [list->vector](#list-vector), [vector->string](#vector-string), [string->vector](#string-vector), [vector-copy](#vector-copy), [vector-append](#vector-append)

Non functional features: [vector-set!](#vector-set), [vector-copy!](#vector-copy-1), [vector-fill!](#vector-fill)

# vector
`(vector obj ...)`, *procedure*

Returns a newly allocated vector whose elements contain the given arguments. It is analogous to list.

```scheme
(vector 'a 'b 'c)          ==>  ['a 'b 'c]
(vector 1)                 ==>  [1]
(vector 1 '(2 3) 4)        ==>  [1 '(2 3) 4]
(vector 1 #(2 3) 4)        ==>  [1 [2 3] 4]
```

# make-vector
`(make-vector k)`, *procedure*
`(make-vector k fill)`, *procedure*

Returns a newly allocated vector of k elements. If a second argument is given, then each element is initialized to fill.
Otherwise the initial contents of each element is unspecified.

```scheme
(make-vector 7)      ==>  [#false #false #false #false #false #false #false]
(make-vector 7 3)    ==>  [3 3 3 3 3 3 3]
(make-vector 0)      ==>  []
```

# vector?
`(vector? obj)`, *procedure*

Returns #true if obj is a vector; otherwise returns #false.

```scheme
(vector? [1 2 3])       ==>  #true
(vector? [])            ==>  #true
(vector? #())           ==>  #true
(vector? '(1 2 3))      ==>  #false
(vector? 0)             ==>  #false
(vector? ['(1 2) '(3)]) ==>  #true
```

# vector-length
`(vector-length vector)`, *procedure*

Returns the number of elements in vector as an exact integer.

```scheme
(vector-length [1 2 3])          ==>  3
(vector-length ['(1 2) 3 '(4)])  ==>  3
(vector-length [])               ==>  0
(vector-length '())              ==>  #false
(vector-length 7)                ==>  #false
```

# vector-ref
`(vector-ref vector k)`, *procedure*

The vector-ref procedure returns the content of element *k* of vector *vector*.
- `1` is for first element (not a `0`!!).
- `vector-length - 1` is for last element.

> Note: The *primop* `ref` is another way to access vector elements indexed by **one**.  
> Negative indices are for counting from the last element to the first.

```scheme
(vector-ref [1 2 3 4 5] 0)       ==>  1
(vector-ref [1 2 3 4 5] 3)       ==>  4
(vector-ref [1 2 3 4 5] 4)       ==>  5
(vector-ref [1 2 3 4 5] 5)       ==>  #false
(vector-ref [1 2 3 4 5] 9)       ==>  #false

(ref [1 2 3 4 5] 0)          ==>  #false
(ref [1 2 3 4 5] 1)          ==>  1
(ref [1 2 3 4 5] 5)          ==>  5
(ref [1 2 3 4 5] -1)         ==>  5
(ref [1 2 3 4 5] -5)         ==>  1
(ref [1 2 3 4 5] -9)         ==>  #false
```

# vector->list
`(vector->list vector)`, *procedure*  
`(vector->list vector start)`, *procedure*  
`(vector->list vector start end)`, *procedure*

Returns a newly allocated list of the objects contained in the elements [*start* .. *end*) of vector *vector*.

```scheme
(vector->list '[doh dah didah])       ==>  '(doh dah didah)
(vector->list '[doh dah didah] 1)     ==>  '(dah didah)
(vector->list '[doh dah didah] 1 2)   ==>  '(dah)
(vector->list '[doh dah didah] 1 9)   ==>  '(dah didah #false #false #false #false #false #false)
```

# list->vector
`(list->vector list)`, *procedure*

Returns a newly created vector initialized to the elements of the list *list*.

```scheme
(list->vector '(1 2 3 4 5))          ==>  [1 2 3 4 5]
(list->vector '())                   ==>  []
```

# vector->string
`(vector->string vector)`, *procedure*
`(vector->string vector start)`, *procedure*
`(vector->string vector start end)`, *procedure*

Returns a newly allocated string of the objects contained in the elements [*start* .. *end*) of vector *vector*. Vector elements are treated as runes, not as utf-8 encoded characters.

```scheme
(vector->string [#\ä #\я #\7])      ==>  "äя7"
(vector->string [#\ä #\я #\7] 1)    ==>  "я7"
(vector->string [#\ä #\я #\7] 1 2)  ==>  "я"
(vector->string [#\ä #\я #\7] 1 5)  ==>  #false
```

# string->vector
`(string->vector string)`, *procedure*  
`(string->vector string start)`, *procedure*  
`(string->vector string start end)`, *procedure*

Returns a newly created vector initialized to the elements [*start* .. *end*) of the string *string*. String elements are treated as runes, not as utf-8 encoded characters.

```scheme
(string->vector "äя7")      ==>  [228 1103 55]
(string->vector "äя7" 1)    ==>  [1103 55]
(string->vector "äя7" 1 2)  ==>  [1103]
(string->vector "äя7" 1 5)  ==>  [1103 55]
```

# vector-copy
`(vector-copy vector)`, *procedure*  
`(vector-copy vector start)`, *procedure*  
`(vector-copy vector start end)`, *procedure*

Returns a newly allocated copy of the elements [*start* .. *end*) of the given vector *vector*.

```scheme
(vector-copy [1 2 3 4 5])              ==>  [1 2 3 4 5]
(vector-copy [1 2 3 4 5] 1)            ==>  [2 3 4 5]
(vector-copy [1 2 3 4 5] 1 3)          ==>  [2 3]
(vector-copy [1 2 3 4 5] 1 9)          ==>  [2 3 4 5 #false #false #false #false]

(eq? (vector-copy [1 2 3]) [1 2 3])    ==>  #false
(eqv? (vector-copy [1 2 3]) [1 2 3])   ==>  #false
(equal? (vector-copy [1 2 3]) [1 2 3]) ==>  #true

(let ((v [1 2 3]))
   (eq? (vector-copy v) v))            ==>  #false
(let ((v [1 2 3]))
   (eqv? (vector-copy v) v))           ==>  #false
(let ((v [1 2 3]))
   (equal? (vector-copy v) v))         ==>  #true
```

# vector-append
`(vector-append vector ...)`, *procedure*

Returns a newly allocated vector whose elements are the concatenation of the elements of the given *vectors*.

```scheme
(vector-append [1 2] [3] [4 5])       ==>  [1 2 3 4 5]
(vector-append [])                    ==>  []
(vector-append [7])                   ==>  [7]
```

Non functional features
=======================

Ol provides limited support for non-functional (in sense of [paradigm](https://en.wikipedia.org/wiki/Functional_programming)) features.
Please only use them if you know what you are doing! This can lead to unexpected behavior in your program.

# vector-set!
`(vector-set! vector k obj)`, *procedure*

Stores *obj* in element *k* of vector *vector*. *Obj* must be enum, symbol, or constant.
> Note: The *promop* `set-ref!` is another way to set vector element indexed by **one**.  
> Negative indices are for counting from the last element to the first..

```scheme
(let ((v [1 2 3 4 5]))
   (vector-set! v 3 #true)
   v)                              ==>  [1 2 3 #true 5]
(let ((v [1 2 3 4 5]))
   (vector-set! v 3 #true)
   v)                              ==>  [1 2 3 #true 5]

(let ((v [1 2 3 4 5]))
   (set-ref! v 3 'x)
   v)                              ==>  [1 2 'x 4 5]
(let ((v [1 2 3 4 5]))
   (set-ref! v -2 'y)
   v)                              ==>  [1 2 3 'y 5]
```

# vector-copy!
`(vector-copy! to at from)`, *procedure*  
`(vector-copy! to at from start)`, *procedure*  
`(vector-copy! to at from start end)`, *procedure*

Copies the elements [*start* .. *end*) of vector *from* to vector *to*, starting at *at*. *Obj* must be enum, symbol, or constant.

```scheme
(let ((a [1 2 3 4 5])
      (b [11 12 13]))
   (vector-copy! a 1 b)
   a)                              ==>  [1 11 12 13 5]
(let ((a [1 2 3 4 5])
      (b [11 12 13]))
   (vector-copy! a 1 b 2)
   a)                              ==>  [1 13 3 4 5]
(let ((a [1 2 3 4 5])
      (b [11 12 13]))
   (vector-copy! a 3 b 1 2)
   a)                              ==>  [1 2 3 12 5]
```

# vector-fill!
`(vector-fill! vector fill)`, *procedure*  
`(vector-fill! vector fill start)`, *procedure*  
`(vector-fill! vector fill start end)`, *procedure*

Stores *fill* in the elements [*start* .. *end*) of vector *vector*. *Obj* must be enum, symbol, or constant.

```scheme
(let ((a [1 2 3 4 5]))
   (vector-fill! a 'x)
   a)                              ==>  '[x x x x x]
(let ((a [1 2 3 4 5]))
   (vector-fill! a 'y 3)
   a)                              ==>  '[1 2 3 y y]
(let ((a [1 2 3 4 5]))
   (vector-fill! a 'z 3 4)
   a)                              ==>  '[1 2 3 z 5]
```
