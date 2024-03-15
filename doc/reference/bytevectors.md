Bytevectors
===========

*Bytevectors* represent blocks of binary data. They are fixed-length sequences of bytes, where a *byte* is an exact integer in the range from 0 to 255 inclusive.

The *length* of a bytevector is the number of elements that it contains.
This number is a non-negative integer that is fixed when the bytevector is created.

The *valid indexes* of a bytevector are the exact non-negative integers less than the length of the bytevector, starting at index **zero** as with vectors.

Bytevectors are written using the notation `#u8(...)` and function `bytevector`.

```scheme
#u8()          ===  #u8()
#u8(1 2 3 4)   ===  #u8(1 2 3 4)

(bytevector)   ===  #u8()
(bytevector 0 1 2 127 128 255)
               ===  #u8(0 1 2 127 128 255)

; all values wider than 8 bits will be truncated
(bytevector 254 255 256 257 258 7778885551232190)
               ==>  #u8(254 255 0 1 2 190)

```
## TOC

[#u8()](#bytevectors), [bytevector](#bytevectors),
[make-bytevector](#make-bytevector),
[bytevector-length](#bytevector-length),
[bytevector-u8-ref](#bytevector-u8-ref),
[bytevector-u8-set!](#bytevector-u8-set!),
[bytevector-copy](#bytevector-copy), [bytevector-copy!](#bytevector-copy!),
[bytevector-append](#bytevector-append),
[utf8->string](#utf8-string),
[string->utf8](#string-utf8).

# bytevector?
`(bytevector? obj)`, *procedure*

Returns #true if *obj* is a bytevector. Otherwise, #false is returned.

```scheme
(bytevector? 1)            ==>  #false
(bytevector? "#u8(1)")     ==>  #false
(bytevector? "12")         ==>  #false
(bytevector? [1 2 3])      ==>  #false
(bytevector? #true)        ==>  #false
(bytevector? {'a 7})       ==>  #false

(bytevector? #u8())        ==>  #true
(bytevector? #u8(7 7 7))   ==>  #true
(bytevector? (bytevector 1 2 3))
                           ==>  #true
```

# make-bytevector
`(make-bytevector k)`, *procedure*  
`(make-bytevector k byte)`, *procedure*

Returns a newly allocated bytevector of length *k*. If *byte* is given, then all elements of the bytevector are initialized to *byte*, otherwise the contents of each element are 0.

Values wider than 8 bits are not supported!

```scheme
(make-bytevector 0)        ==>  #u8()
(make-bytevector 7)        ==>  #u8(0 0 0 0 0 0 0)
(make-bytevector 2 12)     ==>  #u8(12 12)
(make-bytevector 0 33)     ==>  #u8()
(make-bytevector "3")      ==>  #false
(make-bytevector -3)       ==>  #false
(make-bytevector 7/5)      ==>  #false
```

# bytevector-length
`(bytevector-length bv)`, *procedure*  
`size` is the same as `bytevector-length` when applied to bytevectors.

Returns the length of bytevector *bv* in bytes as an exact integer.

```scheme
(bytevector-length #u8())  ==>  0
(bytevector-length #u8(1 2 3))
                           ==>  3
(bytevector-length (make-bytevector 1000000))
                           ==>  1000000
```

# bytevector-u8-ref
`(bytevector-u8-ref bv k)`, *procedure*  
`ref` is the same as `bytevector-u8-ref` when applied to bytevectors.

Returns the *k*th byte of bytevector *bv*, starting from 0.  
Negative *k* means "from the end of bytevector", starting from -1.

```scheme
(bytevector-u8-ref #u8(10 11 12 13 14 15) 0)   ==>  10
(bytevector-u8-ref #u8(10 11 12 13 14 15) 2)   ==>  12
(bytevector-u8-ref #u8(10 11 12 13 14 15) 5)   ==>  15
(bytevector-u8-ref #u8(10 11 12 13 14 15) 9)   ==>  #false
(bytevector-u8-ref #u8(10 11 12 13 14 15) -1)  ==>  15
(bytevector-u8-ref #u8(10 11 12 13 14 15) -6)  ==>  10
(bytevector-u8-ref #u8(10 11 12 13 14 15) -7)  ==>  #false
```

# bytevector-u8-set!
`(bytevector-u8-set! bv k byte)`, *procedure*

Stores *byte* as the *k*th byte of bytevector *bv*.

```scheme
> (define bv #u8(1 2 3 4 5))
> (bytevector-u8-set! bv 2 7)
> bv
#u8(1 2 7 4 5)

; change last one element
> (bytevector-u8-set! bv -1 9)
> bv
#u8(1 2 7 4 9)

; try to change something out of bounds
> (bytevector-u8-set! bv 99 2)
> bv
#u8(1 2 7 4 9)
```

# bytevector-copy
`(bytevector-copy bv)`, *procedure*  
`(bytevector-copy bv start)`, *procedure*  
`(bytevector-copy bv start end)`, *procedure*

Returns a newly allocated bytevector containing the bytes in bytevector *bv* between *start* and *end*. Default value of *start* is 0, and default value of *end* is a length of bytevector.

```scheme
> (define bv #u8(0 1 2 3 4 5 6 7 8 9))

(bytevector-copy bv)             ==>  #u8(0 1 2 3 4 5 6 7 8 9)
(bytevector-copy bv 0)           ==>  #u8(0 1 2 3 4 5 6 7 8 9)
(bytevector-copy bv 0 10)        ==>  #u8(0 1 2 3 4 5 6 7 8 9)

(bytevector-copy bv 1)           ==>  #u8(1 2 3 4 5 6 7 8 9)
(bytevector-copy bv 5)           ==>  #u8(5 6 7 8 9)
(bytevector-copy bv 100)         ==>  #false

(bytevector-copy bv 1 1)         ==>  #u8()
(bytevector-copy bv 5 5)         ==>  #u8()
(bytevector-copy bv 100 100)     ==>  #false
(bytevector-copy bv 1 2)         ==>  #u8(1)
(bytevector-copy bv 5 7)         ==>  #u8(5 6)
(bytevector-copy bv 5 100)       ==>  #false
```

# bytevector-copy!
`(bytevector-copy! to at from)`, *procedure*  
`(bytevector-copy! to at from start)`, *procedure*  
`(bytevector-copy! to at from start end)`, *procedure*

Copies the bytes of bytevector *from* between *start* and *end* to bytevector *to*, starting at *at*. Default value of *start* is 0, and default value of *end* is a length of bytevector *from*.

```scheme
> (define to (make-bytevector 10 9))
> (define from #u8(1 2 3 4 5 6 7 8))

> (bytevector-copy! to 1 from)
> to
#u8(9 1 2 3 4 5 6 7 8 9)

> (bytevector-copy! to 4 from 6 8)
> to
#u8(9 1 2 3 7 8 6 7 8 9)
```

# bytevector-append
`(bytevector-append ...)`, *procedure*

Returns a newly allocated bytevector whose elements are the concatenation of the elements in the given bytevectors.

```scheme
(bytevector-append)                         ==>  #u8()
(bytevector-append #u8() #u8() #u8())       ==>  #u8()
(bytevector-append #u8(0 1 2))              ==>  #u8(0 1 2)
(bytevector-append #u8(0 1 2) #u8(3 4 5))   ==>  #u8(0 1 2 3 4 5)
```

# utf8->string
`(utf8->string bv)`, *procedure*  
;; `(utf8->string bv start)`, *procedure*  
;; `(utf8->string bv start end)`, *procedure*

Decodes the bytes of a bytevector *bv* between *start* and *end* and returns the corresponding string (possibly Unicode).

```scheme
(utf8->string #u8(83 117 110))              ==>  "Sun"
(utf8->string #u8(206 137 206 187 206 185 206 191 207 130))
                                            ==>  "Ήλιος"
```

# string->utf8
`(string->utf8 str)`, *procedure*  
;; `(string->utf8 str start)`, *procedure*  
;; `(string->utf8 str start end)`, *procedure*

Encodes the characters of a string *str* between *start* and *end* and returns the corresponding bytevector.

```scheme
(string->utf8 "Sun")       ==>  #u8(83 117 110)
(string->utf8 "Ήλιος")     ==>  #u8(206 137 206 187 206 185 206 191 207 130)
```
