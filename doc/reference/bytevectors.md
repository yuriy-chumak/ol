Bytevectors
===========

Bytevectors represent blocks of binary data. They are fixed-length sequences of bytes, where a byte is an exact integer in the range from 0 to 255 inclusive.

The length of a bytevector is the number of elements that it contains.
This number is a non-negative integer that is fixed when the bytevector is created.

The valid indexes of a bytevector are the exact non-negative integers less than the length of the bytevector, starting at index **zero** as with vectors.

Bytevectors are written using the notation `#u8(byte ...)` and `(bytevector byte ...)`.

```scheme
#u8()          ===  #u8()
#u8(1 2 3 4)   ===  #u8(1 2 3 4)
(bytevector)   ===  #u8()
(bytevector 0 1 2 127 128 255)
               ===  #u8(0 1 2 127 128 255)
; all values larger than one byte will be truncated
(bytevector 254 255 256 257 258 7778885551232190)
               ==>  #u8(254 255 0 1 2 190)
```
## TOC

[#u8](#bytevectors), [bytevector](#bytevectors),
[make-bytevector](#make-bytevector),
[bytevector-length](#bytevector-length),
[bytevector-u8-ref](#bytevector-u8-ref),
[bytevector-u8-set!](#bytevector-u8-set!),
[bytevector-copy](#bytevector-copy), [bytevector-copy!](#bytevector-copy!),
[bytevector-append](#bytevector-append),
[utf8->string](#utf8->string),
[string->utf8](#string->utf8).

# bytevector?
`(bytevector? obj)`, *procedure*

Returns #true if obj is a bytevector. Otherwise, #false is returned.

```scheme
(bytevector? 1)       ==>  #false
(bytevector? #u8())   ==>  #true
```

# make-bytevector
`(make-bytevector k)`, *procedure*
`(make-bytevector k byte)`, *procedure*

Returns a newly allocated bytevector of length k. If byte is given, then all elements of the bytevector are initialized to byte, otherwise the contents of each element are 0.

# bytevector-length
`(bytevector-length bv)`, *procedure*

Returns the length of bytevector in bytes as an exact inte-
ger.

# bytevector-u8-ref
`(bytevector-u8-ref bv k)`, *procedure*
`ref` is the same as `bytevector-u8-ref` when applied to bytevectors.

Returns the *k*th byte of bytevector *bv*, starting from 0.  
Negative *k* means "from the end of bytevector", starting from -1.

# bytevector-u8-set!
`(bytevector-u8-set! bv k byte)`, *procedure*

Stores *byte* as the *k*th byte of bytevector *bv*.

# bytevector-copy
`(bytevector-copy bv)`, *procedure*
`(bytevector-copy bv start)`, *procedure*
`(bytevector-copy bv start end)`, *procedure*

Returns a newly allocated bytevector containing the bytes in bytevector *bv* between *start* and *end*.

# bytevector-copy!
`(bytevector-copy! to at from)`, *procedure*
`(bytevector-copy! to at from start)`, *procedure*
`(bytevector-copy! to at from start end)`, *procedure*

Copies the bytes of bytevector from between start and end to bytevector to, starting at at.

# bytevector-append
`(bytevector-append bv ...)`, *procedure*

Returns a newly allocated bytevector whose elements are the concatenation of the elements in the given bytevectors.

```scheme
(bytevector-append #u8(0 1 2) #u8(3 4 5))   ==>  #u8(0 1 2 3 4 5)
```

# utf8->string
`(utf8->string bv)`, *procedure*
`(utf8->string bv start)`, *procedure*
`(utf8->string bv start end)`, *procedure*

# string->utf8
`(string->utf8 str)`, *procedure*
`(string->utf8 str start)`, *procedure*
`(string->utf8 str start end)`, *procedure*
