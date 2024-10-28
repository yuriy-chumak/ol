Serialization/Deserialization
=============================

The `(otus fasl)` library implements the serialization of object into a bytes list, and the parsing of a bytes list into the corresponding object.
The format is used internally to store memory images on disk. The files with .fasl suffix used when booting up Ol are just fasl-encoded functions.

[encode2](#encode2), [fasl-encode](#fasl-encode), [serialize](#serialize),
[decode2](#decode2), [fasl-decode](#fasl-decode), [deserialize](#deserialize)


# fasl-encode
`(fasl-encode obj)`, *procedure*

The fasl-encode procedure returns a list of fasl-encoded *obj*.

```scheme
(fasl-encode 0)       ==>  '(0 0 0)
(fasl-encode 1)       ==>  '(0 0 1)
(fasl-encode -1)      ==>  '(0 32 1)
(fasl-encode "abc")   ==>  '(2 3 3 97 98 99 0)
(fasl-encode 'abc)    ==>  '(2 3 3 97 98 99 1 4 1 1 0)
(fasl-encode 3/7)     ==>  '(1 42 2 0 0 3 0 0 7 0)
(fasl-encode #i1)     ==>  '(2 44 8 0 0 0 0 0 0 240 63 0)
(fasl-encode [1 2 3]) ==>  '(1 2 3 0 0 1 0 0 2 0 0 3 0)
```

# encode2
`(encode2 obj)`, *procedure*

The encode2 procedure returns lazy list of fasl-encoded *obj*. See examples for [fasl-encode](#fasl-encode).

# serialize
`(serialize obj)`, *procedure*

Same as [fasl-encode](#fasl-encode).

# decode2
`(decode2 stream fail)`, &procedure*

The decode2 procedure returns fasl-decoded object from *stream*, or *fail* if not decoded. See examples for [fasl-decode](#fasl-decode).
The symbols decoded uninterned!

# fasl-decode
`(fasl-decode stream error)`, *procedure*

The fasl-decode procedure returns fasl-decoded object from *stream*, or *fail* if not decoded. The symbols decoded uninterned!

```scheme
(fasl-decode '(0 0 0) #f)                       ==>  0
(fasl-decode '(0 0 1) #f)                       ==>  1
(fasl-decode '(0 32 1) #f)                      ==>  -1
(fasl-decode '(2 3 3 97 98 99 0) #f)            ==>  "abc"
(fasl-decode '(2 3 3 97 98 99 1 4 1 1 0) #f)    ==>  'abc
(fasl-decode '(1 42 2 0 0 3 0 0 7 0) #f)        ==>  3/7
(fasl-decode '(2 44 8 0 0 0 0 0 0 240 63 0) #f) ==>  #i1
(fasl-decode '(1 2 3 0 0 1 0 0 2 0 0 3 0) #f)   ==>  [1 2 3]
```

# deserialize
`(deserialize stream error)`, *procedure*

Like [fasl-decode](#fasl-decode), but symbols decoded interned.
