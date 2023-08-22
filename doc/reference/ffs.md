(owl ff)
========

Finite Functions
----------------

Ol provides out-of-the-box key-value dictionaries called "finite functions".
Such name means that you can use these dictionaries as regular functions along with using them as a dictionary.

The finite functions (ffs) are internally represented as red-black trees, so performance is much better than widely used association lists.

```scheme
> (define numbers {
     1 "one"
     7 "seven"
     3 "three" })
> numbers
'#ff((1 . "one") (3 . "three") (7 . "seven"))

> (numbers 1)
"one"

; the optional second argument will be returned if the key is not found
> (numbers 7 #f)
"seven"

> (numbers 33 "unknown")
"unknown"
```

As a key you *should* use objects that are comparable in sense of `eq?`. Such as
* symbols,
* #true, #false, #null, #empty, #eof
* small numbers (enums),
  * -16777215 .. +16777215 for 32-bit machines
  * -72057594037927935 .. +72057594037927935 for 64-bit machines.

> in fact, you *can* use any object as a key, but you must remember that `({ "me" 111} "me")` will fail while `(let ((me "me")) ({ me 111} me))` will ok.

The value `#empty` (`#e`, and `empty` which is deprecated) is an empty finite function. It contains no mappings.

[get](#get), [getf](#getf), [put](#put), [put!](#put-1), [del](#del),
[keys](#keys),
[make-ff](#make-ff), [list->ff](#list-ff), [ff->list](#ff-list), [alist->ff](#alist-ff), [ff->alist](#alist-ff), [pairs->ff](#pairs-ff), [ff->pairs](#ff-pairs),
[ff-for-each](#ff-for-each), [ff-map](#ff-map), [ff-fold](#ff-fold), [ff-foldr](#ff-foldr),
[ff-diff](#ff-diff), [ff-union](#ff-union), [ff-replace](#ff-replace)

# get
`(get ff key default)`, *procedure*

Wrapper for direct ff call. Returns the default value if *ff* is not ff, instead of a runtime error.

```scheme
> (define numbers {
     1 "one"
     7 "seven"
     3 "three" })

> (get numbers 1 "not found")
"one"

> (get numbers 77 "not found")
"not found"

> (get 'not-an-ff 1 "not found")
"not found"
```

# getf
`(getf ff key)`, *procedure*

Alias for `(get ff key #false)`.

# put
`(put ff key value)`, *procedure*

Returns a (possibly rebalanced) copy of *ff* with added new *key*-*value* pair.

```scheme
> (put #empty 7 42)
'#ff((7 . 42))

> (put {1 2 3 "4"} 7 'itsme)
'#ff((1 . 2) (3 . "4") (7 . itsme))
```

# put!
`(put! ff key value)`, *procedure*

> This is a non-functional feature. Use them only if you know what you are doing! This can lead to unexpected behavior of your program.

Modifies existing ff by changing value of an existing key-value pair to new value.

# del
`(del ff key)`, *procedure*

# keys
`(keys ff)`, *procedure*

```scheme
> (keys { 1 "one"
          7 "seven"
          3 "three" })
'(1 3 7)
```

# make-ff
`(make-ff list)`, *procedure*

Same as `list->ff`.

# list->ff
`(list->ff list)`, *procedure*

```scheme
> (list->ff '())
#empty

> (list->ff '(1 2 3 4))
'#ff((1 . 2) (3 . 4))

> (list->ff '(1 2 3 4 1 8))
'#ff((1 . 8) (3 . 4))
```

# ff->list
# alist->ff
`(alist->ff alist)`, *procedure*

```scheme
> (alist->ff '())
#empty

> (alist->ff '((1 . 2) (3 . 4)))
'#ff((1 . 2) (3 . 4))

> (alist->ff '((1 . 2) (3 . 4) (1 . 8)))
'#ff((1 . 8) (3 . 4))
```

# ff->alist

# pairs->ff
`(pairs->ff pairs)`, *procedure*

Same as `alist->ff`.

# ff->pairs
`(ff->pairs ff)`, *procedure*

Same as `ff->alist`.

# ff-for-each
# ff-map
# ff-fold
# ff-foldr

# ff-diff
# ff-union
# ff-replace
