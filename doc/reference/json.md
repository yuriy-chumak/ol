(file json)
===========

JSON as a text-based data exchange format, and is *a de facto* industry standard for data exchange.
Ol provides a library for working naturally with JSON-formed files, strings and streams.

```scheme
> (import (file json))

> (define s "{'name':'me','phone-number':223323232}")
> (read-json-string s)
#ff((name . "me") (phone-number . 223323232))

> (stringify {
     'name "John"
     'phone 33443323
  })
"{\"name\":\"John\",\"phone\":33443323}"
```

## TOC
[read-json](#read-json)
[read-json-file](#read-json-file)
[read-json-port](#read-json-port)
[read-json-string](#read-json-string)
[read-json-stream](#read-json-stream)
[write-json](#write-json)
[write-json-file](#write-json-file)
[stringify](#stringify)

# read-json
`(read-json pss)`, *procedure*

Reads a json data from port, string, or stream.  
Reads valid json data, i.e. number, string, boolean, array, or object (collection of a key-value pairs).

```scheme
> (import (file json))

;; Numbers
> (read-json "123")
123

> (read-json "1e17")
100000000000000000

; number with a dot is read as inexact
> (read-json "2.135")
2.13499999

> (read-json "2.5e17")
2.5e17

;; Strings
> (read-json "\"hello\"")
"hello"

; ' is allowed as a string delimiter,
;   for the readability
> (read-json "'hello'")
"hello"

; unicode fully supported, sure
> (read-json "'χαιρετισμός'")
"χαιρετισμός"

;; Booleans
> (read-json "true")
#true

> (read-json "false")
#false

;; Objects
> (read-json "{'name':'me','phone-number':223323232}")
#ff((name . "me") (phone-number . 223323232))

; objects represented as ff's, so field access can be very fast
> (let ((obj (read-json "{'name':'me','phone':223323232}")))
     (if obj (obj 'name)))
"me"
```

# read-json-file
`(read-json-file filename)`, *procedure*



```scheme
> (import (file json))

> (read-json-file "tests/apple.json")
#ff((size . "Large") (fruit . "Apple") (color . "Red"))
```

# read-json-port
`(read-json-port port)`, *procedure*

```scheme
```

# read-json-string
`(read-json-string port)`, *procedure*

```scheme
```

# write-json
`(write-json something)`, *procedure*

```scheme
```

# write-json-file
`(write-json-file something)`, *procedure*

```scheme
```

# stringify
`(stringify something)`, *procedure*

```scheme
```
