(file json)
===========

JSON is *a de facto* industry standard for data exchange.
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
read-json
read-json-file
read-json-port
read-json-string
read-json-stream
write-json
write-json-file
stringify

# read-json
`(read-json pss)`, *procedure*

Reads a json data from port, string, or stream.

```scheme
> (import (file json))
> (read-json-string "{'name':'me','phone-number':223323232}")
#ff((name . "me") (phone-number . 223323232))
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
