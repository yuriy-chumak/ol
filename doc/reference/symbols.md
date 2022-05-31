Symbols
=======

Symbols are objects whose usefulness rests on the fact that two symbols are identical (in the sense of *eq?* and *eqv?*) if and only if their names are spelled the same way.
For instance, they can be used the way enumerated values are used in other languages.

[symbol?](#symbol), [symbol=?](#symbol-1), [symbol->string](#symbol-string), [string->symbol](#string-symbol), [string->uninterned-symbol](#string-uninterned-symbol)

# symbol?
`(symbol? obj)`, *procedure*

```scheme
(symbol? 'foo)           ==>  #true
(symbol? '|   |)         ==>  #true
(symbol? (car '(a b)))   ==>  #true
(symbol? "bar")          ==>  #false
(symbol? '())            ==>  #false
(symbol? #f)             ==>  #false
```

# symbol=?
`(symbol=? ...)`, *procedure*

Returns #true if all the arguments are symbols and all have the same names in the sense of string=?.


```scheme
(symbol=? 'a 'b 'x 12 'x)    ==>  #false
(symbol=? 'a 'b 'x 'Q 'x)    ==>  #false
(symbol=? 'a 'a 'a 'a 'a)    ==>  #true
(symbol=?)                   ==>  #false
```

# symbol->string
`(symbol->string symbol)`, *procedure*

Returns the name of *symbol* as a string, but without adding escapes.

```scheme
(symbol->string 'flying-fish)   ==>  "flying-fish"
(symbol->string 'Martin)        ==>  "Martin"
(symbol->string '||)            ==>  ""
(symbol->string '|a b	xx|)    ==>  "a b\txx"
```

# string->symbol
`(string->symbol string)`, *procedure*

Returns the symbol whose name is string.

```scheme
(string->symbol "mISSISSIppi") ==>  'mISSISSIppi
(string->symbol "")            ==>  '||
(string->symbol "a b\txx")     ==>  '|a b	xx|

(eq? (string->symbol "x") 'x)  ==>  #true
```

# string->uninterned-symbol
`(string->uninterned-symbol string)`, *procedure*

*Uninterned symbols* are a symbols, which defeat write/read invariance, and also violate the rule that two symbols are the same if and only if their names are spelled the same.

```scheme
(eq? (string->uninterned-symbol "x") 'x)  ==>  #false
```
