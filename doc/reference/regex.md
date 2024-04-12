Regular Expressions
===================

`(owl regex)` library implements a mostly complete POSIX-compatible regular expressions.

You can declare the regular expression directly using Ol's builtin syntax, or you can convert the regex string representation into the actual regular expression itself. Regexes usage as simple as any other function - just use it with single string or list argument.

Three types of regular expressions are supported: simple match, match with matched return, replace, and split. Additional string to regex function is provided.

# Simple matching regular expression
`(m/.../ str-or-list)`, *procedure*

```scheme
(m/a/ "hello")              ==>  #false
(m/a/ "aloha")              ==>  #true
```

# Matching with returning matched part
`(g/.../ str-or-list)`, *procedure*

```scheme
(g/a/ "hello")              ==>  #false
(g/a/ "aloha")              ==>  '(#\a)
```

# Replace
`(s/.../.../)`, *procedure*

```scheme
(s/[a-z]/#/ "ABabcCDE")     ==>  "AB#bcCDE"
(s/[a-z]/#/g "ABabcCDE")    ==>  "AB###CDE"
```

# Split
`(c/.../`), *procedure*

```scheme
(c/ / "ab cde f  ghi")      ==> '("ab" "cde" "f" "" "ghi")
(c/ +/ "ab cde f  ghi")     ==> '("ab" "cde" "f" "ghi")
(c/(\d)\1/ "12112122112")   ==> '("12" "21" "" "2")
```


# string->regex
`(string->regex str)`, *procedure*

```scheme
```
