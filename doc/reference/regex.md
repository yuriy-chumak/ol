Regular Expressions
===================

`(owl regex)` library implements a mostly complete POSIX-compatible regular expressions.

You can declare the regular expression directly using Ol's builtin syntax, or you can convert the regex string representation into the actual regular expression itself. Regexes usage as simple as any other function - just use it with single string or list argument.

Four types of regular expressions are supported:
simple [match](#simple-matching-regular-expression),
match with [matched return](#matching-with-returning-matched-part),
[replace](#replace),
and [split](#split).  
Additional [string to regex](#string-regex) helper function is provided.

# Simple matching regular expression
`(m/.../ str-or-stream)`, *procedure*

```scheme
(m/a/ "hello")              ==>  #false
(m/a/ "aloha")              ==>  #true
(m/a/ (string->list "abc")) ==>  #true
(m/a/ (str-iter "xaz"))     ==>  #true

;; Characters
; . - any character
(m/./ "a")                  ==>  #true
(m/./ "7")                  ==>  #true
(m/./ "")                   ==>  #false

; \d - one digit from 0 to 9
(m/\d/ "1")                 ==>  #true
(m/\d/ "7")                 ==>  #true
(m/\d/ "b")                 ==>  #false
; \D - is not a \d
(m/\D/ "1")                 ==>  #false
(m/\D/ "7")                 ==>  #false
(m/\D/ "b")                 ==>  #true

; \w - one word character (0-9, a-z, a-Z, and _)
(m/\w/ "2")                 ==>  #true
(m/\w/ "x")                 ==>  #true
(m/\w/ "Y")                 ==>  #true
(m/\w/ "_")                 ==>  #true
(m/\w/ "%")                 ==>  #false
(m/\w/ "а́")                 ==>  #false
(m/\w/ ".")                 ==>  #false
; \W - is not a \w
(m/\W/ "2")                 ==>  #false
(m/\W/ "x")                 ==>  #false
(m/\W/ "Y")                 ==>  #false
(m/\W/ "_")                 ==>  #false
(m/\W/ "%")                 ==>  #true
(m/\W/ "а́")                 ==>  #true
(m/\W/ ".")                 ==>  #true

; \s - one whitespace (space, tab, return, newline, vtab, formfeed)
(m/\s/ " ")                 ==>  #true
(m/\s/ "\t")                ==>  #true
(m/\s/ "\n")                ==>  #true
(m/\s/ "\f")                ==>  #true
(m/\s/ "\b")                ==>  #false
; \S - is not a \s
(m/\S/ " ")                 ==>  #false
(m/\S/ "\t")                ==>  #false
(m/\S/ "\n")                ==>  #false
(m/\S/ "\f")                ==>  #false
(m/\S/ "\b")                ==>  #true

; special characters (dot, backslash, newline, etc.)
(m/\./ "a")                 ==>  #false
(m/\./ ".")                 ==>  #true
(m/\\/ "a")                 ==>  #false
(m/\\/ "\\")                ==>  #true
(m/\\w/ "\\x")              ==>  #false
(m/\\w/ "\\w")              ==>  #true
(m/\n/ "
")                          ==>  #true

; ^ - line begin, $ - line end
(m/^a/ "abc")               ==>  #true
(m/^a/ "bac")               ==>  #false
(m/a$/ "abc")               ==>  #false
(m/a$/ "cba")               ==>  #true
(m/^me$/ "me")              ==>  #true
(m/^me$/ "ame")             ==>  #false
(m/^me$/ "mea")             ==>  #false

;; Quantifiers
; + - one or more (greedy)
(m/^a+$/ "")                ==>  #false
(m/^a+$/ "a")               ==>  #true
(m/^a+$/ "aaa")             ==>  #true
(m/^a+b+c+$/ "abbccc")      ==>  #true
(m/^a+b+c+$/ "aaaccc")      ==>  #false

; * - zero or more times
(m/^a*$/ "")                ==>  #true
(m/^a*$/ "a")               ==>  #true
(m/^a*$/ "aaa")             ==>  #true
(m/^a*b*c*$/ "abbccc")      ==>  #true
(m/^a*b*c*$/ "aaaccc")      ==>  #true
(m/^a*b*c*$/ "")            ==>  #true

; ? - once or none
(m/^a?$/ "")                ==>  #true
(m/^a?$/ "a")               ==>  #true
(m/^a?$/ "aaa")             ==>  #false
(m/^a?b?c?$/ "abc")         ==>  #true
(m/^a?b?c?$/ "abcc")        ==>  #false
(m/^a?b?c?$/ "ac")          ==>  #true
(m/^a?b?c?$/ "b")           ==>  #true
(m/^a?b?c?$/ "")            ==>  #true

; {n} - exactly n times
(m/^a{3}$/ "")              ==>  #false
(m/^a{3}$/ "a")             ==>  #false
(m/^a{3}$/ "aaa")           ==>  #true
(m/^a{3}$/ "aaaa")          ==>  #false
(m/b{2}/ "ababc")           ==>  #false
(m/b{2}/ "ababbc")          ==>  #true

; {n,m} - between n and m times (including n and m)
(m/^a{2,4}$/ "a")           ==>  #false
(m/^a{2,4}$/ "aa")          ==>  #true
(m/^a{2,4}$/ "aaa")         ==>  #true
(m/^a{2,4}$/ "aaaa")        ==>  #true
(m/^a{2,4}$/ "aaaaa")       ==>  #false

; {n,} - exactly n or more than n times
(m/^a{2,}$/ "a")            ==>  #false
(m/^a{2,}$/ "aa")           ==>  #true
(m/^a{2,}$/ "aaa")          ==>  #true
(m/^a{2,}$/ "aaaaaaaaaa")   ==>  #true

; | - Alternation (OR operand)
(m/a|b/ "a")                ==>  #true
(m/a|b/ "b")                ==>  #true
(m/a|b/ "c")                ==>  #false
(m/a|b|c/ "a")              ==>  #true
(m/a|b|c/ "b")              ==>  #true
(m/a|b|c/ "c")              ==>  #true
(m/a|b|c/ "d")              ==>  #false

; [...] - One of the characters in the brackets
(m/[abc]/ "a")               ==>  #true
(m/[abc]/ "c")               ==>  #true
(m/[abc]/ "d")               ==>  #false

;; Grouping
; (...) - capturing group, counting started from 1
; \n - contents of group n (from 1 to 9)
(m/(.)\1b\1/ "aaba")        ==>  #true
(m/(.)\1b\1/ "ccbc")        ==>  #true
(m/(.)\1b\1/ "aabc")        ==>  #false
(m/^(ab)+$/ "ababa")        ==>  #false
(m/^(ab)+$/ "ababab")       ==>  #true
(m/^(a(bc)*)+x$/
   "aabcabcbcabcbcbcx")     ==>  #true
(m/^(a(bc|de)*)+x$/
   "aabcadedeabcbcbcx")     ==>  #true

; (?: … ) - non-capturing group
(m/^(?:ab)(c)\1$/ "abcc")   ==>  #true

;; Lookarounds
; (?= … ) - positive lookahead
(m/(?=\d{10})\d{5}/ "0123456789")  ==>  #true
(m/(?=\d{10})\d{5}/ "01234567")    ==>  #false
(m/(?=\d{10})\d{5}/ "012345678a")  ==>  #false
(m/(?=\w{10})\d{5}/ "012345678a")  ==>  #true

; (?! … ) - negative lookahead
(m/(?!theatre)the\w+/ "theme")     ==>  #true
(m/(?!theatre)the\w+/ "theatre")   ==>  #false

; (?<= … ) - positive lookbehind
(m/^a+(?<=aa)b/ "ab")        ==>  #false
(m/^a+(?<=aa)b/ "aab")       ==>  #true

; (?<! … )
(m/^\w+(?<!aa)b/ "ab")        ==>  #true
(m/^\w+(?<!aa)b/ "aab")       ==>  #false
(m/^\w+(?<!aa)b/ "aaab")      ==>  #false
(m/^\w+(?<!aa)b/ "accb")      ==>  #true

;; Lazy Quantifiers
; ...

; others
(m/t[ah]i/ "anything")      ==>  #true
(m/t[ah]i/ "contained")     ==>  #true
(m/t[ah]i/ "triangle")      ==>  #false
```

# Matching with returning matched part
`(g/.../ str-or-list)`, *procedure*

```scheme
(g/a/ "hello")              ==>  #false
(g/a/ "aloha")              ==>  "a"
```

# Replace
`(s/.../.../)`, *procedure*

```scheme
(s/[a-z]/#/ "ABabcCDE")     ==>  "AB#bcCDE"
(s/[a-z]/#/g "ABabcCDE")    ==>  "AB###CDE"

(s/(a??)(a??)(a??)x/1\1 2\2 3\3/ "aax")
                            ==>  "1 2a 3a"
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
