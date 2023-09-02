Infix Notation
==============

Lisp is a very simple and elegant language.
Unfortunately, the generally accepted mathematical notation contradicts the Lisp one, which brings some inconvenience in the transfer of formulas into the language. So, the `infix-notation` macro was introduced.

# infix-notation
`(infix-notation expression)`, *macro*

Converts an *expression* in infix notation to prefix.  
`\\` is a shortcut for `infix-notation`. `\\` is experimental and may change in the future.

Note the use of spaces!
* `a + b` is a valid sum of `a` and `b`
* `a+b` is a symbol `|a+b|` which may or may not bound in the current environment,
* `a !` is a symbol `a` and a function `!`,
* `a!` is a symbol `|a!|`,
* but `18!` is a number `18` and a function `!`,
* spaces before and after `(` and `)` may be ommited,
* spaces around `,` may be ommited,

```scheme
; preamble:
> (import (math infix-notation))
> (define a 3)
> (define b 5)
> (define c 7)
> (define ^ expt)
> (define (f x) (\\ x + 2))
> (define (g x y) (\\ 2 * x + 3 * y + 1))

; simple arithmetic:
(\\  2 + 3)                 ==>   5
(\\  2 + 3 * 7)             ==>  23
(\\  (2 + 3) * 7)           ==>  35

; some operators have right associativity:
(\\  2 ^ 3 ^ 4)             ==>  2417851639229258349412352
(\\  2 ^ (3 ^ 4))           ==>  2417851639229258349412352
(\\  (2 ^ 3) ^ 4)           ==>  4096

; symbols are supported, sure:
(\\  a + b)                 ===  (+ a b)
(\\  b + a)                 ===  (+ b a)
(\\  a + b)                 ==>  8

(\\  a + b * c)             ===  (+ a (* b c))
(\\  (a + b) * c)           ===  (* (+ a b) c)
(\\  a * (b + c))           ===  (* a (+ b c))
(\\  a / b + c / d)         ===  (+ (/ a b) (/ c d))
(\\  (a + b) * (c + d))     ===  (* (+ a b) (+ c d))
(\\  a + b * c + d)         ===  (+ (+ a (* b c)) d)

(\\  a + b * c)             ==>  38
(\\  (a + b) * c)           ==>  56

; well, let's try some functions:
(\\  f(4))                  ==>   6
(\\  f(3 + 5))              ==>  10
(\\  g(3, 5))               ==>  22
(\\  g(3 5))                ==>  22 ; commas may be ommited

(\\  f(g(f(4),f(f(8)))) )   ===  (f (g (f 4) (f (f 8))))
(\\  f(g(f(4),f(f(8)))) )   ==>  51

; how about suffix functions:
(\\  18!   )                ==>  6402373705728000
(\\  18 !  )                ==>  6402373705728000
(\\  (18)! )                ==>  6402373705728000
(\\  (10 + 8)! )            ==>  6402373705728000
```

## Extending and Customizing
We may add custom operators:
```scheme
> (import (math infix-notation))

; let's define "power" operator
> (define (⊡ a b) (expt a b))

; add this operator with higher priority (+ has 2, * has 3)
> (define \\operators (put \\operators '⊡ 4))

; now use it
(\\  2 + 11 ⊡ 5)            ==>  161053
(\\  (2 + 11) ⊡ 5)          ==>  371293
```

We may extend our math with custom suffixes:

```scheme
> (import (math infix-notation))

; let's define "Fourth power" function
> (define (⁴ x) (* x x x x))

; add this function to the postfix dictionary
> (define \\postfix-functions (put \\postfix-functions '⁴ #t))

; now use it
(\\  2⁴ )                   ==>  16
(\\  2 ⁴ )                  ==>  16
(\\  (2)⁴ )                 ==>  16
(\\  (2 + 1)⁴ )             ==>  81

; and try it with a function
> (define (f x) (\\ x + 2))

(\\  f(2)⁴ )                ==> 256
(\\  f(2⁴) )                ==>  18
```

## Popular Mistakes

Spaces with symbols are very important!  
Check this code:
```scheme
> (import (math infix-notation))

; define some symbol value
> (define a 3)

;  and some dumb symbol |a!|
> (define a! 777)

(\\  a !)                   ==>  6    ; is a factorial of 3
(\\  a!)                    ==>  777  ; is a value of |a!|
```