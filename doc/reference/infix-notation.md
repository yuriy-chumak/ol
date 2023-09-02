Infix Notation
==============

Lisp is a very simple and elegant language.
Unfortunately, the generally accepted mathematical notation contradicts the Lisp one, which brings some inconvenience in the transfer of formulas into the language. So, the `infix-notation` macro was inctroduced.

# infix-notation
`(infix-notation expression)`, *macro*

Converts an *expression* in infix notation to prefix.  
Note: `\\` is a shortcut for `infix-notation`. `\\` is experimental and may change in the future.


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
(\\  2 + 3 )                ==>   5
(\\  2 + 3 * 7 )            ==>  23
(\\  (2 + 3) * 7 )          ==>  35

; some operators have right associativity:
(\\  2 ^ 3 ^ 4 )            ==>  2417851639229258349412352
(\\  (2 ^ 3) ^ 4 )          ==>  4096

; symbols are supported, sure:
(\\  a + b )                ===  (+ a b)
(\\  b + a )                ===  (+ b a)
(\\  a + b )                ==>  8

(\\  a + b * c )            ===  (+ a (* b c))
(\\  (a + b) * c )          ===  (* (+ a b) c)
(\\  a * (b + c) )          ===  (* a (+ b c))
(\\  a / b + c / d )        ===  (+ (/ a b) (/ c d))
(\\  (a + b) * (c + d) )    ===  (* (+ a b) (+ c d))
(\\  a + b * c + d )        ===  (+ (+ a (* b c)) d)

(\\  a + b * c )            ==>  38
(\\  (a + b) * c )          ==>  56

; well, let's try some functions:

(\\  f(4) )                 ==>   6
(\\  f(3 + 5) )             ==>  10
(\\  g(3, 5) )              ==>  22
(\\  g(3 5) )               ==>  22 ; commas may be ommited

```