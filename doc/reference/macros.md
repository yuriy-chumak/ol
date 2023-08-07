macro system (macros)
=====================

* Please note, that the Ol classic macro syntax extension is experimental, and can be changed before the release of the next Ol version.

Macros are said to be what makes Lisp *Lisp*. Ol supports both Scheme hygienic macros and the classic macro system along with Ol extensions.

While hygienic macros are just declarative syntax descriptions, classic macros are full-blown Lisp procedures with the full power of any other Lisp procedure (including continuations, coroutines, i/o, etc). The return value of a classic macro is a list representing the new program.

```scheme
```

[define-syntax](#define-syntax),  
[define-macro](#define-macro), [define-lazy-macro](#define-lazy-macro), [define-instant-macro](#define-instant-macro)

# define-syntax
`(define-syntax name syntax-rules)`, *primop*

```scheme
> (define-syntax and
     (syntax-rules ()
        ((and) #true)
        ((and a) a)
        ((and a . b)
           (if a (and . b) #false))))

(and)          ==>  #true
(and #f)       ==>  #false
(and #t #f #t) ==>  #false

; `let` via lambdas and `letrec`
> (define-syntax let
     (syntax-rules (letrec)
        ((let ((var val) ...) exp . rest)
           ((lambda (var ...) exp . rest) val ...))
        ((let name ((var init) ...) exp . rest)
           (letrec ((name (lambda (var ...) exp . rest))) (name init ...)))))
> (let ((a 12))
     a)
12

> (let loop ((n 10) (x 0))
     (if (= n 0)
        x
        (loop (- n 1) (+ x 2))))
20
```

# define-macro
`(define-macro name (lambda (args)))`, *primop*

Good old macros, with instant macro expanding and delayed evaluation.

```scheme
> (define-macro square (lambda (x) `(* ,x ,x)))
> (define-macro define-q-square (lambda (x)
     `(define ,x ,(square q))))

> (define q 3)
> (define-q-square a)
; a defined as q²
> a
9

> (define q 4)
> (define-q-square a)
; delayed evaluation, so new "q" value used.
> a
16

> (define-macro square (lambda (x) `(+ ,x ,x)))
> (define-q-square a)
; instant macro expanding, so old "square" macro used
> a
16
```

# define-instant-macro
`(define-instant-macro name (lambda (args) ...))`, *primop*

* primop name is a subject to discussion and may be changed.

Good old macros, with instant macro expanding and instant evaluation.

```scheme
; instant evaluation, so q must exist before macro declaration
> (define q 5)
> (define-instant-macro square (lambda (x) `(* ,x ,x)))
> (define-instant-macro define-q-square (lambda (x)
     `(define ,x ,(square q))))

> (define q 3)
> (define-q-square a)
; instant evaluation, so old "q" value used.
> a
25

> (define-macro square (lambda (x) `(+ ,x ,x)))
> (define-q-square a)
; instant macro expanding, so old "square" macro used
> a
25
```

# define-lazy-macro
`(define-lazy-macro name (lambda (args) ...))`, *primop*

* primop name is a subject to discussion and may be changed.

Good old macros, with delayed macro expanding and delayed evaluation.

```scheme
> (define-lazy-macro square (lambda (x) `(* ,x ,x)))
> (define-lazy-macro define-q-square (lambda (x)
     `(define ,x ,(square q))))

> (define q 3)
> (define-q-square a)
; a defined as q²
> a
9

> (define q 4)
> (define-q-square a)
; delayed evaluation, so new "q" value used.
> a
16

> (define-macro square (lambda (x) `(+ ,x ,x)))
> (define-q-square a)
; delayed macro expanding, so new "square" macro used
> a
8
```
