Otus Lisp
=========

Otus Lisp (Ol in short) is a purely functional dialect of Lisp.

It implements an extended subset of the R<sup>7</sup>RS Scheme
([PDF](https://small.r7rs.org/attachment/r7rs.pdf)), including
but not limited to some SRFIs. It is tiny (~ 64KB), embeddable
and cross-platform;  provides a portable, high level interface
to call code written in another language (c, python, lua, etc).

Reference
=========

This is an Ol reference, not a Scheme. Scheme R<sup>7</sup>RS differences decribed in the main [README](https://github.com/yuriy-chumak/ol#r7rs-differences) file.

Examples are provided in two forms:
the equivalence ("==>") form,  
```scheme
; arrows mean equivalence in sence of `equal?`
(+ 1 2 3)  ==>  6
(+ 1 2 3)  <=>  (+ 3 2 1)
```

and interactive ("> ") form.  
```scheme
; it shows behavior as if someone typing code
; in an interactive ol session
> #i1.2
1.199999999

> (let ((N 10000))
      (define (sign n)
         (if (zero? (mod n 2)) + -))
      (fold (lambda (f x i)
               ((sign i) f (/ #i4 x)))
         #i4
         (iota N 3 2)
         (iota N 1)))
3.14169264
```

All provided examples are tested with Ol's latest build each time the code is submitted to GitHub.


List of Standard procedures
---------------------------

- [Pairs and Lists](pairs-and-lists.md).
- [Equivalence predicates](equivalence-predicates.md).
- [Booleans](booleans.md).
- [Numerical Operations](numerical-operations.md).
- [Symbols](symbols.md).
- [Characters](characters.md).
- [Vectors](vectors.md).
- [Lazy Lists](lazy-lists.md).
- [Sorting](sorting.md).
- [Coroutines and Actors](async.md).
- [Strings](strings.md), TBD.
- [Bytevectors](bytevectors.md), TBD.
- [Control features](control-features.md), TBD.
- [Exceptions](exceptions.md), TBD.
- [Environments and evaluation](environments-and-evaluation.md), TBD.
- [Input and output](input-and-output.md), TBD.
- [System interface](system-interface.md), TBD.

- [Dictionaries](dictionaries.md), TBD.
- [Lazy evaluations](lazy-evaluations.md), TBD.

- [Serialization/Deserialization](serialization-deserialization.md)

Alphabetic Index of Definitions of Concepts, Keywords, and Procedures
=====================================================================
|         |         |         |         |         |         |         |
|:-------:|:-------:|:-------:|:-------:|:-------:|:-------:|:-------:|
| [!](#!) | [#](##) | [A](#a) | [B](#b) | [C](#c) | [D](#d) | [E](#e) |
| [F](#f) | [G](#g) | [H](#h) | [I](#i) | [J](#j) | [K](#k) | [L](#l) |
| [M](#m) | [N](#n) | [O](#o) | [P](#p) | [Q](#q) | [R](#r) | [S](#s) |
| [T](#t) | [U](#u) | [V](#v) | [W](#w) | [X](#x) | [Y](#y) | [Z](#z) |

## <a name="!"></a>!
[=](numerical-operations.md#-)
[<](numerical-operations.md#-1)
[<=](numerical-operations.md#-3)
[>](numerical-operations.md#-2)
[>=](numerical-operations.md#-4)
[+](numerical-operations.md#-5)
[-](numerical-operations.md#-)
[*](numerical-operations.md#-6)
[/](numerical-operations.md#-7)
## <a name="#"></a>#
[#null](pairs-and-lists.md#null)
## A
[abs](numerical-operations.md#abs)
[actor](async.md#actor)
[actor-linked](async.md#actor-linked)
[append](pairs-and-lists.md#append)
[assq](pairs-and-lists.md#assq)
[assv](pairs-and-lists.md#assv)
[assoc](pairs-and-lists.md#assoc)
[async](async.md#async)
[async-linked](async.md#async-linked)
[await](async.md#await)
[await-linked](async.md#await-linked)
## B
[boolean?](boolean.md#boolean)
[boolean=?](boolean.md#boolean-1)
## C
[car](pairs-and-lists.md#car)
[cdr](pairs-and-lists.md#cdr)
[caar](pairs-and-lists.md#caar--cddr)
[cadr](pairs-and-lists.md#caar--cddr)
[cdar](pairs-and-lists.md#caar--cddr)
[cddr](pairs-and-lists.md#caar--cddr)
[caaar](pairs-and-lists.md#caar--cddr)
[caadr](pairs-and-lists.md#caar--cddr)
[cadar](pairs-and-lists.md#caar--cddr)
[caddr](pairs-and-lists.md#caar--cddr)
[cdaar](pairs-and-lists.md#caar--cddr)
[cdadr](pairs-and-lists.md#caar--cddr)
[cddar](pairs-and-lists.md#caar--cddr)
[cdddr](pairs-and-lists.md#caar--cddr)
[caaaar](pairs-and-lists.md#caar--cddr)
[caaadr](pairs-and-lists.md#caar--cddr)
[caadar](pairs-and-lists.md#caar--cddr)
[caaddr](pairs-and-lists.md#caar--cddr)
[cadaar](pairs-and-lists.md#caar--cddr)
[cadadr](pairs-and-lists.md#caar--cddr)
[caddar](pairs-and-lists.md#caar--cddr)
[cadddr](pairs-and-lists.md#caar--cddr)
[cdaaar](pairs-and-lists.md#caar--cddr)
[cdaadr](pairs-and-lists.md#caar--cddr)
[cdadar](pairs-and-lists.md#caar--cddr)
[cdaddr](pairs-and-lists.md#caar--cddr)
[cddaar](pairs-and-lists.md#caar--cddr)
[cddadr](pairs-and-lists.md#caar--cddr)
[cdddar](pairs-and-lists.md#caar--cddr)
[cddddr](pairs-and-lists.md#caar--cddr)
[ceiling](numerical-operations.md#ceiling)
[char?](characters.md#char)
[char->integer](characters.md#char-integer)
[check-mail](async.md#check-mail)
[complex?](numerical-operations.md#complex)
[cons](pairs-and-lists.md#cons)
[cons*](pairs-and-lists.md#cons-1)
## D
[decode2](serialization-deserialization.md#decode2)
[denominator](numerical-operations.md#denominator)
[deserialize](serialization-deserialization.md#deserialize)
[digit-value](characters.md#digit-value)
[drop](pairs-and-lists.md#drop)
## E
[encode2](serialization-deserialization.md#encode2)
[eq?](equivalence-predicates.md#eq)
[eqv?](equivalence-predicates.md#eqv)
[equal?](equivalence-predicates.md#equal)
[even?](numerical-operations.md#even)
[exact?](numerical-operations.md#exact)
## F
[fasl-decode](serialization-deserialization.md#fasl-decode)
[fasl-encode](serialization-deserialization.md#fasl-encode)
[finite?](numerical-operations.md#finite)
[floor](numerical-operations.md#floor)
[fold](pairs-and-lists.md#fold)
[foldr](pairs-and-lists.md#foldr)
## G
## H
## I
[inexact?](numerical-operations.md#inexact)
[infinite?](numerical-operations.md#infinite)
[integer?](numerical-operations.md#integer)
[integer->char](characters.md#integer-char)
[isort](sorting.md#isort)
[iota](pairs-and-lists.md#iota)
## J
## K
## L
[length](pairs-and-lists.md#length)
[list](pairs-and-lists.md#list)
[list?](pairs-and-lists.md#list-1)
[list->vector](vectors.md#list-vector)
[list-copy](pairs-and-lists.md#list-copy)
[list-ref (lref)](pairs-and-lists.md#list-ref-lref)
[list-set!](pairs-and-lists.md#list-set)
[list-tail](pairs-and-lists.md#list-tail)
[lrange](pairs-and-lists.md#lrange)
## M
[mail](async.md#mail)
[make-list](pairs-and-lists.md#make-list)
[make-vector](vectors.md#make-vector)
[map](pairs-and-lists.md#map)
[max](numerical-operations.md#max)
[memq](pairs-and-lists.md#memq)
[memv](pairs-and-lists.md#memv)
[member](pairs-and-lists.md#member)
[mergesort](sorting.md#mergesort)
[min](numerical-operations.md#min)
[modulo](numerical-operations.md#modulo)
## N
[nan?](numerical-operations.md#nan)
[natural?](numerical-operations.md#natural)
[negative?](numerical-operations.md#negative)
[not](pairs-and-lists.md#not)
[null?](pairs-and-lists.md#null-1)
[number?](numerical-operations.md#number)
[numerator](numerical-operations.md#numerator)
## O
[odd?](numerical-operations.md#odd)
## P
[pair?](pairs-and-lists.md#pair)
[positive?](numerical-operations.md#positive)
## Q
[quicksort](sorting.md#quicksort)
[quotient](numerical-operations.md#quotient)
## R
[rational?](numerical-operations.md#rational)
[rationalize](numerical-operations.md#rationalize)
[real?](numerical-operations.md#real)
[remainder](numerical-operations.md#remainder)
[repeat](pairs-and-lists.md#repeat)
[reverse](pairs-and-lists.md#reverse)
[round](numerical-operations.md#round)
## S
[set-car!](pairs-and-lists.md#set-car)
[set-cdr!](pairs-and-lists.md#set-cdr)
[serialize](serialization-deserialization.md#serialize)
[sleep](async.md#sleep)
[sort](sorting.md#sort)
[square](numerical-operations.md#square)
[sqrt](numerical-operations.md#sqrt)
[string->symbol](symbols.md#string-symbol)
[string->uninterned-symbol](symbols.md#string-uninterned-symbol)
[string->vector](vectors.md#string-vector)
[symbol?](symbols.md#symbol)
[symbol=?](symbols.md#symbol-1)
[symbol->string](symbols.md#symbol-string)
## T
[take](pairs-and-lists.md#take)
[truncate](numerical-operations.md#truncate)
## U
## V
[vector](vectors.md#vector)
[vector?](vectors.md#vector-1)
[vector->list](vectors.md#vector-list)
[vector->string](vectors.md#vector-string)
[vector-append](vectors.md#vector-append)
[vector-copy](vectors.md#vector-copy)
[vector-copy!](vectors.md#vector-copy-1)
[vector-fill!](vectors.md#vector-fill)
[vector-length](vectors.md#vector-length)
[vector-ref](vectors.md#vector-ref)
[vector-set!](vectors.md#vector-set)
## W
[wait-mail](async.md#wait-mail)
## X
## Y
## Z
[zero?](numerical-operations.md#zero)
