Otus Lisp
=========

Otus Lisp (Ol in short) is a purely functional dialect of Lisp.

Ol implements an extended subset of the R<sup>7</sup>RS Scheme
([PDF](https://small.r7rs.org/attachment/r7rs.pdf)), including
but not limited to some SRFIs. It's tiny (~ 64KB), embeddable,
and cross-platform;  provides a portable, high level interface
to call code written in another language (c, python, lua, etc).

Reference
=========

This is an Ol reference, not a Scheme. Scheme R<sup>7</sup>RS differences described in the [DIFFERENCES](../R7RS-DIFFERENCES.md) file.

Examples are provided in two forms:
1. in the equivalence (in sense of [equal?](equivalence-predicates.md#equal)) form using symbols "==>" and "===",
```scheme
(+ 1 2 3)  ==>  6
[1 2 3 4]  ===  (make-vector '(1 2 3 4))
```

2. in the interactive form using prompt symbol "> ", which shows the behavior as if someone were typing code in an interactive ol session.
```scheme
> #i1.2
1.199999999

> (let ((N 10000))
      (define (sign n)
         (if (even? n) + -))
      (fold (lambda (f x i)
               ((sign i) f (/ #i4 x)))
         #i4
         (iota N 3 2)
         (iota N 1)))
3.14169264
```

All provided examples are tested with the Ol's latest build each time the code is submitted to GitHub.


List of Standard procedures
---------------------------

- [Pairs and Lists](pairs-and-lists.md).
- [Equivalence predicates](equivalence-predicates.md).
- [Booleans](booleans.md).
- [Numerical Operations](numerical-operations.md).
- [Symbols](symbols.md).
- [Characters](characters.md).
- [Vectors](vectors.md).
- [Bytevectors](bytevectors.md).
- [Lazy Evaluations](lazy-evaluations.md).
- [Sorting](sorting.md).
- [Coroutines and Actors](async.md).
- [Macro System](macros.md).
- [Finite Functions](ffs.md).
- [Infix Notation](infix-notation.md).
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
[\\\\](infix-notation.md)
## <a name="#"></a>#
[#null](pairs-and-lists.md#null)
[#u8()](bytevectors.md#bytevectors)
## A
[abs](numerical-operations.md#abs)
[actor](async.md#actor)
[actor-linked](async.md#actor-linked)
[alist->ff](ffs.md#alist-ff)
[append](pairs-and-lists.md#append)
[assq](pairs-and-lists.md#assq)
[assv](pairs-and-lists.md#assv)
[assoc](pairs-and-lists.md#assoc)
[async](async.md#async)
[async-linked](async.md#async-linked)
[await](async.md#await)
[await-linked](async.md#await-linked)
## B
[bytevector](bytevectors.md#bytevectors)
[bytevector-append](bytevectors.md#bytevector-append)
[bytevector-copy](bytevectors.md#bytevector-copy)
[bytevector-copy!](bytevectors.md#bytevector-copy-1)
[bytevector-length](bytevectors.md#bytevector-length)
[bytevector-u8-ref](bytevectors.md#bytevector-u8-ref)
[bytevector-u8-set!](bytevectors.md#bytevector-u8-set)
[boolean?](booleans.md#boolean)
[boolean=?](booleans.md#boolean-1)
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
[define-instant-macro](macros.md#define-instant-macro)
[define-lazy-macro](macros.md#define-lazy-macro)
[define-macro](macros.md#define-macro)
[define-syntax](macros.md#define-syntax)
[del](ffs.md#del)
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
[ff->alist](ffs.md#alist-ff)
[ff->list](ffs.md#ff-list)
[ff->pairs](ffs.md#ff-pairs)
[ff-diff](ffs.ms#ff-diff)
[ff-fold](ffs.md#ff-fold)
[ff-foldr](ffs.md#ff-foldr)
[ff-map](ffs.md#ff-map)
[ff-for-each](ffs.md#ff-for-each)
[ff-replace](ffs.md#ff-replace)
[ff-union](ffs.md#ff-union)
[finite?](numerical-operations.md#finite)
[floor](numerical-operations.md#floor)
[fold](pairs-and-lists.md#fold)
[foldr](pairs-and-lists.md#foldr)
## G
[get](ffs.md#get)
[getf](ffs.md#getf)
## H
## I
[inexact?](numerical-operations.md#inexact)
[infinite?](numerical-operations.md#infinite)
[infix-notation](infix-notation.md)
[integer?](numerical-operations.md#integer)
[integer->char](characters.md#integer-char)
[isort](sorting.md#isort)
[iota](pairs-and-lists.md#iota)
## J
## K
[keys](ffs.md#keys)
## L
[length](pairs-and-lists.md#length)
[list](pairs-and-lists.md#list)
[list?](pairs-and-lists.md#list-1)
[list->ff](ffs.md#list-ff)
[list->vector](vectors.md#list-vector)
[list-copy](pairs-and-lists.md#list-copy)
[list-ref (lref)](pairs-and-lists.md#list-ref-lref)
[list-set!](pairs-and-lists.md#list-set)
[list-tail](pairs-and-lists.md#list-tail)
[lrange](pairs-and-lists.md#lrange)
## M
[mail](async.md#mail)
[make-ff](ffs.md#make-ff)
[make-list](pairs-and-lists.md#make-list)
[make-bytevector](bytevectors.md#make-bytevector)
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
[pairs->ff](ffs.md#pairs-ff)
[positive?](numerical-operations.md#positive)
[put](ffs.md#put)
[put!](ffs.md#put-1)
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
[string->utf8](bytevectors.md#string-utf8)
[string->vector](vectors.md#string-vector)
[symbol?](symbols.md#symbol)
[symbol=?](symbols.md#symbol-1)
[symbol->string](symbols.md#symbol-string)
## T
[take](pairs-and-lists.md#take)
[truncate](numerical-operations.md#truncate)
## U
[utf8->string](bytevectors.md#utf8-string)
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
