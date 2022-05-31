Otus Lisp
=========

Otus Lisp (Ol in short) is a purely functional dialect of Lisp.

It implements an extended subset of the R<sup>7</sup>RS Scheme
([PDF](https://small.r7rs.org/attachment/r7rs.pdf)), including
but not limited to some SRFIs. It is tiny (~ 64KB), embeddable
and cross-platform.  Provides a portable, high-level interface
to call code written in another language.

This is Ol reference, not a Scheme. Scheme R7RS differences decribed in the
main [README](https://github.com/yuriy-chumak/ol#r7rs-differences) file.

```scheme
; this is example of provided examples
(car *version*) ==> "OL"
```

List of Standard procedures
---------------------------

- [Pairs and Lists](pairs-and-lists.md).
- [Equivalence predicates](equivalence-predicates.md).
- [Booleans](booleans.md).
- [Symbols](symbols.md).
- [Numbers](numbers.md), TBD.
- [Characters](characters.md), TBD.
- [Strings](strings.md), TBD.
- [Vectors](vectors.md), TBD.
- [Bytevectors](bytevectors.md), TBD.
- [Control features](control-features.md), TBD.
- [Exceptions](exceptions.md), TBD.
- [Environments and evaluation](environments-and-evaluation.md), TBD.
- [Input and output](input-and-output.md), TBD.
- [System interface](system-interface.md), TBD.

- [Dictionaries](dictionaries.md), TBD.
- [Lazy evaluations](lazy-evaluations.md), TBD.

Alphabetic Index of Definitions of Concepts, Keywords, and Procedures
=====================================================================
|         |         |         |         |         |         |         |
|:-------:|:-------:|:-------:|:-------:|:-------:|:-------:|:-------:|
| [!](#!) | [#](##) | [A](#a) | [B](#b) | [C](#c) | [D](#d) | [E](#e) |
| [F](#f) | [G](#g) | [H](#h) | [I](#i) | [J](#j) | [K](#k) | [L](#l) |
| [M](#m) | [N](#n) | [O](#o) | [P](#p) | [Q](#q) | [R](#r) | [S](#s) |
| [T](#t) | [U](#u) | [V](#v) | [W](#w) | [X](#x) | [Y](#y) | [Z](#z) |

## !
[#null](pairs-and-lists.md#null)
## A
[append](pairs-and-lists.md#append)
[assq](pairs-and-lists.md#assq)
[assv](pairs-and-lists.md#assv)
[assoc](pairs-and-lists.md#assoc)
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
[cons](pairs-and-lists.md#cons)
[cons*](pairs-and-lists.md#cons-1)
## D
[drop](pairs-and-lists.md#drop)
## E
[eq?](equivalence-predicates.md#eq)
[eqv?](equivalence-predicates.md#eqv)
[equal?](equivalence-predicates.md#equal)
## F
## G
## H
## I
[iota](pairs-and-lists.md#iota)
## J
## K
## L
[length](pairs-and-lists.md#length)
[list](pairs-and-lists.md#list)
[list?](pairs-and-lists.md#list-1)
[list-copy](pairs-and-lists.md#list-copy)
[list-ref (lref)](pairs-and-lists.md#list-ref-lref)
[list-set!](pairs-and-lists.md#list-set)
[list-tail](pairs-and-lists.md#list-tail)
[lrange](pairs-and-lists.md#lrange)
## M
[make-list](pairs-and-lists.md#make-list)
[memq](pairs-and-lists.md#memq)
[memv](pairs-and-lists.md#memv)
[member](pairs-and-lists.md#member)
## N
[not](pairs-and-lists.md#not)
[null?](pairs-and-lists.md#null-1)
## O
## P
[pair?](pairs-and-lists.md#pair)
## Q
## R
[repeat](pairs-and-lists.md#repeat)
[reverse](pairs-and-lists.md#reverse)
## S
[set-car!](pairs-and-lists.md#set-car)
[set-cdr!](pairs-and-lists.md#set-cdr)
[symbol?](symbols.md#symbol)
[symbol=?](symbols.md#symbol-1)
[symbol->string](symbols.md#symbol-string)
[string->symbol](symbols.md#string-symbol)
[string->uninterned-symbol](symbols.md#string-uninterned-symbol)
## T
[take](pairs-and-lists.md#take)
## U
## V
## W
## X
## Y
## Z
