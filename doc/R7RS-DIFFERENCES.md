R<sup>7</sup>RS DIFFERENCES
---------------------------

Why Ol does not implement the full R<sup>7</sup>RS standard?
Because Ol aims to be:
  - as simple as possible (I'm sure it's one of the simplest languages),
  - as compact as possible (half a megabyte including the compiler),
  - as predictable as possible (same behavior on a wide range of hardware, even without a floating-point coprocessor).

As a result, we have a very simple, elegant, and portable language with a pleasant syntax that is enjoyable to program in.

Below is a list of differences between the Ol implementation and the [R<sup>7</sup>RS](https://small.r7rs.org/attachment/r7rs.pdf) standard.

Major differences:

* 2.1. Identifiers
  * Ol is always **case-sensitive**, while in Scheme, the case *can be configured using* `#!fold-case` *and* `#!no-fold-case`.
* 4.1.6. Assignments
  * Ol **does not have** `set!`.
* 4.3. Macros
  * Ol has **three additional** MACRO declarations (`define-macro`, `define-instant-macro`, and `define-lazy-macro`, which differ in parameter evaluation stages) in addition to the hygienic `define-syntax`, while Scheme has *only the hygienic one*.
* 6.2.5. Syntax of numeric constants
  * Numbers WITH A FRACTIONAL PART are considered **exact** in Ol, but *inexact* in Scheme.
* 6.8. Vectors and 6.7. Strings
  * NEGATIVE INDICES for vectors (and strings) are **allowed** in Ol (and denote indexing from the end), but are *not allowed* in Scheme.
* Other important language features missing in Scheme
  * Ol provides INFIX NOTATION for mathematical expressions (through the `\\` macro), while Scheme contains only *prefix notation*.
  * Ol provides built-in REGULAR EXPRESSIONS, which are *not* in Scheme.
  * Ol provides a built-in ASSOCIATIVE DICTIONARY (known as `ff`), which is *not* in Scheme.

Complete list:

* 2.1. Identifiers
  * Ol is always **case-sensitive**, while in Scheme, the case *can be configured using* `#!fold-case` *and* `#!no-fold-case`.
  * `|\t\t|` and `|\x9;\x9;|` are **different** in Ol, but *the same* in Scheme.
    - note: in such cases, just use real tabs, newlines, and everything else except `|`.

* 4.1.5. Conditional expressions
  * Ol provides an **extended `if`** with the keywords `then` and `else` in the following forms:
    - `(if <condition> then <then1-clause>..<thenN-clause>)`,
    - `(if <condition> <then-clause> else <else1-clause>..<elseN-clause>)`,
    - `(if <condition> then <then1-clause>..<thenN-clause> else <else1-clause>..<elseN-clause>)`.

* 4.1.6. Assignments
  * Ol **does not have** `set!`.
    - explanation: Ol is a purely functional language.
    - note: use `define` instead of the first `set!`.
    - note: but if you need it, dynamic variables are available through the `(scheme dynamic-bindings)` library, although using them may slow down your program.
    - note: a (somewhat limited) implementation of `set-car!`, `set-cdr!`, and `set-ref!` is provided. Please avoid using them.

* 4.1.7. Inclusion
  * Ol **does not have** `include` and `include-ci`.
    - note: use libraries (`import`) instead.
    - note: the `,load` command is available in the REPL (synonyms are `,l` and `,include`).

* 4.2.1. Conditional expressions
  * In Ol, the result of the `when` expression is the value **returned by the last expression**, but is *undefined* in Scheme.
  * In Ol, the result of the `unless` expression is the value **returned by the last expression**, but is *undefined* in Scheme.
  * An extended form of `case` is available in Ol.

* 4.2.5. Delayed execution
  * ~~Ol **does not have** `delay-force`, `promise?`.~~
    - note: it already exists, added in Ol 2.5.

* 4.2.7. Exception handling
  * Ol **does not have** `guard`.
    - note: but `with-exception-handler` and `raise` work as expected.

* 4.3. Macros
  * Ol **does not have** `let-syntax` and `letrec-syntax`.
    - note: use the global `define-syntax` instead.
  * Ol has **three additional** MACRO declarations (`define-macro`, `define-instant-macro`, and `define-lazy-macro`, which differ in parameter evaluation stages) in addition to the hygienic `define-syntax`, while Scheme has *only the hygienic one*.

* 5.5. Definition of record types
  * Ol **does not have** `define-record-type`.

* 5.6.1. Syntax of libraries
  * Ol **does not have** `include-ci` due to fundamental ambiguity.
    - note: `include` is perfectly fine.
  * **Additional** library terms: `prefix`, `version`, `license`, `keywords`, `description`.

* 6.1. The equivalence predicate `eqv?`
  * `(eqv? +nan.0 +nan.0)` is **#true** in Ol, but *undefined* in Scheme. The same applies to `+inf.0` and `-inf.0`.

* 6.2.5. Syntax of numeric constants
  * Numbers WITH A FRACTIONAL PART are considered **exact** in Ol, but *inexact* in Scheme.
    - explanation: inexactness (working with floating-point numbers) can be disabled by compiler options or not supported by the target platform. Ol is designed to have the same behavior of the program regardless of the runtime environment. Of course, unless you are using inexact numbers directly.

* 6.2.6. Numeric operations
  - note: `complex?` is the same as `number?`, as in Scheme.
  - `integer?` for inexact numbers always returns **#false** in Ol, but may be *#true* in Scheme if `(= number (round number))`.
    - explanation: inexactness is inexactness, we may lose the fractional part and not notice it; therefore, we will be a little paranoid.
  - `sqrt` is **included** in the standard library, while it is *not included* in Scheme.
    - explanation: because of its frequent use.

* 6.4. Pairs and lists
  * The behavior of `memq` and `assq` with small numbers (passed by value) as the first argument is fully **defined** in Ol, but is *undefined* in Scheme.
    - note: these numbers are treated as regular elements by `memq` and `assq`.

* 6.6. Characters (text literals)
  * A CHARACTER in Ol is a **small number** (passed by value), but in Scheme, they have a separate type *char*.
    - explanation: that's how it happened historically.
      In Ol, there are two types of numbers: small numbers (that fit in the virtual machine register and are *values*) and large numbers (any other, they are *references*). Small numbers in strings are used as *runes* (or *glyphs* if you want) to represent both ANSI and Unicode literals.
      An additional type for literals with the requirement to use `char->integer` and `integer->char` functions each time would be too cumbersome.

* Ol **has** built-in REGULAR EXPRESSIONS, which are *not* in Scheme.
  - note: you can use `m/<pattern>/` for matching, `g/<pattern>/` for capturing, `s/<expression>/<new-expression>/` with an optional 'g' suffix for replacement, and `c/<pattern>/` for splitting strings and lists.
