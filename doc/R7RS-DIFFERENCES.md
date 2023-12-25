R<sup>7</sup>RS DIFFERENCES
---------------------------

Why doesn't Ol implement the full R<sup>7</sup>RS standard? Because Ol tries to be:
  - as simple as possible (i'm sure it's one of the simplest languages),
  - as small as possible (less than half Meg for a full language),
  - as fast as possible (avoids unnecessary code executions),
  - and as predictable as possible (exactly the same behavior for a wide range of different hardware, even without a floating point math processor).

And voila, we have a very simple, elegant and portable language with a nice syntax
that is a pleasure to program in.


This is almost a complete list of differences between Ol and R<sup>7</sup>RS.

* 2.1. Identifiers
  * `|\t\t|` and `|\x9;\x9;|` are **different** in Ol, but *the same* in Scheme.
  * Ol is definitely **case sensitive**, but Sheme is *configurable with #!fold-case and #!no-fold-case*.

* 4.1.5. Conditionals
  * Ol provides **extended `if`** with `then` and `else` keywords in forms:
    * `(if <condition> then <then1-clause>..<thenN-clause>)`,
    * `(if <condition> <then-clause> else <else1-clause>..<elseN-clause>)`,
    * `(if <condition> then <then1-clause>..<thenN-clause> else <else1-clause>..<elseN-clause>)`.

* 4.1.6. Assignments
  * **No** `set!` in Ol.
    - explanation: Ol is a purely functional language.
    - note: Use `define` instead.
    - note: Limited support of `set-car!`, `set-cdr!`, and `set-ref!` functions are provided.
    - note: Dynamic variables are available via `(scheme dynamic-bindings)` library. With some speed impact, sure.

* 4.1.7. Inclusion
  * **No** `include` and `include-ci` in Ol.
    - note: Use libraries (`import`, `define-library`, `export`, etc.) instead.
    - note: Top level ",load" REPL command is available.

* 4.2.1. Conditionals
  * Result of the `when` expression is value **returned by the last expression** in Ol, but *unspecified* in Scheme.
  * Result of the `unless` expression is value **returned by the last expression** in Ol, but *unspecified* in Scheme.
  * Extended form of `case` is available in Ol.

* 4.2.5. Delayed evaluation
  * **No** `delay-force`, `promise?` in Ol.
    - note: `delay` and `force` exists, sure.
    - note: `make-promise` exist too.

* 4.2.7. Exception handling
  * **No** `guard` in Ol.

* 4.3. Macros
  * **No** `let-syntax` and `letrec-syntax` in Ol.
    - note: Use global `define-syntax` instead.
  * Ol have **three more** macro declarations (`define-macro`, `define-instant-macro` and `define-lazy-macro`, which differs in the parameters evaluation stages) in addition to the hygienic `define-syntax`, while Scheme have *only hygienic one*.

* 5.5. Record-type definitions
  * **No** `define-record-type` in Ol.

* 5.6.1. Library Syntax
  * **No** `include-ci` due to fundamental ambiguity.
  * **Additional** library terms `prefix`, `version`, `license`, `keywords`, `description`.

* 6.1. Equivalence predicate `eqv?`
  * `(eqv? +nan.0 +nan.0)` is **#true** in Ol, but *unspecified* in Scheme. The same is for `+inf.0` and `-inf.0`.

* 6.2.5. Syntax of numerical constants
  * `Numbers without precision` considered to be **exact** in Ol, but *inexact* in Scheme.
    - explanation: Inexactness can be disabled by compiler features or/and unsupported by platform. Ol designed with respect for the same functionality of the program, independently of inexactness support (unless we directly use inexact numbers, sure).

* 6.2.6. Numerical operations
  * note: `complex?` is the same as `number?`, like in Scheme.
  * `integer?` for inexact numbers always returns **#false** in Ol, but can be *#true* in Scheme when `(= number (round number))`.
    - explanation: Inexactness is an inexactness, we may lose the fractional part and not to be noticed about. So let's be a little paranoid.
  * `sqrt` is **included** in base library profile while *not included* in Scheme
    - explanation: due to frequent use.

* 6.4. Pairs and lists
  * `memq` and `assq` behavior with 'short' numbers (aka 'enumerations') as first argument is fully **specified** in Ol, but *unspecified* in Scheme.
    - *note: those numbers processed by memq and assq as usual elements.*

* 6.6. Characters
  * CHARACTERS in Ol are **small numbers** (aka 'enums'), but are *characters* in Scheme.
    - explanation: This is for a historical reason.
      Ol supports two types of numbers - 'small' numbers (enums) and 'long' numbers (any other numbers). 'Small' numbers are used as 'runes' (or 'glyphs' in other word) inside strings for Unicode support. An additional 'character' type with requirement to use the char->integer and integer->char functions every time is too boring and slow. Thanks.
    - note: Ol supports full Unicode 15.0.0 (2022 Sep 13) character set.
    - note: If you want to print a character in the form of a letter (or a digit, etc.) use `write-char` function, i.e. instead of `(print #\λ)` do the `(write-char #\λ)`, otherwise you will print a number 955; or `(print (string #\λ))`, if you want.

* 6.7. Strings
  * NEGATIVE indices in `substring` are **valid** in Ol, but *invalid* in Scheme.
    - note: "-1" is the last rune of the string, "-2" is before the last element, etc.

* 6.8. Vectors
  * NEGATIVE indices of a vector are **valid** in Ol, but *invalid* in Scheme.
    - note: Negative vector indices can be used to access the n-th element from the end of the vector. This means that "-1" is the last element of the vector, "-2" is before the last element, "-n" is the n-th element from the end of the vector.

* 6.10. Control features
  * `apply` arguments count is **limited to 256** in Ol, but *unlimited* in Scheme.
    - note: Use `fold` instead in such cases, like `(apply + '(1 2 3))` -> `(fold + '(1 2 3))`.

* Ol has builtin **regular expressions** while Scheme *not*.
  * note: you can use `m/<pattern>/`, `s/<expression>/<new-expression>/` with optional 'g' suffix, and `c/<pattern>/` as functions to match, change and split the string.
