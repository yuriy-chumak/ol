http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-19.html#node_chap_E

Appendix E

Language changes
This chapter describes most of the changes that have been made to Scheme since the “Revised5 Report” [14] was published:

Scheme source code now uses the Unicode character set. Specifically, the character set that can be used for identifiers has been greatly expanded.

Identifiers can now start with the characters ->.

Identifiers and symbol literals are now case-sensitive.

Identifiers and representations of characters, booleans, number objects, and . must be explicitly delimited.

# is now a delimiter.

Bytevector literal syntax has been added.

Matched square brackets can be used synonymously with parentheses.

The read-syntax abbreviations #' (for syntax), #` (for quasisyntax), #, (for unsyntax), and #,@ (for unsyntax-splicing have been added; see section 4.3.5.)

# can no longer be used in place of digits in number representations.

The external representation of number objects can now include a mantissa width.

Literals for NaNs and infinities were added.

String and character literals can now use a variety of escape sequences.

Block and datum comments have been added.

The #!r6rs comment for marking report-compliant lexical syntax has been added.

Characters are now specified to correspond to Unicode scalar values.

Many of the procedures and syntactic forms of the language are now part of the (rnrs base (6)) library. Some procedures and syntactic forms have been moved to other libraries; see figure 15.

identifier  moved to
assoc (rnrs lists (6))
assv  (rnrs lists (6))
assq  (rnrs lists (6))
call-with-input-file    (rnrs io simple (6))
call-with-output-file   (rnrs io simple (6))
char-upcase (rnrs unicode (6))
char-downcase     (rnrs unicode (6))
char-ci=?   (rnrs unicode (6))
char-ci<?   (rnrs unicode (6))
char-ci>?   (rnrs unicode (6))
char-ci<=?  (rnrs unicode (6))
char-ci>=?  (rnrs unicode (6))
char-alphabetic?  (rnrs unicode (6))
char-numeric?     (rnrs unicode (6))
char-whitespace?  (rnrs unicode (6))
char-upper-case?  (rnrs unicode (6))
char-lower-case?  (rnrs unicode (6))
close-input-port  (rnrs io simple (6))
close-output-port (rnrs io simple (6))
current-input-port      (rnrs io simple (6))
current-output-port     (rnrs io simple (6))
display     (rnrs io simple (6))
do    (rnrs control (6))
eof-object? (rnrs io simple (6))
eval  (rnrs eval (6))
delay (rnrs r5rs (6))
exact->inexact    (rnrs r5rs (6))
force (rnrs r5rs (6))
inexact->exact    (rnrs r5rs (6))
member      (rnrs lists (6))
memv  (rnrs lists (6))
memq  (rnrs lists (6))
modulo      (rnrs r5rs (6))
newline     (rnrs io simple (6))
null-environment  (rnrs r5rs (6))
open-input-file   (rnrs io simple (6))
open-output-file  (rnrs io simple (6))
peek-char   (rnrs io simple (6))
quotient    (rnrs r5rs (6))
read  (rnrs io simple (6))
read-char   (rnrs io simple (6))
remainder   (rnrs r5rs (6))
scheme-report-environment     (rnrs r5rs (6))
set-car!    (rnrs mutable-pairs (6))
set-cdr!    (rnrs mutable-pairs (6))
string-ci=? (rnrs unicode (6))
string-ci<? (rnrs unicode (6))
string-ci>? (rnrs unicode (6))
string-ci<=?      (rnrs unicode (6))
string-ci>=?      (rnrs unicode (6))
string-set! (rnrs mutable-strings (6))
string-fill!      (rnrs mutable-strings (6))
with-input-from-file    (rnrs io simple (6))
with-output-to-file     (rnrs io simple (6))
write (rnrs io simple (6))
write-char  (rnrs io simple (6))
Figure 15:  Identifiers moved to libraries
The base language has the following new procedures and syntactic forms: letrec*, let-values, let*-values, real-valued?, rational-valued?, integer-valued?, exact, inexact, finite?, infinite?, nan?, div, mod, div-and-mod, div0, mod0, div0-and-mod0, exact-integer-sqrt, boolean=?, symbol=?, string-for-each, vector-map, vector-for-each, error, assertion-violation, assert, call/cc, identifier-syntax.

The following procedures have been removed: char-ready?, transcript-on, transcript-off, load.

The case-insensitive string comparisons (string-ci=?, string-ci<?, string-ci>?, string-ci<=?, string-ci>=?) operate on the case-folded versions of the strings rather than as the simple lexicographic ordering induced by the corresponding character comparison procedures.

Libraries have been added to the language.

A number of standard libraries are described in a separate report [24].

Many situations that “were an error” now have defined or constrained behavior. In particular, many are now specified in terms of the exception system.

The full numerical tower is now required.

The semantics for the transcendental functions has been specified more fully.

The semantics of expt for zero bases has been refined.

In syntax-rules forms, a _ may be used in place of the keyword.

The let-syntax and letrec-syntax no longer introduce a new environment for their bodies.

For implementations that support NaNs or infinities, many arithmetic operations have been specified on these values consistently with IEEE 754.

For implementations that support a distinct -0.0, the semantics of many arithmetic operations with regard to -0.0 has been specified consistently with IEEE 754.

Scheme's real number objects now have an exact zero as their imaginary part.

The specification of quasiquote has been extended. Nested quasiquotations work correctly now, and unquote and unquote-splicing have been extended to several operands.

Procedures now may or may not refer to locations. Consequently, eqv? is now unspecified in a few cases where it was specified before.

The mutability of the values of quasiquote structures has been specified to some degree.

The dynamic environment of the before and after procedures of dynamic-wind is now specified.

Various expressions that have only side effects are now allowed to return an arbitrary number of values.

The order and semantics for macro expansion has been more fully specified.

Internal definitions are now defined in terms of letrec*.

The old notion of program structure and Scheme's top-level environment has been replaced by top-level programs and libraries.

The denotational semantics has been replaced by an operational semantics based on an earlier semantics for the language of the “Revised5 Report” [14, 18].
