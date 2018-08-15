http://people.csail.mit.edu/jaffer/r4rs_10.html#SEC75

Notes

Language changes

This section enumerates the changes that have been made to Scheme since the "Revised^3 report" [R3RS] was published.

Although implementations may extend Scheme, they must offer a syntactic mode that adds no reserved words and preempts no lexical conventions of Scheme.
Implementations may report violations of implementation restrictions.

It is no longer specified whether the empty list counts as true or as false in conditional expressions. It should be noted that the IEEE standard for Scheme requires the empty list to count as true [IEEEScheme].

The sets defined by boolean?, pair?, symbol?, number?, char?, string?, vector?, and procedure? are required to be disjoint.

The variables bound by a lambda, let, letrec, and do must not contain duplicates.

Nested begin expressions containing definitions are treated as a sequence of definitions.

The eqv? procedure is no longer required to be true of any two empty strings or two empty vectors.

The syntax of numerical constants has been changed, and the exactness implied by each syntax has been specified.

The semantics of many numerical procedures have been clarified.

Rationalize has been restricted to two arguments and its specification clarified.

The number->string and string->number procedures have been changed.

Integer->char now requires an exact integer argument.

The specification of the force procedure has been weakened. The previous specification was unimplementable.

Variables removed: t, nil.

Procedures removed: approximate, last-pair.

Procedures added: list?, peek-char.

Syntaxes made essential: case, and, or, quasiquote.

Procedures made essential:

reverse        char-ci=?        make-string
max            char-ci<?        string-set!
min            char-ci>?        string-ci=?
modulo         char-ci<=?       string-ci<?
gcd            char-ci>=?       string-ci>?
lcm            char-alphabetic? string-ci<=?
floor          char-numeric?    string-ci>=?
ceiling        char-whitespace? string-append
truncate       char-lower-case? open-input-file
round          char-upper-case? open-output-file
number->string char-upcase      close-input-port
string->number char-downcase    close-output-port

Procedures required to accept more general numbers of arguments: append, +, *, - (one argument), / (one argument), =, <, >, <=, >=, map, for-each.

A macro facility has been added as an appendix to this report.
