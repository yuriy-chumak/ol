; otus lisp language
(define-library (otus lisp)

   (export
      (exports (r5rs srfi-1))  ; * List Library
      (exports (r5rs srfi-87)) ; * => in case clauses ; builtin (scheme core)
;      (exports (scheme r5rs iteration)) ; 4.2.4 Iteration (do)
      (exports (r5rs characters))
      (exports (scheme base))

      (exports (owl list))
      (exports (owl rlist))
      (exports (owl list-extra))
      (exports (owl ff))
      (exports (owl io))
      (exports (owl lazy))
      (exports (owl string))
      (exports (owl symbol))
      (exports (owl sort))
      (exports (owl vector))
      (exports (owl render))
      (exports (owl interop))
      (exports (owl fasl))
      (exports (owl time))
      (exports (owl regex))
      (exports (owl math-extra))
      (exports (owl math))
      (exports (owl tuple))

      read)

      ;defined? ; todo: move to right library

   (import
      (scheme core)
      (r5rs srfi-1)
      (r5rs srfi-87)
      (r5rs characters)
      (scheme base)

      (owl list)
      (owl rlist)
      (owl list-extra)
      (owl tuple)
      (owl ff)
      (owl io)
      (owl time)
      (owl lazy)
      (owl math-extra)
      (owl string)
      (owl symbol)
      (owl sort)
      (owl fasl)
      (owl vector)
      (owl regex)
      (owl render)
      (owl interop)
      (owl math)

      (only (lang sexp) read)
))