; otus lisp language
(define-library (otus lisp)

   (export
      (exports (r5rs full))

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
      (exports (owl equal))
      (exports (owl render))
      (exports (owl interop))
      (exports (owl fasl))
      (exports (owl time))
      (exports (owl regex))
      (exports (owl math-extra))
      (exports (owl math))
      (exports (owl tuple))

      ;defined? ; todo: move to right library
      wait)    ; todo: move to right library

   (import
      (r5rs core)
      (r5rs full)

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
      (owl equal)
      (owl regex)
      (owl render)
      (owl interop)
      (owl math)
))