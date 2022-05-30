#!/usr/bin/env ol

(import (owl parse))
(import (lang sexp))

; collects all ```scheme ... ``` code blocks
(define parser
   (greedy+ (let-parse* (
         (text (let-parse* (
                     (text (lazy* byte))
                     (skip (word "```scheme" #f)))
                  text))
         (code (let-parse* (
                     (code (lazy* byte))
                     (skip (word "```" #f)))
                  code)))
      code)))

(import (lang eval))
(import
   (scheme cxr))

(define ok '(#true))
(for-each (lambda (filename)
      (for-each (lambda (test)
            (let loop ((test test))
               (define expressions (try-parse (greedy+ sexp-parser) test #false))
               (when expressions
                  (define sexps (car expressions))
                  (let subloop ((sexps sexps))
                     (let*((query tail sexps)
                           (arrow tail tail)
                           (answer tail tail))
                        (define a (eval query *toplevel*))
                        (define b (eval answer *toplevel*))
                        (unless (equal? a b)
                           (print "test error:")
                           (print "  " query " IS NOT EQUAL TO " answer)
                           (set-car! ok #false))
                        (unless (null? tail)
                           (subloop tail))))
                  (loop (cdr expressions)))))
         (car (try-parse parser (force (file->bytestream filename)) #f))))
   '(
      "pairs-and-lists.md"
      ;"equivalence-predicates.md"
      ;"numbers.md"
      "booleans.md"
      ;"symbols.md"
      ;"characters.md"
      ;"strings.md"
      ;"vectors.md"
      ;"bytevectors.md"
      ;"control-features.md"
      ;"exceptions.md"
      ;"environments-and-evaluation.md"
      ;"input-and-output.md"
      ;"system-interface.md"
      ;"dictionaries.md"
      ;"lazy-evaluations.md"
   ))
(exit (car ok))