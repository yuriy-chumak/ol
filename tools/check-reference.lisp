#!/usr/bin/env ol

(import (owl parse))
(import (lang sexp))
(import (scheme repl))

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
(define red "\x1B;[22;31m")
(define green "\x1B;[22;32m")
(define end "\x1B;[0m")

(import (lang eval))
(import
   (scheme char)
   (scheme cxr))
(import (scheme inexact))

(define ok '(#true))
(for-each (lambda (filename)
      (print "  testing " filename "...")
      (for-each (lambda (test)
            (let loop ((test test))
               (define expressions (try-parse (greedy+ sexp-parser) test #false))
               (when expressions
                  (define sexps (car expressions))
                  (let subloop ((sexps sexps))
                     (let*((query tail sexps)
                           (arrow tail tail)
                           (answer tail tail))
                        (define a (eval query (interaction-environment)))
                        (define b (eval answer (interaction-environment)))
                        (unless (equal? a b)
                           (print "test error:")
                           (print "  " red query " IS NOT EQUAL TO " answer end)
                           (set-car! ok #false))
                        (unless (null? tail)
                           (subloop tail))))
                  (loop (cdr expressions)))))
         (car (try-parse parser (force (file->bytestream filename)) #f))))
   *vm-args*)
(exit (car ok))