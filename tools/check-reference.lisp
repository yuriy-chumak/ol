#!/usr/bin/env ol

(import (owl parse))
(import (lang sexp))
(import (scheme repl))
(import (lang intern))

(define RED "\x1B;[22;31m")
(define GREEN "\x1B;[22;32m")
(define YELLOW "\x1B;[22;33m")
(define END "\x1B;[0m")

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
   (append code '(#\newline)))))

(define |> code\n answer|
   (let-parse* (
         (code (greedy+ (let-parse* (
                  (skip (greedy* whitespace-or-comment))
                  (prefix (word "> " #t)) ; строка запроса
                  (code sexp-parser)
                  (skip (imm #\newline))) ; trailing newlines
            code)))
         (answer (either
            (let-parse* (
                  (skip (greedy* (imm #\space))) ; leading spaces
                  (skip (imm #\newline)))
               #null)
            (let-parse* (
                  (skip (greedy* (imm #\space))) ; leading spaces
                  (text (lazy* rune))
                  (skip (word "\n\n" #t)))  ; обязательный маркер конца примера
               text))))
      (cons code answer)))

(define |code ==> answer|
   (let-parse* (
         (skip (greedy* whitespace-or-comment))
         (code sexp-parser)
         (arrow sexp-parser)  (verify (or (equal? arrow '==>)
                                          (equal? arrow '<=>)) 'invalid-arrow-symbol)
         (answer sexp-parser)
         (skip (greedy* whitespace-or-comment)))
      (cons code answer)))

; evaluator
(import (lang eval))
(import (lang macro))
(define (supereval exp env)
   (define repl__ (lambda (env in)
                     (repl env in evaluate)))
   (let loop ((exps exp) (env env)  (out ""))
      (if (null? exps)
         ['ok out env]
      else
         (let ((exp (car exps)))
            (case (eval-repl exp env repl__ evaluate)
               (['ok exp env]
                  (loop (cdr exps) env exp))
               (['fail reason]
                  (runtime-error RED (list "ERROR: can't evaluate" exp))))))))

(import
   (scheme char)
   (scheme cxr))
(import (scheme inexact))

(define ok '(#true))
(define (error code answer)
   (print "test error:")
   (print "  " YELLOW code RED " IS NOT EQUAL TO " GREEN answer END)
   (set-car! ok #false))

(for-each (lambda (filename)
      (print "  testing " filename "...")
      (for-each (lambda (code-block)
            (let loop ((code-block code-block) (env (interaction-environment)))
               (cond
                  ((try-parse |> code\n answer| code-block #false) => (lambda (expressions)
                     (let*((code (caar expressions))
                           (answer (cdar expressions))
                           (answer (s/[ \n]+/ /g (list->string answer)))
                           (env (vector-apply (supereval code env) (lambda (ok? test env)
                              (define buffer (open-output-string))
                              (write test buffer)
                              (define actual (get-output-string buffer))
                              (if (and (not (null? (cdar expressions)))
                                       (not (string=? answer actual)))
                                 (error code answer))
                              env))))
                        (loop (cdr expressions) env))))
                  ((try-parse |code ==> answer| code-block #false) => (lambda (expressions)
                     (let*((code (caar expressions))
                           (answer (cdar expressions)))
                        ;; (print "code/answer: " code " / " answer)
                        (define a (eval code env))
                        (define b (eval answer env))
                        (unless (equal? a b)
                           (error code answer))
                        (loop (cdr expressions) env))))
                  (else
                     (unless (or (null? code-block)
                                 (equal? code-block '(10))
                                 (equal? code-block '(10 10)))
                        (print code-block)
                        (print "incorrect samples block:\n```scheme\n" RED (bytes->string code-block) END "```"))))))
         (car (or (try-parse parser (force (file->bytestream filename)) #f) '(())))))
   *vm-args*)
(exit (car ok))