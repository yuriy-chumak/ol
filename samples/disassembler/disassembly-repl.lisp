#!/usr/bin/env ol

(import (lang eval))

(define (function->name env function)
   (call/cc (lambda (return)
      (let loop ((kvs (ff-iter env)))
         (cond
            ((null? kvs) (return "lambda"))
            ((pair? kvs)
               (let ((k (caar kvs))
                     (v (cdar kvs)))
                  (let ((v (ref v 2)))
                     (if (function? v)
                        (if (eq? v function)
                           (return (symbol->string k)))
                        (let ((v (ref v 2)))
                           (if (function? v)
                              (if (eq? v function)
                                 (return (symbol->string k))))))))
               (loop (cdr kvs)))
            (else (loop (kvs))))))))

(define (decode-value env l)
   (cond
      ((function? l)
         (string-append "#<" (function->name env l) ">"))
      ((list? l)
         (map (lambda (r) (if (not (function? r)) r (decode-value env r))) l))
      ((vector? l)
         (vector-map (lambda (r) (if (not (function? r)) r (decode-value env r))) l))
      (else l)))



; -- cache --
(coroutine 'cache (lambda ()
   (let loop ((cache {}) (id 1))
      (let*((envelope (wait-mail))
            (sender msg envelope))
         (define got (cache msg #f))
         (mail sender (cons got id))
         (if got
            (loop cache id)
         else
            (loop (put cache msg id) (+ id 1)))))))


(define spaces "                                                                                                                                                                                                                                                                                                            ")

(define (step n)
   (syscall 1 stdout spaces (+ n n)))

(define (dis value level)
   (if (function? value)
   then
      (define dis (disassembly value))
      (if dis
      then
         (step level) (print "name: " (decode-value *toplevel* value))
         (step level) (print "type: " (dis 'type))
         (step level) (print "code: " (decode-value *toplevel* (dis 'code)))
         (step level) (print "disassembly '(length command . args):")
         (for-each (lambda (line)
               (step level)
               (print line))
            (dis 'disassembly))
      else
         (step level) (print "can't disassembly " value))
   else
      (step level) (print value " is not function")))

(define (dump func level)
   (step level) (print level ":")
   (define seen (await (mail 'cache func)))
   (if (car seen)
   then
      (step level) (print "seen as --" (cdr seen) "--")
      (step level) (print "name: " (decode-value *toplevel* func))
   else
      (step level) (print "NEW ONE --" (cdr seen) "--")
      (case (type func)
         (63 ; constructor
            (step level) (print "CONSTRUCTOR")
            (define constructor (ref func 1))
            (dump constructor level)
            (print "///////////////////////////")
            (define attic (ref constructor 3))
            (if (pair? attic)
               (for-each (lambda (pair)
                     (print "dumping pair " pair)
                     (print "dumping car:")
                     (dump (car pair) (+ level 1))
                     (print "dumping cdr:")
                     (dump (cdr pair) (+ level 1)))
                  attic)))
         (type-bytecode
            (dis func level))
         (type-procedure
            (dis func level)
            (define procedure (vm:cast func type-vector))
            (define args (cdr (vector->list procedure)))
            (for-each (lambda (x n)
                  (step level) (print ">clo " n)
                  (dump x (+ level 1)))
               args (iota (length args) 2)))
         (type-closure
            (dis func level)
            (step level) (print "sub-procedure dump:")
            (dump (ref func 1) (+ level 1))

            (define closure (vm:cast func type-vector))
            (define args (cdr (vector->list closure)))
            (for-each (lambda (x n)
                  (step level) (print ">clo " n)
                  (dump x (+ level 1)))
               args (iota (length args) 2)))
         (else
            (step level) 
            (case (type func)
               (type-string
                  (print "string: " func))
               (type-symbol
                  (print "symbol: " func))
               (else
                  (print "not a function: " func)))))))

(define (dump-lib lib)
   (for-each (lambda (sym)
         (print "---------------------------------------------")
         (print (car sym))
            (if (vector? (cdr sym))
            then
               (print (cdr sym))
               (case (cdr sym)
                  (['macro func]
                     (dump func 0))
                  (['defined val]
                     (print val)
                     (case val
                        (['value func]
                           (dump func 0))
                        (else
                           (print "--hmmm--"))))
                  (else
                     (print "--hmmm--")))))
      (ff->alist lib)))

'(for-each (lambda (lib)
      (print "==========================================================================================")
      (print (car lib))
      (print)
      (dump-lib (cdr lib)))
   *libraries*)

(define repl (or
   (fasl-load "repl" #f)
   (fasl-load "../../repl" #f)))
(if repl
   (dump repl 0))
(print "ok.")
