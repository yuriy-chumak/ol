#!/usr/bin/env ol

(import (file json))

(define PREFIX "real_")
(define GL2ES "gl2es_")

(print "#pragma once
#include <GLES3/gl3.h>

")

(define json (read-json stdin))
(ff-for-each (lambda (name arguments)
      (define return (ref arguments 1))
      (define args (cdr (vector->list arguments)))

      ;; external function
      (for-each display (list return " " "(* " PREFIX name ")"))
      (display "(")
      (unless (null? args)
         (let loop ((args args))
            (display (car args))
            (unless (null? (cdr args))
               (display ", ")
               (loop (cdr args)))))
      (print ");"))
   json)
