#!/usr/bin/env ol
(import (lib sqlite))
(import (otus ffi))

; вспомогательный макрос для собрать в кучку все bor
(define (OR . args) (fold bor 0 args))
(define sqlite3_context* type-vptr)

(define database (make-sqlite3))
(sqlite3_open ":memory:" database)

; create extension function
(define calculate (vm:pin (cons
   (list fft-int sqlite3_context* fft-int (list type-vptr))
   (lambda (context argc argv)
      (print "argc: " argc)
      (print "argv: " argv)

      (let ((v (sqlite3_value_int (car argv))))
         (print "source value: " v)
         (let ((r (* v 777)))
            (print "mul by 777: " r)

            (sqlite3_result_int context r)))))))
(define calculate-callback (make-callback calculate))

(sqlite3_create_function_v2 database "compress" 1 SQLITE_UTF8 #f calculate-callback #f #f #f)


; sample table
(sqlite:query database "CREATE TABLE test (id INTEGER)")
(sqlite:query database "INSERT INTO test VALUES (3)")

(print "for simple select: "
   (sqlite:value database "SELECT id FROM test"))

(print "for extension select: "
   (sqlite:value database "SELECT compress(id) FROM test"))

(sqlite3_close database)
