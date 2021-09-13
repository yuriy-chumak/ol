#!/usr/bin/env ol
(import (lib sqlite))
(import (otus ffi))

; create a function
(import (lib sha1)) ; sha1:digest and base64:encode
(import (otus ffi)) ; making callbacks

; create extension function:
; https://www.sqlite.org/c3ref/create_function.html
(define calculate (vm:pin (cons
   ; function signature
   ; result-type arg1-type arg2-type arg3-type(s)
   (list fft-int sqlite3_context* fft-int (list type-vptr))
   ; function
   (lambda (context argc argv)
      (print "Log Debug:")
      (define v (sqlite3_value_text (car argv)))
      (print "  function argument: " v)
      (define r (base64:encode (sha1:digest v)))
      (print "  calculated result: " r)
      ; return sha1 in base64
      (sqlite3_result_text context r -1 #false))
)))
; convert lisp function into ffi callback
(define sha1-callback (make-callback calculate))

; -- main code --
(define database (make-sqlite3))
(sqlite3_open ":memory:" database)
(sqlite3_create_function_v2 database "SHA1" 1 SQLITE_UTF8 #f sha1-callback #f #f #f)

; create sample table:
(sqlite:query database "CREATE TABLE test (id INTEGER PRIMARY KEY, hash TEXT)")
; fill table with data:
(for-each (lambda (text)
      (sqlite:query database "INSERT INTO test (hash)
                              VALUES (SHA1(?))" text))
   '( "First"
      "A Password"
      "Long-long-long-long-long-long-long-long-long-long-long word"
      ":)"))

; print table content
(sqlite:for-each (sqlite:query database "SELECT id,hash FROM test")
      (lambda (id hash)
         (print id ": " hash)))
; done.
(sqlite3_close database)
