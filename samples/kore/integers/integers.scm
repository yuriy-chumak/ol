#!/usr/bin/env ol

(import (otus fasl)
   (otus ffi))
(import (lib kore))

(define (page req)
   (http_populate_get req)

   (let*((out "analyze result:\n")

         ;; byte
         (byte (box 0))
         (out (if (eq? (http_argument_get_byte req "id" byte) 1)
               (string-append out "byte: " (number->string (unbox byte)) "\n")
               out))

         ;; int16/uint16
         (int (box 0))
         (out (if (eq? (http_argument_get_int16 req "id" int) 1)
               (string-append out "int16: " (number->string (unbox int)) "\n")
               out))
         (out (if (eq? (http_argument_get_uint16 req "id" int) 1)
               (string-append out "uint16: " (number->string (unbox int)) "\n")
               out))

         ;; int32/uint32
         (int (box 0))
         (out (if (eq? (http_argument_get_int32 req "id" int) 1)
               (string-append out "int32: " (number->string (unbox int)) "\n")
               out))
         (out (if (eq? (http_argument_get_uint32 req "id" int) 1)
               (string-append out "uint32: " (number->string (unbox int)) "\n")
               out))

         ;; int64/uint64
         (int (box 0)) ; we should allocate large number
         (out (if (eq? (http_argument_get_int64 req "id" int) 1)
               (string-append out "int64: " (number->string (unbox int)) "\n")
               out))
         (out (if (eq? (http_argument_get_uint64 req "id" int) 1)
               (string-append out "uint64: " (number->string (unbox int)) "\n")
               out))

         ;; float/double
         (int (box (inexact 0))) ; we should allocate large number
         (out (if (eq? (http_argument_get_float req "id" int) 1)
               (string-append out "float: " (number->string (unbox int)) "\n")
               out))
         (out (if (eq? (http_argument_get_double req "id" int) 1)
               (string-append out "double: " (number->string (unbox int)) "\n")
               out))

         ;; end with newline
         (out (string-append out "\n")))
      
      (print "out: " out)
      ; 200 OK
      (define response (string->utf8 out))
      (http_response req 200 response (size response)))

   KORE_RESULT_OK)

(fasl-save (vm:new 63 (lambda (args)
      kore-constructor!
      (vm:pin page)))
   "tmp.bin")
