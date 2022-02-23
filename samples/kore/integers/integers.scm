#!/usr/bin/env ol

(import (otus fasl)
   (otus ffi))
(import (lib kore))

(define (page req)
   (http_populate_get req)

   (let*((out "analyze result:\n")

         (byte (box 0))
         (out (if (eq? (http_argument_get_byte req "id" byte) 1)
               (string-append out "byte: " (number->string (unbox byte)) "\n")
               out))
         ;; (- (print byte))

         (int16 (box 0))
         (out (if (eq? (http_argument_get_int16 req "id" int16) 1)
               (string-append out "int16: " (number->string (unbox int16)) "\n")
               out))
         ;; (- (print int16))

         (int32 (box 0))
         (out (if (eq? (http_argument_get_int32 req "id" int32) 1)
               (string-append out "int32: " (number->string (unbox int32)) "\n")
               out))
         ;; (- (print int32))

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
